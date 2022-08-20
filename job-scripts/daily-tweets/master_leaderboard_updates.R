# Post daily tweets with leaderboard updates

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggimage))
suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(rtweet))

## authenticate via web browser
token <- readRDS('/home/balco/.rtweet_token_bot.rds')

# region being updated
update_region <- case_when(
  lubridate::hour(Sys.time()) < 8 ~ 'europe',
  lubridate::hour(Sys.time()) < 16 ~ 'americas',
  TRUE ~ 'asia',
)

# nice region name
nice_region <- switch(
  update_region,
  'europe' = 'Europe',
  'americas' = 'Americas',
  'asia' = 'Asia-Pacific'
)

# champions names / codes / images from set JSONs
data_champs <- lorr::get_cards_data(
  select = c('name', 'cardCode', 'rarity')
) %>% 
  filter(rarity == "Champion", nchar(cardCode) <= 8) %>% 
  select(-rarity) %>% 
  mutate(croppedPath = paste0(
    'https://raw.githubusercontent.com/shaobaili3/LoR_Master/master/UI/src/assets/images/cards/cropped/',
    cardCode,
    '-cropped.png'
  ))

# regions names / abbreviations / logos from global JSON
data_regions <- lorr::get_regions_data() %>% 
  mutate(nameRef = case_when(
    nameRef == "PiltoverZaun" ~ "Piltover",
    nameRef == "Targon" ~ "MtTargon",
    TRUE ~ nameRef
  ))

# create connection to MySQL database
con <- lorr::create_db_con()

# leaderboard data
max_date <- tbl(con, 'leaderboard_daily') %>% 
  filter(region == update_region) %>% 
  summarise(max(day, na.rm = TRUE)) %>% 
  pull()

data <- tbl(con, 'leaderboard_daily') %>% 
  filter(region == update_region) %>% 
  filter(day >= local(max_date-lubridate::days(1))) %>% 
  select(name, rank, lp, day) %>% 
  collect()

# top 3 played decklists
data_decks <- tbl(con, 'ranked_patch_decklists') %>% 
  filter(region == update_region, time_frame == 3, is_master == 1) %>% 
  arrange(desc(match)) %>% 
  head(3) %>% 
  collect()

# highest wr decks
data_meta <- tbl(con, 'ranked_patch_archetypes') %>% 
  filter(region == update_region, time_frame == 3, is_master == 1) %>% 
  mutate(pr = match / sum(match, na.rm = TRUE)) %>% 
  arrange(desc(match)) %>% 
  head(10) %>% 
  collect()

# most played version of the deck (for aggregated archetypes)
most_played_version <- tbl(con, 'ranked_match_metadata_30d') %>% 
  filter(region == update_region, game_start_time_utc >= local(Sys.time()-lubridate::days(1))) %>% 
  left_join(tbl(con, 'ranked_match_info_30d'), by = 'match_id') %>% 
  filter(player_rank == 2) %>% 
  count(archetype, sort = TRUE) %>%
  left_join(tbl(con, 'utils_archetype_aggregation'), by = c('archetype' = 'old_name')) %>% 
  mutate(new_name = coalesce(new_name, archetype)) %>% 
  filter(new_name %in% local(data_meta$archetype)) %>% 
  group_by(new_name) %>% 
  slice_max(n = 1, order_by = n, with_ties = FALSE) %>% 
  ungroup() %>% 
  select(aggregate = new_name, original = archetype) %>% 
  collect()

# last date available
max_date <- max(data$day)

# post tweet
make_tweet <- function(token, status, as_reply = FALSE){
  
  # if needed, get id of the tweet to reply to
  if(as_reply){
    reply_id <- get_my_timeline(token = token, n = 1) %>% pull(status_id)
  } else {
    reply_id <- NULL
  }
  
  # make tweet
  suppressMessages(
    post_tweet(token = token, status = status, in_reply_to_status_id = reply_id)
  )

}

# perform GET calls but with a delay in them
get_slowly <- function(..., delay = 0.1){
  
  Sys.sleep(delay)
  
  GET(...)
  
}

# get country of a player from name and shard
get_country <- function(name, shard){
  
  # manual fix for asia / apac shard
  if(shard == 'asia'){ shard <- 'apac' }
  
  get_call <- sprintf("https://runeterra.ar/Users/get/country/%s/%s", shard, name) %>% 
    utils::URLencode() %>% 
    get_slowly()
  
  status <- status_code(get_call)
  
  content <- content(get_call)
  
  country <- ifelse(status == 200, content, NA_character_) #%>% 
  #str_flatten(collapse = " ")
  
  return(country)
  
}

# get emoji from country code or name
get_country_emoji <- function(country){
  
  if(!is.na(country)){ 
    
    # manual fixes for wrong countries
    if(country == 'id'){return(as.character(emo::ji('indonesia')))}
    if(country == 'hk'){return(as.character(emo::ji('hong_kong')))}
    if(country == 'cl'){return(as.character(emo::ji('chile')))}
    if(country == 'tt'){return(as.character(emo::ji('trinidad_tobago')))}
    if(country == 'sa'){return(as.character(emo::ji('saudi_arabia')))}
    if(country == 'no'){return(as.character(emo::ji('norway')))}
    if(country == 'ae'){return(as.character(emo::ji('united_arab_emirates')))}
    
    tryCatch({
      
      country <- emo::ji(country)  
      
    },
    
    error = function(e){
      
      tryCatch({
        
        country <<- country %>%  
          countrycode::countrycode(
            origin = 'iso2c', 
            destination = 'country.name') %>% 
          emo::ji()
      },
      error = function(e){
        
        country <<- ''
        
      }
      
      )
      
    }
    
    )
    
  } else {
    country <- ''
  }
  
  country <- as.character(country)
  
  return(country)
  
}

# run this thing only if there are at least 10 people in master
if(nrow(data) > 20){
  
  # quick reshaping
  data <- data %>% 
    mutate(day = if_else(day == max(day), 'today', 'yesterday')) %>% 
    group_by(name, day) %>%
    slice_max(n = 1, order_by = lp, with_ties = FALSE) %>%
    ungroup() %>%
    pivot_wider(names_from = day, values_from = c(lp, rank))
  
  # top 20 highest climbers
  data_2 <- data %>% 
    mutate(lp_diff = lp_today - lp_yesterday) %>% 
    filter(lp_diff > 0) %>% 
    slice_max(n = 20, order_by = lp_diff, with_ties = FALSE) %>% 
    select(name, lp_diff)
  
  # add country info and emoji
  data_2 <- data_2 %>% 
    mutate(
      country = map_chr(.x = name, .f = get_country, shard = update_region),
      emoji = map_chr(.x = country, .f = get_country_emoji)
    )
  
  # for the top 3 pull deck
  top3_players <- data_2 %>% 
    slice_max(n = 3, order_by = lp_diff, with_ties = FALSE) %>% 
    pull(name)
  
  # pull deck
  top3_puuids <- tbl(con, 'utils_players') %>% 
    filter(gameName %in% top3_players, region == update_region) %>% 
    select(puuid, gameName) %>% 
    collect() %>% 
    filter(gameName %in% top3_players) # these forces exact matching
  
  data_top3 <- tbl(con, 'ranked_match_info_30d') %>% 
    filter(puuid %in% local(top3_puuids$puuid)) %>% 
    left_join(tbl(con, 'ranked_match_metadata_30d'), by = 'match_id') %>% 
    filter(region == update_region, game_start_time_utc >= local(Sys.time()-lubridate::days(1))) %>% 
    count(puuid, archetype, deck_code, game_outcome) %>% 
    collect()
  
  deck_top3 <- data_top3 %>% 
    left_join(top3_puuids, by = 'puuid') %>%
    pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0) %>% 
    {if('tie' %in% colnames(.)) . else mutate(., tie = 0)} %>% 
    mutate(match = win + loss + tie) %>% 
    group_by(gameName, archetype, deck_code) %>% 
    summarise(across(c(match, win), sum), .groups = 'drop') %>% 
    group_by(gameName) %>% 
    slice_max(n = 1, order_by = match, with_ties = FALSE) %>% 
    ungroup() %>% 
    mutate(wr = win / match)
  
  deck_top3 <- deck_top3 %>% 
    left_join(data_2, by = c('gameName' = 'name')) %>% 
    arrange(desc(lp_diff)) %>% 
    mutate(tweet = row_number()) %>% 
    split(.$tweet) %>% 
    map(mutate, string = paste0(
      'Most played deck by ', gameName, 
      ' to climb +', lp_diff, ' LP: \n\n',
      deck_code, '\n\n',
      archetype, ' - N: ', match, ' - WR: ', scales::percent(wr, accuracy = .1)
    )) %>% 
    map(pull, string) %>% 
    map_chr(paste0, collapse = "\n")
  
  # convert to string
  data_2 <- data_2 %>% 
    mutate(id = row_number(), tweet = ceiling(id/5)) %>%
    mutate(id = case_when(
      id == 1 ~ "🥇",
      id == 2 ~ "🥈",
      id == 3 ~ "🥉",
      TRUE ~ as.character(id)
    )) %>% 
    split(.$tweet) %>% 
    map(mutate, string = paste0(id, '. ', name, ' ', emoji, ' [+', lp_diff, ']')) %>% 
    map(pull, string) %>% 
    map_chr(paste0, collapse = "\n")
  
  # for each tweet, get previous id and post it as an answer
  tweet_2_header <- sprintf('%s - %s \n #LoR TOP 20 Master Players that Gained more LP \n\n', nice_region, format(max_date, '%d %B, %Y'))
  
  # post 1st tweet
  suppressMessages(
    post_tweet(token = token, status = paste0(tweet_2_header, data_2[1]))
  )

  # post the others as replies
  if(length(data_2) > 1){
    walk(
      .x = 2:length(data_2), 
      .f = ~make_tweet(token = token, status = paste0(tweet_2_header, data_2[.]), as_reply = TRUE)
    ) 
  }
  
  # post the others as replies
  if(length(deck_top3) > 1){
    walk(
      .x = 1:length(deck_top3), 
      .f = ~make_tweet(token = token, status = deck_top3[.], as_reply = TRUE)
    ) 
  }
  
  # unluckiest 20
  data_3 <- data %>% 
    mutate(lp_diff = lp_today - lp_yesterday) %>% 
    filter(lp_diff < 0) %>% 
    slice_min(n = 20, order_by = lp_diff, with_ties = FALSE) %>% 
    select(name, lp_diff)
  
  # add country info and emoji
  data_3 <- data_3 %>% 
    mutate(
      country = map_chr(.x = name, .f = get_country, shard = update_region),
      emoji = map_chr(.x = country, .f = get_country_emoji)
    )
  
  # for the top 3 pull deck
  bot3_players <- data_3 %>% 
    slice_min(n = 3, order_by = lp_diff, with_ties = FALSE) %>% 
    pull(name)
  
  # pull deck
  bot3_puuids <- tbl(con, 'utils_players') %>% 
    filter(gameName %in% bot3_players, region == update_region) %>% 
    select(puuid, gameName) %>% 
    collect() %>% 
    filter(gameName %in% bot3_players) # these forces exact matching
  
  data_bot3 <- tbl(con, 'ranked_match_info_30d') %>% 
    filter(puuid %in% local(bot3_puuids$puuid)) %>% 
    left_join(tbl(con, 'ranked_match_metadata_30d'), by = 'match_id') %>% 
    filter(region == update_region, game_start_time_utc >= local(Sys.time()-lubridate::days(1))) %>% 
    count(puuid, archetype, deck_code, game_outcome) %>% 
    collect()
  
  deck_bot3 <- data_bot3 %>% 
    left_join(bot3_puuids, by = 'puuid') %>%
    pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0) %>% 
    {if('tie' %in% colnames(.)) . else mutate(., tie = 0)} %>% 
    mutate(match = win + loss + tie) %>% 
    group_by(gameName, archetype, deck_code) %>% 
    summarise(across(c(match, win), sum), .groups = 'drop') %>% 
    group_by(gameName) %>% 
    slice_max(n = 1, order_by = match, with_ties = FALSE) %>% 
    ungroup() %>% 
    mutate(wr = win / match)
  
  deck_bot3 <- deck_bot3 %>% 
    left_join(data_3, by = c('gameName' = 'name')) %>% 
    arrange(lp_diff) %>% 
    mutate(tweet = row_number()) %>% 
    split(.$tweet) %>% 
    map(mutate, string = paste0(
      'Most played deck by ', gameName, 
      ' to lose ', lp_diff, ' LP: \n\n',
      deck_code, '\n\n',
      archetype, ' - N: ', match, ' - WR: ', scales::percent(wr, accuracy = .1)
    )) %>% 
    map(pull, string) %>% 
    map_chr(paste0, collapse = "\n")
  
  # convert to string
  data_3 <- data_3 %>% 
    mutate(id = row_number(), tweet = ceiling(id/5)) %>% 
    mutate(id = case_when(
      id == 1 ~ "🥇",
      id == 2 ~ "🥈",
      id == 3 ~ "🥉",
      TRUE ~ as.character(id)
    )) %>% 
    split(.$tweet) %>% 
    map(mutate, string = paste0(id, '. ', name, ' ', emoji, ' [', lp_diff, ']')) %>% 
    map(pull, string) %>% 
    map_chr(paste0, collapse = "\n")
  
  # for each tweet, get previous id and post it as an answer
  tweet_3_header <- sprintf('%s - %s \n #LoR TOP 20 Unluckiest Master Players \n\n', nice_region, format(max_date, '%d %B, %Y'))
  
  # post 1st tweet
  suppressMessages(
    post_tweet(token = token, status = paste0(tweet_3_header, data_3[1]))
  )

  # post the others as replies
  if(length(data_3) > 1){
    walk(
      .x = 2:length(data_3), 
      .f = ~make_tweet(token = token, status = paste0(tweet_3_header, data_3[.]), as_reply = TRUE)
    )
  }
  
  # post the others as replies
  if(length(deck_bot3) > 1){
    walk(
      .x = 1:length(deck_bot3), 
      .f = ~make_tweet(token = token, status = deck_bot3[.], as_reply = TRUE)
    ) 
  }
  
  
  # top 3 played decklists of the day
  data_5 <- data_meta %>% 
    head(3) %>% 
    mutate(
      winrate = scales::percent(win / match, accuracy = .1),
      emoji = case_when(
        row_number() == 1 ~ "🥇",
        row_number() == 2 ~ "🥈",
        row_number() == 3 ~ "🥉",
        TRUE ~ ''
    )) %>% 
    mutate(string = paste0(emoji, ' ', archetype, ' (N: ', match, ' - WR: ', winrate, ')')) %>% 
    pull(string) %>% 
    paste0(collapse = "\n")
  
  data_plot <- data_meta %>% 
    left_join(most_played_version, by = c('archetype' = 'aggregate')) %>% 
    mutate(image = str_remove_all(original, "No Champions ")) %>% 
    mutate(image = str_replace_all(image, set_names(data_champs$cardCode, paste0(data_champs$name, "\\b")))) %>% 
    separate(image, into = c("tmp1", "tmp2"), sep = "\\(", fill = "right") %>% 
    mutate(tmp2 = str_replace_all(tmp2, pattern = " ", replacement = "x_x")) %>% 
    mutate(tmp2 = ifelse(is.na(tmp2), tmp2, paste0("(", tmp2))) %>% 
    unite(col = image, tmp1, tmp2, sep = "", na.rm = TRUE) %>% 
    mutate(image = str_replace_all(image, pattern = " ", replacement = "_")) %>%
    mutate(image = ifelse(grepl("[A-Z]{4}", image), paste0(str_sub(image, 2, 4), "_", str_sub(image, 5, 7)), image)) %>% 
    mutate(image = str_replace_all(image, pattern = "\\(|\\)", replacement = "x")) %>%
    separate(col = image, into = sprintf("image_%s", 1:7), fill = "right", sep = "_") %>% 
    mutate(winrate = win / match, match = as.numeric(match)) %>% 
    select(archetype, match, winrate, pr, starts_with('image_')) %>% 
    select(where(function(x) any(!is.na(x)))) %>% 
    mutate(across(starts_with("image_"), ~str_replace_all(., set_names(data_champs$croppedPath, data_champs$cardCode)))) %>% 
    mutate(across(starts_with("image_"), ~str_replace_all(., set_names(data_regions$iconAbsolutePath, paste0("x", data_regions$abbreviation, "x"))))) %>% 
    mutate(archetype = factor(archetype, levels = data_meta$archetype, ordered = TRUE)) %>% 
    pivot_longer(cols = -c(archetype, starts_with('image_'))) %>%
    mutate(name = factor(name, levels = c('match', 'pr', 'winrate'), ordered = TRUE)) %>% 
    mutate(name_numeric = as.numeric(name))
  
  max_champs_play <- ncol(data_plot) - 4
  
  plot_5 <- data_plot %>% 
    rowwise() %>% 
    mutate(disp_images = sum(!is.na(c_across(starts_with("image_"))))) %>%
    ungroup() %>% 
    ggplot(aes(x = name_numeric, y = reorder(archetype, desc(archetype)))) +
    geom_rect(aes(xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = Inf), fill = '#E8D3B9', alpha = 0.002) +
    geom_tile(aes(alpha = if_else(name == 'pr', value, if_else(name == 'winrate', (value-0.4)/2, 0)), fill = name)) +
    ggfittext::geom_fit_text(aes(label = archetype, xmin = -((4/4)+((max_champs_play-disp_images)*3/10)), xmax = 0.5), 
                             size = 18, reflow = TRUE, padding.x = grid::unit(3, "mm"), padding.y = grid::unit(3, "mm"), place = "right") +
    shadowtext::geom_shadowtext(
      aes(
        label = if_else(name == 'match', scales::comma(value, accuracy = 1), scales::percent(value, accuracy = .1)),
        color = case_when(
          name != 'winrate' ~ 'not_colored',
          value > 0.6 ~ 'super_pos', 
          value > 0.55 ~ 'pos', 
          value > 0.45 ~ 'not_changed', 
          value > 0.4 ~ 'neg', 
          TRUE ~ 'super_neg'
        )
      ), 
      bg.r = 0.1, size = 12, na.rm = TRUE, bg.color = 'white') +
    {if(max_champs_play > 0) geom_image(aes(image = image_1, x = -(max_champs_play+2)*4/10), size = 0.045, asp = 2) } +
    {if(max_champs_play > 1) geom_image(aes(image = image_2, x = -(max_champs_play+1)*4/10), size = 0.045, asp = 2, na.rm = TRUE) } +
    {if(max_champs_play > 2) geom_image(aes(image = image_3, x = -(max_champs_play-0)*4/10), size = 0.045, asp = 2, na.rm = TRUE) } +
    {if(max_champs_play > 3) geom_image(aes(image = image_4, x = -(max_champs_play-1)*4/10), size = 0.045, asp = 2, na.rm = TRUE) } +
    {if(max_champs_play > 4) geom_image(aes(image = image_5, x = -(max_champs_play-2)*4/10), size = 0.045, asp = 2, na.rm = TRUE) } +
    {if(max_champs_play > 5) geom_image(aes(image = image_6, x = -(max_champs_play-3)*4/10), size = 0.045, asp = 2, na.rm = TRUE) } +
    {if(max_champs_play > 6) geom_image(aes(image = image_7, x = -(max_champs_play-4)*4/10), size = 0.045, asp = 2, na.rm = TRUE) } +
    scale_y_discrete(labels = element_blank()) +
    scale_x_continuous(
      breaks = c((-(max_champs_play+2)*4/10+0.25)/2, 1:3), 
      labels = c('Archetype', '# of Match', 'Playrate', 'Winrate'), 
      position = "top", 
      limits = c(-(max_champs_play+2)*4/10-0.1, 3.3)
    ) +
    scale_fill_manual(values = c('pr' = 'steelblue', 'winrate' = 'forestgreen')) +
    scale_color_manual(
      values = c(
        'not_colored' = 'black', 
        'super_neg' = '#B81D13', 
        'super_pos' = "#046507", 
        'pos' = '#149414', 
        'neg' = 'coral2', 
        'not_changed' = '#EFB700'
      )) +
    labs(
      title = sprintf('%s - %s \nMost Played Archetypes at Master Rank', nice_region, format(max_date, '%d %B, %Y')), 
      x = element_blank(), 
      y = element_blank()
    ) +
    theme_classic(base_size = 20) +
    theme(
      legend.position = 'none', 
      axis.text.x = element_text(size = 20), 
      axis.text.y = element_text(size = 20),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  
  ggsave(filename = "/home/balco/dev/lor-meta-report/templates/tweet-plots/plot.png", plot = plot_5, width = 12, height = 8, dpi = 180)
  
  # for each tweet, get previous id and post it as an answer
  tweet_5_header <- sprintf('%s - %s \n #LoR TOP Archetypes at Master Rank by Playrate \n\n', nice_region, format(max_date, '%d %B, %Y'))
  
  # post 1st tweet
  suppressMessages(
    post_tweet(
      token = token, 
      status = paste0(tweet_5_header, data_5), 
      media = "/home/balco/dev/lor-meta-report/templates/tweet-plots/plot.png"
    )
  )

  # top 3 played decklists of the day
  data_4 <- data_decks %>% 
    mutate(
      winrate = scales::percent(winrate, accuracy = .1),
      tweet = row_number()
    ) %>% 
    split(.$tweet) %>% 
    map(mutate, emoji = case_when(
      tweet == 1 ~ "🥇",
      tweet == 2 ~ "🥈",
      tweet == 3 ~ "🥉",
      TRUE ~ as.character(tweet)
    )) %>% 
    map(mutate, string = paste0(emoji, '. ', deck_code, ' (', archetype, ') \n\n', '# Match: ', match, ' - WR: ', winrate)) %>% 
    map(pull, string) %>% 
    map_chr(paste0, collapse = "\n")
  
  # for each tweet, get previous id and post it as an answer
  tweet_4_header <- sprintf('%s - %s \n #LoR Most 3 played Decklists at Master Rank \n\n', nice_region, format(max_date, '%d %B, %Y'))
  
  # post the others as replies
  if(length(data_4) > 1){
    walk(
      .x = 1:length(data_4), 
      .f = ~make_tweet(token = token, status = paste0(tweet_4_header, data_4[.]), as_reply = TRUE)
    )
  }
  
}

DBI::dbDisconnect(con)
