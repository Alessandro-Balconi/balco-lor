# Hourly leaderboard update

# Update the master leaderboards every hour

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(httr))      # http requests
suppressPackageStartupMessages(library(lubridate)) # work with dates
suppressPackageStartupMessages(library(rtweet))    # make tweets

# 2. functions ----

# create a nice date from  date object
nice_date <- function(date, short_month = TRUE){
  
  paste(day(date), month(date, label = TRUE, abbr = short_month), year(date), sep = " ")
  
}

# same as scales::comma, but has a nicer format
scales_comma_plus <- function(x, accuracy = 1){
  
  sapply(
    X = x,
    FUN = function(x) case_when(
      is.na(x) ~ paste0("‼ NEW"),
      x==0 ~ paste0("↔ ", scales::comma(x, accuracy = accuracy)),
      x>0 ~ paste0("↑ +", scales::comma(x, accuracy = accuracy)),
      x<0 ~ paste0("↓ ", scales::comma(x, accuracy = accuracy)),
      TRUE ~ NA_character_ # should never happen; just an additional check
    ),
    USE.NAMES = FALSE
  )
  
}

# run call to update leaderboard for the selected region (region must be one of "europe", "americas", "asia")
update_leaderboard <- function(region){
  
  # base url to perform API call
  base.url <- sprintf("https://%s.api.riotgames.com/", region) # americas, asia, europe, sea

  # GET call
  get_leaderboard <- GET(base.url, path = "/lor/ranked/v1/leaderboards", add_headers("X-Riot-Token" = api_key), config = config(connecttimeout = 60))
  
  # if status == 200 (good response)
  if(get_leaderboard$status_code == 200){
    
    # get content of the leaderboard
    leaderboard <- get_leaderboard %>% content() %>% unname() %>% bind_rows()
    
    # fix rank (it starts from 0, should start from 1)
    if(nrow(leaderboard) > 0){ leaderboard <- leaderboard %>% mutate(rank = rank + 1) }
    
    # choose table name based on region
    sql_collection <- switch(
      region,
      "europe" = "leaderboard_eu",
      "americas" = "leaderboard_na",
      "asia" = "leaderboard_asia"
    )
    
    # if no master players, initialize empty table
    if(nrow(leaderboard) == 0){ leaderboard <- tibble(name = as.character(), rank = as.double(), lb = as.double()) }
    
    # update leadeboard in SQL
    DBI::dbWriteTable(conn = con, name = sql_collection, value = leaderboard, overwrite = TRUE, row.names = FALSE)
    
    # hour at which the snapshot is taken (UTC time)
    daily_hour <- switch(
      region,
      "europe" = 0,
      "americas" = 8,
      "asia" = 16
    )
    
    # once a day, also save a daily leaderboard snapshot (& make tweets)
    if(lubridate::hour(Sys.time()) == daily_hour & lubridate::minute(Sys.time()) > 29){
      
      # daily leaderboard name
      daily_collection <- paste0(sql_collection, "_daily")
      
      # make plot with changes vs yesterday
      if(nrow(leaderboard) >= 10 & FALSE){
        
        # store api keys
        twitter_creds <- config::get("twitter", file = "/home/balco/my_rconfig.yml")
        
        # authenticate via web browser
        token <- create_token(
          app = twitter_creds$app_name,
          consumer_key = twitter_creds$api_key,
          consumer_secret = twitter_creds$secret_api_key,
          access_token = twitter_creds$access_token,
          access_secret = twitter_creds$secret_access_token
        )
        
        # check today's top players and how they were doing yesterday
        new <- tbl(con, sql_collection) %>% collect()
        old <- tbl(con, daily_collection) %>% collect()
        
        # filter top 10
        new_10 <- new %>% filter(rank <= 10)
        old_10 <- old %>% filter(name %in% new_10$name) 
        
        # remove duplicate names 
        old_10 <- old_10 %>% 
          group_by(name) %>% 
          slice_min(n = 1, order_by = rank, with_ties = FALSE) %>% 
          ungroup()
        
        # join and calculate changes
        diff_10 <- left_join(new_10, old_10, by = c('name')) %>% 
          mutate(rank_gain = rank.y - rank.x, lp_gain = lp.x - lp.y)
        
        # make plot
        p <- ggplot(diff_10, aes(x = reorder(name, lp.x))) +
          geom_col(aes(y = lp.x), fill = "#13294b", color = "steelblue", alpha = 1) +
          geom_text(aes(label = "", y = 1.1*lp.x), size = 6) +
          geom_text(aes(label = scales::comma(lp.x, accuracy = 1), y = lp.x), size = 6, hjust = -0.5, vjust = -0.5) +
          geom_text(
            size = 5, hjust = -0.375, vjust = 1.5, 
            aes(label = scales_comma_plus(lp_gain, accuracy = 1), y = lp.x,
                color = case_when(is.na(lp_gain) ~ "na", lp_gain==0 ~ "same", lp_gain>0 ~ "pos", lp_gain<0 ~ "neg")
            )
          ) +
          ggfittext::geom_fit_text(
            aes(label = scales::comma(rank.x, accuracy = 1), ymin = -max(lp.x)/10, ymax = 0), hjust = -0.5, vjust = -0.5,
            size = 18, reflow = TRUE, padding.x = grid::unit(3, "mm"), padding.y = grid::unit(3, "mm"), place = "top") +
          ggfittext::geom_fit_text(
            aes(label = scales_comma_plus(rank_gain, accuracy = 1), ymin = -max(lp.x)/10, ymax = 0,
                color = case_when(is.na(rank_gain) ~ "na", rank_gain==0 ~ "same", rank_gain>0 ~ "pos", rank_gain<0 ~ "neg")
            ), size = 18, reflow = FALSE, padding.x = grid::unit(3, "mm"), padding.y = grid::unit(3, "mm"), place = "bottom"
          ) +
          ggfittext::geom_fit_text(
            aes(label = name, ymin = -((max(lp.x)/4)+(max(lp.x)/10)), ymax = -max(lp.x)/10),
            size = 18, reflow = TRUE, padding.x = grid::unit(3, "mm"), padding.y = grid::unit(3, "mm"), place = "center") +
          theme_classic(base_size = 18) +
          scale_x_discrete(labels = element_blank()) +
          scale_y_continuous(breaks = NULL, labels = scales::comma_format(accuracy = 1)) +
          scale_color_manual(values = c("pos" = "#149414", "neg" = "coral2", "na" = "steelblue", "same" = "#EFB700")) +
          coord_flip() +
          theme(legend.position = "none") +
          labs(
            x = "Player", 
            y = 'LP', 
            title = sprintf('TOP 10 - %s Master Leadeboard', str_to_title(region)), 
            subtitle = sprintf('Date: %s - Rank & LP change in the past 24 hours', nice_date(Sys.Date())))
        
        # save plot
        ggsave(filename = "/home/balco/dev/lor-meta-report/templates/tweet-plots/leaderboard.png", plot = p, width = 12, height = 8, dpi = 180)
        
        # remove duplicate names
        new <- new %>% group_by(name) %>% slice_min(n = 1, order_by = rank, with_ties = FALSE) %>% ungroup()
        old <- old %>% group_by(name) %>% slice_min(n = 1, order_by = rank, with_ties = FALSE) %>% ungroup()
        
        # join and calculate changes
        diff <- left_join(new, old, by = c('name')) %>% 
          mutate(rank_gain = rank.y - rank.x, lp_gain = lp.x - lp.y)
        
        # best 3 players of the day:
        top_3 <- diff %>%
          replace_na(list(lp_gain = 0)) %>% 
          slice_max(n = 3, order_by = lp_gain, with_ties = FALSE) %>% 
          pull(name)
        
        # worst 3
        bot_3 <- diff %>%
          replace_na(list(lp_gain = 0)) %>% 
          slice_min(n = 3, order_by = lp_gain, with_ties = FALSE) %>% 
          pull(name)
        
        # collect their info from players db
        top_3 <- tbl(con, 'lor_players') %>% 
          filter(region == local(region), gameName %in% top_3) %>% 
          collect()

        bot_3 <- tbl(con, 'lor_players') %>% 
          filter(region == local(region), gameName %in% bot_3) %>% 
          collect()
        
        # get their puuids
        top_3_puuids <- top_3 %>% pull(puuid)
        bot_3_puuids <- bot_3 %>% pull(puuid)
        
        # extract their matches played
        top_3_decks <- tbl(con, 'lor_match_info_v2') %>% 
          mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATETIME)")) %>% 
          filter(puuid %in% local(top_3_puuids), game_start_time_utc >= local(Sys.time() - hours(24))) %>% 
          count(puuid, archetype, deck_code, game_outcome) %>% 
          collect()
        
        # extract their matches played
        bot_3_decks <- tbl(con, 'lor_match_info_v2') %>% 
          mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATETIME)")) %>% 
          filter(puuid %in% local(bot_3_puuids), game_start_time_utc >= local(Sys.time() - hours(24))) %>% 
          count(puuid, archetype, deck_code, game_outcome) %>% 
          collect()
        
        # data with users to tweet in reply
        top_replies <- top_3_decks %>% 
          left_join(top_3, by = 'puuid') %>% 
          pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0) %>% 
          mutate(match = sum(c_across(c(win, loss, matches('tie'))))) %>% 
          unite(col = player_id, gameName, tagLine, sep= '#') %>% 
          group_by(player_id) %>% 
          slice_max(n = 1, order_by = match, with_ties = FALSE) %>% 
          ungroup()

        bot_replies <- bot_3_decks %>% 
          left_join(bot_3, by = 'puuid') %>% 
          pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0) %>% 
          mutate(match = sum(c_across(c(win, loss, matches('tie'))))) %>% 
          unite(col = player_id, gameName, tagLine, sep= '#') %>% 
          group_by(player_id) %>% 
          slice_max(n = 1, order_by = match, with_ties = FALSE) %>% 
          ungroup()
        
        # add lp changes info
        top_replies = top_replies %>% 
          mutate(tmp = str_remove(player_id, pattern = '#.*')) %>% 
          left_join(diff %>% select(name, lp_gain), by = c('tmp' = 'name'))
        
        bot_replies = bot_replies %>% 
          mutate(tmp = str_remove(player_id, pattern = '#.*')) %>% 
          left_join(diff %>% select(name, lp_gain), by = c('tmp' = 'name'))
        
        # post tweet
        post_tweet(
          status = sprintf('test might be wrong!!! \n \n %s - TOP 10 Master players in %s', nice_date(Sys.Date()), region),
          media = '/home/balco/dev/lor-meta-report/templates/tweet-plots/leaderboard.png',
          token = token
        )
        
        # get id of the tweet just posted
        my_timeline <- get_timeline('Balco21', token = token, n = 1)

        # post reply
        post_tweet(
          status = sprintf('%s - %s - Unluckiest Master players of the past 24 hours:', nice_date(Sys.Date()), str_to_title(region)),
          token = token, 
          in_reply_to_status_id = my_timeline$status_id[1]
        )

        # post reply
        post_tweet(
          status = sprintf('%s - %s - Best Master players of the past 24 hours:', nice_date(Sys.Date()), str_to_title(region)),
          token = token, 
          in_reply_to_status_id = my_timeline$status_id[1]
        )
        
        # get ids of just tweeted tweets
        my_timeline_2 <- get_timeline('Balco21', token = token, n = 2)
        
        # for each top user, make a reply tweet with most played deck
        for(i in seq_len(length(top_replies))){
          
          reply_id <- my_timeline_2$status_id[1]

          # status to tweet
          status_tweet <- sprintf(
            "Most played deck from %s to gain %s LP in the last 24 hours: %s - %s (N: %s, W: %s)",
            top_replies$player_id[i],
            top_replies$lp_gain[i],
            top_replies$archetype[i],
            top_replies$deck_code[i],
            top_replies$match[i],
            top_replies$win[i]
          )
          
          # post reply
          post_tweet(
            status = status_tweet,
            token = token, 
            in_reply_to_status_id = reply_id
          )

        }
        
        # for each bot user, make a reply tweet with most played deck
        for(i in seq_len(length(bot_replies))){
          
          reply_id <- my_timeline_2$status_id[2]

          # status to tweet
          status_tweet <- sprintf(
            "Most played deck from %s to lose %s LP in the last 24 hours: %s - %s (N: %s, W: %s)",
            bot_replies$player_id[i],
            bot_replies$lp_gain[i],
            bot_replies$archetype[i],
            bot_replies$deck_code[i],
            bot_replies$match[i],
            bot_replies$win[i]
          )
          
          # post reply
          post_tweet(
            status = status_tweet,
            token = token, 
            in_reply_to_status_id = reply_id
          )
          
        }

      }
      
      # update leadeboard in SQL
      DBI::dbWriteTable(conn = con, name = daily_collection, value = leaderboard, overwrite = TRUE, row.names = FALSE)
      
    }
    
    # wait to prevent too many API calls
    Sys.sleep(0.05)
    
  }
  
}

# 3. set parameters ----

# api key
api_key <- config::get("riot_api", file = "/home/balco/my_rconfig.yml")

# MySQL credentials
sql_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# 4. connect to db ----

# close previous connections to MySQL database (if any)
if(exists("con")){ DBI::dbDisconnect(con) }

# create connection to MySQL database
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  db_host = "127.0.0.1",
  user = sql_creds$uid,
  password = sql_creds$pwd,
  dbname = sql_creds$dbs
)

# 5. launch function calls ----

update_leaderboard(region = "europe")
update_leaderboard(region = "americas")
update_leaderboard(region = "asia")

DBI::dbDisconnect(con)