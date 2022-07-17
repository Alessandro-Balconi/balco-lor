# HOW TO USE THIS SCRIPT:
# 1. update the parameters right below these comments [ss_id and the seasonal_match_time]
# 2. THE DAY OF THE SEASONAL: start a local job that runs this script -> should be a trycatch wrapper to restart if it fails
# 3. (OPTIONAL) remove rows from the db at the end of the seasonal (SEE LAST LINE OF CODE)

# CONTROLLARE CHE QUESTA TABELLA SIA VUOTA
#DBI::dbExecute(conn = con, statement = "DELETE FROM seasonal_match_data;")

# import must have packages (others will be imported later)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(googlesheets4)) # manage google sheets API
suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library(jsonlite))

options(googlesheets4_quiet = TRUE)
httr::set_config(httr::config(http_version = 0))

# set google API Key & Oauth credentials
google_creds <- config::get("google", file = "/home/balco/my_rconfig.yml")
gargle::oauth_app_from_json(google_creds$client_secret)
googlesheets4::gs4_auth_configure(api_key = google_creds$api_key)
googlesheets4::gs4_auth(path = google_creds$auth_path)

# google spreadsheet id
ss_id <- "1CV68XxcbXn04gBcC8KPNmTrdhMh14c6FfX6oFlJdn3E"

# google spreadsheet id with the parameters 
params_ss_id <- "1pCixyJwIRkcceX3W9cwsd6tNNpOK4E75i88K1pXCN8A"

# SEASONAL START TIMES OF MATCHES
seasonal_match_time <- tibble::tribble(
  ~round, ~start_time,
  1, as_datetime(paste0(Sys.Date(), " 09:55:00")),
  2, as_datetime(paste0(Sys.Date(), " 11:00:00")),
  3, as_datetime(paste0(Sys.Date(), " 12:05:00")),
  4, as_datetime(paste0(Sys.Date(), " 13:10:00")),
  5, as_datetime(paste0(Sys.Date(), " 14:15:00")),
  6, as_datetime(paste0(Sys.Date(), " 15:45:00")),
  7, as_datetime(paste0(Sys.Date(), " 16:50:00")),
  8, as_datetime(paste0(Sys.Date(), " 17:55:00")),
  9, as_datetime(paste0(Sys.Date(), " 19:00:00"))
)

# setup ----

# credentials
.mysql_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# api key
.api_key <- config::get("riot_api", file = "/home/balco/my_rconfig.yml")

# connect to mysql db
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  db_host  = "127.0.0.1",
  user     = .mysql_creds$uid,
  password = .mysql_creds$pwd,
  dbname   = .mysql_creds$dbs
)

# API path
base.url           <- "https://europe.api.riotgames.com/" # americas, asia, europe, sea
path_match_history <- "lor/match/v1/matches/by-puuid/"
path_match_info    <- "lor/match/v1/matches/"

# get most recent set number (to read sets JSONs)
last_set <- "https://dd.b.pvp.net/latest/core/en_us/data/globals-en_us.json" %>% 
  GET() %>% 
  content(encoding = "UTF-8") %>% 
  fromJSON() %>% 
  .[["sets"]] %>% 
  mutate(set = str_extract(nameRef, pattern = "[0-9]+")) %>% 
  mutate(set = as.numeric(set)) %>% 
  summarise(max(set, na.rm = TRUE)) %>% 
  pull()

# champions names / codes / regions from set JSONs
data_champs <- map_dfr(
  .x = 1:last_set,
  .f = function(x) {
    sprintf("https://dd.b.pvp.net/latest/set%1$s/en_us/data/set%1$s-en_us.json", x) %>% 
      GET() %>%  
      content(encoding = "UTF-8") %>% 
      fromJSON() %>% 
      as_tibble()
  },
  .id = "set"
) %>% 
  filter(rarity == "Champion") %>% 
  select(name, cardCode, regionRefs) %>%
  filter(nchar(cardCode) <= 8) # additional check because sometimes Riot messes up

# regions names / abbreviations / logos from global JSON
data_regions <- "https://dd.b.pvp.net/latest/core/en_us/data/globals-en_us.json" %>% 
  GET() %>% 
  content(encoding = "UTF-8") %>% 
  fromJSON() %>% 
  .[["regions"]] %>% 
  mutate(nameRef = case_when(
    nameRef == "PiltoverZaun" ~ "Piltover",
    nameRef == "Targon" ~ "MtTargon",
    TRUE ~ nameRef
  )) %>% 
  mutate(abbreviation = if_else(abbreviation %in% data_champs$name, 'RU', abbreviation)) # fix RU champs

# extract region from monoregion champs
get_monoregion <- function(champs){
  
  # split champions
  champs <- str_split(champs, pattern = " ") %>% unlist()
  
  # get monoregions
  data_champs %>% 
    filter(cardCode %in% champs) %>% 
    mutate(n_regions = map_int(regionRefs, length)) %>% 
    filter(n_regions == 1) %>% 
    pull(cardCode) %>%
    paste0(collapse = " ") %>% 
    str_remove_all(pattern = "[0-9]")
  
}

# extract infos from a match
extract_match_info <- function(match_id){
  
  # wait a bit to prevent too many calls
  Sys.sleep(0.66)
  
  # extract info
  match_data <- GET(
    base.url, 
    path = paste0(path_match_info, match_id), 
    add_headers("X-Riot-Token" = .api_key), 
    config = config(connecttimeout = 60)
  ) %>%
    content() %>% 
    .[['info']]
  
  # if match not collected, return
  if(is.null(match_data)){ return() }
  
  # convert from list to df
  match_info <- match_data %>% 
    as_tibble() %>%
    distinct(game_mode, game_start_time_utc) %>% 
    mutate(match_id = match_id, .before = 1)
  
  # extract players info
  players_info <- match_data$players %>% 
    bind_rows() %>% 
    unnest(cols = factions, keep_empty = TRUE) %>% 
    mutate(factions = str_extract(factions, pattern = "(?<=_)(.+)(?=\\_)")) %>% 
    select(-c(deck_id, order_of_play))
  
  # merge match info and players info
  data <- bind_cols(match_info, players_info)
  
  # reshape (keeping only 1 row per deck)
  data <- data %>% 
    group_by(match_id, game_mode, game_start_time_utc, puuid, deck_code, game_outcome) %>%
    mutate(id = row_number()) %>% 
    ungroup() %>% 
    pivot_wider(names_from = id, values_from = factions, names_prefix = "faction_")
  
  # return result
  return(data)
  
}

# get match round from time
get_match_round <- function(match_time){
  
  seasonal_match_time %>% 
    filter(match_time >= start_time, match_time < end_time) %>% 
    pull(round)
  
}

# add end time (aka before the start of the next)
seasonal_match_time <- seasonal_match_time %>% 
  mutate(end_time = lead(start_time)) %>% 
  replace_na(list(end_time = max(seasonal_match_time$start_time + minutes(65))))

# list of already collected matches
already_collected <- tbl(con, 'seasonal_match_data') %>% 
  distinct(match_id) %>% 
  pull()

# keep fetching data while seasonal is running
while(Sys.time() < max(seasonal_match_time$end_time)){
  
  # delay in minutes before adding match to spreadsheet
  minutes_delay <- pull(range_read(ss = params_ss_id, sheet = 'Delay', col_names = FALSE, .name_repair = make.names))
  
  # MANUALLY UPDATE PLAYER LIST
  italian_players <- pull(range_read(ss = params_ss_id, sheet = 'Player List', col_names = FALSE, .name_repair = make.names))
  
  # get puuids from player list ----
  
  df_players <- tbl(con, 'utils_players') %>% 
    mutate(player = paste(gameName, tagLine, sep = "#")) %>% 
    filter(region == 'europe', player %in% italian_players) %>% 
    select(player, puuid) %>% 
    collect()
  
  # players with missing puuid
  missing_puuids <- setdiff(tolower(italian_players), tolower(df_players$player))
  
  # if needed, collect them
  if(length(missing_puuids) > 0){
    
    # call API to get missing players name+tag 
    get_new_players <- map(
      .x = missing_puuids,
      .f = function(x) GET(
        base.url, 
        path = paste0("/riot/account/v1/accounts/by-riot-id/", str_replace(utils::URLencode(x, reserved = TRUE), pattern = "%23", replacement = "/")), 
        add_headers("X-Riot-Token" = .api_key), 
        config = config(connecttimeout = 60)
      )
    )
    
    # extract content in JSON format
    missing_puuids <- map_dfr(get_new_players, content) %>%
      {if("status" %in% colnames(.)) select(., -status) %>% drop_na() else . }
    
    if(ncol(missing_puuids) > 0 ){
      
      missing_puuids <- missing_puuids %>% 
        mutate(player = paste(gameName, tagLine, sep = "#"), .keep = 'unused')
      
      # add those players to the list of italians
      df_players <- bind_rows(df_players, missing_puuids)
      
    }
    
  }
  
  # get recent match_id of all players ----
  
  # call to get list of matches
  get_match_id <- map(
    .x = set_names(df_players$puuid, df_players$player),
    .f = slowly(
      function(x) GET(
        base.url, 
        path = paste0(path_match_history, x, "/ids") , 
        add_headers("X-Riot-Token" = .api_key), 
        config = config(connecttimeout = 60)
      ),
      rate = rate_delay(0.5)
    )
  )
  
  # list of most recent 20 matches of each player
  matches_list <- get_match_id %>% 
    map(content) %>% 
    map(unlist) %>% 
    as_tibble() %>% 
    pivot_longer(cols = everything(), names_to = "player", values_to = "match_id")
  
  # keep only new matches
  new_matches <- setdiff(matches_list$match_id, already_collected)
  
  # get info of new matches of all players ----
  
  # if needed, fetch new matches data
  if(length(new_matches) > 0){
    
    # extract data from matches
    data <- map_dfr(.x = new_matches, .f = extract_match_info)
    
    # remove matches where there is no deck code
    data <- data %>% 
      filter(deck_code != "")
    
    if(nrow(data) > 0){
      
      # extract card codes from deck code
      data <- data %>% 
        distinct(deck_code) %>%
        mutate(cards_list = map_chr(deck_code, function(x) { lordecks::get_decklist_from_code(x, format = "simple") %>% paste0(collapse = " ") } )) %>% 
        left_join(x = data, y = ., by = "deck_code")
      
      # keep only relevant columns
      data <- data %>% 
        select(all_of(c("match_id", "game_mode", "game_start_time_utc", "puuid", "deck_code", "game_outcome", "faction_1", "faction_2", "cards_list")))
      
      # get deck champions & archetype
      data <- data %>%
        distinct(across(c(starts_with("faction_"), cards_list))) %>% 
        mutate(
          cards = map_chr(cards_list, str_flatten, collapse = " "),
          champs = str_extract_all(cards, pattern = paste(data_champs$cardCode, collapse = "|")),
          champs = map_chr(champs, str_flatten, collapse = " "),
          champs_factions = map_chr(champs, get_monoregion)
        ) %>% 
        left_join(data_regions %>% select(faction_abb1 = abbreviation, nameRef), by = c("faction_1" = "nameRef")) %>% 
        left_join(data_regions %>% select(faction_abb2 = abbreviation, nameRef), by = c("faction_2" = "nameRef")) %>%
        unite(col = factions, faction_abb1, faction_abb2, sep = " ") %>% 
        mutate(
          factions = str_remove_all(factions, pattern = " NA|NA "),
          across(c(champs, champs_factions, factions),  function(x) unname(sapply(x, function(x) { paste(sort(trimws(strsplit(x[1], ' ')[[1]])), collapse=' ')} ))),
          no_fix = map2_lgl(.x = factions, .y = champs_factions, .f = ~grepl(pattern = .x, x = .y)),
          champs_factions = str_replace_all(champs_factions, pattern = " ", replacement = "|"),
          champs_factions = paste0(champs_factions, "| "),
          factions_to_add = str_remove_all(factions, pattern = champs_factions),
          archetype = if_else(no_fix, champs, sprintf("%s (%s)", champs, factions_to_add))
        ) %>% 
        left_join(data, ., by = c("faction_1", "faction_2", "cards_list")) %>% 
        select(-c(cards_list, champs_factions, no_fix, starts_with('faction'), cards, champs))
      
      # make archetype name nicer
      data <- data %>% 
        mutate(archetype = str_replace_all(archetype, set_names(data_champs$name, data_champs$cardCode))) %>% 
        mutate(across(archetype, function(x) ifelse(grepl("^( )", x), paste0("No Champions", x), x))) 
      
      # add new matches to db ----
      
      # fix time column format
      data <- data %>% 
        mutate(time = as_datetime(game_start_time_utc), .keep = 'unused', .after = match_id)
      
      # save results to db
      data %>% 
        DBI::dbWriteTable(conn = con, name = "seasonal_match_data", value = ., append = TRUE, row.names = FALSE)
      
      # list of already collected matches
      already_collected <- c(already_collected, new_matches)
      
      # update gs4 ----
      
      # pull seasonal data from db
      gs4_data <- tbl(con, 'seasonal_match_data') %>% 
        filter(time >= local(Sys.time()-days(3)), game_mode == 'SeasonalTournamentLobby', time <= local(Sys.time()-minutes(minutes_delay))) %>%
        select(time, deck_code, game_outcome, archetype, puuid) %>% 
        collect()
      
      # use player names
      gs4_data <- gs4_data %>% 
        inner_join(df_players, by = 'puuid') %>% 
        select(-puuid)
      
      # add round number to data
      gs4_data <- gs4_data %>% 
        mutate(round = map_dbl(time, get_match_round))
      
      # "anonymize" player tags
      gs4_data <- gs4_data %>% 
        mutate(player = str_remove_all(player, pattern = '#.*'))
      
      # player lineups
      lineups <- gs4_data %>% 
        count(player, archetype, deck_code, game_outcome) %>% 
        pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0) %>% 
        {if("win"  %in% colnames(.)) . else mutate(., win  = 0)} %>% 
        {if("loss" %in% colnames(.)) . else mutate(., loss = 0)} %>% 
        {if("tie"  %in% colnames(.)) . else mutate(., tie  = 0)} %>% 
        mutate(match = loss+win+tie, winrate = scales::percent(win / match, accuracy = .1)) %>% 
        select(-c(win, loss, tie))
      
      # aesthetical fixes
      lineups <- lineups %>%
        rename_with(str_replace_all, pattern = "_", replacement = " ") %>% 
        rename_with(str_to_title)
      
      # get matches data
      score <- gs4_data %>% 
        count(player, round, game_outcome) %>% 
        pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0) %>% 
        {if("win"  %in% colnames(.)) . else mutate(., win  = 0)} %>% 
        {if("loss" %in% colnames(.)) . else mutate(., loss = 0)} %>% 
        {if("tie"  %in% colnames(.)) select(., -tie) else .}
      
      # add all rounds info
      score <- crossing(
        player = unique(score$player), 
        round = seq(1:nrow(seasonal_match_time))
      ) %>% 
        left_join(score, by = c('player', 'round'))
      
      # this will overwrite match results where needed
      overwrite_score <- range_read(ss = params_ss_id, sheet = 'Match Results', col_names = TRUE, .name_repair = tolower)
      
      if(nrow(overwrite_score) > 0){
        
        overwrite_score <- overwrite_score %>% 
          rename(win_new = win, loss_new = loss)
        
        score <- score %>% 
          left_join(overwrite_score, by = c("player", 'round')) %>% 
          mutate(win = coalesce(win_new, win), loss = coalesce(loss_new, loss)) %>% 
          select(-c(win_new, loss_new))
        
      }
      
      # get overall score for each player
      overall <- score %>% 
        replace_na(list(win = 0, loss = 0)) %>% 
        mutate(
          has_won = ifelse(win > loss & win >= 2, 1, 0), 
          has_lost = ifelse(loss > win & loss >= 2, 1, 0)
        ) %>% 
        group_by(player) %>% 
        summarise(wins = sum(has_won, na.rm = TRUE), losses = sum(has_lost, na.rm = TRUE), .groups = 'drop') %>% 
        unite(col = overall, wins, losses, sep = "-")
      
      # complete data and reshape
      score <- score %>% 
        unite(col = score, c(win, loss), sep = "-") %>% 
        mutate(score = ifelse(score == "NA-NA", '-', score)) %>% 
        pivot_wider(names_from = round, values_from = score, values_fill = "-", names_prefix = "round_")
      
      # add overall score
      score <- score %>% 
        left_join(overall, by = 'player') %>% 
        arrange(desc(overall))
      
      # aesthetical fixes
      score <- score %>% 
        rename_with(str_replace_all, pattern = "_", replacement = " ") %>% 
        rename_with(str_to_title)
      
      # spreadsheet info
      info <- tibble(
        " " = c(
          sprintf("Ultimo Aggiornamento: %s UTC", Sys.time()), 
          "Se volete esser aggiunti a questo foglio (o se trovate degli errori), contattatemi su discord (Balco#7067).",
          "Nei casi in cui non sia possibile risalire al risultato tramite API (es. BYE, drop, match terminati 1-1 causa timer, 
match mancanti per eventuali problemi alle API), fatemelo sapere e aggiungo."
        )
      )
      
      # update all sheets of the spreadsheet
      with_gs4_quiet(range_write(data = info,    ss = ss_id, sheet = "Info"   , reformat = FALSE))
      with_gs4_quiet(range_write(data = lineups, ss = ss_id, sheet = "Lineups", reformat = FALSE))
      with_gs4_quiet(range_write(data = score,   ss = ss_id, sheet = "Score",   reformat = FALSE))
      
      # adjust spacing of columns in the spreadsheet
      #walk(.x = sheet_names(ss_id), .f = ~with_gs4_quiet(range_autofit(ss = ss_id, sheet = ., dimension = "columns")))
      
    }
    
  }
  
  # print "1" at the end of the cycle
  print('1')
  
}

DBI::dbDisconnect(con)
