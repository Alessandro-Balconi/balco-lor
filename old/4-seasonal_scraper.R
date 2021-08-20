# Read data from API, save to MySQL server in correct format

# I.   libraries ----

library(DBI)
library(RMySQL)
library(httr)
library(jsonlite)
library(tidyverse)
library(magrittr)
library(lubridate)
library(reticulate)

# II.  functions ----

# get player name from PUUID
get_name_from_id <- function(puuid){
  
  if(puuid %in% puuid_name$id){
    
    name <- puuid_name %>% 
      filter(id == puuid) %>% 
      pull(name)
    
  } else {
    
    # API Call
    call <- GET(base.url, path = paste0("riot/account/v1/accounts/by-puuid/", puuid), add_headers("X-Riot-Token" = api_key)) %>% 
      content()
    
    # wait to avoid API rate limit
    Sys.sleep(1.2)
    
    # extract relevant info
    name <- call %>% extract2("gameName")
    
  }
  
  return(name)
  
}

# extract card list from deck code
get_cards_from_code <- function(code){
  
  cards <- deck(code) %>% 
    extract2("cards") %>%
    map("card_code") %>% 
    map_chr(c) 
  
  return(cards)
  
}

# get factions from a match
get_factions <- function(match){
  
  p1_fac <- match %>% 
    extract2("players") %>% 
    extract2(1) %>% 
    extract2("factions") %>% 
    unlist() %>%
    reduce2(c('faction_', '_Name'), c('', ''),  .init = ., str_replace)
  
  # if deck 1 is mono faction, set second faction to NA
  if(length(p1_fac) == 1){ p1_fac <- c(p1_fac, NA) }
  
  p2_fac <- match %>% 
    extract2("players") %>% 
    extract2(2) %>% 
    extract2("factions") %>% 
    unlist() %>%
    reduce2(c('faction_', '_Name'), c('', ''),  .init = ., str_replace)
  
  c(p1_fac, p2_fac)
  
}

# get players info from a match
get_players_info <- function(match){
  
  # get players id
  p1_id <- match %>% extract2("players") %>% extract2(1) %>% extract2("puuid")
  p2_id <- match %>% extract2("players") %>% extract2(2) %>% extract2("puuid")
  
  # return result
  c(p1_id, p2_id)
  
}

# get deck codes from a match
get_deck_codes <- function(match){
  
  # get deck codes
  deck1 <- match %>% extract2("players") %>% extract2(1) %>% extract2("deck_code")
  deck2 <- match %>% extract2("players") %>% extract2(2) %>% extract2("deck_code")
  
  #return result
  c(deck1, deck2)
  
}

# get match info from match id
get_match_info <- function(id, puuid){
  
  # get all match informations from API
  match <- GET(base.url, path = paste0(path_match_info, id), add_headers("X-Riot-Token" = api_key)) %>% 
    content() %>%
    extract2("info")
  
  # keep only valid ranked matches
  if(is.null(match)){ return(NA) }
  if(match$game_mode != "SeasonalTournamentLobby" & match$game_type != "SeasonalTournamentLobby"){ return(NA) }
  
  # extract needed informations from match
  factions <- get_factions(match)
  players  <- get_players_info(match)
  decks    <- get_deck_codes(match)
  
  res <- tibble(
    date        = match$game_start_time_utc %>% str_remove_all(pattern = "T.*$"),
    faction_1_1 = factions[1],
    faction_1_2 = factions[2],
    faction_2_1 = factions[3],
    faction_2_2 = factions[4],
    deck_1      = decks[1],
    deck_2      = decks[2],
    player_1_id = players[1],
    player_2_id = players[2],
    winner      = if(match %>% extract2("players") %>% extract2(1) %>% .$game_outcome == "win"){ player_1_id } else { player_2_id }
  )
  
  return(res)
  
}

# drop NA elements from a list
drop_na_list <- function(list){
  
  list <- list[!is.na(list)]
  
  return(list)
  
}

# keep only NA elements in a list (opposite of drop_na_list)
keep_na_list <- function(list){
  
  list <- list[is.na(list)]
  
  return(list)
  
}

# III. parameters ----

# today's date (used for refreshing leaderboard daily)
current_day <- Sys.Date()

# API path
base.url           <- "https://europe.api.riotgames.com/" # americas, asia, europe, sea
path_account       <- "riot/account/v1/accounts/by-riot-id/"
path_match_history <- "lor/match/v1/matches/by-puuid/"
path_match_info    <- "lor/match/v1/matches/"

# API Key
api_key <- "RGAPI-eff920ab-b6f0-45eb-b7ec-480871a6fe10"

# install / update "lor_deckcodes" (TO DO EVERY NEW EXPANSION)
#py_install("lor_deckcodes", pip = TRUE)

# load python modules
lor_deckcodes <- import("lor_deckcodes")
deck <- lor_deckcodes$LoRDeck$from_deckcode

# master leaderboard (to check who is master and who is not)
leaderboard <- GET("https://europe.api.riotgames.com/", path = "/lor/ranked/v1/leaderboards", add_headers("X-Riot-Token" = api_key)) %>%
  content() %>%
  extract2("players") %>%
  map_dfr(as_tibble)

# IV.  connect to db ----

# create connection to database
con <- dbConnect(
  MySQL(),
  db_host = "127.0.0.1",
  user = "balco",
  password = "Macosanes0!",
  dbname = "db_prova"
)

# get currently known mapping of puuid - player name
puuid_name <- tbl(con, "lor_match") %>% 
  select(matchid, date, player_1_id, player_2_id, player_1_name, player_2_name) %>%
  collect() %>%
  distinct() %>% 
  pivot_longer(cols = -c(matchid, date)) %>% 
  separate(col = name, into = c("player", "number", "what")) %>% 
  select(-player) %>% 
  pivot_wider(names_from = what, values_from = value) %>%
  group_by(id) %>% 
  slice_max(n = 1, order_by = date, with_ties = FALSE) %>% 
  ungroup() %>% 
  distinct(id, name)

# list of currently available puuids (from master player if there is any, from all players otherwise)
puuid_list <- leaderboard %>%
  filter(rank <= 700) %>% 
  left_join(puuid_name, by = "name") %>%
  drop_na(id) %>% 
  pull(id)

# matches already collected
old_matches <- tibble(
  matchid = character(),
  date    = POSIXct()
)

# non ranked matches of the last week (initialized empty)
useless_matches <- tibble(
  matchid = character(),
  date    = POSIXct()
)

# V. call api ----

i <- 1
run <- 1

while(TRUE){
  
  if(current_day != Sys.Date()){

    # close previous connections to MySQL database
    dbDisconnect(con)

    # create connection to database (every day, because sometimes it goes away...)
    con <- dbConnect(
      MySQL(),
      db_host = "127.0.0.1",
      user = "balco",
      password = "Macosanes0!",
      dbname = "db_prova"
    )

    # update current day
    current_day <- Sys.Date()

    # print message
    print(sprintf("Restarted MySQL server on: %s", current_day))

  }
  
  # initialize puuid to collect match from
  current_puuid <- puuid_list[i]
  
  # get recent matches played
  match_history <- GET(base.url, path = paste0(path_match_history, current_puuid, "/ids"), add_headers("X-Riot-Token" = api_key)) %>% 
    content() %>% 
    unlist()
  
  # on the very first run, check only most recent games at the start of the cycle
  if(is.na(match_history)[1]){ next }
  
  if(run == 1){
    
    n_match_to_check <- min(length(match_history), ceiling(i/50))

  } else {
    
    n_match_to_check <- length(match_history)
    
  }
  
  match_history <- match_history[1:n_match_to_check]
  
  # extract informations only of matches not yet analyzed
  match_history <- match_history %>% 
    setdiff(old_matches$matchid) %>% 
    setdiff(useless_matches$matchid)
  
  # add message about the player currently checked
  print(sprintf("Now checking player: %s", puuid_name$name[puuid_name$id == current_puuid]))
  print(sprintf("Number of games to analyze: %s", length(match_history)))
  print(sprintf("Current UTC time: %s (CEST is UTC+2)", Sys.time()))
  
  # bypass error 503
  if(!is.na(match_history[1]) & match_history[1] == "Service unavailable"){
    
    # print error message
    print("Service is unavailable; skipping current iteration.")
    
    # wait a minute
    Sys.sleep(60)
    
    next
    
  }
  
  # otherwise, perform operations
  if(!is.na(match_history[1])){
    
    # get match info
    match_infos <- match_history %>%
      set_names(match_history) %>%
      lapply(function(x){
        Sys.sleep(36)
        get_match_info(x, puuid = current_puuid)
      }) 
    
    # get ids of non ranked games
    new_useless <- match_infos %>% 
      keep_na_list() %>% 
      names()
    
    # print info message
    print(sprintf("Number of non-seasonal games analyzed: %s", length(new_useless)))
    
    # update "useless_matches" with non ranked games just analyzed
    if(length(new_useless) != 0){
      
      useless_matches <- tibble(matchid = new_useless, date = rep(Sys.Date(), length(new_useless))) %>%
        bind_rows(useless_matches, .)
      
    }
    
    # remove non ranked games and go back to tibble
    match_infos <- match_infos %>% 
      drop_na_list() %>% 
      bind_rows(.id = "matchid")
    
    # if i have at least 1 new match, get additional info and save to database
    if(nrow(match_infos) != 0){
      
      # get list of cards from deck code
      match_infos <- match_infos %>%
        mutate(across(starts_with("deck_"), .fns = list(cards = ~map(., .f = get_cards_from_code)))) %>% 
        mutate(across(c(deck_1_cards, deck_2_cards), function(x) map_chr(x, ~reduce(., .f = paste, sep = " "))))
      
      # get player names from puuid and check if they are master
      match_infos <- match_infos %>% 
        mutate(across(starts_with("player_"), .fns = list(name = ~map_chr(., .f = get_name_from_id)), .names = "player_{1:2}_{fn}"))
      
      # save match info to database
      match_infos %>% 
        dbWriteTable(conn = con, name = "lor_seasonal", value = ., append = TRUE, row.names = FALSE) 
      
      # matches already collected
      old_matches <- tibble(matchid = match_infos$matchid, date = ymd(match_infos$date)) %>%
        bind_rows(old_matches, .)
      
    }
    
    # print message when finishing checking a player
    print(sprintf("Number of new seasonal games added to database: %s", nrow(match_infos)))
    print(sprintf("Finished checking player: %s", puuid_name$name[puuid_name$id == current_puuid]))
    
  }
  
  # update counter
  if(i < length(puuid_list)){ 
    
    i <- i + 1 
    
  } else { 
    
    i <- 1 
    
    run <- 2
    
    puuid_list <- tbl(con, "lor_seasonal") %>% 
      select(matchid, date, player_1_id, player_2_id) %>%
      collect() %>%
      pivot_longer(cols = -c(matchid, date)) %>%
      count(value, sort = TRUE) %>% 
      pull(value) %>% 
      setdiff(., puuid_list)
    
  }
  
  # should be useless, but prevents too many API calls
  if(length(match_history) == 0){ Sys.sleep(18) } else { Sys.sleep(1.2) }
  
}
