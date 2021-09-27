# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(mongolite))
suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))

# 2. connect to db & load data ----

# connect to db
m_match   <- mongo(url = "mongodb://balco:n0nLadimentico@localhost:27017/admin", collection = "lor_match_info")
m_player  <- mongo(url = "mongodb://balco:n0nLadimentico@localhost:27017/admin", collection = "lor_player")

# 3. set api parameters ----

# API path
base.url           <- "https://europe.api.riotgames.com/" # americas, asia, europe, sea
path_match_history <- "lor/match/v1/matches/by-puuid/"
path_match_info    <- "lor/match/v1/matches/"

# initialize parameters
i <- 1 # cycle parameter

# 4. define functions ----

#' Add a player to the player database (if not present; otherwise, update its info)
# 
#' @param player - the output of a content(GET(...)) call to "/riot/account/v1/accounts/by-puuid/"
#' 
add_player_to_db <- function(player){
  
  # add check to add player only if the call went well; otherwise just do nothing...
  if(!is.null(player$puuid)){
    
    # if we don't have the player, add it; else update it's info (maybe the gameName or tagLine changed)
    check_puuid   <- sprintf('{"puuid":"%s"}', player$puuid)
    update_fields <- sprintf('{"$set":{"gameName":"%s", "tagLine":"%s"}}', player$gameName, player$tagLine)
    
    # run update
    m_player$update(check_puuid, update_fields, upsert = TRUE)
    
  }
  
}

# 5. make calls ----

api_key <- "RGAPI-ae4106eb-969c-4957-ba08-724b72ad70ac"

while(TRUE){
  
  # at the start of each cycle, initialize list of players to extract match from
  # if there are >50 masters, get their match; else get last season masters (if I still have them stored); else just get who played ranked recently
  if(i == 1){
    
    # print message to console
    cat(sprintf("New cycle: %s UTC", Sys.time()))
    
    # clean database from matches unable to collect (so they can be collected again)
    #m_match$remove('{"status.status_code":{"$in": [403, 503]}}') [these makes sense only if I also save matchids of these games]
    m_match$remove('{"status.status_code":{"$exists": true}}')
    
    # get leaderboard
    get_leaderboard <- GET(base.url, path = "/lor/ranked/v1/leaderboards", add_headers("X-Riot-Token" = api_key), config = config(connecttimeout = 60))
    
    # if status == 200 (good response)
    if(get_leaderboard$status_code == 200){
      
      # get content of the leaderboard
      leaderboard <- get_leaderboard %>% content() %>% unname() %>% bind_rows()
      
      # list of players in master (the ones we are collecting games from)
      master_players <- leaderboard %>% 
        {if(nrow(.) > 0) pull(., name) else NA_character_ }
      
      # if I have at least 50 players in master I collect their data; else i collect from players that recently played ranked matches
      if(length(master_players) >= 50){
        
        # print message to console
        cat(sprintf(" - There are %s master players; collecting match data from them. \n", length(master_players)))
        
        # get puuid of players from "m_player" database
        player_data <- m_player$find() %>% 
          as_tibble()
        
        # filter only master players
        puuid_list <- player_data %>% 
          filter(gameName %in% master_players) %>% 
          pull(puuid)
        
      } else if(exists("puuid_list")){
        
        # print message to console
        cat(sprintf(" - There are only %s master players; collecting match data from previous season' masters. \n", length(master_players)))
        
      } else {
        
        # print message to console
        cat(sprintf(" - There are only %s master players; collecting match data from people who recently played ranked games. \n", length(master_players)))
        
        # if there are less than 50 master in the leaderboard, just use all players that we have collected from data
        player_data <- m_match$find(
          query = '{"info.game_type":"Ranked"}', 
          fields = '{"info.players.puuid" : true, "info.game_start_time_utc" : true, "_id": false}'
        ) %>% 
          .[["info"]]
        
        # filter only players that recently played ranked matches
        player_data <- player_data %>% 
          as_tibble() %>% 
          mutate(game_start_time_utc = as_date(game_start_time_utc)) %>%
          filter(game_start_time_utc >= Sys.Date() - days(3))
        
        # get puuids
        puuid_list <- player_data$players %>% 
          bind_rows() %>% 
          distinct(puuid) %>% 
          pull()
        
      }
      
    }
    
  } # END cycle initialization
  
  # puuid to focus on
  puuid_i <- puuid_list[i]
  
  # collect matches
  get_matches <- GET(base.url, path = paste0(path_match_history, puuid_i, "/ids") , add_headers("X-Riot-Token" = api_key), config = config(connecttimeout = 60))
  
  # if status == 200 (good response)
  if(get_matches$status_code == 200){
    
    # extract contents of the call
    matches <- get_matches %>% content(as = "parsed") %>% unlist()
    
    # check if we have already analyzed any of those
    matches <- setdiff(x = matches, y = m_match$distinct("metadata.match_id"))
    
    # get info from new matches
    match_list <- lapply(
      X = matches,
      FUN = function(x){
        Sys.sleep(1)
        GET(base.url, path = paste0(path_match_info, x), add_headers("X-Riot-Token" = api_key), config = config(connecttimeout = 60))
      }
    )
    
    # extract content in JSON format
    match_content <- map(match_list, ~content(., as = "text", encoding = "UTF-8"))
    
    # convert "game_start_time_utc" to MongoDB Date format before saving
    match_content <- match_content %>% 
      map(.f = ~str_replace(., pattern = "\\.[0-9]{7}\\+[0-9]{2}\\:[0-9]{2}", replacement = "Z"))
    
    # make sure that all match contents are in valid JSON format
    valid_json <- match_content %>% map_lgl(validate)
    
    if(sum(valid_json == TRUE) != length(match_content)){
      
      print(sprintf("%s invalid match JSONs collected; removing them.", length(match_content) - sum(valid_json == TRUE)))
      
    }
    
    match_content <- match_content[valid_json]
    
    # save to database
    map(.x = match_content, .f = ~m_match$insert(.))
    
    # convert "game_start_time_utc" to MongoDB class Date 
    m_match$update(
      query  = '{}',
      update = '[{"$set":{"info.game_start_time_utc": { "$toDate": "$info.game_start_time_utc" }}}]', 
      multiple = TRUE
    )
    
    # check if there was any ranked match
    rankeds <- match_content %>% 
      map_lgl(., ~grepl("Ranked", .))
    
    # extract puuid of the players in those matches
    new_puuids <- match_content %>%
      .[rankeds] %>% 
      map(fromJSON) %>% 
      map("metadata") %>%
      bind_rows() %>% 
      {if(nrow(.) > 0) distinct(., participants) %>% pull() else NA_character_}
    
    # if we have players, extract their info
    if(sum(is.na(new_puuids)) == 0){
      
      # call API to get new players name+tag 
      get_new_players <- map(
        .x = new_puuids,
        .f = function(x) GET(base.url, path = paste0("/riot/account/v1/accounts/by-puuid/", x), add_headers("X-Riot-Token" = api_key), config = config(connecttimeout = 60))
      )
      
      # extract content in JSON format
      new_players <- map(get_new_players, content)
      
      # for each player, check if we already have it or not; if we don't, add it. else update its info
      map(.x = new_players, .f = add_player_to_db)
      
    }
    
    # update i
    if(i < length(puuid_list)){ i <- i + 1 } else { i <- 1 }
    
  }
  
  # if status == 500 skip the player (there's a problem with Riot API, he'll keep failing, so let's just move on)
  if(get_matches$status_code == 500){
    
    # update i
    if(i < length(puuid_list)){ i <- i + 1 } else { i <- 1 }
    
  }
  
  # wait to prevent too many calls (more if there were no matches analyzed)
  if(get_matches$status_code == 200 & length(matches) > 0){ Sys.sleep(0.03 + max(0, 18 - length(matches))) } else { Sys.sleep(0.03 + 18) }
  
}
