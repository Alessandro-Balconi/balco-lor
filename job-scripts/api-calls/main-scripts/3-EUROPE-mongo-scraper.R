# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(mongolite))
suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(lubridate))

# 2. connect to db & load data ----

# credentials
mongo_creds <- config::get("mongodb", file = "/home/balco/my_rconfig.yml")
mysql_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# connect to db
m_match   <- mongo(url = sprintf("mongodb://%s:%s@localhost:27017/admin", mongo_creds$uid, mongo_creds$pwd), collection = "lor_match_info")

# close previous connections to MySQL database (if any)
if(exists("con")){ DBI::dbDisconnect(con) }

# connect to mysql db
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  db_host = "127.0.0.1",
  user = mysql_creds$uid,
  password = mysql_creds$pwd,
  dbname = mysql_creds$dbs
)

# 3. set api parameters ----

# API path
base.url           <- "https://europe.api.riotgames.com/" # americas, asia, europe, sea
path_match_history <- "lor/match/v1/matches/by-puuid/"
path_match_info    <- "lor/match/v1/matches/"

# initialize parameters
i <- 1 # cycle parameter

# api key
api_key <- config::get("riot_api", file = "/home/balco/my_rconfig.yml")

# 4. define functions ----

#' Add a player to the player database (if not present; otherwise, update its info)
# 
#' @param player - the output of a content(GET(...)) call to "/riot/account/v1/accounts/by-puuid/"
#' 
add_player_to_db <- function(player, region = 'europe'){
  
  # add check to add player only if the call went well; otherwise just do nothing...
  if(!is.null(player$puuid)){
    
    # run update
    DBI::dbExecute(
      conn = con,
      statement = sprintf(
        "REPLACE INTO lor_players
        (puuid, gameName, tagLine, region)
        VALUES
        (%s);",
        paste0("'", paste0(c(player$puuid, player$gameName, player$tagLine, region), collapse = "', '"), "'")
      )
    )
    
  }
  
}

# 5. make calls ----

while(TRUE){
  
  # at the start of each cycle, initialize list of players to extract match from
  if(i == 1){
    
    # print message to console
    cat(sprintf("START: %s UTC", Sys.time()))
    
    # clean database from matches unable to collect (so they can be collected again)
    #m_match$remove('{"status.status_code":{"$in": [403, 503]}}') [these makes sense only if I also save matchids of these games]
    m_match$remove('{"status.status_code":{"$exists": true}}')
    
    # matches already collected (to prevent collecting them again)
    # could be done just once but doing it every cicle for safety
    already_in_mongo <- m_match$aggregate('[{"$group":{"_id":"$metadata.match_id"}}]') %>% pull()
    
    # number of matches in the db at the start of a cycle
    n_start <- length(already_in_mongo)
    
    # get leaderboard
    get_leaderboard <- GET(base.url, path = "/lor/ranked/v1/leaderboards", add_headers("X-Riot-Token" = api_key), config = config(connecttimeout = 60))
    
    # if status == 200 (good response)
    if(get_leaderboard$status_code == 200){
      
      # get content of the leaderboard
      leaderboard <- get_leaderboard %>% content() %>% unname() %>% bind_rows()
      
      # list of players in master (the ones we are collecting games from)
      master_players <- leaderboard %>% 
        {if(nrow(.) > 0) pull(., name) else NA_character_ }
      
      # save current master players
      if(length(master_players) >= 10){
        
        # save masters (for when next season hits)
        saveRDS(object = master_players, file = "/home/balco/dev/lor-meta-report/templates/master_leaderboards/europe.rds")
        
      } else if (Sys.time()-lubridate::days(30) >= R.utils::lastModified("/home/balco/dev/lor-meta-report/templates/master_leaderboards/old_europe.rds")){
        
        old_masters <- readRDS(file = "/home/balco/dev/lor-meta-report/templates/master_leaderboards/europe.rds")
        
        cat(sprintf(" - Saving Old Master Leaderboards: %s", Sys.time()))
        
        saveRDS(object = old_masters, file = "/home/balco/dev/lor-meta-report/templates/master_leaderboards/old_europe.rds")
        
      }
      
      # last season master players (collecting data from them)
      old_master_players <- readRDS("/home/balco/dev/lor-meta-report/templates/master_leaderboards/old_europe.rds")
      
      # get players to read match data from (old season masters + current masters)
      player_data <- tbl(con, 'lor_players') %>%
        filter(region == 'europe') %>%
        select(-region) %>%
        collect()
      
      # filter only master players
      puuid_list <- player_data %>% 
        filter(gameName %in% c(master_players, old_master_players)) %>% 
        pull(puuid)
      
      cat(sprintf(" - %s players (%s masters, %s plat+).", length(puuid_list), length(master_players), length(setdiff(old_master_players, master_players))))
      
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
    matches <- setdiff(x = matches, y = already_in_mongo)
    
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
    
    # add to "already_in_mongo" the ones we collected (status_code = 200)
    new_matches <- tibble(
      id = matches,
      status = match_list %>% map_dbl('status_code')
    ) %>% 
      filter(status == 200) %>% 
      pull(id)
    
    already_in_mongo <- c(already_in_mongo, new_matches)

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
  
  # if we just finished a cycle, send number of match collected in this cycle
  if(i == 1){
    
    # number of matches collected in this cycle
    n_cycle <- length(already_in_mongo) - n_start
    
    # print log
    cat(sprintf(" - END: %s new matches. \n", n_cycle))
    
  }
  
  # wait to prevent too many calls (more if there were no matches analyzed)
  if(get_matches$status_code == 200 & length(matches) > 0){ Sys.sleep(0.03 + max(0, 18 - length(matches))) } else { Sys.sleep(0.03 + 18) }
  
}
