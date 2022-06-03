# Hourly leaderboard update

# Update the master leaderboards every hour

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(httr))      # http requests
suppressPackageStartupMessages(library(lubridate)) # work with dates

# 2. functions ----

# run call to update leaderboard for the selected region (region must be one of "europe", "americas", "asia")
update_leaderboard <- function(region){
  
  # base url to perform API call
  base.url <- sprintf("https://%s.api.riotgames.com/", region) # americas, asia, europe, sea
  
  # fastest way to fix the change in endpoint (asia -> sea)
  if(region == 'asia'){ base.url <- "https://sea.api.riotgames.com/" }
  
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
    
    ### TEMPORARY !!! SEND EMPTY LEADERBOARD SINCE THE CURRENT ONE IS BUGGED ###
    #leaderboard <- tibble(name = as.character(), rank = as.double(), lb = as.double())
    
    # time of the update
    upd_time <- tibble(
      table_name = sql_collection,
      time = Sys.time() %>% as.character()
    )
    
    # run update
    DBI::dbExecute(
      conn = con,
      statement = sprintf(
        "REPLACE INTO utils_update_time
        (table_name, time)
        VALUES
        (%s);",
        paste0("'", paste0(c(upd_time$table_name, upd_time$time), collapse = "', '"), "'")
      )
    )
    
    # update leadeboard in SQL
    DBI::dbWriteTable(conn = con, name = sql_collection, value = leaderboard, overwrite = TRUE, row.names = FALSE)
    
    # hour at which the snapshot is taken (UTC time)
    daily_hour <- switch(
      region,
      "europe" = 0,
      "americas" = 8,
      "asia" = 16
    )
    
    # once a day, also save a daily leaderboard snapshot
    if(lubridate::hour(Sys.time()) == daily_hour & lubridate::minute(Sys.time()) > 29){
      
      # calculate current day (based on region)
      cur_date = Sys.Date()
      if(region %in% c('europe', 'americas')){ cur_date = cur_date - days(1) }
      
      # add leaderboard region, day info
      leaderboard <- leaderboard %>% 
        mutate(region = region, day = cur_date)
      
      # update leadeboard in SQL
      DBI::dbWriteTable(conn = con, name = 'leaderboard_daily', value = leaderboard, append = TRUE, row.names = FALSE)
      
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