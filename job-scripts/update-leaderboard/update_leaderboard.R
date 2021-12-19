# Hourly leaderboard update

# Update the master leaderboards every hour

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(httr))      # http requests

# 2. functions ----

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
      "europe" = 22,
      "americas" = 14,
      "asia" = 6
    )
    
    # once a day, also save a daily leaderboard snapshot (& make tweets)
    if(lubridate::hour(Sys.time()) == daily_hour & lubridate::minute(Sys.time()) < 29){
      
      # daily leaderboard name
      daily_collection <- paste0(sql_collection, "_daily")
      
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
  dbname = "db_prova"
)

# 5. launch function calls ----

update_leaderboard(region = "europe")
update_leaderboard(region = "americas")
update_leaderboard(region = "asia")

DBI::dbDisconnect(con)