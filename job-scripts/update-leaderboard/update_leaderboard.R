# Hourly leaderboard update

# Update the master leaderboards every hour

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(mongolite)) # connect to MongoDB
suppressPackageStartupMessages(library(httr))      # http requests

# 2. functions ----

# run call to update leaderboard for the selected region (region must be one of "europe", "americas", "asia")
update_leaderboard <- function(region){
  
  # base url to perform API call
  base.url <- sprintf("https://%s.api.riotgames.com/", region) # americas, asia, europe, sea

  # GET call
  get_leaderboard <- GET(base.url, path = "/lor/ranked/v1/leaderboards", add_headers("X-Riot-Token" = api_key))
  
  # if status == 200 (good response)
  if(get_leaderboard$status_code == 200){
    
    # get content of the leaderboard
    leaderboard <- get_leaderboard %>% content() %>% unname() %>% bind_rows()
    
    # fix rank (it starts from 0, should start from 1)
    leaderboard <- leaderboard %>% 
      mutate(rank = rank + 1)
    
    # name of the collection to update
    db_collection <- switch(
      region,
      "europe" = "lor_leaderboard",
      "americas" = "lor_leaderboard_na",
      "asia" = "lor_leaderboard_asia"
    )
    
    dbu_collection <- switch(
      region,
      "europe" = "lor_leaderboard_update",
      "americas" = "lor_leaderboard_update_na",
      "asia" = "lor_leaderboard_update_asia"
    )
    
    # connect to databases
    m_lb      <- mongo(url = "mongodb://balco:n0nLadimentico@localhost:27017/admin", collection = db_collection)
    m_lb_upd  <- mongo(url = "mongodb://balco:n0nLadimentico@localhost:27017/admin", collection = dbu_collection)
    
    # update leaderboard to db
    m_lb$remove(query = "{}")
    m_lb$insert(leaderboard)
    
    # update "lb_update"
    lb_update <- Sys.time()
    
    # uodate leaderboard update time
    m_lb_upd$remove(query = "{}")
    m_lb_upd$insert(data.frame(lb_update))
    
    # wait to prevent too many API calls
    Sys.sleep(0.05)
    
  }
  
}

# 3. set parameters ----

api_key <- "RGAPI-ae4106eb-969c-4957-ba08-724b72ad70ac"

# 4. launch function calls ----

update_leaderboard(region = "europe")
update_leaderboard(region = "americas")
update_leaderboard(region = "asia")
