# Weekly database cleanup

# Removes matches older than 1 month from the standard MySQL collections and stores them in the "db_old" version of those databases
# Also updates the "historical" weekly playrates table
# Runs weekly on Wednesday morning (why? idk)

tictoc::tic()

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(lubridate)) # working with dates

# 2. connect to database ----

# load mysql db credentials
db_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# close previous connections to MySQL database (if any)
if(exists("con")){ DBI::dbDisconnect(con) }

# connect to db
con <- DBI::dbConnect(
  RMySQL::MySQL(),
  db_host = "127.0.0.1",
  user = db_creds$uid,
  password = db_creds$pwd,
  dbname = "db_prova"
)

# 3. extract old matches from dbs ----

match_times_na <- tbl(con, "lor_match_info_na") %>% 
  distinct(match_id, game_start_time_utc) %>% 
  collect()

old_matchid_na <- match_times_na %>% 
  mutate(game_start_time_utc = as.POSIXct(game_start_time_utc)) %>% 
  filter(date(game_start_time_utc) <  Sys.Date() - days(30)) %>% 
  pull(match_id)

match_times_eu <- tbl(con, "lor_match_info") %>% 
  distinct(match_id, game_start_time_utc) %>% 
  collect()

old_matchid_eu <- match_times_eu %>% 
  mutate(game_start_time_utc = as.POSIXct(game_start_time_utc)) %>% 
  filter(date(game_start_time_utc) <  Sys.Date() - days(30)) %>% 
  pull(match_id)

match_times_asia <- tbl(con, "lor_match_info_asia") %>% 
  distinct(match_id, game_start_time_utc) %>% 
  collect()

old_matchid_asia <- match_times_asia %>% 
  mutate(game_start_time_utc = as.POSIXct(game_start_time_utc)) %>% 
  filter(date(game_start_time_utc) <  Sys.Date() - days(30)) %>% 
  pull(match_id)

# 4. add those to "old_db" and remove them from main collections ----

if(length(old_matchid_na) > 0){
  
  old_na <- tbl(con, "lor_match_info_na") %>% 
    filter(match_id %in% old_matchid_na) %>% 
    collect()
  
  DBI::dbWriteTable(con, "lor_match_info_old_na", value = old_na, append = TRUE, row.names = FALSE)
  
  delete_query <- paste0("DELETE FROM lor_match_info_na WHERE (match_id IN ('", paste0(old_matchid_na, collapse = "','"), "'));")
  
  DBI::dbSendQuery(con, delete_query)
  
}

if(length(old_matchid_eu) > 0){
  
  old_eu <- tbl(con, "lor_match_info") %>% 
    filter(match_id %in% old_matchid_eu) %>% 
    collect()
  
  DBI::dbWriteTable(con, "lor_match_info_old_eu", value = old_asia, append = TRUE, row.names = FALSE)
  
  delete_query <- paste0("DELETE FROM lor_match_info WHERE (match_id IN ('", paste0(old_matchid_eu, collapse = "','"), "'));")
  
  DBI::dbSendQuery(con, delete_query)
  
}

if(length(old_matchid_asia) > 0){
  
  old_asia <- tbl(con, "lor_match_info_asia") %>% 
    filter(match_id %in% old_matchid_asia) %>% 
    collect()
  
  DBI::dbWriteTable(con, "lor_match_info_old_asia", value = old_asia, append = TRUE, row.names = FALSE)
  
  delete_query <- paste0("DELETE FROM lor_match_info_asia WHERE (match_id IN ('", paste0(old_matchid_asia, collapse = "','"), "'));")
  
  DBI::dbSendQuery(con, delete_query)
  
}

tictoc::toc()

# send notification
RPushbullet::pbPost(
  "note", 
  title = "Weekly database cleanup", 
  body = sprintf("The weekly MySQL cleanup was performed correctly. Removed %s games from Europe, %s from Americas, %s from Asia.", 
                 length(old_matchid_eu), length(old_matchid_na), length(old_matchid_asia))
)
