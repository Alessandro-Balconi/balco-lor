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
  RMariaDB::MariaDB(),
  db_host = "127.0.0.1",
  user = db_creds$uid,
  password = db_creds$pwd,
  dbname = db_creds$dbs
)

# 3. extract old matches from dbs ----

match_times_v2 <- tbl(con, "lor_match_info_v2") %>% 
  distinct(match_id, game_start_time_utc) %>% 
  collect()

old_matchid_v2 <- match_times_v2 %>% 
  mutate(game_start_time_utc = as.POSIXct(game_start_time_utc)) %>% 
  filter(date(game_start_time_utc) <  Sys.Date() - days(30)) %>% 
  pull(match_id)

# 4. add those to "old_db" and remove them from main collections ----

if(length(old_matchid_v2) > 0){
  
  old_v2 <- tbl(con, "lor_match_info_v2") %>% 
    filter(match_id %in% old_matchid_v2) %>% 
    collect()
  
  already_in_old_db <- tbl(con, 'lor_match_info_old_v2') %>% 
    filter(match_id %in% old_matchid_v2) %>% 
    select(match_id) %>% 
    distinct() %>% 
    collect() %>% 
    pull()
  
  old_v2 <- old_v2 %>% filter(!match_id %in% already_in_old_db)
  
  DBI::dbWriteTable(con, "lor_match_info_old_v2", value = old_v2, append = TRUE, row.names = FALSE)
  
  delete_query <- paste0("DELETE FROM lor_match_info_v2 WHERE (match_id IN ('", paste0(old_matchid_v2, collapse = "','"), "'));")
  
  DBI::dbExecute(con, delete_query)
  
}

DBI::dbDisconnect(con)

tictoc::toc()
