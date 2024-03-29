# Weekly database cleanup

# Removes matches older than 1 month from the standard MySQL collections and stores them in the "db_old" version of those databases
# Runs weekly on Wednesday morning (why? idk)

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

old_matchid <- tbl(con, "expedition_match") %>% 
  mutate(day = sql('CAST(game_start_time_utc AS DATE)')) %>%
  filter(day < local(Sys.Date()-days(90))) %>% 
  distinct(match_id) %>% 
  pull()

# 4. add those to "old_db" and remove them from main collections ----

if(length(old_matchid) > 0){
  
  old <- tbl(con, "expedition_match") %>% 
    filter(match_id %in% old_matchid) %>% 
    collect()
  
  already_in_old_db <- tbl(con, 'expedition_match_old') %>% 
    filter(match_id %in% old_matchid) %>% 
    distinct(match_id) %>% 
    pull()
  
  old <- old %>% filter(!match_id %in% already_in_old_db)
  
  DBI::dbWriteTable(con, "expedition_match_old", value = old, append = TRUE, row.names = FALSE)
  
  delete_query <- paste0("DELETE FROM expedition_match WHERE (match_id IN ('", paste0(old_matchid, collapse = "','"), "'));")
  
  DBI::dbExecute(con, delete_query)
  
}

DBI::dbDisconnect(con)
