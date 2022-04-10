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

old_matchid_v2 <- tbl(con, "lor_match_info_v2") %>% 
  mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATE)")) %>% 
  filter(game_start_time_utc < local(Sys.Date()-days(30))) %>% 
  distinct(match_id) %>% 
  collect() %>% 
  pull()

# 4. add those to "old_db" and remove them from main collections ----

if(length(old_matchid_v2) > 0){
  
  already_in_old_db <- tbl(con, 'ranked_match_metadata') %>% 
    filter(match_id %in% old_matchid_v2) %>% 
    select(match_id) %>% 
    collect() %>% 
    pull()
  
  old_v2 <- tbl(con, "lor_match_info_v2") %>% 
    filter((match_id %in% old_matchid_v2) & (!match_id %in% already_in_old_db)) %>% 
    collect() %>% 
    mutate(game_start_time_utc = as_datetime(game_start_time_utc)) %>% 
    mutate(is_master = is_master + 1) # changed from: 0 plat+, 1 master to: 0 any, 1 plat+, 2 master
  
  old_metadata <- old_v2 %>% 
    group_by(match_id, game_start_time_utc, game_version, total_turn_count, region) %>% 
    summarise(match_rank = sum(is_master, na.rm = TRUE), .groups = 'drop') %>% 
    collect()
  
  old_info <- old_v2 %>% 
    select(match_id, puuid, deck_code, game_outcome, order_of_play, faction_1, faction_2, cards, archetype, player_rank = is_master)
  
  DBI::dbWriteTable(con, "ranked_match_metadata", value = old_metadata, append = TRUE, row.names = FALSE)
  DBI::dbWriteTable(con, "ranked_match_info", value = old_info, append = TRUE, row.names = FALSE)
  
  delete_query <- paste0("DELETE FROM lor_match_info_v2 WHERE (match_id IN ('", paste0(old_matchid_v2, collapse = "','"), "'));")
  
  DBI::dbExecute(con, delete_query)
  
}

DBI::dbDisconnect(con)

tictoc::toc()
