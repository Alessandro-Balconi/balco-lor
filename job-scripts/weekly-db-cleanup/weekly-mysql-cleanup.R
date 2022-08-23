# Weekly database cleanup

# Removes matches older than 1 month from the standard MySQL collections and stores them in the "db_old" version of those databases
# Also updates the "historical" weekly playrates table
# Runs weekly on Wednesday morning (why? idk)

tictoc::tic()

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(lubridate)) # working with dates

# 2. connect to database ----

# connect to db
con <- lorr::create_db_con()

# 3. extract old matches from dbs ----

old_matchid_v2 <- tbl(con, 'ranked_match_metadata_30d') %>% 
  filter(game_start_time_utc < local(Sys.Date()-days(30))) %>% 
  pull(match_id)

# old_matchid_v2 <- lorr::get_db_query(
#   query = "
#   SELECT
#     match_id
#   FROM
#     ranked_match_metadata_30d
#   WHERE
#     game_start_time_utc >= '{Sys.Date() - days(30)}'
#   "
# )[[1]]

# 4. add those to "old_db" and remove them from main collections ----

if(length(old_matchid_v2) > 0){
  
  already_in_old_db <- tbl(con, 'ranked_match_metadata') %>% 
    filter(match_id %in% old_matchid_v2) %>% 
    pull(match_id)
  
  old_metadata <- tbl(con, 'ranked_match_metadata_30d') %>% 
    filter((match_id %in% old_matchid_v2) & (!match_id %in% already_in_old_db)) %>% 
    collect()
  
  old_info <- tbl(con, 'ranked_match_info_30d') %>% 
    filter((match_id %in% old_matchid_v2) & (!match_id %in% already_in_old_db)) %>% 
    collect()
  
  DBI::dbWriteTable(con, "ranked_match_metadata", value = old_metadata, append = TRUE, row.names = FALSE)
  DBI::dbWriteTable(con, "ranked_match_info", value = old_info, append = TRUE, row.names = FALSE)
  
  delete_query2 <- paste0("DELETE FROM ranked_match_metadata_30d WHERE (match_id IN ('", paste0(old_matchid_v2, collapse = "','"), "'));")
  delete_query3 <- paste0("DELETE FROM ranked_match_info_30d WHERE (match_id IN ('", paste0(old_matchid_v2, collapse = "','"), "'));")
  
  DBI::dbExecute(con, delete_query2)
  DBI::dbExecute(con, delete_query3)
  
}

DBI::dbDisconnect(con)

tictoc::toc()
