# FINAL VERSION THAT WORKS IN A UTOPIC WORLD -----------------------------------

# # Script to update the "utils_ranked_patch_decklists_cards" table
# # - If patch is less than 3 days old, overwrite table
# # - Else, append new decks
# 
# # import libraries
# library(lorr)
# 
# # get time of last table update
# last_update <- get_db_query(
#   query = "
#   SELECT
#     time
#   FROM
#     utils_update_time
#   WHERE
#     table_name = 'utils_ranked_patch_decklists_cards'
#   "
# )[[1]]
# 
# # for the first 3 days of a patch, completely rewrite the table; else just append
# if(lorr::get_patch_release_date() > (last_update-3*86400)){
#   
#   # get all decks played in the patch
#   
#   # clear main table
#   execute_db_query(query = "TRUNCATE TABLE utils_ranked_patch_decklists_cards")
#   
# } else {
#   
#   # get only NEW decks played in the patch since last update
#   
# }
# 
# # insert decks into the table

# VERSION THAT WORKS IN A REAL WORLD -------------------------------------------

# # This script updates the "ranked_patch_decklists" table
# library(lorr)
# 
# # fetch updated data and store in temporary table
# execute_db_query(
#   query = '/home/balco/dev/lor-meta-report/queries/job-scripts/daily-data-models/tmp_utils_ranked_patch_decklists_cards',
#   patch_release_date = lorr::get_patch_release_date(),
#   bigint = 'numeric'
# )
# 
# # drop main table
# execute_db_query(query = "DROP TABLE utils_ranked_patch_decklists_cards")
# 
# # recreate main table with the updated data from the tmp table
# execute_db_query(
#   query = "
#   CREATE TABLE utils_ranked_patch_decklists_cards AS
#   SELECT * FROM tmp_utils_ranked_patch_decklists_cards
#   "
# )
# 
# # clear temporary table
# execute_db_query('DROP TABLE tmp_utils_ranked_patch_decklists_cards')
# 
# # update "utils_update_time" table
# execute_db_query(
#   query = "
#   REPLACE INTO utils_update_time
#   (table_name, time)
#   VALUES
#   ('utils_ranked_patch_decklists_cards', '{update_time}');
#   ",
#   update_time = as.character(Sys.time())
# )

# CURRENT VERSION --------------------------------------------------------------

# Creates a MySQL table with decklists information for the current patch (from data of MySQL databases)

# 1. libraries ----

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))

# 2. connect to db & load data ----

# create connection to MySQL database
con <- lorr::create_db_con()

# current patch release date
min_date <- lorr::get_patch_release_date()
#min_date <- as.POSIXct("2021-12-14 18:00:00 UTC") # hotfix date

# 3 table of current patch v2 ----

# extract data from MySQL
data_v2 <- tbl(con, "ranked_match_metadata_30d") %>%
  filter(game_start_time_utc >= min_date) %>%
  left_join(tbl(con, 'ranked_match_info_30d'), by = 'match_id') %>% 
  left_join(tbl(con, 'utils_archetype_aggregation'), by = c('archetype' = 'old_name')) %>% 
  mutate(archetype = coalesce(new_name, archetype)) %>% 
  count(archetype, deck_code, cards) %>% 
  filter(n >= 5) %>% 
  select(-n) %>% 
  collect() %>% 
  ungroup()

# reshape df
data_v2 <- data_v2 %>% 
  separate(col = cards, into = sprintf('card_%s', 1:40), sep = ' ', fill = 'right') %>% 
  pivot_longer(cols = -c(archetype, deck_code), values_drop_na = TRUE, names_to = NULL) %>% 
  separate(col = value, into = c('count', 'card_code'), sep = ':')

# 4. save to MySQL db ----

if(nrow(data_v2) >  0){
  
  data_v2 %>% 
    DBI::dbWriteTable(conn = con, name = "utils_ranked_patch_decklists_cards", value = ., overwrite = TRUE, row.names = FALSE) 
  
  DBI::dbExecute(
    conn = con,
    statement = sprintf(
      "REPLACE INTO utils_update_time
        (table_name, time)
        VALUES
        (%s);",
      paste0("'", paste0(c('utils_ranked_patch_decklists_cards', as.character(Sys.time())), collapse = "', '"), "'")
    )
  )
  
}

DBI::dbDisconnect(con)
