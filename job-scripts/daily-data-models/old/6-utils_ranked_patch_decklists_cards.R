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

# CURRNT VERSION ---------------------------------------------------------------

# This script updates the "ranked_patch_decklists" table
library(lorr)

# fetch updated data and store in temporary table
execute_db_query(
  query = '/home/balco/dev/lor-meta-report/queries/job-scripts/daily-data-models/tmp_utils_ranked_patch_decklists_cards.sql',
  patch_release_date = lorr::get_patch_release_date(),
  bigint = 'numeric'
)

# drop main table
execute_db_query(query = "DROP TABLE utils_ranked_patch_decklists_cards")

# recreate main table with the updated data from the tmp table
execute_db_query(
  query = "
  CREATE TABLE utils_ranked_patch_decklists_cards AS
  SELECT * FROM tmp_utils_ranked_patch_decklists_cards
  "
)

# clear temporary table
execute_db_query('DROP TABLE tmp_utils_ranked_patch_decklists_cards')

# update "utils_update_time" table
execute_db_query(
  query = "
  REPLACE INTO utils_update_time
  (table_name, time)
  VALUES
  ('utils_ranked_patch_decklists_cards', '{update_time}');
  ",
  update_time = as.character(Sys.time())
)
