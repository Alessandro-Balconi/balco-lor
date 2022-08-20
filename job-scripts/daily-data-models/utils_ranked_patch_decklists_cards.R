# Script to update the "utils_ranked_patch_decklists_cards" table
# - If patch is less than 3 days old, overwrite table
# - Else, append new decks

# import libraries
library(lorr)

# get time of last table update
last_update <- get_db_query(
  query = "
  SELECT
    time
  FROM
    utils_update_time
  WHERE
    table_name = 'utils_ranked_patch_decklists_cards'
  "
)[[1]]

# for the first 3 days of a patch, completely rewrite the table; else just append
if(lorr::get_patch_release_date() > (last_update-3*86400)){
  
  # get all decks played in the patch
  
  # clear main table
  execute_db_query(query = "TRUNCATE TABLE utils_ranked_patch_decklists_cards")
  
} else {
  
  # get only NEW decks played in the patch since last update
  
}

# insert decks into the table