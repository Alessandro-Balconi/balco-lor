# This script updates the "utils_deck_code_cards" table
suppressPackageStartupMessages(library(lorr))

# fetch updated data and store in temporary table
execute_db_query(
  query = '/home/balco/dev/lor-meta-report/queries/job-scripts/daily-data-models/utils_deck_code_cards.sql',
  bigint = 'numeric'
)

# update "utils_update_time" table
execute_db_query(
  query = "
  REPLACE INTO utils_update_time
  (table_name, time)
  VALUES
  ('utils_deck_code_cards', '{update_time}');
  ",
  update_time = as.character(Sys.time())
)
