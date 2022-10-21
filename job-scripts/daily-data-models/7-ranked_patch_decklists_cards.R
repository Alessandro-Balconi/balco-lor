# This script updates the "ranked_patch_decklists_cards" table
suppressPackageStartupMessages(library(lorr))

# fetch updated data and store in temporary table
execute_db_query(
  query = '/home/balco/dev/lor-meta-report/queries/job-scripts/daily-data-models/tmp_ranked_patch_decklists_cards.sql',
  bigint = 'numeric'
)

# drop main table
execute_db_query(query = "DROP TABLE ranked_patch_decklists_cards")

# recreate main table as tmp table
execute_db_query(
  query = "
  CREATE TABLE ranked_patch_decklists_cards AS
  SELECT * FROM tmp_ranked_patch_decklists_cards
  "
)

# clear temporary table
execute_db_query('DROP TABLE tmp_ranked_patch_decklists_cards')

# update "utils_update_time" table
execute_db_query(
  query = "
  REPLACE INTO utils_update_time
  (table_name, time)
  VALUES
  ('ranked_patch_decklists_cards', '{update_time}');
  ",
  update_time = as.character(Sys.time())
)
