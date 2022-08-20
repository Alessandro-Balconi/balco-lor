# This script updates the "ranked_patch_matchups" table
library(lorr)

# fetch updated data and store in temporary table
execute_db_query(
  query = '/home/balco/dev/lor-meta-report/queries/job-scripts/daily-data-models/tmp_ranked_patch_matchups.sql',
  update_time = Sys.time(),
  patch_release_date = lorr::get_patch_release_date(),
  bigint = 'numeric'
)

# clear data from main table
execute_db_query(query = "TRUNCATE TABLE ranked_patch_matchups")

# insert updated data into main table
execute_db_query(
  query = "
  INSERT INTO ranked_patch_matchups
  SELECT * FROM tmp_ranked_patch_matchups
  "
)

# clear temporary table
execute_db_query('DROP TABLE tmp_ranked_patch_matchups')

# update "utils_update_time" table
execute_db_query(
  query = "
  REPLACE INTO utils_update_time
  (table_name, time)
  VALUES
  ('ranked_patch_matchups', '{update_time}');
  ",
  update_time = as.character(Sys.time())
)