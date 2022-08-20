# This script updates the "ranked_patch_decklists" table
library(lorr)

# fetch updated data and store in temporary table
execute_db_query(
  query = '/home/balco/dev/lor-meta-report/queries/job-scripts/daily-data-models/tmp_ranked_patch_decklists.sql',
  update_time = Sys.time(),
  patch_release_date = lorr::get_patch_release_date(),
  bigint = 'numeric'
)

# clear data from main table
execute_db_query(query = "TRUNCATE TABLE ranked_patch_decklists")

# insert updated data into main table
execute_db_query(
  query = "
  INSERT INTO ranked_patch_decklists
  SELECT * FROM tmp_ranked_patch_decklists
  "
)

# clear temporary table
execute_db_query('DROP TABLE tmp_ranked_patch_decklists')