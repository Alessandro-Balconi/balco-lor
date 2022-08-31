# This script updates the "ranked_patch_decklists" table
library(lorr)

# fetch updated data and store in temporary table
execute_db_query(
  query = '/home/balco/dev/lor-meta-report/queries/job-scripts/daily-data-models/tmp_ranked_patch_decklists.sql',
  update_time = Sys.time(),
  patch_release_date = lorr::get_patch_release_date(),
  bigint = 'numeric'
)

# drop main table
execute_db_query(query = "DROP TABLE ranked_patch_decklists")

# recreate main table with the updated data from the tmp table
execute_db_query(
  query = "
  CREATE TABLE ranked_patch_decklists AS
  SELECT * FROM tmp_ranked_patch_decklists
  "
)

# clear temporary table
execute_db_query('DROP TABLE tmp_ranked_patch_decklists')
