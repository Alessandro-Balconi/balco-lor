# Runs daily jobs --------------------------------------------------------------
# This one runs at 4:15, 12:15 and 20:15 UTC
cat(sprintf("--- %s --- \n", Sys.time()))

# Send new matches from MongoDB to MySQL ---------------------------------------
lorr::launch_scripts_in_folder(
  folder = "/home/balco/dev/lor-meta-report/job-scripts/daily-mongo-to-sql/",
  log_message = "MongoDB to MySQL: ",
  discord_username = "Daily MongoDB to MySQL Update",
  discord_message = "Manual intervention required. "
)
