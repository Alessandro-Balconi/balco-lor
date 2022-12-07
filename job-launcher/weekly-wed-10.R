# Runs analysis + shows error if anything happens
# This script runs every Wednesday in the morning
tryCatch({
  cat(sprintf("--- %s --- \n", Sys.Date()))
  source("/home/balco/dev/lor-meta-report/job-scripts/weekly-db-cleanup/5-weekly-mongodb-cleanup.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/weekly-db-cleanup/weekly-mysql-cleanup.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/data-models/3-ranked_weekly_region_ngames.R")
}, error = function(e) {
  lorr::send_discord_message(
    username = "Weekly database cleanup",
    message = sprintf("Manual intervention required. (%s)", e$message)
  )
  print(e)
})
