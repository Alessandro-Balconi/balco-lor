# Runs analysis + shows error if anything happens
# This script runs every Wednesday in the morning
tryCatch({
  cat(sprintf("--- %s --- \n", Sys.Date()))
  source("/home/balco/dev/lor-meta-report/job-scripts/weekly-db-cleanup/5-weekly-mongodb-cleanup.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/weekly-db-cleanup/weekly-mysql-cleanup.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/data-models/3-ranked_region_weekly_ngames.R")
}, error = function(e) {
  RPushbullet::pbPost(
    "note",
    title = "Weekly database cleanup",
    body = "There was an error during the database cleanup. Manual intervention required."
  )
  print(e)
})