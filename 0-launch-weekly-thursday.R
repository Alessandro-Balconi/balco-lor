# Runs analysis + shows error if anything happens
# This script runs every Thursday (in the morning, around 10.00 CEST)
tryCatch({
  cat(sprintf("--- %s --- \n", Sys.Date()))
  source("/home/balco/dev/lor-meta-report/job-scripts/mongo-to-sql/8-mysql-patch-history.R")
}, error = function(e) {
  RPushbullet::pbPost(
    "note",
    title = "Weekly Patch History Update",
    body = "There was an error during the patch history update. Manual intervention required."
  )
  print(e)
})

rm(list = ls(all.names = TRUE))
tryCatch({
  tictoc::tic()
  source("/home/balco/dev/lor-meta-report/job-scripts/mongo-to-sql/9-expeditions-mongo-to-sql.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/weekly-db-cleanup/expedition-mysql-cleanup.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/data-models/1-expeditions_cards.R")
  tictoc::toc()
}, error = function(e) {
  RPushbullet::pbPost(
    "note",
    title = "Weekly Expedition Update",
    body = "There was an error during the expedition update. Manual intervention required."
  )
  print(e)
})