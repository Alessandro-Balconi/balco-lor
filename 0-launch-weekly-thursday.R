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