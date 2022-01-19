# Runs analysis + shows error if anything happens
tryCatch({
  source("/home/balco/dev/lor-meta-report/job-scripts/api-calls/main-scripts/1-ASIA-mongo-scraper.R")
}, error = function(e) {
  RPushbullet::pbPost(
    "note", 
    title = "LoR-Meta ASIA server", 
    body = "There was an error. Restarting process..."
  )
  print(e)
}, finally = {
  Sys.sleep(180)
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/api-calls/1-launch-asia.R")
})