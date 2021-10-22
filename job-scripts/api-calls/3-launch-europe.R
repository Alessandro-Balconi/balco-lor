# Runs analysis + shows error if anything happens
tryCatch({
  source("/home/balco/dev/lor-meta-report/job-scripts/api-calls/main-scripts/3-EUROPE-mongo-scraper.R")
}, error = function(e) {
  RPushbullet::pbPost(
    "note", 
    title = "LoR-Meta EUROPE server", 
    body = "Restarting process..."
  )
  print(e)
}, finally = {
  Sys.sleep(180)
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/api-calls/3-launch-europe.R")
})