# Runs analysis + shows error if anything happens
tryCatch({
  source("/home/balco/dev/lor-meta-report/job-scripts/api-calls/main-scripts/2-AMERICAS-mongo-scraper.R")
}, error = function(e) {
  RPushbullet::pbPost(
    "note", 
    title = "LoR-Meta AMERICAS server", 
    body = "There was an error during API calls for AMERICAS server. Manual restart required."
  )
  print(e)
}, finally = {
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/api-calls/main-scripts/2-AMERICAS-mongo-scraper.R")
})