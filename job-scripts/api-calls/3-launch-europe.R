# Runs analysis + shows error if anything happens
tryCatch({
  source("/home/balco/dev/lor-meta-report/job-scripts/api-calls/main-scripts/3-EUROPE-mongo-scraper.R")
}, error = function(e) {
  RPushbullet::pbPost(
    "note", 
    title = "LoR-Meta EUROPE server", 
    body = "There was an error during API calls for EUROPE server. Manual restart required."
  )
  print(e)
})