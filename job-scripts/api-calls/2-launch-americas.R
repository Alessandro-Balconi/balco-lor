# Runs analysis + shows error if anything happens
tryCatch({
  source("/home/balco/dev/lor-meta-report/job-scripts/api-calls/main-scripts/2-AMERICAS-mongo-scraper.R")
}, error = function(e) {
  RPushbullet::pbPost(
    "note", 
    title = "LoR-Meta AMERICAS server", 
    body = sprintf("Restarting process... (%s)", e$message)
  )
  print(e)
}, finally = {
  Sys.sleep(180)
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/api-calls/2-launch-americas.R")
})