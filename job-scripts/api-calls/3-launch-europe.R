# Runs analysis + shows error if anything happens
tryCatch({
  source("/home/balco/dev/lor-meta-report/job-scripts/api-calls/main-scripts/3-EUROPE-mongo-scraper.R")
}, error = function(e) {
  lorr::send_discord_message(
    username = 'LoR-Meta EUROPE server',
    message = sprintf("Restarting process... (%s)", e$message)
  )
  print(e)
}, finally = {
  Sys.sleep(180)
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/api-calls/3-launch-europe.R")
})