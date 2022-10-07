# Runs analysis + shows error if anything happens
tryCatch({
  source("/home/balco/dev/lor-meta-report/other-scripts/seasonal-open-rounds/open_rounds_main_script.R")
}, error = function(e) {
  lorr::send_discord_message(
    username = 'Seasonal Open Rounds Spreadsheet',
    message = sprintf("Restarting process... (%s)", e$message)
  )
  print(e)
}, finally = {
  Sys.sleep(60)
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/other-scripts/seasonal-open-rounds/open_rounds_launcher.R")
})