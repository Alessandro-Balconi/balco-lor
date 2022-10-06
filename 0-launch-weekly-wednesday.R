# Runs analysis + shows error if anything happens
# This script runs every Wednesday (after the daily reports, around 18.30 CEST)
tryCatch({
  cat(sprintf("--- %s --- \n", Sys.Date()))
  Sys.setenv(RSTUDIO_PANDOC = "/usr/lib/rstudio-server/bin/quarto/bin/tools")
  tictoc::tic()
  source("/home/balco/dev/lor-meta-report/job-scripts/weekly-report/1-mysql-report.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/weekly-report/2-publish_report.R")
  tictoc::toc()
}, error = function(e) {
  lorr::send_discord_message(
    username = 'Weekly Report Release',
    message = sprintf("Manual intervention required. (%s)", e$message)
  )
  print(e)
})