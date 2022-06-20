# Runs analysis + shows error if anything happens
# This script runs every Wednesday (after the daily reports, around 18.30 CEST)
tryCatch({
  cat(sprintf("--- %s --- \n", Sys.Date()))
  Sys.setenv(RSTUDIO_PANDOC = "/usr/lib/rstudio-server/bin/pandoc")
  tictoc::tic()
  source("/home/balco/dev/lor-meta-report/job-scripts/weekly-report/1-mysql-report.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/weekly-report/2-publish_report.R")
  tictoc::toc()
}, error = function(e) {
  discordr::create_discord_connection(
    webhook = 'https://discord.com/api/webhooks/940930457070096444/qBSYJH0KETu992oDrdJBH20H1j4yPbBMZm2T3KNKZA5AU1LhRypZshQ0uKly9N_7jeGy',
    username = "Weekly Report Release"
  ) %>% 
    discordr::send_webhook_message(
      message = "There was an error during the relase of the report. Manual intervention required.", 
      conn = .
    )
  print(e)
})