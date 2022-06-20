# Runs analysis + shows error if anything happens
# This script runs every Wednesday in the morning
tryCatch({
  cat(sprintf("--- %s --- \n", Sys.Date()))
  source("/home/balco/dev/lor-meta-report/job-scripts/weekly-db-cleanup/5-weekly-mongodb-cleanup.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/weekly-db-cleanup/weekly-mysql-cleanup.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/data-models/3-ranked_weekly_region_ngames.R")
}, error = function(e) {
  discordr::create_discord_connection(
    webhook = 'https://discord.com/api/webhooks/940930457070096444/qBSYJH0KETu992oDrdJBH20H1j4yPbBMZm2T3KNKZA5AU1LhRypZshQ0uKly9N_7jeGy',
    username = "Weekly database cleanup"
  ) %>% 
    discordr::send_webhook_message(
      message = sprintf("Manual intervention required. (%s)", e$message), 
      conn = .
    )
  print(e)
})
