# Runs analysis + shows error if anything happens
tryCatch({
  source("/home/balco/dev/lor-meta-report/job-scripts/api-calls/main-scripts/1-ASIA-mongo-scraper.R")
}, error = function(e) {
  discordr::create_discord_connection(
    webhook = 'https://discord.com/api/webhooks/940930457070096444/qBSYJH0KETu992oDrdJBH20H1j4yPbBMZm2T3KNKZA5AU1LhRypZshQ0uKly9N_7jeGy',
    username = 'LoR-Meta ASIA server'
  ) %>% 
    discordr::send_webhook_message(
      message = sprintf("Restarting process... (%s)", e$message), 
      conn = .
    )
  Sys.sleep(180)
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/api-calls/1-launch-asia.R")
})