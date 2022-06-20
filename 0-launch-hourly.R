#Runs analysis + shows error if anything happens
tryCatch({
  source("/home/balco/dev/lor-meta-report/job-scripts/update-leaderboard/update_leaderboard.R")
}, error = function(e) {
  discordr::create_discord_connection(
    webhook = 'https://discord.com/api/webhooks/940930457070096444/qBSYJH0KETu992oDrdJBH20H1j4yPbBMZm2T3KNKZA5AU1LhRypZshQ0uKly9N_7jeGy',
    username = "Hourly leaderboard update"
  ) %>% 
    discordr::send_webhook_message(
      message = "There was an error during the leaderboard update. Manual intervention required.", 
      conn = .
    )
  print(e)
})