# Runs analysis + shows error if anything happens
# This script runs every Thursday (in the morning, around 10.00 CEST)
tryCatch({
  cat(sprintf("--- %s --- \n", Sys.Date()))
  source("/home/balco/dev/lor-meta-report/job-scripts/data-models/4-utils_patch_history.R")
}, error = function(e) {
  discordr::create_discord_connection(
    webhook = 'https://discord.com/api/webhooks/940930457070096444/qBSYJH0KETu992oDrdJBH20H1j4yPbBMZm2T3KNKZA5AU1LhRypZshQ0uKly9N_7jeGy',
    username = "Weekly Patch History Update"
  ) %>% 
    discordr::send_webhook_message(
      message = sprintf("Manual intervention required. (%s)", e$message), 
      conn = .
    )
  print(e)
})

rm(list = ls(all.names = TRUE))
tryCatch({
  tictoc::tic()
  #source("/home/balco/dev/lor-meta-report/job-scripts/mongo-to-sql/9-expeditions-mongo-to-sql.R")
  #rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/weekly-db-cleanup/expedition-mysql-cleanup.R")
  #rm(list = ls(all.names = TRUE))
  #source("/home/balco/dev/lor-meta-report/job-scripts/data-models/1-expedition_cards.R")
  tictoc::toc()
}, error = function(e) {
  discordr::create_discord_connection(
    webhook = 'https://discord.com/api/webhooks/940930457070096444/qBSYJH0KETu992oDrdJBH20H1j4yPbBMZm2T3KNKZA5AU1LhRypZshQ0uKly9N_7jeGy',
    username = "Weekly Expedition Update"
  ) %>% 
    discordr::send_webhook_message(
      message = sprintf("Manual intervention required. (%s)", e$message), 
      conn = .
    )
  print(e)
})

rm(list = ls(all.names = TRUE))
tryCatch({
  tictoc::tic()

  #options(gargle_oauth_email = "Balco21@outlook.it")
  options(googlesheets4_quiet = TRUE)
  
  # set google API Key & Oauth credentials
  google_creds <- config::get("google", file = "/home/balco/my_rconfig.yml")
  gargle::oauth_app_from_json(google_creds$client_secret)
  googlesheets4::gs4_auth_configure(api_key = google_creds$api_key)
  googlesheets4::gs4_auth(path = google_creds$auth_path)
  
  # list of files to update
  gs_list <- list.files('/home/balco/dev/lor-meta-report/job-scripts/weekly-google-spreadsheets')
  
  # function to update spreadsheet
  update_ss <- function(ss){
    source(paste0("/home/balco/dev/lor-meta-report/job-scripts/weekly-google-spreadsheets/", ss))
  }
  
  # apply function to all spreadsheets
  lapply(X = gs_list, FUN = update_ss)
  
  tictoc::toc()
}, error = function(e) {
  discordr::create_discord_connection(
    webhook = 'https://discord.com/api/webhooks/940930457070096444/qBSYJH0KETu992oDrdJBH20H1j4yPbBMZm2T3KNKZA5AU1LhRypZshQ0uKly9N_7jeGy',
    username = "Weekly Google Spreadsheets Update"
  ) %>% 
    discordr::send_webhook_message(
      message = sprintf("Manual intervention required. (%s)", e$message), 
      conn = .
    )
  print(e)
})