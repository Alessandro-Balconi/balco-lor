#Runs analysis + shows error if anything happens
cat(sprintf("--- %s --- \n", Sys.time()))

tryCatch({
  
  tictoc::tic()
  source("/home/balco/dev/lor-meta-report/job-scripts/mongo-to-sql/1-ranked-mongo-to-sql.R")
  cat("MongoDB to MySQL: "); tictoc::toc()

}, error = function(e) {
  lorr::send_discord_message(
    username = 'Daily MongoDB to MySQL Update',
    message = sprintf("Manual intervention required. (%s)", e$message)
  )
  print(e)
})

rm(list = ls(all.names = TRUE))

tryCatch({
  
  tictoc::tic()
  source("/home/balco/dev/lor-meta-report/job-scripts/daily-data-models/utils_archetype_aggregation.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/daily-data-models/ranked_daily_archetypes.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/data-models/2-ranked_patch_decklists.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/data-models/7-ranked_patch_matchups.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/data-models/8-ranked_patch_archetypes.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/daily-data-models/utils_ranked_patch_decklists_cards.R")
  cat("Data Models: "); tictoc::toc()
  
}, error = function(e) {
  lorr::send_discord_message(
    username = 'Daily Data Models Update',
    message = sprintf("Manual intervention required. (%s)", e$message)
  )
  print(e)
})

rm(list = ls(all.names = TRUE))

tryCatch({
  
  tictoc::tic()
  source("/home/balco/dev/lor-meta-report/job-scripts/daily-tweets/master_leaderboard_updates.R")
  cat("Twitter Posts: "); tictoc::toc()
  
}, error = function(e) {
  lorr::send_discord_message(
    username = 'Daily Twitter Posts Update',
    message = sprintf("Manual intervention required. (%s)", e$message)
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
  gs_list <- list.files('/home/balco/dev/lor-meta-report/job-scripts/daily-google-spreadsheets')
  
  # function to update spreadsheet
  update_ss <- function(ss){
    source(paste0("/home/balco/dev/lor-meta-report/job-scripts/daily-google-spreadsheets/", ss))
  }
  
  # apply function to all spreadsheets
  lapply(X = gs_list, FUN = update_ss)
  
  # log message after finishing updates
  cat("Google Spreadsheets: "); tictoc::toc()
  
}, error = function(e) {
  lorr::send_discord_message(
    username = 'Daily Google Spreadsheets Update',
    message = sprintf("Manual intervention required. (%s)", e$message)
  )
  print(e)
})