# Runs daily jobs --------------------------------------------------------------
cat(sprintf("--- %s --- \n", Sys.time()))

# Send new matches from MongoDB to MySQL ---------------------------------------
lorr::launch_scripts_in_folder(
  folder = "/home/balco/dev/lor-meta-report/job-scripts/daily-mongo-to-sql/",
  log_message = "MongoDB to MySQL: ",
  discord_username = "Daily MongoDB to MySQL Update",
  discord_message = "Manual intervention required. "
)

# Update data models -----------------------------------------------------------
lorr::launch_scripts_in_folder(
  folder = "/home/balco/dev/lor-meta-report/job-scripts/daily-data-models/",
  log_message = "Data Models: ",
  discord_username = "Daily Data Models Update",
  discord_message = "Manual intervention required. "
)

# Send twitter posts -----------------------------------------------------------
lorr::launch_scripts_in_folder(
  folder = "/home/balco/dev/lor-meta-report/job-scripts/daily-tweets/",
  log_message = "Twitter Posts: ",
  discord_username = "Daily Twitter Posts Update",
  discord_message = "Manual intervention required. "
)

# Update Google spreadsheets ---------------------------------------------------
# set google API Key & Oauth credentials
options(googlesheets4_quiet = TRUE)
google_creds <- config::get("google", file = "/home/balco/my_rconfig.yml")
invisible(gargle::oauth_app_from_json(google_creds$client_secret))
googlesheets4::gs4_auth_configure(api_key = google_creds$api_key)
googlesheets4::gs4_auth(path = google_creds$auth_path)

lorr::launch_scripts_in_folder(
  folder = "/home/balco/dev/lor-meta-report/job-scripts/daily-google-spreadsheets/",
  log_message = "Google Spreadsheets: ",
  discord_username = "Daily Google Spreadsheets Update",
  discord_message = "Manual intervention required. "
)
