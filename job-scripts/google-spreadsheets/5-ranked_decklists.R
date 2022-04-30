# most played decklists for angrybacteria

suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(googlesheets4)) # manage google sheets API

options(gargle_oauth_email = "Balco21@outlook.it")
options(googlesheets4_quiet = TRUE)

# load mysql db credentials
db_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# create connection to MySQL database
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  db_host  = "127.0.0.1",
  user     = db_creds$uid,
  password = db_creds$pwd,
  dbname   = db_creds$dbs
)

# id of the spreadsheet 
ss_id <- "1l9I-g7XboyaBt4wMD3c8E9rDrTpxfPLDR3-_Z24fMPU"

# pick 1000 most played decks of the patch, last_7d, last_3d
list_df <- map(
  .x = c('patch' = 0, 'last_7d' = 1, 'last_3d' = 2),
  .f = function(x){
    tbl(con, 'ranked_patch_decklists') %>% 
      filter(time_frame >= x) %>% 
      mutate(win = match*winrate) %>% 
      group_by(archetype, deck_code) %>% 
      summarise(across(c(match, win), sum, na.rm = TRUE), .groups = 'drop') %>% 
      arrange(desc(match)) %>% 
      head(1000) %>% 
      collect() %>% 
      mutate(winrate = scales::percent(win / match, accuracy = .1)) %>% 
      select(-win)
  }
)

# same but filtering only master data
list_df_master <- map(
  .x = c('patch' = 0, 'last_7d' = 1, 'last_3d' = 2),
  .f = function(x){
    tbl(con, 'ranked_patch_decklists') %>% 
      filter(time_frame >= x, is_master == 1) %>% 
      mutate(win = match*winrate) %>% 
      group_by(archetype, deck_code) %>% 
      summarise(across(c(match, win), sum, na.rm = TRUE), .groups = 'drop') %>% 
      arrange(desc(match)) %>% 
      head(1000) %>% 
      collect() %>% 
      mutate(winrate = scales::percent(win / match, accuracy = .1)) %>% 
      select(-win)
  }
)

# update all sheets of the spreadsheet
with_gs4_quiet(sheet_write(data = list_df[['last_3d']], ss = ss_id, sheet = 'Plat+ - Last 3 days'))
with_gs4_quiet(sheet_write(data = list_df[['last_7d']], ss = ss_id, sheet = 'Plat+ - Last 7 days'))
with_gs4_quiet(sheet_write(data = list_df[['patch'  ]], ss = ss_id, sheet = 'Plat+ - Current Patch'))
with_gs4_quiet(sheet_write(data = list_df_master[['last_3d']], ss = ss_id, sheet = 'Master - Last 3 days'))
with_gs4_quiet(sheet_write(data = list_df_master[['last_7d']], ss = ss_id, sheet = 'Master - Last 7 days'))
with_gs4_quiet(sheet_write(data = list_df_master[['patch'  ]], ss = ss_id, sheet = 'Master - Current Patch'))


# additional information
update <- sprintf("Last update: %s UTC", Sys.time())
info <- "WIPWIPWIP"
info <- tibble(" " = c(update, info))
with_gs4_quiet(sheet_write(data = info,   ss = ss_id, sheet = "Data Information"))

# names of the spreadsheet to update
ss_names <- sheet_names(ss_id)

# adjust spacing of columns in the spreadsheet
map(
  .x = ss_names,
  .f = ~with_gs4_quiet(range_autofit(ss = ss_id, sheet = ., dimension = "columns"))
)

DBI::dbDisconnect(con)