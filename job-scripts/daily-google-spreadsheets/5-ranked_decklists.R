# most played decklists for angrybacteria

suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(googlesheets4)) # manage google sheets API

# create connection to MySQL database
con <- lorr::create_db_con()

# id of the spreadsheet 
ss_id <- "1l9I-g7XboyaBt4wMD3c8E9rDrTpxfPLDR3-_Z24fMPU"

# pick 1000 most played decks of the patch, last_7d, last_3d
list_df <- map(
  .x = c('patch' = 0, 'last_7d' = 1, 'last_3d' = 2),
  .f = function(x){
    tbl(con, 'ranked_patch_decklists') %>% 
      filter(time_frame >= x) %>% 
      group_by(archetype, deck_code) %>% 
      summarise(across(c(match, win), sum, na.rm = TRUE), .groups = 'drop') %>% 
      arrange(desc(match)) %>% 
      head(2000) %>% 
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
sheet_write(data = list_df[['last_3d']], ss = ss_id, sheet = 'Plat+ - Last 3 days')
sheet_write(data = list_df[['last_7d']], ss = ss_id, sheet = 'Plat+ - Last 7 days')
sheet_write(data = list_df[['patch'  ]], ss = ss_id, sheet = 'Plat+ - Current Patch')
sheet_write(data = list_df_master[['last_3d']], ss = ss_id, sheet = 'Master - Last 3 days')
sheet_write(data = list_df_master[['last_7d']], ss = ss_id, sheet = 'Master - Last 7 days')
sheet_write(data = list_df_master[['patch'  ]], ss = ss_id, sheet = 'Master - Current Patch')


# additional information
update <- sprintf("Last update: %s UTC", Sys.time())
info <- tibble(" " = c(update))
sheet_write(data = info,   ss = ss_id, sheet = "Data Information")

# names of the spreadsheet to update
ss_names <- sheet_names(ss_id)

# adjust spacing of columns in the spreadsheet
walk(.x = ss_names, .f = ~range_autofit(ss = ss_id, sheet = .))

DBI::dbDisconnect(con)