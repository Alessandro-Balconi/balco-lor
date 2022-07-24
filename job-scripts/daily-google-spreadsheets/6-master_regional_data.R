# google spreadshett update optimization ----
suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(googlesheets4)) # manage google sheets API

# load mysql db credentials
db_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# create connection to MySQL database
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  db_host = "127.0.0.1",
  user = db_creds$uid,
  password = db_creds$pwd,
  dbname = db_creds$dbs
)

update_spreadsheet <- function(input_region, n, input_time_frame, is_master, ss_id){
  
  # ndays to use to check for patches
  patch_ndays <- switch(as.character(input_time_frame), "0" = 1e3, "1" = 7, "2" = 3, "3" = 1)
  
  # date to be considered
  last_n_days <- local(Sys.time()-lubridate::days(patch_ndays))
  
  # most played archetypes
  top <- tbl(con, 'ranked_patch_matchups') %>% 
    filter(is_master == 1, time_frame >= local(input_time_frame), region == local(input_region)) %>% 
    group_by(archetype_1) %>%
    summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>% 
    arrange(desc(n)) %>% 
    head(n = local(n)) %>% 
    select(archetype_1) %>% 
    collect() %>% 
    pull()
  
  # same but wrapped (nicer looking in spreadsheets)
  top_wrap <- str_wrap(top, width = 8)
  
  # collect matchups data
  x <- tbl(con, 'ranked_patch_matchups') %>%
    filter(is_master == 1, time_frame >= local(input_time_frame), region == local(input_region)) %>% 
    mutate(archetype_1 = if_else(archetype_1 %in% top, archetype_1, 'Other'), archetype_2 = if_else(archetype_2 %in% top, archetype_2, 'Other')) %>% 
    group_by(archetype_1, archetype_2) %>% 
    summarise(across(c(n, win), sum, na.rm = TRUE), .groups = 'drop') %>% 
    mutate(winrate = win / n) %>% 
    select(archetype_1, archetype_2, n, winrate) %>% 
    collect()
  
  # adjust winrate (so that the sum is 100%)
  x <- x %>% 
    left_join(x, by = c('archetype_1' = 'archetype_2', "archetype_2" = "archetype_1")) %>% 
    mutate(across(starts_with("winrate."), .fns = list(function(x) x / (winrate.x + winrate.y)), .names = "{col}_adj")) %>% 
    mutate(n_adj = round((n.x + n.y) / 2, digits = 0)) %>% 
    select(archetype_1, archetype_2, winrate = winrate.x_adj, n = n_adj)
  
  # total number of games played
  total_n <- tbl(con, 'ranked_patch_matchups') %>% 
    filter(is_master == 1, time_frame >= local(input_time_frame), region == local(input_region)) %>% 
    summarise(full_n = sum(n, na.rm = TRUE)) %>% 
    collect() %>% 
    pull()
  
  # top decks info (playrate)
  y <- tbl(con, 'ranked_patch_matchups') %>% 
    filter(is_master == 1, time_frame >= local(input_time_frame), region == local(input_region), archetype_1 %in% top) %>%
    group_by(archetype_1) %>% 
    summarise(games_played = sum(n, na.rm = TRUE), .groups = "drop") %>% 
    collect() %>% 
    mutate(playrate = (games_played / total_n)*100) %>% 
    mutate(playrate = round(playrate, digits = 2)) %>% 
    arrange(-games_played)
  
  # top decks info (winrate)
  yy_wr <- tbl(con, 'ranked_patch_matchups') %>% 
    filter(is_master == 1, time_frame >= local(input_time_frame), region == local(input_region), archetype_1 %in% top) %>%
    group_by(archetype_1) %>% 
    summarise(across(c(n, win), sum, na.rm = TRUE)) %>% 
    mutate(winrate = win / n) %>% 
    select(archetype_1, winrate) %>% 
    collect()

  # prepare matchup table
  x <- x %>% 
    distinct() %>% 
    mutate(across(c(winrate, n), function(x) ifelse(archetype_1 == archetype_2, NA, x))) %>% 
    mutate(across(archetype_2, str_wrap, width = 8)) %>% 
    mutate(across(archetype_1, factor, levels = c(top, 'Other'), ordered = TRUE)) %>% 
    mutate(across(archetype_2, factor, levels = c(top_wrap, 'Other'), ordered = TRUE)) %>% 
    arrange(archetype_1, archetype_2)
  
  # deck information
  deck_info <- y %>% 
    left_join(yy_wr, by = "archetype_1") %>% 
    mutate(winrate = round(winrate*100, digits = 2)) %>% 
    rename(" " = archetype_1)
  
  # generate table with winrates    
  x_wr <- x %>% 
    select(-n) %>%
    mutate(winrate = round(winrate*100, digits = 1)) %>% 
    pivot_wider(names_from = archetype_2, values_from = winrate) %>% 
    rename(" " = archetype_1)
  
  deck_codes <- tbl(con, 'ranked_patch_decklists') %>% 
    filter(is_master == 1, time_frame >= local(input_time_frame), region == local(input_region)) %>% 
    count(deck_code, wt = match) %>% 
    filter(n >= 5) %>% 
    collect() %>% 
    pull(deck_code)
  
  # extract data from MySQL
  list_df_master <- tbl(con, 'ranked_patch_decklists') %>% 
    filter(is_master == 1, time_frame >= local(input_time_frame), region == local(input_region), deck_code %in% deck_codes) %>% 
    group_by(archetype, deck_code) %>% 
    summarise(across(c(match, win), sum, na.rm = TRUE), .groups = 'drop') %>% 
    arrange(desc(match)) %>% 
    head(1000) %>% 
    collect() %>% 
    mutate(winrate = scales::percent(win / match, accuracy = .1)) %>% 
    select(-win)

  # update all sheets of the spreadsheet
  sheet_write(data = deck_info, ss = ss_id, sheet = sprintf("%s - %s days - Meta", str_to_title(input_region), patch_ndays))
  sheet_write(data = x_wr, ss = ss_id, sheet = sprintf("%s - %s days - Matchups", str_to_title(input_region), patch_ndays))
  sheet_write(data = list_df_master, ss = ss_id, sheet = sprintf("%s - %s days - Decklists", str_to_title(input_region), patch_ndays))

}

id_ss <- "1BBQCxDkrdG-CIZAqdMV-wAxnm64O9Rk1y0t2hIfsVgU"

# update all tables
update_spreadsheet(input_region = 'europe',   n = 25, input_time_frame = 3, is_master = 1, ss_id = id_ss)
update_spreadsheet(input_region = 'europe',   n = 40, input_time_frame = 2, is_master = 1, ss_id = id_ss)
update_spreadsheet(input_region = 'americas', n = 40, input_time_frame = 2, is_master = 1, ss_id = id_ss)
update_spreadsheet(input_region = 'europe',   n = 40, input_time_frame = 1, is_master = 1, ss_id = id_ss)
update_spreadsheet(input_region = 'americas', n = 40, input_time_frame = 1, is_master = 1, ss_id = id_ss)

# additional information
update <- sprintf("Last update: %s UTC", Sys.time())
info <- tibble(" " = c(update))
with_gs4_quiet(sheet_write(data = info, ss = id_ss, sheet = "Data Information"))

# names of the spreadsheet to update
ss_names <- sheet_names(id_ss)

# adjust spacing of columns in the spreadsheet
walk(.x = ss_names, .f = ~range_autofit(ss = id_ss, sheet = ., dimension = "columns"))

DBI::dbDisconnect(con)
