# google spreadshett update optimization ----
suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(googlesheets4)) # manage google sheets API

options(gargle_oauth_email = "Balco21@outlook.it")
options(googlesheets4_quiet = TRUE)

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

min_ndays <- local(Sys.time()-lubridate::days(7))

# pull matchup data
data_matchup_v2 <- tbl(con, "ranked_match_metadata_30d") %>%
  filter(game_start_time_utc >= min_ndays) %>% 
  left_join(tbl(con, 'ranked_match_info_30d'), by = 'match_id') %>% 
  left_join(tbl(con, 'utils_archetype_aggregation'), by = c('archetype' = 'old_name')) %>% 
  mutate(archetype = coalesce(new_name, archetype)) %>% 
  mutate(time_frame = case_when(game_start_time_utc >= local(Sys.time()-lubridate::days(3)) ~ 2, game_start_time_utc >= local(Sys.time()-lubridate::days(7)) ~ 1, TRUE ~ 0)) %>% 
  filter(player_rank == 2) %>% 
  select(match_id, game_outcome, archetype, time_frame, region) %>% 
  collect()

# prepare matchup data
data_matchup_v2 <- data_matchup_v2 %>% 
  arrange(match_id, archetype) %>% 
  group_by(match_id) %>% 
  mutate(id = row_number()) %>% 
  ungroup()

# prepare opponent matchup data
opponent_data <- data_matchup_v2 %>% 
  mutate(id = case_when(id == 1 ~ 2, id == 2 ~ 1, TRUE ~ 0)) %>% 
  select(match_id, id, archetype_2 = archetype)

# join the 2
data_matchup_v2 <- data_matchup_v2 %>% 
  left_join(opponent_data, by = c("match_id", "id")) %>% 
  select(-id) %>% 
  rename(archetype_1 = archetype)

# number of matches
n_match <- data_matchup_v2 %>% 
  count(archetype_1, archetype_2, time_frame, region)

data_matchup_v2 <- data_matchup_v2 %>%
  group_by(archetype_1, archetype_2, time_frame, region) %>% 
  summarise(wins = sum(game_outcome == "win"), .groups = "drop") %>% 
  left_join(n_match, by = c("archetype_1", "archetype_2", "time_frame", "region")) %>% 
  mutate(winrate = ifelse(archetype_1 == archetype_2, 0.5, wins / n)) %>% 
  select(-wins)

update_spreadsheet <- function(input_region, n, input_time_frame, is_master, ss_id){
  
  # ndays to use to check for patches
  patch_ndays <- switch(as.character(input_time_frame), "0" = 1e3, "1" = 7, "2" = 3)
  
  # date to be considered
  last_n_days <- local(Sys.time()-lubridate::days(patch_ndays))
  
  # most played archetypes
  top <- data_matchup_v2 %>% 
    filter(time_frame >= local(input_time_frame), region == local(input_region)) %>% 
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
  x <- data_matchup_v2 %>%
    filter(time_frame >= local(input_time_frame), region == local(input_region)) %>% 
    mutate(archetype_1 = if_else(archetype_1 %in% top, archetype_1, 'Other'), archetype_2 = if_else(archetype_2 %in% top, archetype_2, 'Other')) %>% 
    mutate(wins = n*winrate) %>% 
    group_by(archetype_1, archetype_2) %>% 
    summarise(n = sum(n, na.rm = TRUE), wins = sum(wins, na.rm = TRUE), .groups = 'drop') %>% 
    mutate(winrate = wins / n) %>% 
    select(archetype_1, archetype_2, n, winrate) %>% 
    collect()
  
  # adjust winrate (so that the sum is 100%)
  x <- x %>% 
    left_join(x, by = c('archetype_1' = 'archetype_2', "archetype_2" = "archetype_1")) %>% 
    mutate(across(starts_with("winrate."), .fns = list(function(x) x / (winrate.x + winrate.y)), .names = "{col}_adj")) %>% 
    mutate(n_adj = round((n.x + n.y) / 2, digits = 0)) %>% 
    select(archetype_1, archetype_2, winrate = winrate.x_adj, n = n_adj)
  
  # total number of games played
  total_n <- data_matchup_v2 %>% 
    filter(time_frame >= local(input_time_frame), region == local(input_region)) %>% 
    summarise(full_n = sum(n, na.rm = TRUE)) %>% 
    collect() %>% 
    pull()
  
  # top decks info (playrate)
  y <- data_matchup_v2 %>% 
    filter(time_frame >= local(input_time_frame), region == local(input_region), archetype_1 %in% top) %>%
    group_by(archetype_1) %>% 
    summarise(games_played = sum(n, na.rm = TRUE), .groups = "drop") %>% 
    collect() %>% 
    mutate(playrate = (games_played / total_n)*100) %>% 
    mutate(playrate = round(playrate, digits = 2)) %>% 
    arrange(-games_played)
  
  # top decks info (winrate)
  y_wr <- data_matchup_v2 %>% 
    filter(time_frame >= local(input_time_frame), region == local(input_region), archetype_1 %in% top) %>%
    collect()
  
  yy_wr <- y_wr %>% 
    group_by(archetype_1) %>% 
    summarise(winrate = weighted.mean(winrate, w = n, na.rm = TRUE), .groups = "drop")
  
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
  
  deck_codes <- tbl(con, 'ranked_match_metadata_30d') %>% 
    filter(game_start_time_utc >= last_n_days, region == local(input_region)) %>% 
    left_join(tbl(con, 'ranked_match_info_30d'), by = 'match_id') %>% 
    count(deck_code) %>% 
    filter(n >= 5) %>% 
    collect() %>% 
    pull(deck_code)
  
  # extract data from MySQL
  data_decks_v2 <- tbl(con, "ranked_match_metadata_30d") %>%
    filter(game_start_time_utc >= last_n_days, region == local(input_region)) %>% 
    left_join(tbl(con, 'ranked_match_info_30d'), by = 'match_id') %>% 
    filter(deck_code %in% deck_codes) %>% 
    left_join(tbl(con, 'utils_archetype_aggregation'), by = c('archetype' = 'old_name')) %>% 
    mutate(archetype = coalesce(new_name, archetype)) %>% 
    mutate(time_frame = case_when(
      game_start_time_utc >= local(Sys.Date()-lubridate::days(3)) ~ 2, 
      game_start_time_utc >= local(Sys.Date()-lubridate::days(7)) ~ 1, 
      TRUE ~ 0
    )) %>% 
    mutate(is_master = if_else(player_rank == 2, 1, 0)) %>% 
    count(game_outcome, archetype, deck_code, time_frame, is_master) %>% 
    collect() %>% 
    ungroup()
  
  # calculate information
  data_decks_v2 <- data_decks_v2 %>% 
    mutate(across(where(bit64::is.integer64), as.numeric)) %>% 
    pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0)
  
  # reshape, fill, keep only relevant info
  data_decks_v2 <- data_decks_v2 %>%
    {if(!'tie' %in% colnames(.)) mutate(., tie = 0) else . } %>% 
    mutate(match = win + loss + tie) %>% 
    {if(nrow(.)>0) mutate(., winrate = win / match) else . } %>% 
    {if(nrow(.)>0) select(., archetype, deck_code, match, winrate, time_frame, is_master) else . }
  
  # keep only relevant info
  list_df_master <- data_decks_v2 %>% 
        filter(time_frame >= local(input_time_frame), is_master == 1) %>% 
        mutate(win = match*winrate) %>% 
        group_by(archetype, deck_code) %>% 
        summarise(across(c(match, win), sum, na.rm = TRUE), .groups = 'drop') %>% 
        arrange(desc(match)) %>% 
        head(1000) %>% 
        mutate(winrate = scales::percent(win / match, accuracy = .1)) %>% 
        select(-win)
  
  # update all sheets of the spreadsheet
  with_gs4_quiet(sheet_write(data = deck_info, ss = ss_id, sheet = sprintf("%s - %s days - Meta", str_to_title(input_region), patch_ndays)))
  with_gs4_quiet(sheet_write(data = x_wr, ss = ss_id, sheet = sprintf("%s - %s days - Matchups", str_to_title(input_region), patch_ndays)))
  with_gs4_quiet(sheet_write(data = list_df_master, ss = ss_id, sheet = sprintf("%s - %s days - Decklists", str_to_title(input_region), patch_ndays)))

}

id_ss <- "1BBQCxDkrdG-CIZAqdMV-wAxnm64O9Rk1y0t2hIfsVgU"

# update all tables
update_spreadsheet(input_region = 'europe',   n = 40, input_time_frame = 2, is_master = 1, ss_id = id_ss)
update_spreadsheet(input_region = 'americas', n = 40, input_time_frame = 2, is_master = 1, ss_id = id_ss)
update_spreadsheet(input_region = 'europe',   n = 40, input_time_frame = 1, is_master = 1, ss_id = id_ss)
update_spreadsheet(input_region = 'americas', n = 40, input_time_frame = 1, is_master = 1, ss_id = id_ss)

# additional information
update <- sprintf("Last update: %s UTC", Sys.time())
info <- sprintf("WIPWIPWIP")
info <- tibble(" " = c(update, info))
with_gs4_quiet(sheet_write(data = info, ss = id_ss, sheet = "Data Information"))

# names of the spreadsheet to update
ss_names <- sheet_names(id_ss)

# adjust spacing of columns in the spreadsheet
map(
  .x = ss_names,
  .f = ~with_gs4_quiet(range_autofit(ss = id_ss, sheet = ., dimension = "columns"))
)

DBI::dbDisconnect(con)
