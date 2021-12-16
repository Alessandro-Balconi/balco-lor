library(tidyverse)

db_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  db_host = "127.0.0.1",
  user = db_creds$uid,
  password = db_creds$pwd,
  dbname = db_creds$dbs
)

hotfix_time <- as.POSIXct("2021-12-14 18:00:00 UTC")
release_time <- as.POSIXct("2021-12-08 17:30:00 UTC")

n_games <- tbl(con, 'lor_match_info_v2') %>% 
  filter(str_detect(game_version, 'live_2_21_')) %>% 
  mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATETIME)")) %>% 
  mutate(hotfix = case_when(
    game_start_time_utc < release_time ~ 'old', 
    game_start_time_utc < hotfix_time ~ 'before', 
    game_start_time_utc >= hotfix_time ~ 'after', 
    TRUE ~ 'idk'
  )) %>%
  count(hotfix) %>% 
  collect()

# gp: 02BW032
# seedling: 05IO021
# poppy: 05BC041
# explorer: 05BC108
# wayfinder: 01IO050

x <- tbl(con, 'lor_match_info_v2') %>% 
  filter(str_detect(game_version, 'live_2_21_')) %>% 
  filter(str_detect(cards, '01IO050')) %>% 
  mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATETIME)")) %>% 
  mutate(hotfix = case_when(
    game_start_time_utc < release_time ~ 'old', 
    game_start_time_utc < hotfix_time ~ 'before', 
    game_start_time_utc >= hotfix_time ~ 'after', 
    TRUE ~ 'idk'
  )) %>%
  count(game_outcome, hotfix) %>% 
  collect()

x %>% 
  pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0) %>% 
  left_join(n_games, by = 'hotfix') %>% 
  rowwise() %>% 
  mutate(n_match = sum(c_across(c(win, loss, matches('tie'))))) %>% 
  ungroup() %>% 
  mutate(pr = n_match / n, wr = win / n_match) %>% 
  select(hotfix, pr, wr)

y <- tbl(con, 'lor_match_info_v2') %>% 
  filter(str_detect(game_version, 'live_2_21_')) %>% 
  filter(archetype == 'Ezreal Kennen (IO)') %>% 
  mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATETIME)")) %>% 
  filter(game_start_time_utc >= hotfix_time) %>%
  count(deck_code, game_outcome) %>% 
  collect()

y %>% 
  pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0) %>% 
  rowwise() %>% 
  mutate(n_match = sum(c_across(c(win, loss, matches('tie'))))) %>% 
  ungroup() %>% 
  mutate(wr = win / n_match) %>% 
  arrange(-n_match)
  