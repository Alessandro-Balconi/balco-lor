min_n_tot = 100         # minimum number of games played 
min_share = 0.8         # minimum percentage of games played in a single shard
min_wr = 0.5            # minimum winrate
master_only = TRUE      # filter only master data?
min_date = "2022-01-19" # minimum date

# REMEMBER TO LOAD PACKAGES AND CONNECT TO DB !

current_patch <- tbl(con, "utils_patch_history") %>% 
  collect() %>% 
  arrange(desc(release_date)) %>% 
  mutate(new_change = lag(change)) %>% 
  replace_na(list(new_change = 0)) %>% 
  mutate(cum_change = cumsum(new_change)) %>% 
  filter(cum_change == min(cum_change)) %>% 
  pull(patch_regex) %>% 
  paste0(collapse = "|")

# start collecting matches only 24 hours after the patch (or check if there is min_date)
if(exists('min_date')){
  
  min_date <- ymd(min_date)
  
} else {
  
  min_date <- tbl(con, "lor_match_info_v2") %>% 
    filter(str_detect(game_version, current_patch)) %>%
    select(game_start_time_utc) %>% 
    mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATETIME)")) %>% 
    summarise(min_date = min(game_start_time_utc, na.rm = TRUE)) %>% 
    collect() %>% 
    mutate(min_date = min_date + lubridate::days(1)) %>% 
    pull()
  
}

x = tbl(con, 'lor_match_info_v2') %>% 
  filter(str_detect(game_version, current_patch)) %>%
  mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATE)")) %>% 
  filter(game_start_time_utc >= min_date) %>% 
  {if(master_only) filter(., is_master == 1) else . } %>% 
  count(region, archetype, game_outcome) %>% 
  ungroup() %>% 
  collect()

x = x %>% 
  mutate(n = as.numeric(n)) %>% 
  pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0) %>% 
  rowwise() %>% 
  mutate(n = sum(c_across(c(win, loss, matches('tie'))))) %>% 
  ungroup() %>% 
  select(region, archetype, win, n)

x_n = x %>% 
  select(-win) %>% 
  pivot_wider(names_from = region, values_from = n, values_fill = 0) %>% 
  mutate(n_tot = asia + europe + americas)

res = x_n %>% 
  filter(n_tot >= min_n_tot) %>% 
  pivot_longer(cols = -c(archetype, n_tot)) %>% 
  mutate(rate = value / n_tot) %>% 
  filter(rate >= min_share)

res = res %>% 
  select(archetype, region = name, n_tot, n_region = value, share = rate) %>% 
  arrange(-share)

res = res %>% 
  left_join(x, by = c('archetype', 'region', 'n_region' = 'n')) %>% 
  mutate(wr_region = win / n_region) %>% 
  select(-win) %>% 
  filter(wr_region >= 0.5)

decks = tbl(con, 'lor_match_info_v2') %>% 
  filter(archetype %in% local(res$archetype)) %>% 
  filter(str_detect(game_version, current_patch)) %>% 
  mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATE)")) %>% 
  filter(game_start_time_utc >= min_date) %>% 
  {if(master_only) filter(., is_master == 1) else . } %>% 
  count(archetype, deck_code, game_outcome) %>% 
  collect()

decks = decks %>% 
  pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0) %>% 
  rowwise() %>% 
  mutate(n = sum(c_across(c(win, loss, matches('tie'))))) %>% 
  ungroup()

decks = decks %>% 
  group_by(archetype) %>% 
  slice_max(n = 1, order_by = n, with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(wr_deck = win / n) %>% 
  select(archetype, deck_code, n_deck = n, wr_deck)

res %>% left_join(decks, by = 'archetype') %>% write_csv('./data-dumps/monoshard.csv')

players = tbl(con, 'lor_match_info_v2') %>% 
  filter(archetype %in% local(res$archetype)) %>% 
  filter(str_detect(game_version, current_patch)) %>% 
  mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATETIME)")) %>% 
  filter(game_start_time_utc >= min_date) %>% 
  {if(master_only) filter(., is_master == 1) else . } %>% 
  count(archetype, puuid) %>% 
  collect()

pl = tbl(con, 'lor_players') %>% collect()

players = players %>% 
  mutate(n_tot = sum(n), share = n / n_tot) %>% 
  ungroup() %>% 
  rename(n_player = n) 

players = players %>% 
  arrange(-share) %>% 
  left_join(pl, by = 'puuid') %>% 
  unite(col = player, gameName, tagLine, sep = "#") %>% 
  select(-puuid) %>% 
  relocate(region, player, .after = archetype)

players = res %>% 
  select(archetype, region) %>% 
  left_join(players, by = c('archetype', 'region')) %>% 
  arrange(-n_tot, -share)

players %>% 
  group_by(archetype, region) %>% 
  slice_max(n = 1, order_by = share, with_ties = TRUE) %>% 
  ungroup() %>% 
  left_join(players %>% count(archetype, sort = TRUE, name = 'n_players'), by = 'archetype') %>% 
  select(archetype, region, n_tot, n_players, most_dedicated_player = player, n_player, share) %>% 
  arrange(-n_players) %>% 
  write_csv('./data-dumps/monoshard_players.csv')
