x = tbl(con, 'lor_match_info_v2') %>% 
  count(puuid, region) %>%
  left_join(tbl(con, 'lor_players'), by = c('puuid', 'region')) %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  collect()

focus_region = 'europe'
focus_player = 'RickoRex'
focus_puuid = x %>% filter(region == focus_region, gameName == focus_player) %>% pull(puuid)

focus_matchids = tbl(con, 'lor_match_info_v2') %>% 
  filter(region == local(focus_region), puuid == local(focus_puuid)) %>% 
  distinct(match_id) %>% 
  collect() %>% 
  pull()

data = tbl(con, 'lor_match_info_v2') %>% 
  filter(region == local(focus_region), match_id %in% local(focus_matchids)) %>% 
  mutate(day = sql('CAST(game_start_time_utc AS DATE)')) %>% 
  select(match_id, day, puuid, game_outcome, archetype) %>% 
  collect()

p_data = data %>% 
  filter(puuid == focus_puuid) %>% 
  count(archetype, game_outcome) %>% 
  pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0) %>% 
  rowwise() %>% 
  mutate(n = sum(c_across(c(win, loss, matches('tie'))))) %>% 
  ungroup() %>% 
  mutate(wr = win / n)

p_data %>% arrange(-n)
p_data %>% filter(n >= 10) %>% arrange(-wr)

data = data %>% 
  left_join(tbl(con, 'lor_players') %>% filter(region == local(focus_region), puuid %in% local(data$puuid)) %>% collect(), by = 'puuid') %>% 
  select(-region)

data %>% 
  filter(gameName != focus_player) %>% 
  unite(col = player, gameName, tagLine, sep = '#') %>% 
  count(player, game_outcome) %>% 
  pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0) %>% 
  rowwise() %>% 
  mutate(n = sum(c_across(c(win, loss, matches('tie'))))) %>% 
  ungroup() %>% 
  mutate(wr = win / n) %>% 
  arrange(-n) %>% 
  filter(!str_detect(player, focus_player))
