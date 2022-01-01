x = tbl(con, 'lor_match_info_v2') %>% 
  #filter(is_master == 1) %>% 
  mutate(ts = sql('CAST(game_start_time_utc AS DATE)')) %>% 
  filter(ts >= local(ymd('2021-12-09')) & ts < local(Sys.Date())) %>%
  count(ts) %>%
  arrange(ts) %>% 
  collect()

y = tbl(con, 'lor_match_info_old_v2') %>% 
  #filter(is_master == 1) %>% 
  mutate(ts = sql('CAST(game_start_time_utc AS DATE)')) %>% 
  filter(ts >= local(ymd('2021-10-21')) & ts < local(ymd('2021-12-08'))) %>%
  count(ts) %>%
  arrange(ts) %>% 
  collect()

w = tbl(con, 'lor_match_info_old_v2') %>% 
  #filter(is_master == 1) %>% 
  mutate(ts = sql('CAST(game_start_time_utc AS DATE)')) %>% 
  filter(ts >= local(ymd('2021-08-27')) & ts < local(ymd('2021-10-20'))) %>%
  count(ts) %>%
  arrange(ts) %>% 
  collect()

x = x %>% 
  mutate(n = as.numeric(n)) %>% 
  mutate(season = 'Magic Misadventures', day = row_number())
  
y = y %>% 
  mutate(n = as.numeric(n)) %>% 
  mutate(season = 'Between Worlds', day = row_number())

w = w %>% 
  mutate(n = as.numeric(n)) %>% 
  mutate(season = 'Beyond the Bandlewood', day = row_number())

z = bind_rows(w, x,y)

ggplot(z, aes(x = day, y = n, color = season)) +
  geom_line(size = 1.5) +
  geom_point(alpha = 0.3, size = 3) +
  expand_limits(y = 0) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma_format()) +
  theme(legend.position = 'bottom') +
  labs(
    x = 'Day of the Season', 
    y = '# of Matches', 
    title = 'Matches collected by day of the season', 
    color = 'Season',
    subtitle = 'Plat+ data'
    #subtitle = 'Master data'
  )
  