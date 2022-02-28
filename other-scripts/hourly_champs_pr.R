new = tribble(
  ~name, ~code,
  "Gnar", "05BC161",
  "Galio", "05DE009",
  "Udyr", "05FR013",
  "Yuumi", "05BC029"
)

x = tbl(con, 'lor_match_info_v2') %>%
  filter(str_detect(champs, local(paste0(new$code, collapse = "|")))) %>%
  mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATETIME)")) %>% 
  mutate(day = sql("CAST(game_start_time_utc AS DATE)")) %>% 
  mutate(hour = sql("HOUR(game_start_time_utc)")) %>% 
  count(day, hour, champs, game_outcome) %>%
  collect() %>%
  ungroup()

x = x %>%
  mutate(n = as.numeric(n)) %>%
  pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0) %>%
  {if (!'tie' %in% colnames(.)) mutate(., tie = 0) else .} %>%
  mutate(match = win + loss + tie)

xx = x %>% 
  separate(col = champs, into = sprintf('champ_%s', 1:6), sep = " ", fill = 'right') %>% 
  pivot_longer(cols = starts_with('champ_')) %>% 
  drop_na(value) %>% 
  select(-name)

y = xx %>% 
  filter(value %in% new$code) %>% 
  left_join(new, by = c('value' = 'code')) %>% 
  unite(col = 'time', day, hour, sep = " ") %>% 
  mutate(time = as_datetime(paste0(time, ":00:00"))) %>% 
  group_by(time, name) %>% 
  summarise(across(where(is.numeric), sum), .groups = 'drop')

min_ts = tbl(con, 'lor_match_info_v2') %>%
  filter(str_detect(champs, local(paste0(new$code, collapse = "|")))) %>%
  mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATETIME)")) %>% 
  summarise(min = min(game_start_time_utc, na.rm = TRUE)) %>% 
  pull()

n = tbl(con, 'lor_match_info_v2') %>%
  mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATETIME)")) %>% 
  filter(game_start_time_utc >= local(min_ts)) %>% 
  mutate(day = sql("CAST(game_start_time_utc AS DATE)")) %>% 
  mutate(hour = sql("HOUR(game_start_time_utc)")) %>% 
  count(day, hour) %>% 
  collect()

n = n %>% 
  ungroup() %>% 
  unite(col = 'time', day, hour, sep = " ") %>% 
  mutate(time = as_datetime(paste0(time, ":00:00"))) %>% 
  mutate(n = as.numeric(n))

y = y %>%
  left_join(n, by = 'time') %>% 
  arrange(name, time) %>% 
  group_by(name) %>% 
  mutate(across(c(win, match, n), .fns = list(cum = cumsum), .names = "{col}_{fn}")) %>% 
  ungroup()

y = y %>% 
  mutate(wr_cum = win_cum / match_cum, pr_cum = match_cum / n_cum)

# p1 = y %>%
#   filter(time > min(time)) %>% 
#   ggplot(aes(x = time, y = pr_cum, color = name)) +
#   geom_line(size = 1) +
#   geom_point(size = 2, alpha = 0.3) +
#   geom_label(aes(x = time+lubridate::hours(8), label = scales::percent(pr_cum, accuracy = .1)), show.legend = FALSE, data = y %>% group_by(name) %>% slice_max(n = 1, order_by = time, with_ties = FALSE) %>% ungroup()) +
#   theme_bw() +
#   scale_y_continuous(labels = scales::percent_format()) +
#   scale_color_brewer(palette = 'Set1') +
#   theme(legend.position = 'bottom') +
#   labs(
#     x = 'Hour',
#     y = 'Playrate',
#     title = 'Playrate over Time for new champions',
#     color = 'Champion'
#   )
# 
# p1

yy = y %>%
  filter(time > min(time)) %>%
  mutate(wr = win / match, pr = match / n) %>% 
  select(time, name, pr, ) %>% 
  group_by(name) %>% 
  mutate(pr_ma = forecast::ma(pr, order = 25))

p2 = yy %>% 
  ggplot(aes(x = time, color = name)) +
  geom_line(aes(y = pr_ma), size = 1.5, na.rm = TRUE) +
  geom_point(aes(y = pr), size = 2, alpha = 0.25) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_brewer(palette = 'Set1') +
  theme(legend.position = 'bottom') +
  labs(
    x = 'Hour',
    y = 'Playrate',
    title = 'Playrate for new champions',
    subtitle = 'Line shows a daily Moving Average',
    color = 'Champion'
  )

p2