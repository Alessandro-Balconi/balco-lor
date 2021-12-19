library(jsonlite)
library(httr)

# gp: 02BW032
# seedling: 05IO021
# poppy: 05BC041
# explorer: 05BC108
# wayfinder: 01IO050

data_cards <- map_dfr(
  .x = 1:5,
  .f = function(x) {
    sprintf("https://dd.b.pvp.net/latest/set%1$s/en_us/data/set%1$s-en_us.json", x) %>% 
      GET() %>%  
      content(encoding = "UTF-8") %>% 
      fromJSON() %>% 
      as_tibble()
  },
  .id = "set"
) %>% 
  select(name, cardCode)


min_date = as.POSIXct('2021-12-09')

data_match = tbl(con, 'lor_match_info_v2') %>% 
  mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATE)")) %>% 
  filter(game_start_time_utc >= local(min_date)) %>% 
  count(game_start_time_utc) %>% 
  collect()

# gp: 02BW032
# seedling: 05IO021
# poppy: 05BC041
# explorer: 05BC108
# wayfinder: 01IO050
focus_cards = c('02BW032', '05IO021', '05BC041', '05BC108', '01IO050')

for(i in seq_len(length(focus_cards))){
  
  focus_card = focus_cards[i]
  
  data_card = tbl(con, 'lor_match_info_v2') %>% 
    filter(str_detect(cards, local(focus_card))) %>% 
    mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATE)")) %>% 
    filter(game_start_time_utc >= local(min_date)) %>% 
    count(game_start_time_utc, game_outcome) %>% 
    collect()
  
  data_i = data_card %>% 
    pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0) %>% 
    mutate(match = sum(c_across(c(loss, win, matches('tie'))))) %>% 
    mutate(winrate = win / match) %>% 
    select(-c(loss, win, matches('tie'))) %>% 
    left_join(data_match, by = 'game_start_time_utc') %>% 
    mutate(playrate = match / n) %>% 
    select(-c(match, n))
  
  data_i = data_i %>% 
    mutate(card = focus_card)
  
  if(i == 1){ data = data_i } else { data = bind_rows(data, data_i) }
  
}

data = data %>% 
  left_join(data_cards, by = c('card' = 'cardCode'))

ggplot(data, aes(x = game_start_time_utc, y = playrate)) +
  geom_line(size = 1.5, color = 'royalblue') +
  geom_vline(aes(xintercept = lubridate::ymd("2021-12-14")), linetype = 'dotted', color = 'coral', size = 1) +
  geom_point(size = 3, color = 'royalblue', alpha = 0.5) +
  expand_limits(y = 0) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  labs(x = 'Day', y = 'Playrate', title = 'Playrate over time') +
  facet_wrap(~name, nrow = 2)

ggplot(data, aes(x = game_start_time_utc, y = winrate)) +
  geom_line(size = 1.5, color = 'forestgreen') +
  geom_vline(aes(xintercept = lubridate::ymd("2021-12-14")), linetype = 'dotted', color = 'coral', size = 1) +
  geom_point(size = 3, color = 'forestgreen', alpha = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  theme_bw() +
  labs(x = 'Day', y = 'Winrate', title = 'Winrate over time',
       subtitle = 'Dotted line shows the hotfix date') +
  facet_wrap(~name, nrow = 2)
