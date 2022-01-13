# NON CANCELLARE PUO ESSER UTILE IN FUTURO

patches = "live_2_21_|live_3_00_"

archetypes = tbl(con, 'lor_match_info_v2') %>% 
  filter(str_detect(game_version, patches)) %>% 
  count(archetype, sort = TRUE) %>% 
  head(25) %>% 
  collect() %>% 
  pull(archetype)

x = tbl(con, 'lor_match_info_v2') %>% 
  filter(str_detect(game_version, patches)) %>% 
  #mutate(day = sql("CAST(game_start_time_utc AS DATE)")) %>% 
  #filter(day >= '2021-12-21') %>% 
  #filter(str_detect(game_version, patches), archetype %in% archetypes) %>% 
  count(game_outcome, order_of_play) %>% 
  collect()

xx = x %>% 
  pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0) %>% 
  mutate(across(c(where(bit64::is.integer64)), as.numeric)) %>% 
  mutate(order_of_play = ifelse(order_of_play == 0, '1st', '2nd')) %>% 
  mutate(n = loss + win + tie, wr = win / n) %>% 
  ungroup()

xx %>% 
  mutate(wr = scales::percent(wr, accuracy = .0001), across(c(loss, win, tie, n), scales::comma)) %>% relocate(n, .after = order_of_play)


# ugly as fuck but works
test = xx %>% mutate(test = map2(.x = win, .y = n, .f = ~binom.test(x = .x, n = .y)))


low = rep(0, nrow(test))
hi  = rep(0, nrow(test))

for(i in 1:nrow(test)){
  
  tmp = test$test[[i]]$conf.int
  low[i] = tmp[1]
  hi[i]  = tmp[2]
}

xxx = xx %>% 
  ungroup() %>% 
  select(archetype, order_of_play, wr, n, win) %>% 
  mutate(ci_low = low, ci_hi = hi)

xxx = xxx %>% 
  mutate(archetype = factor(archetype, ordered = TRUE, levels = archetypes)) %>% 
  arrange(archetype)

tot = xxx %>% 
  group_by(archetype) %>% 
  summarise(n = sum(n), win = sum(win), .groups = "drop") %>% 
  mutate(wr = win / n)

ggplot(xxx %>% filter(archetype %in% archetypes[16:25]), aes(x = archetype)) +
  geom_linerange(aes(ymin = ci_low, ymax = ci_hi, group = order_of_play, color = order_of_play), position = position_dodge(width = 1), size = 1) +
  geom_label(aes(label = scales::percent(wr, accuracy = .1), y = wr, group = order_of_play, color = order_of_play), position = position_dodge(width = 1), size = 5, show.legend = FALSE) +
  #geom_text(aes(y = 0.44, group = order_of_play, color = order_of_play, label = scales::percent(wr, accuracy = .1)), vjust = 1, position = position_dodge(width = 1), size = 5) +
  geom_point(aes(y = wr), data = tot %>% filter(archetype %in% archetypes[16:25])) +
  geom_text(aes(y = 0.42, label = scales::percent(wr, accuracy = .1)), data = tot %>% filter(archetype %in% archetypes[16:25]), vjust = -1, size = 6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  theme_light() +
  theme(legend.position = 'bottom', axis.text.x = element_text(angle = 20, hjust = 1)) +
  labs(
    x = 'Archetype', 
    y = 'Winrate', 
    color = 'Order of Play', 
    title = 'Archetype Winrate by Order of Play', 
    subtitle = 'Plat+ data from patch: 2.21, 3.0; Black is the overall winrate, red the winrate going 1st, blue the winrate going 2nd')


ggplot(xx, aes(x = day, y = n)) + geom_line(size = 1) + geom_point(size = 3, alpha = 0.3) + theme_bw() + expand_limits(y = 0)
ggplot(xx, aes(x = day, y = wr)) + geom_line(aes(y = (wr+lag(wr)+lead(wr))/3), size = 1, na.rm = TRUE) + geom_point(size = 3, alpha = 0.3) + theme_bw()
