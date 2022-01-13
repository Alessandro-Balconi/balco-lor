# aggro vs mid vs control

patches = "live_2_21_|live_3_00_"

archetypes = tbl(con, 'lor_match_info_v2') %>% 
  filter(str_detect(game_version, patches)) %>% 
  count(archetype, sort = TRUE) %>% 
  head(25) %>% 
  collect() %>% 
  pull(archetype)

x = tbl(con, 'lor_match_info_v2') %>% 
  filter(str_detect(game_version, patches), archetype %in% archetypes) %>% 
  count(archetype, game_outcome, total_turn_count) %>% 
  collect()

xx = x %>% 
  ungroup() %>% 
  pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0) %>% 
  mutate(across(c(where(bit64::is.integer64)), as.numeric)) %>% 
  mutate(n = loss + win + tie, wr = win / n) %>% 
  arrange(archetype, total_turn_count)

xxx = xx %>%
  mutate(cuts = cut(total_turn_count, breaks = c(5,10,15,20,25,30,Inf), include_lowest = TRUE, ordered_result = TRUE)) %>%
  drop_na(cuts) %>% 
  group_by(archetype, cuts) %>% 
  summarise(n = sum(n), win = sum(win), .groups = "drop") %>% 
  mutate(wr = win / n)

labels = xxx %>% 
  group_by(archetype) %>% 
  slice_max(n = 1, order_by = wr) %>% 
  ungroup() %>% 
  mutate(label = case_when(
    cuts %in% c('(5,10]', '(10,15]') ~ 'aggro',
    cuts %in% c('(15,20]', '(20,25]') ~ 'midrange',
    cuts %in% c('(25,30]', '(30,Inf]') ~ 'control',
    TRUE ~ 'other'
  )) %>% 
  select(archetype, label)

xxx = xxx %>% 
  left_join(labels, by = 'archetype')

ggplot(xxx, aes(x = cuts, y = wr, group = archetype, color = archetype)) +
  stat_summary(fun = sum, geom = "line", size = 1) +
  geom_point(size = 3, alpha = 0.3) +
  geom_label(aes(label = archetype, x = '(30,Inf]')) +
  theme_bw() +
  theme(legend.position = "bottom") +
  #scale_color_brewer(palette = 'Set3') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "# of Priority Changes", y = 'Winrate', title = 'Winrate by Game Lenght', color = "Archetype") +
  facet_wrap(~label)
  