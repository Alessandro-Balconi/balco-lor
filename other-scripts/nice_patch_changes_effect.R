library(tidyverse)

# load mysql db credentials
db_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# create connection to MySQL database
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  db_host  = "127.0.0.1",
  user     = db_creds$uid,
  password = db_creds$pwd,
  dbname   = db_creds$dbs
)

# remove credentials
rm(db_creds)

# same as scales::percent, but has a nicer format
scales_percent_plus <- function(x, accuracy = .1){
  
  sapply(
    X = x,
    FUN = function(x) case_when(
      is.na(x) ~ paste0("‼ NEW"),
      abs(x)<=(accuracy/200) ~ paste0("↔ ", scales::percent(abs(x), accuracy = accuracy)),
      x>0 ~ paste0("↑ +", scales::percent(x, accuracy = accuracy)),
      x<0 ~ paste0("↓ ", scales::percent(x, accuracy = accuracy)),
      TRUE ~ NA_character_ # should never happen; just an additional check
    ),
    USE.NAMES = FALSE
  )
  
}

x = tbl(con, 'ranked_daily_archetypes') %>%
  filter(patch %in% c('3.5', '3.6')) %>% 
  left_join(tbl(con, 'utils_archetype_aggregation'), by = c('archetype' = 'old_name')) %>% 
  mutate(archetype = coalesce(new_name, archetype)) %>% 
  group_by(patch, archetype) %>% 
  summarise(across(c(win, match), sum, na.rm = TRUE), .groups = 'drop') %>% 
  collect()

ndecks = x %>% 
  filter(patch == '3.6') %>% 
  summarise(n = sum(match)) %>% 
  pull(n)

xx = x %>% 
  group_by(patch) %>% 
  summarise(tot_match = sum(match), .groups = 'drop') %>% 
  left_join(x, by = 'patch') %>% 
  mutate(pr = match / tot_match, wr = win / match) %>% 
  select(archetype, patch, pr, wr) %>% 
  pivot_wider(names_from = patch, values_from = c(pr, wr)) %>% 
  arrange(-pr_3.6) %>% 
  head(15) %>% 
  mutate(pr_change = pr_3.6 - pr_3.5, wr_change = wr_3.6 - wr_3.5)

xx = xx %>% 
  mutate(archetype = factor(archetype, levels = unique(xx$archetype), ordered = TRUE))

xx %>% 
  pivot_longer(cols = -c(archetype)) %>%
  mutate(name = factor(name, levels = c('pr_3.6', 'wr_3.6', 'pr_3.5', 'wr_3.5', 'pr_change', 'wr_change'), ordered = TRUE)) %>%
  mutate(what_tile = if_else(str_detect(name, 'change'), 'change' , if_else(str_detect(name, 'wr'), 'wr', 'pr'))) %>% 
  ggplot(aes(x = name, y = reorder(archetype, desc(archetype)))) +
  geom_tile(aes(alpha = if_else(what_tile == 'pr', value, if_else(what_tile == 'wr', (value-0.4)/2, 0)), fill = what_tile)) +
  shadowtext::geom_shadowtext(
    aes(
      label = if_else(str_detect(name, 'change', negate = TRUE), scales::percent(value, accuracy = .1), scales_percent_plus(value, accuracy = .1)),
      color = case_when(
        str_detect(name, 'change', negate = TRUE) ~ "not_colored", 
        value > 0.05 ~ 'super_pos', 
        value > 0.005 ~ 'pos', 
        value < -0.05 ~ 'super_neg', 
        value < -0.005 ~ 'neg', 
        TRUE ~ 'not_changed'
      )
    ), 
    bg.r = 0.08, size = 12, na.rm = TRUE) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_x_discrete(labels = c('Playrate (3.6)', 'Winrate (3.6)', 'Playrate (3.5)', 'Winrate (3.5)', 'ΔPlayrate', 'ΔWinrate'), position = "top") +
  scale_fill_manual(values = c('pr' = 'steelblue', 'wr' = 'forestgreen', 'change' = 'white')) +
  scale_color_manual(values = c('not_colored' = 'white', 'super_neg' = '#B81D13', 'super_pos' = "#046507", 'pos' = '#149414', 'neg' = 'coral2', 'not_changed' = '#EFB700')) +
  labs(title = 'Early data from Patch 3.6', subtitle = sprintf('Plat+ - %s decks analyzed', scales::comma(ndecks, accuracy = 1)), x = element_blank(), y = 'Archetype') +
  theme_classic(base_size = 15) +
  theme(legend.position = 'none', axis.text.x = element_text(size = 20))

DBI::dbDisconnect(con)