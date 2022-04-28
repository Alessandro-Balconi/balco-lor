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

xx = x %>% 
  group_by(patch) %>% 
  summarise(tot_match = sum(match), .groups = 'drop') %>% 
  left_join(x, by = 'patch') %>% 
  mutate(pr = match / tot_match, wr = win / match) %>% 
  select(archetype, patch, pr, wr) %>% 
  pivot_wider(names_from = patch, values_from = c(pr, wr)) %>% 
  arrange(-pr_3.5)

xx = xx %>% 
  mutate(archetype = factor(archetype, levels = unique(xx$archetype), ordered = TRUE))

ndecks = x %>% 
  filter(patch == '3.6') %>% 
  summarise(n = sum(match)) %>% 
  pull(n)

xxx = xx %>% 
  head(10) %>% 
  mutate(pr_change = pr_3.6 - pr_3.5, wr_change = wr_3.6 - wr_3.5)

xxx %>% 
  pivot_longer(cols = -c(archetype)) %>%
  #pivot_longer(cols = -c(archetype, matches('_change'))) %>%
  mutate(name = factor(name, levels = c('pr_3.5', 'wr_3.5', 'pr_3.6', 'wr_3.6', 'pr_change', 'wr_change'), ordered = TRUE)) %>%
  mutate(fill_tile = if_else(str_detect(name, 'wr_'), 0 , 1)) %>% 
  ggplot(aes(x = name, y = reorder(archetype, desc(archetype)))) +
  geom_tile(aes(alpha = if_else(fill_tile == 1, value, 0)), fill = 'steelblue') +
  shadowtext::geom_shadowtext(
    aes(
      label = if_else(str_detect(name, 'change', negate = TRUE), scales::percent(value, accuracy = .1), scales_percent_plus(value, accuracy = .1)),
      color = if_else(str_detect(name, 'change', negate = TRUE), "white", if_else(value > 0, 'forestgreen', 'red'))
    ), 
    size = 12, na.rm = TRUE) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_x_discrete(labels = c('Playrate (3.5)', 'Winrate (3.5)', 'Playrate (3.6)', 'Winrate (3.6)', 'ΔPlayrate', 'ΔWinrate'), position = "top") +
  scale_color_manual(values = c('white' = 'white', 'forestgreen' = 'forestgreen', 'red' = 'red')) +
  labs(title = 'Early data from Patch 3.6', subtitle = sprintf('Plat+ - %s decks analyzed', ndecks), x = element_blank(), y = 'Archetype') +
  theme_classic(base_size = 15) +
  theme(legend.position = 'none', axis.text.x = element_text(size = 20))

DBI::dbDisconnect(con)