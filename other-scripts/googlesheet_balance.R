
suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(googlesheets4)) # manage google sheets API

# load mysql db credentials
db_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# create connection to MySQL database
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  db_host = "127.0.0.1",
  user = db_creds$uid,
  password = db_creds$pwd,
  dbname = "db_prova"
)

patch <- tbl(con, "lor_patch_history") %>% 
  collect()

patches <- patch %>% 
  arrange(-last_patch) %>% 
  mutate(new_change = lag(change)) %>% 
  replace_na(list(new_change = 0)) %>% 
  mutate(cum_change = cumsum(new_change)) %>% 
  filter(cum_change == min(cum_change)) %>%
  arrange(last_patch) %>% 
  pull(value) %>% 
  paste0(collapse = ", ")

top25 <- tbl(con, "lor_matchup_table") %>% 
  count(archetype_1, sort = TRUE) %>% 
  head(n = 25) %>% 
  select(archetype_1) %>% 
  collect() %>% 
  pull()

x <- tbl(con, "lor_matchup_table") %>% 
  filter(archetype_1 %in% top25, archetype_2 %in% top25) %>% 
  collect()

total_n <- tbl(con, "lor_matchup_table") %>% 
  summarise(full_n = sum(n, na.rm = TRUE)) %>% 
  collect() %>% 
  pull()

y <- tbl(con, "lor_matchup_table") %>% 
  filter(archetype_1 %in% top25) %>%
  group_by(archetype_1) %>% 
  summarise(games_played = sum(n, na.rm = TRUE), .groups = "drop") %>% 
  collect() %>% 
  mutate(playrate = (games_played / total_n)*100) %>% 
  mutate(playrate = round(playrate, digits = 2)) %>% 
  arrange(-playrate)

 y_wr <- tbl(con, "lor_matchup_table") %>% 
   filter(archetype_1 %in% top25) %>%
   collect()
 
yy_wr <- y_wr %>% 
   group_by(archetype_1) %>% 
   summarise(winrate = weighted.mean(winrate, w = n, na.rm = TRUE), .groups = "drop")

x <- x %>% 
  distinct() %>% 
  mutate(across(starts_with("archetype_"), factor, levels = top25, ordered = TRUE)) %>%
  mutate(across(c(winrate, n), function(x) ifelse(archetype_1 == archetype_2, NA, x))) %>% 
  arrange(archetype_1, archetype_2)
    
x_wr <- x %>% 
  select(-n) %>%
  mutate(winrate = round(winrate*100, digits = 2)) %>% 
  pivot_wider(names_from = archetype_2, values_from = winrate) %>% 
  rename(" " = archetype_1)

x_n <- x %>% 
  select(-winrate) %>% 
  pivot_wider(names_from = archetype_2, values_from = n) %>% 
  rename(" " = archetype_1)

update <- sprintf("Last update: %s UTC", Sys.time())
patches <- sprintf("Patch Analyzed: %s", patches)

info <- tibble(
  " " = c(update, patches)
)

deck_info <- y %>% 
  left_join(yy_wr, by = "archetype_1") %>% 
  mutate(winrate = round(winrate*100, digits = 2)) %>% 
  rename(" " = archetype_1)

sheet_write(data = x_wr, ss = "1Xlh2kg7gLzvqugqGPpI4PidAdM5snggbJ44aRLuik5E", sheet = "Winrate")
sheet_write(data = x_n, ss = "1Xlh2kg7gLzvqugqGPpI4PidAdM5snggbJ44aRLuik5E", sheet = "Number of Games")
sheet_write(data = deck_info, ss = "1Xlh2kg7gLzvqugqGPpI4PidAdM5snggbJ44aRLuik5E", sheet = "Decks Information")
sheet_write(data = info, ss = "1Xlh2kg7gLzvqugqGPpI4PidAdM5snggbJ44aRLuik5E", sheet = "Data Information")
