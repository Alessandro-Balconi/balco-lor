suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(googlesheets4)) # manage google sheets API

options(gargle_oauth_email = "Balco21@outlook.it")
options(googlesheets4_quiet = TRUE)

# load mysql db credentials
db_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# create connection to MySQL database
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  db_host = "127.0.0.1",
  user = db_creds$uid,
  password = db_creds$pwd,
  dbname = db_creds$dbs
)

# patches from which data is analyzed
patches <- tbl(con, "utils_patch_history") %>% 
  collect() %>% 
  arrange(desc(release_date)) %>% 
  mutate(new_change = lag(change)) %>% 
  replace_na(list(new_change = 0)) %>% 
  mutate(cum_change = cumsum(new_change)) %>% 
  filter(cum_change == min(cum_change)) %>%
  arrange(release_date) %>% 
  pull(patch) %>% 
  paste0(collapse = ", ")

# most 60 played archetypes
top60 <- tbl(con, "ranked_patch_matchups") %>% 
  group_by(archetype_1) %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>% 
  arrange(desc(n)) %>% 
  head(n = 60) %>% 
  select(archetype_1) %>% 
  collect() %>% 
  pull()

top60_wrap <- str_wrap(top60, width = 8)

# collect only matchups among the top60
x <- tbl(con, "ranked_patch_matchups") %>% 
  filter(archetype_1 %in% top60, archetype_2 %in% top60) %>% 
  collect()

x <- x %>% 
  group_by(archetype_1, archetype_2) %>% 
  summarise(
    winrate = weighted.mean(winrate, w = n),
    n = sum(n, na.rm = TRUE),
    .groups = "drop"
  )

# total number of games played
total_n <- tbl(con, "ranked_patch_matchups") %>% 
  summarise(full_n = sum(n, na.rm = TRUE)) %>% 
  collect() %>% 
  pull()

# top 60 decks info (playrate)
y <- tbl(con, "ranked_patch_matchups") %>% 
  filter(archetype_1 %in% top60) %>%
  group_by(archetype_1) %>% 
  summarise(games_played = sum(n, na.rm = TRUE), .groups = "drop") %>% 
  collect() %>% 
  mutate(playrate = (games_played / total_n)*100) %>% 
  mutate(playrate = round(playrate, digits = 2)) %>% 
  arrange(-playrate)

# top 60 decks info (winrate)
y_wr <- tbl(con, "ranked_patch_matchups") %>% 
  filter(archetype_1 %in% top60) %>%
  collect()

yy_wr <- y_wr %>% 
  group_by(archetype_1) %>% 
  summarise(winrate = weighted.mean(winrate, w = n, na.rm = TRUE), .groups = "drop")

# prepare matchup table
x <- x %>% 
  distinct() %>% 
  mutate(across(c(winrate, n), function(x) ifelse(archetype_1 == archetype_2, NA, x))) %>% 
  mutate(across(archetype_2, str_wrap, width = 8)) %>% 
  mutate(across(archetype_1, factor, levels = top60, ordered = TRUE)) %>% 
  mutate(across(archetype_2, factor, levels = top60_wrap, ordered = TRUE)) %>% 
  arrange(archetype_1, archetype_2)

# generate table with winrates    
x_wr <- x %>% 
  select(-n) %>%
  mutate(winrate = round(winrate*100, digits = 1)) %>% 
  pivot_wider(names_from = archetype_2, values_from = winrate) %>% 
  rename(" " = archetype_1)

# generate table with number of games
x_n <- x %>% 
  select(-winrate) %>% 
  pivot_wider(names_from = archetype_2, values_from = n) %>% 
  rename(" " = archetype_1)

# additional information
update <- sprintf("Last update: %s UTC", Sys.time())
patches <- sprintf("Patch Analyzed: %s", patches)

info <- tibble(
  " " = c(update, patches)
)

# deck information
deck_info <- y %>% 
  left_join(yy_wr, by = "archetype_1") %>% 
  mutate(winrate = round(winrate*100, digits = 2)) %>% 
  rename(archetype = archetype_1)

# id of the spreadsheet 
ss_id <- "1Y30Rng9sA7WVZLjRDs2OBnpJGkHLgtjDMdY8mjkKBZg"

# update all sheets of the spreadsheet
with_gs4_quiet(sheet_write(data = x_wr,      ss = ss_id, sheet = "Winrate"))
with_gs4_quiet(sheet_write(data = x_n,       ss = ss_id, sheet = "Number of Games"))
with_gs4_quiet(sheet_write(data = deck_info, ss = ss_id, sheet = "Decks Information"))
with_gs4_quiet(sheet_write(data = info,      ss = ss_id, sheet = "Data Information"))

# names of the spreadsheet to update
ss_names <- sheet_names(ss_id)

# adjust spacing of columns in the spreadsheet
map(
  .x = ss_names,
  .f = ~with_gs4_quiet(range_autofit(ss = ss_id, sheet = ., dimension = "columns"))
)

DBI::dbDisconnect(con)
