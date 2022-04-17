# setup ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(googlesheets4)) # manage google sheets API
suppressPackageStartupMessages(library(jsonlite)) # work with JSON objects
suppressPackageStartupMessages(library(httr)) # make GET calls

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

# fucntion to get names of top % masters from region
get_top_masters <- function(region, prop = 0.1, add_region = TRUE){
  
  # get correct region leaderboard from db
  leaderboard <- switch(
    region,
    'europe' = 'leaderboard_eu',
    'americas' = 'leaderboard_na',
    'asia' = 'leaderboard_asia'
  )
  
  # count number of masters in the leaderboard
  n_masters <- tbl(con, leaderboard) %>% summarise(n = 1.0*n()) %>% pull()
  
  # number of people to collect (last rank of the persone to collect)
  max_rank <- floor(prop*n_masters)
  
  # get players + add region if needed
  players <- tbl(con, leaderboard) %>%
    filter(rank <= max_rank) %>% 
    select(name) %>%
    collect() %>% 
    {if(add_region) mutate(., region = region) else . }
  
  # return players
  return(players)
  
}

# get most recent set number (to read sets JSONs)
last_set <- "https://dd.b.pvp.net/latest/core/en_us/data/globals-en_us.json" %>% 
  GET() %>% 
  content(encoding = "UTF-8") %>% 
  fromJSON() %>% 
  .[["sets"]] %>% 
  mutate(set = str_extract(nameRef, pattern = "[0-9]+")) %>% 
  mutate(set = as.numeric(set)) %>% 
  summarise(max(set, na.rm = TRUE)) %>% 
  pull()

# champions names / codes / regions from set JSONs
db_cards <- map_dfr(
  .x = 1:last_set,
  .f = function(x) {
    sprintf("https://dd.b.pvp.net/latest/set%1$s/en_us/data/set%1$s-en_us.json", x) %>% 
      GET() %>%  
      content(encoding = "UTF-8") %>% 
      fromJSON() %>% 
      as_tibble()
  },
  .id = "set"
) %>% 
  select(name, cardCode) %>%
  filter(nchar(cardCode) <= 8) # additional check because sometimes Riot messes up

# fetch data ----

# players to collect from each region
df_players <- map_dfr(.x = c('europe', 'americas', 'asia'), .f = get_top_masters)

# db with player info
db_players <- tbl(con, 'lor_players') %>% 
  filter(gameName %in% local(df_players$name)) %>% 
  collect()

# add puuid info to df_players
df_players <- df_players %>% 
  left_join(db_players, by = c('name' = 'gameName', 'region'))

# main df with cards data
df <- tbl(con, 'lor_match_info_v2') %>% 
  mutate(day = sql('CAST(game_start_time_utc AS DATE)')) %>% 
  filter(day >= local(Sys.Date()-lubridate::days(7)), puuid %in% local(df_players$puuid)) %>% 
  count(puuid, archetype, deck_code, cards) %>% 
  collect()

# preprocessing ----

# replace puuid with riot id
df <- df %>% 
  ungroup() %>% 
  left_join(df_players, by = 'puuid') %>% 
  unite(col = player, name, tagLine, sep = '#') %>% 
  select(-puuid) %>% 
  arrange(archetype, desc(n))

# most played lists by player
df_players_list <- df %>% 
  with_groups(.groups = c(archetype, deck_code, player, region), .f = summarise, n = sum(n)) %>% 
  with_groups(.groups = player, .f = slice_max, order_by = n, n = 3, with_ties = FALSE) %>% 
  mutate(n = as.numeric(n)) %>% 
  arrange(region, player, desc(n)) %>% 
  relocate(region, player, archetype, deck_code, n)

# "anonymize" player tags
df_players_list <- df_players_list %>% 
  mutate(player = str_remove_all(player, pattern = '#.*'))

# df with cards info
df_cards <- df %>% 
  select(archetype, cards, n) %>% 
  separate(col = cards, into = sprintf('card_%s', 1:40), sep = " ", fill = 'right') %>% 
  pivot_longer(cols = starts_with('card_')) %>% 
  drop_na() %>% 
  separate(col = value, into = c('count', 'card'), sep = ":") %>% 
  select(-name)

# count occurences by card count, card, archetype
df_cards <- df_cards %>% 
  group_by(archetype, card, count) %>% 
  summarise(n = sum(n), .groups = 'drop_last') %>% 
  mutate(n_card = sum(n), .before = n) %>% 
  ungroup(card) %>% 
  mutate(n_arch = max(n_card), .before = n_card) %>% 
  ungroup()

# add card name to df_cards (& fix columns class)
df_cards <- df_cards %>% 
  mutate(card = str_replace_all(card, set_names(db_cards$name, db_cards$cardCode))) %>% 
  mutate(across(where(bit64::is.integer64), as.numeric))

# count occurences of card not being played
df_count_0 <- df_cards %>% 
  with_groups(.groups = c(archetype, card, n_arch, n_card), .f = summarise, n = mean(n_arch - n_card)) %>% 
  distinct() %>% 
  mutate(count = 0, .after = card)

# add it to main df_cards & reshape
df_cards <- df_cards %>% 
  mutate(count = as.numeric(count)) %>% 
  bind_rows(df_count_0) %>% 
  mutate(n_perc = n / n_arch) %>% 
  select(-c(n_arch, n_card, n)) %>% 
  pivot_wider(names_from = count, values_from = n_perc, values_fill = 0, names_prefix = 'n_') %>% 
  mutate(n_ovr = n_3 + n_2 + n_1, .after = card)

# make nicer and save to spreadsheet
df_cards_nice <- df_cards %>% 
  arrange(archetype, desc(n_ovr), desc(n_3), desc(n_2), desc(n_1), desc(n_0)) %>% 
  mutate(across(where(is.numeric), function(x) round(x*100, digits = 1))) %>% 
  rename_with(.cols = starts_with('n_'), .fn = function(x) str_replace(x, pattern = '^n_', replacement = 'perc_'))

# make cards table with core / flex info and qty
df_cards_table <- df_cards %>% 
  arrange(archetype, desc(n_ovr), desc(n_3), desc(n_2), desc(n_1), desc(n_0)) %>% 
  mutate(what = ifelse(n_ovr >= 0.8, "Core", ifelse(n_ovr >= 0.1, "Flex", 'Tech Card'))) %>% 
  mutate(across(c(n_3, n_2, n_1), function(x) x / n_ovr)) %>% 
  select(-c(n_ovr, n_0)) %>% 
  pivot_longer(cols = -c(archetype, card, what)) %>% 
  filter(value >= 0.25) %>% 
  mutate(name = str_remove(name, pattern = '^n_')) %>% 
  with_groups(.groups = c(archetype, card, what), .f = mutate, value = row_number()) %>% 
  pivot_wider(names_from = value, values_from = name, names_prefix = 'x') %>% 
  unite(col = quantity, c(x3, x2, x1), sep = ' or ', na.rm = TRUE)

# update spreadsheet ----

# additional information
update <- sprintf("Last update: %s UTC", Sys.time())
info_1 <- "NB: data is taken from ranked matches of the top 10% masters of each server in the past 7 days (e.g. if there are 1000 masters in the Americas server, data will be collected only from the top 100)."
info_2 <- "The 'Players Lists' page shows the 3 most played decklists for every player."
info <- tibble(" " = c(update, info_1, info_2))

# id of the spreadsheet 
ss_id <- "1IezJoQ5IY-paK5Njdvz7ZGSFPeuRAIWZT6ZGxQthIJM"

# update all sheets of the spreadsheet
with_gs4_quiet(sheet_write(data = df_cards_table,  ss = ss_id, sheet = "Cards Table"     ))
with_gs4_quiet(sheet_write(data = df_players_list, ss = ss_id, sheet = "Players Lists"   ))
with_gs4_quiet(sheet_write(data = info,            ss = ss_id, sheet = "Data Information"))
with_gs4_quiet(sheet_write(data = df_cards_nice,   ss = ss_id, sheet = "Database"        ))

# names of the spreadsheet to update
ss_names <- sheet_names(ss_id)

# adjust spacing of columns in the spreadsheet
map(
  .x = ss_names,
  .f = ~with_gs4_quiet(range_autofit(ss = ss_id, sheet = ., dimension = "columns"))
)

DBI::dbDisconnect(con)