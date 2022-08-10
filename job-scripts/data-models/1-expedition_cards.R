# Convert information from 'expedition_match' table into a ready to use cards table

# This task is performed weekly right after the 'expedition_match' db update

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(lubridate)) # working with dates
suppressPackageStartupMessages(library(jsonlite))  # convert JSON to R objects
suppressPackageStartupMessages(library(httr))      # http requests

# 2. connect to db & load data ----

# create connection to MySQL database
con <- lorr::create_db_con()

# get most recent set number (to read sets JSONs)
last_set <- lorr::last_set()

# cards names / codes / rarity from set JSONs
data_cards <- get_cards_data(select = c('rarity', 'name', 'cardCode')) %>%
  filter(nchar(cardCode) <= 8)

# most recent day in the table (it means we have updated up to this point)
last_day = tbl(con, 'expedition_cards') %>% 
  summarise(maxts = max(day, na.rm = TRUE)) %>%
  collect() %>% 
  pull() %>% 
  {if(is.na(.)) ymd('2000-01-01') else . }

# new matches that need to be added
data = tbl(con, 'expedition_match') %>% 
  mutate(day = sql("CAST(game_start_time_utc AS DATE)")) %>% 
  filter(day > local(last_day), day <= local(Sys.Date()-days(3))) %>% 
  select(match_id, day, game_version, game_outcome, cards, is_master) %>% 
  collect()

# 4. prepare table ----

# add info of number of humans in each match
opponent = data %>% 
  count(match_id, name = 'opponent') %>% 
  mutate(opponent = ifelse(opponent %% 2 == 0, 'human', 'bot'))

data = data %>% 
  left_join(opponent, by = 'match_id') %>% 
  select(-match_id)

# table with n of games (will be saved)
n_games = data %>% 
  count(day, is_master, opponent)

# split a deck into single cards
data = data %>% 
  separate(col = cards, into = sprintf('card_%s', 1:40), sep = " ", fill = "right")

data = data %>% 
  pivot_longer(cols = -c(day, game_version, game_outcome, opponent, is_master)) %>% 
  drop_na(value)

# separate card name from card count
data = data %>% 
  separate(col = value, into = c('count', 'card'), sep = ":") %>% 
  select(-c(name, count))

# pivot wider adding game outcome info to each card
data = data %>% 
  count(day, game_version, card, is_master, opponent, game_outcome) %>% 
  pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0)

# make game version column a bit nicer
data = data %>% 
  mutate(across(game_version, ~word(string = ., start = 2, end = -2, sep = "_")))
  
# fix the problem of game_version updating a day too early (when I have days with 2 different game_versions, assign the earlier one)
fix_gv = data %>%
  distinct(day, game_version) %>% 
  mutate(num_game_version = str_remove(game_version, "_") %>% as.numeric()) %>%
  group_by(day) %>% 
  slice_min(n = 1, order_by = num_game_version, with_ties = FALSE) %>% 
  ungroup() %>% 
  select(day, fix_game_version = game_version)

data = data %>% 
  left_join(fix_gv, by = 'day') %>%
  mutate(game_version = fix_game_version) %>% 
  select(-fix_game_version)

# if no ties this week, add column of zeros
if(!"tie" %in% colnames(data)){ data = data %>% mutate(tie = 0) }

# add number of decks containing that card in that day for that category
data = data %>% 
  mutate(n = win + loss + tie)

# add card name
data = data %>% 
  left_join(data_cards, by = c('card' = 'cardCode'))

# relocate columns
data = data %>% 
  relocate(day, is_master, opponent, card, name, rarity, game_version, win, loss, tie, n)

# 5. save to MySQL db ----

# save matches to db v2
if(nrow(data) >  0){
  
  data %>% 
    DBI::dbWriteTable(conn = con, name = "expedition_cards", value = ., append = TRUE, row.names = FALSE) 
  
  n_games %>% 
    DBI::dbWriteTable(conn = con, name = "expedition_ngames", value = ., append = TRUE, row.names = FALSE) 
  
}

DBI::dbDisconnect(con)

# send update message on discord
lorr::send_discord_message(
  username = 'balco-lor.com',
  message = sprintf(
    "Weekly Expedition Update! \n %s new matches found since last update.", 
    scales::comma(nrow(opponent))
  )
)
