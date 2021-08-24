# This script performs all the preprocessing needed before running the report:
# 
# 1. Write historical region playrate to "lor_region_play" database
# 2. Write recent matches to "lor_match_recent" database
# 3. Define Deck Archetypes using Latent Dirichlet Allocation (on "lor_match_recent" data) & save to database

# I.   libraries ----

library(httr)
library(rvest)
library(DBI)
library(RMySQL)
library(tidyverse)
library(magrittr)
library(lubridate)
library(reticulate)
library(reshape2)
#library(tidytext)
#library(topicmodels)

# II.  functions ----

# extract card list from deck code
get_cards_from_code <- function(code){
  
  cards <- deck(code) %>% 
    extract2("cards") %>%
    map("card_code") %>% 
    map_chr(c) 
  
  return(cards)
  
}

# III. parameters ----

# load python modules
lor_deckcodes <- import("lor_deckcodes")
deck <- lor_deckcodes$LoRDeck$from_deckcode

# IV.  connect to db & load data ----

# create connection to database
con <- dbConnect(
  MySQL(),
  db_host = "127.0.0.1",
  user = "X",
  password = "X",
  dbname = "db_prova"
)

# load complete database
data_tot <- tbl(con, "lor_match") %>% 
  collect()

# list of codes of all runeterra cards
card_codes <- read_html("https://leagueoflegends.fandom.com/wiki/List_of_cards_from_Legends_of_Runeterra") %>% 
  html_table(fill = TRUE) %>% 
  extract2(2) %>% 
  as_tibble(.name_repair = make.names) %>% 
  select(1, 2, 5) %>% 
  filter(nchar(X) <= 8) %>% # keep only collectible cards 
  mutate(Region = case_when( # same region names as API
    Region == "Piltover & Zaun" ~ "Piltover",
    Region == "Shadow Isles" ~ "ShadowIsles",
    Region == "Targon" ~ "MtTargon", 
    TRUE ~ Region
  ))

# before doing anything, make sure to remove all duplicates from databases (there shouldn't be many but it might happen)
data_tot <- data_tot %>%
  distinct() %>% 
  group_by(matchid) %>% # consistency check, should happen to very few cases and only on season reset / before prod key
  slice_max(n = 1, order_by = player_1_is_mst+player_2_is_mst, with_ties = FALSE) %>% 
  ungroup()

data_tot %>% 
  dbWriteTable(conn = con, name = "lor_match", value = ., overwrite = TRUE, row.names = FALSE)

# 1. Write historical region playrate to "lor_region_play" database ----

# number of games collected by isoweek
match_by_week <- data_tot %>% 
  mutate(week = isoweek(date), year = year(date)) %>% 
  group_by(week, year) %>% 
  count(name = "match") %>% 
  ungroup()

# group by week and summarise
lor_region_play <- data_tot %>% 
  mutate(date = ymd(date)) %>% 
  select(date, starts_with("faction")) %>%
  pivot_longer(cols = -date, names_to = "key", values_to = "region") %>% 
  mutate(week = isoweek(date), year = year(date)) %>%
  group_by(week, year, region) %>%
  summarise(n = n(), date = ceiling_date(max(date), unit = "week", week_start = 1) - days(1), .groups = "drop") %>% 
  left_join(match_by_week, by = c("week", "year")) %>%
  filter(match > 100) %>% # filter to ensure the very first weeks are removed
  drop_na(region) %>% 
  mutate(playrate = n / (match*2))

# save to database
lor_region_play %>% 
  dbWriteTable(conn = con, name = "lor_region_play", value = ., overwrite = TRUE, row.names = FALSE) 

# 2. Write recent matches to "lor_match_recent" database ----

# keep only last 2 weeks of data
data <- data_tot %>% 
  mutate(date = ymd(date)) %>% 
  mutate(week = isoweek(date)) %>% 
  filter(week >= max(week) - 1) %>% 
  select(-week)

# save to database
data %>% 
  dbWriteTable(conn = con, name = "lor_match_recent", value = ., overwrite = TRUE, row.names = FALSE) 

# # 3. Define Deck Archetypes using Latent Dirichlet Allocation (on "lor_match_recent" data) & save to database ----
# 
# # keep only deck cards for now
# data_arch <- data %>% 
#   select(matchid, deck_1_cards, deck_2_cards)
# 
# # pivot longer
# data_arch <- data_arch %>% 
#   pivot_longer(cols = -matchid)
# 
# # simplify "name" column
# data_arch <- data_arch %>% 
#   mutate(name = str_remove(name, pattern = "_cards"))
# 
# # unite "matchid" and "name" into a "key" column
# data_arch <- data_arch %>% 
#   unite(col = key, name, matchid, sep = "..")
# 
# # separate cards
# data_arch <- data_arch %>% 
#   mutate(across(value, ~map(., .f = str_split, pattern = " "))) %>%
#   mutate(across(value, ~map(., .f = 1))) %>% 
#   unnest(value)
# 
# # convert to DocumentTermMatrix object
# data_dtm <- data_arch %>% 
#   mutate(count = 1) %>% 
#   cast_dtm(document = key, term = value, count)
# 
# # perform Latent Dirichlet Allocation
# # K = 5 :   52.13 sec elapsed
# # K = 10:  ?
# # K = 20:  352.96 sec elapsed
# # K = 25 SEMBRA UN BUON PARAMETRO
# # K = 30: ?
# tictoc::tic()
# data_lda <- data_dtm %>% 
#   LDA(k = 20, control = list(seed = 210595))
# tictoc::toc()
# 
# # convert back to tibble, keeping beta information
# data_beta <- tidy(data_lda, matrix = "beta")
# 
# # add card name
# data_beta <- data_beta %>% 
#   left_join(card_codes, by = c("term" = "X"))
# 
# # most decisive cards in defining an archetype
# key_cards <- data_beta %>%
#   group_by(topic) %>%
#   slice_max(beta, n = 10) %>% 
#   ungroup() %>%
#   arrange(topic, -beta)
# 
# ### HERE IS WHERE I MANUALLY CHECK THE ARCHETYPES AND LABEL THEM ###
# arch_names <- tibble(
#   topic = seq(from = 1, to = 10),
#   arch = c(
#     "Overwhelm Shurima Freljord",
#     "Matron Combo",
#     "Puffcaps Freljord",
#     "Barrier Demacia",
#     "Scargrounds Midrange",
#     "Targon Invoke",
#     "Discard Aggro",
#     "Demacia Rally",
#     "Spider Aggro",
#     "Heal Tempo",
#     "Frostbite Midrange",
#     "Deep",
#     "Ledros Atrocity",
#     "Lee Targon",
#     "Nasus Atrocity",
#     "SI Shurima Aggro",
#     "Ezreal Draven",
#     "Dragon Demacia Targon",
#     "P&Z Pile",
#     "Pirate Aggro"
#   )
# )
# 
# # extract gamma parameter
# data_gamma <- tidy(data_lda, matrix = "gamma")
# 
# # archetypes played for every matchid
# match_arch <- data_gamma %>% 
#   group_by(document) %>%
#   slice_max(gamma) %>%
#   ungroup() %>% 
#   select(-gamma) %>% 
#   separate(col = document, into = c("deck_number", "matchid"), sep = "\\.\\.") %>%
#   left_join(arch_names, by = "topic") %>% 
#   select(-topic) %>% 
#   pivot_wider(names_from = deck_number, values_from = arch, names_prefix = "arch_")
# 
# # join back with main data
# data <- data %>% 
#   left_join(match_arch, by = "matchid")
# 
# # save database to "lor_match_arch"
# data %>% 
#   dbWriteTable(conn = con, name = "lor_arch", value = ., overwrite = TRUE, row.names = FALSE) 
