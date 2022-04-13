# Convert information from MongoDB (BSON) to tabular format and store in MySQL

# All Servers - Expeditions Matches
# This task is performed with daily frequency at 16.00 UTC

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(mongolite)) # connect to MongoDB
suppressPackageStartupMessages(library(jsonlite))  # convert JSON to R objects
suppressPackageStartupMessages(library(httr))      # http requests

# 2. parameters ----

# import python deck decoder
lor_deckcodes <- reticulate::import("lor_deckcodes")

# load db credentials
mongo_creds <- config::get("mongodb", file = "/home/balco/my_rconfig.yml")
mysql_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# 3. functions ----

# extract region from monoregion champs
get_monoregion <- function(champs){
  
  # split champions
  champs <- str_split(champs, pattern = " ") %>% unlist()
  
  # get monoregions
  data_champs %>% 
    filter(cardCode %in% champs) %>% 
    mutate(n_regions = map_int(regionRefs, length)) %>% 
    filter(n_regions == 1) %>% 
    pull(cardCode) %>%
    paste0(collapse = " ") %>% 
    str_remove_all(pattern = "[0-9]")
  
}

# get PUUIDs of master players (from name)
get_master_puuids <- function(region, master_names){
  
  tbl(con, 'lor_players') %>% 
    filter(region == local(region), gameName %in% local(master_names)) %>% 
    collect() %>% 
    {if(nrow(.) > 0) pull(., puuid) else NA_character_}
  
}

# 4. connect to db & load data ----

# connect to MongoDB
m_db_eu <- mongo(url = sprintf("mongodb://%s:%s@localhost:27017/admin", mongo_creds$uid, mongo_creds$pwd), collection = "lor_match_info")
m_db_na <- mongo(url = sprintf("mongodb://%s:%s@localhost:27017/admin", mongo_creds$uid, mongo_creds$pwd), collection = "lor_match_info_na")
m_db_as <- mongo(url = sprintf("mongodb://%s:%s@localhost:27017/admin", mongo_creds$uid, mongo_creds$pwd), collection = "lor_match_info_asia")

# close previous connections to MySQL database (if any)
if(exists("con")){ DBI::dbDisconnect(con) }

# create connection to MySQL database
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  db_host = "127.0.0.1",
  user = mysql_creds$uid,
  password = mysql_creds$pwd,
  dbname = mysql_creds$dbs
)

# get matches already in sql
already_in_sql <- tbl(con, "expedition_match") %>% 
  distinct(match_id) %>% 
  collect() %>% 
  pull() %>%
  paste0(collapse = '\", \"') %>% 
  paste0("\"", ., "\"")

# convert "game_start_time_utc" to MongoDB class Date & read new data from MongoDB
data_eu <- m_db_eu$find(query = sprintf('{"info.game_mode":"Expeditions", "metadata.match_id" : { "$nin" : [ %s ] } }', already_in_sql))
data_na <- m_db_na$find(query = sprintf('{"info.game_mode":"Expeditions", "metadata.match_id" : { "$nin" : [ %s ] } }', already_in_sql))
data_as <- m_db_as$find(query = sprintf('{"info.game_mode":"Expeditions", "metadata.match_id" : { "$nin" : [ %s ] } }', already_in_sql))

data <- bind_rows('europe' = data_eu, 'americas' = data_na, 'asia' = data_as, .id = 'region')

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
data_champs <- map_dfr(
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
  filter(rarity == "Champion") %>% 
  select(name, cardCode, regionRefs) %>%
  filter(nchar(cardCode) <= 8) # additional check because sometimes Riot messes up

# master leaderboard 
as_leaderboard <- tbl(con, "leaderboard_asia") %>% pull(name)
eu_leaderboard <- tbl(con, "leaderboard_eu") %>% pull(name)
na_leaderboard <- tbl(con, "leaderboard_na") %>% pull(name)

# puuids of last season master players
as_old_leaderboard <- readRDS("/home/balco/dev/lor-meta-report/templates/master_leaderboards/old_asia.rds")
eu_old_leaderboard <- readRDS("/home/balco/dev/lor-meta-report/templates/master_leaderboards/old_europe.rds")
na_old_leaderboard <- readRDS("/home/balco/dev/lor-meta-report/templates/master_leaderboards/old_americas.rds")

# get puuids from names
as_master_puuids <- get_master_puuids(region = 'asia',     master_names = as_leaderboard)
eu_master_puuids <- get_master_puuids(region = 'europe',   master_names = eu_leaderboard)
na_master_puuids <- get_master_puuids(region = 'americas', master_names = na_leaderboard)

master_puuids = c(as_master_puuids, eu_master_puuids, na_master_puuids)

as_old_master_puuids <- get_master_puuids(region = 'asia',     master_names = as_old_leaderboard)
eu_old_master_puuids <- get_master_puuids(region = 'europe',   master_names = eu_old_leaderboard)
na_old_master_puuids <- get_master_puuids(region = 'americas', master_names = na_old_leaderboard)

old_master_puuids = c(as_old_master_puuids, eu_old_master_puuids, na_old_master_puuids)

# 5. convert from BSON to tabular ----

# unpack data & remove useless column
data <- data %>%
  unpack(cols = -region) %>% 
  select(-participants)

# remove matches that have wrong game_start_time_utc format (should be few and will get added later on anyway)
data <- data %>% 
  filter(str_detect(game_start_time_utc, pattern = "[0-9]{4}-"))

# unnest "players" column
data <- data %>% 
  unnest(cols = players, keep_empty = TRUE)

# remove useless column
data <- data %>% 
  select(-c(data_version, deck_id))

# unpack factions column & make nicer
data <- data %>% 
  mutate(factions = map_chr(factions, str_flatten, collapse = " ")) %>% 
  mutate(factions = str_remove_all(factions, pattern = "faction_|_Name"))

# fix change in game version
data <- data %>%
  mutate(game_version = str_replace_all(game_version, pattern = '-', replacement = '_'))

# make game_start_time_utc a date
data <- data %>% 
  mutate(game_start_time_utc = str_remove(game_start_time_utc, pattern = ".000Z")) %>% 
  mutate(game_start_time_utc = str_replace(game_start_time_utc, pattern = "T", replacement = " "))

# extract card codes from deck code
data <- data %>%
  distinct(deck_code) %>%
  mutate(cards_list = map(.x = deck_code, .f = lor_deckcodes$decode$decode_deck)) %>%
  left_join(x = data, y = ., by = "deck_code")

# get deck champions & cards
data <- data %>%
  distinct(across(c(factions, cards_list))) %>%
  mutate(
    cards = map_chr(cards_list, str_flatten, collapse = " "),
    champs = str_extract_all(cards, pattern = paste(data_champs$cardCode, collapse = "|")),
    champs = map_chr(champs, str_flatten, collapse = " "),
    across(champs, function(x) unname(sapply(x, function(x) { paste(sort(trimws(strsplit(x[1], ' ')[[1]])), collapse=' ')} )))
  ) %>%
  left_join(data, ., by = c("factions", "cards_list")) %>%
  select(-cards_list)
 
# add check whether the player is master or not
data <- data %>%
  mutate(is_master = case_when(
    puuid %in% master_puuids ~ 2,
    puuid %in% old_master_puuids ~ 1,
    TRUE ~ 0
  ))

# 6. save to MySQL db ----

# save matches to db v2
if(nrow(data) >  0){

  data %>%
    DBI::dbWriteTable(conn = con, name = "expedition_match", value = ., append = TRUE, row.names = FALSE)

}

DBI::dbDisconnect(con)
