# Convert information from MongoDB (BSON) to tabular format and store in MySQL

# Americas Server
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

# 4. connect to db & load data ----

# connect to MongoDB
m_db <- mongo(url = sprintf("mongodb://%s:%s@localhost:27017/admin", mongo_creds$uid, mongo_creds$pwd), collection = "lor_match_info_na")

# close previous connections to MySQL database (if any)
if(exists("con")){ DBI::dbDisconnect(con) }

# create connection to MySQL database
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  db_host = "127.0.0.1",
  user = mysql_creds$uid,
  password = mysql_creds$pwd,
  dbname = "db_prova"
)

# get matches already in sql
already_in_sql <- tbl(con, "lor_match_info_na") %>% 
  distinct(match_id) %>% 
  collect() %>% 
  pull() %>%
  paste0(collapse = '\", \"') %>% 
  paste0("\"", ., "\"")

# convert "game_start_time_utc" to MongoDB class Date 
m_db$update(
  query  = '{}',
  update = '[{"$set":{"info.game_start_time_utc": { "$toDate": "$info.game_start_time_utc" }}}]', 
  multiple = TRUE
)

# read new data from MongoDB
data <- m_db$find(query = sprintf('{"info.game_type":"Ranked", "metadata.match_id" : { "$nin" : [ %s ] } }', already_in_sql))

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

# regions names / abbreviations / logos from global JSON
data_regions <- "https://dd.b.pvp.net/latest/core/en_us/data/globals-en_us.json" %>% 
  GET() %>% 
  content(encoding = "UTF-8") %>% 
  fromJSON() %>% 
  .[["regions"]] %>% 
  mutate(nameRef = case_when(
    nameRef == "PiltoverZaun" ~ "Piltover",
    nameRef == "Targon" ~ "MtTargon",
    TRUE ~ nameRef
  ))

# 5. convert from BSON to tabular ----

# unpack data & remove useless column
data <- data %>%
  unpack(cols = everything()) %>% 
  select(-participants)

# remove matches that have wrong game_start_time_utc format (should be few and will get added later on anyway)
data <- data %>% 
  filter(str_detect(game_start_time_utc, pattern = "[0-9]{4}-"))

# THIS SHOULD NOT BE NEEDED BUT IT IS, WHY???
data <- data %>% 
  distinct()

# unnest "players" column
data <- data %>% 
  unnest(cols = players, keep_empty = TRUE)

# remove useless column
data <- data %>% 
  select(-c(data_version, deck_id))

# unpack factions column
data <- data %>% 
  unnest(cols = factions, keep_empty = TRUE)

# make faction column nicer
data <- data %>% 
  mutate(factions = str_extract(factions, pattern = "(?<=_)(.+)(?=\\_)"))

# reshape (keeping only 1 row per deck)
data <- data %>% 
  group_by(match_id, puuid, deck_code, game_outcome) %>%
  mutate(id = row_number()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = id, values_from = factions, names_prefix = "faction_")

# extract card codes from deck code
data <- data %>% 
  distinct(deck_code) %>% 
  mutate(cards_list = map(.x = deck_code, .f = lor_deckcodes$decode$decode_deck)) %>% 
  left_join(x = data, y = ., by = "deck_code")

# get deck champions & archetype
data <- data %>%
  distinct(across(c(starts_with("faction_"), cards_list))) %>% 
  mutate(
    cards = map_chr(cards_list, str_flatten, collapse = " "),
    champs = str_extract_all(cards, pattern = paste(data_champs$cardCode, collapse = "|")),
    champs = map_chr(champs, str_flatten, collapse = " "),
    champs_factions = map_chr(champs, get_monoregion)
  ) %>% 
  left_join(data_regions %>% select(faction_abb1 = abbreviation, nameRef), by = c("faction_1" = "nameRef")) %>% 
  left_join(data_regions %>% select(faction_abb2 = abbreviation, nameRef), by = c("faction_2" = "nameRef")) %>%
  unite(col = factions, faction_abb1, faction_abb2, sep = " ") %>% 
  mutate(
    factions = str_remove_all(factions, pattern = " NA|NA "),
    across(c(champs, champs_factions, factions),  function(x) unname(sapply(x, function(x) { paste(sort(trimws(strsplit(x[1], ' ')[[1]])), collapse=' ')} ))),
    no_fix = map2_lgl(.x = factions, .y = champs_factions, .f = ~grepl(pattern = .x, x = .y)),
    champs_factions = str_replace_all(champs_factions, pattern = " ", replacement = "|"),
    champs_factions = paste0(champs_factions, "| "),
    factions_to_add = str_remove_all(factions, pattern = champs_factions),
    archetype = if_else(no_fix, champs, sprintf("%s (%s)", champs, factions_to_add))
  ) %>% 
  left_join(data, ., by = c("faction_1", "faction_2", "cards_list")) %>% 
  select(-c(cards_list, champs_factions, factions, no_fix, factions_to_add))

# make archetype name nicer
data <- data %>% 
  mutate(archetype = str_replace_all(archetype, set_names(data_champs$name, data_champs$cardCode))) %>% 
  mutate(across(archetype, function(x) ifelse(grepl("^( )", x), paste0("No Champions", x), x))) 

# 6. save to MySQL db ----

# first initialization of database
#DBI::dbWriteTable(conn = con, name = "lor_match_info_na", value = data, row.names = FALSE)

# save matches to db
if(nrow(data) >  0){
  
  data %>% 
    DBI::dbWriteTable(conn = con, name = "lor_match_info_na", value = ., append = TRUE, row.names = FALSE) 
  
}

DBI::dbDisconnect(con)
