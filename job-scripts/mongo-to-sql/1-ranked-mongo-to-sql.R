# Convert information from MongoDB (BSON) to tabular format and store in MySQL

# This task is performed every 8 hours at 00:15, 8:15, 16:15 UTC

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(lubridate)) # work with dates
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
  )) %>% 
  mutate(abbreviation = if_else(abbreviation %in% data_champs$name, 'RU', abbreviation)) # fix RU champs


# function to pull region specific data and make the update
mongo_to_sql <- function(input_region){
  
  # name of the region-specific mongo collection 
  mongo_collection <- switch(
    input_region,
    'europe'   = 'lor_match_info',
    'americas' = 'lor_match_info_na',
    'asia'     = 'lor_match_info_asia'
  )
  
  # connect to MongoDB
  m_db <- mongo(url = sprintf("mongodb://%s:%s@localhost:27017/admin", mongo_creds$uid, mongo_creds$pwd), collection = mongo_collection)
  
  # get matches already in sql
  focus_time <- Sys.time()-days(7)
  mongo_focus_time <- as.character(focus_time) %>% str_replace(pattern = " ", replacement = "T") %>% paste0(., ".000Z")
  
  already_in_sql <- tbl(con, "ranked_match_metadata_30d") %>% 
    filter(region == input_region, game_start_time_utc >= focus_time) %>% 
    distinct(match_id) %>% 
    collect() %>% 
    pull() %>%
    paste0(collapse = '\", \"') %>% 
    paste0("\"", ., "\"")
  
  # read new data from MongoDB
  data <- m_db$find(query = sprintf('{"info.game_type":"Ranked", "info.game_start_time_utc" : { "$gte" : "%s"}, "metadata.match_id" : { "$nin" : [ %s ] } }', mongo_focus_time, already_in_sql))
  
  # name of the region-specific leaderboard table in MySQL
  tbl_leaderboard <- switch(
    input_region,
    'europe'   = 'leaderboard_eu',
    'americas' = 'leaderboard_na',
    'asia'     = 'leaderboard_asia'
  )
  
  # master leaderboard 
  leaderboard <- tbl(con, tbl_leaderboard) %>%
    collect() %>% 
    pull(name)
  
  # get PUUIDs of master players
  master_puuids <- tbl(con, 'utils_players') %>% 
    filter(region == input_region, gameName %in% local(leaderboard)) %>% 
    collect() %>% 
    {if(nrow(.) > 0) pull(., puuid) else NA_character_}
  
  # old master leaderboard
  old_master_players <- readRDS(paste0("/home/balco/dev/lor-meta-report/templates/master_leaderboards/old_", input_region, ".rds"))

  # get PUUIDs of old master players (current plat+)
  plat_puuids <- tbl(con, 'utils_players') %>% 
    filter(region == input_region, gameName %in% local(old_master_players)) %>% 
    collect() %>% 
    {if(nrow(.) > 0) pull(., puuid) else NA_character_}
  
  # 5. convert from BSON to tabular ----
  
  if(nrow(data) == 0){
    
    discordr::create_discord_connection(
      webhook = 'https://discord.com/api/webhooks/940930457070096444/qBSYJH0KETu992oDrdJBH20H1j4yPbBMZm2T3KNKZA5AU1LhRypZshQ0uKly9N_7jeGy',
      username = sprintf("%s - Daily database update", toupper(input_region))
    ) %>% 
      discordr::send_webhook_message(
        message = "No new matches found since the last update. Check for issues.", 
        conn = .
      )
    
  } else {
    
    # unpack data & remove useless column
    data <- data %>%
      unpack(cols = everything()) %>% 
      select(-participants)
    
    # fix date format
    data <- data %>% 
      mutate(game_start_time_utc = str_remove(game_start_time_utc, pattern = ".000Z")) %>% 
      mutate(game_start_time_utc = str_remove(game_start_time_utc, pattern = "Z")) %>% 
      mutate(game_start_time_utc = str_replace(game_start_time_utc, pattern = "T", replacement = " "))
    
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
    
    # fix change in game version
    data <- data %>%
      mutate(game_version = str_replace_all(game_version, pattern = '-', replacement = '_')) %>% 
      mutate(game_version = str_replace_all(game_version, pattern = '_green_', replacement = '_'))
    
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
      #mutate(cards_list = map(deck_code, lordecks::get_decklist_from_code, format = 'simple')) %>% 
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
    
    # add player rank (master, plat+, any)
    data <- data %>% 
      mutate(
        player_rank = case_when(
          puuid %in% master_puuids ~ 2,
          puuid %in% plat_puuids ~ 1,
          TRUE ~ 0
        )
      )
    
    # add region
    data <- data %>% 
      mutate(region = input_region)

    # 6. save to MySQL db ----
    
    # save matches to db v2
    if(nrow(data) >  0){
      
      data %>%
        group_by(match_id, game_start_time_utc, game_version, total_turn_count, region) %>%
        summarise(match_rank = sum(player_rank, na.rm = TRUE), .groups = 'drop') %>%
        DBI::dbWriteTable(conn = con, name = "ranked_match_metadata_30d", value = ., append = TRUE, row.names = FALSE)
      
      data %>%
        select(match_id, puuid, deck_code, game_outcome, order_of_play, faction_1, faction_2, cards, archetype, player_rank) %>%
        DBI::dbWriteTable(conn = con, name = "ranked_match_info_30d", value = ., append = TRUE, row.names = FALSE)
      
    }
 
  }
   
}

# call function
mongo_to_sql('europe')
mongo_to_sql('americas')
mongo_to_sql('asia')

DBI::dbDisconnect(con)
