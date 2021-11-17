m_db <- mongolite::mongo(url = sprintf("mongodb://%s:%s@localhost:27017/admin", db_creds$uid, db_creds$pwd), collection = "lor_match_info_asia")
m_pl <- mongolite::mongo(url = sprintf("mongodb://%s:%s@localhost:27017/admin", db_creds$uid, db_creds$pwd), collection = "lor_player_asia")

pl <- m_pl$find()

x_legna <- m_db$find(query = '{"info.game_start_time_utc" : { "$gte" : { "$date" : "2021-11-16T10:00:00Z" } } }')

data <- x_legna %>%
  unpack(cols = everything()) %>% 
  select(-participants)

data <- data %>% 
  unnest(cols = players, keep_empty = TRUE)

data <- data %>% 
  left_join(pl, by = 'puuid') %>% 
  select(-puuid)

saveRDS(data, file = 'scarico_asia.rds')