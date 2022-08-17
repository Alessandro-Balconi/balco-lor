# Creates a MySQL table with decklists information for the current patch (from data of MySQL databases)

# 1. libraries ----

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))

# 2. connect to db & load data ----

# create connection to MySQL database
con <- lorr::create_db_con()

# current patch release date
min_date <- lorr::get_patch_release_date()
#min_date <- as.POSIXct("2021-12-14 18:00:00 UTC") # hotfix date

# 3 table of current patch v2 ----

# extract data from MySQL
data_v2 <- tbl(con, "ranked_match_metadata_30d") %>%
  filter(game_start_time_utc >= min_date) %>%
  left_join(tbl(con, 'ranked_match_info_30d'), by = 'match_id') %>% 
  left_join(tbl(con, 'utils_archetype_aggregation'), by = c('archetype' = 'old_name')) %>% 
  mutate(archetype = coalesce(new_name, archetype)) %>% 
  count(archetype, deck_code, cards) %>% 
  filter(n >= 5) %>% 
  select(-n) %>% 
  collect() %>% 
  ungroup()

# reshape df
data_v2 <- data_v2 %>% 
  separate(col = cards, into = sprintf('card_%s', 1:40), sep = ' ', fill = 'right') %>% 
  pivot_longer(cols = -c(archetype, deck_code), values_drop_na = TRUE, names_to = NULL) %>% 
  separate(col = value, into = c('count', 'card_code'), sep = ':')

# 4. save to MySQL db ----

if(nrow(data_v2) >  0){
  
  data_v2 %>% 
    DBI::dbWriteTable(conn = con, name = "utils_ranked_patch_decklists_cards", value = ., overwrite = TRUE, row.names = FALSE) 
  
  DBI::dbExecute(
    conn = con,
    statement = sprintf(
      "REPLACE INTO utils_update_time
        (table_name, time)
        VALUES
        (%s);",
      paste0("'", paste0(c('utils_ranked_patch_decklists_cards', as.charactetr(Sys.time())), collapse = "', '"), "'")
    )
  )
  
}

DBI::dbDisconnect(con)
