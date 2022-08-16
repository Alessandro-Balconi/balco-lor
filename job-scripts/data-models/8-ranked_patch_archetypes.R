# Creates a MySQL table with decklists information for the current patch (from data of MySQL databases)

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package

# 2. connect to db & load data ----

# create connection to MySQL database
con <- lorr::create_db_con()

# current patch release date
min_date <- lorr::get_patch_release_date()
#min_date <- as.POSIXct("2021-12-14 18:00:00 UTC") # hotfix date

# 3. table of current patch v2 ----

# extract data from MySQL
data_v2 <- tbl(con, "ranked_match_metadata_30d") %>%
  filter(game_start_time_utc >= min_date) %>% 
  left_join(tbl(con, 'ranked_match_info_30d'), by = 'match_id') %>% 
  left_join(tbl(con, 'utils_archetype_aggregation'), by = c('archetype' = 'old_name')) %>% 
  mutate(
    archetype = coalesce(new_name, archetype),
    time_frame = case_when(
      game_start_time_utc >= local(Sys.time()-lubridate::days(1)) ~ 3, 
      game_start_time_utc >= local(Sys.time()-lubridate::days(3)) ~ 2, 
      game_start_time_utc >= local(Sys.time()-lubridate::days(7)) ~ 1, 
      TRUE ~ 0),
    is_master = if_else(player_rank == 2, 1, 0)
  ) %>% 
  count(game_outcome, archetype, region, time_frame, is_master) %>% 
  collect() %>% 
  ungroup()

# calculate information
data_v2 <- data_v2 %>% 
  pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0) %>% 
  {if(!'tie' %in% colnames(.)) mutate(., tie = 0) else . } %>% 
  mutate(match = win + loss + tie) %>% 
  {if(nrow(.)>0) select(., archetype, region, time_frame, is_master, match, win) else . }

# 4. save to MySQL db ----

if(nrow(data_v2) >  0){
  
  data_v2 %>% 
    DBI::dbWriteTable(conn = con, name = "ranked_patch_archetypes", value = ., overwrite = TRUE, row.names = FALSE) 
  
  # time of the update
  upd_time <- tibble(
    table_name = 'ranked_patch_archetypes',
    time = Sys.time() %>% as.character()
  )
  
  # run update
  DBI::dbExecute(
    conn = con,
    statement = sprintf(
      "REPLACE INTO utils_update_time
        (table_name, time)
        VALUES
        (%s);",
      paste0("'", paste0(c(upd_time$table_name, upd_time$time), collapse = "', '"), "'")
    )
  )
  
}

DBI::dbDisconnect(con)
