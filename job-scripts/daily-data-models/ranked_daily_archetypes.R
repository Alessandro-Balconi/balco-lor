# Creates a MySQL table with daily archetype informations (from data of MySQL databases)

# This task is performed with daily frequency at 16.30 UTC

# 1. libraries ----

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(lubridate))

# 4. connect to db & load data ----

# create connection to MySQL database
con <- lorr::create_db_con()

# 5. prepare table ----

# get patch data
patches <- tbl(con, 'utils_patch_history') %>% 
  collect() %>% 
  mutate(patch_day = as_date(release_date)) %>% 
  mutate(previous_patch = lag(patch)) %>% 
  select(patch, patch_regex, previous_patch, patch_day)

# count values per day
data <- tbl(con, 'ranked_match_metadata_30d') %>%
  mutate(day = sql("CAST(game_start_time_utc AS DATE)")) %>%
  filter(day >= local(Sys.Date()-days(3))) %>% 
  left_join(tbl(con, 'ranked_match_info_30d'), by = 'match_id') %>%
  mutate(
    hhour = hour(game_start_time_utc),
    pre_post_patch = if_else(hhour < 16, 'pre', 'post'),
    is_master = if_else(player_rank == 2, 1, 0)
  ) %>%
  count(day, game_version, pre_post_patch, archetype, region, is_master, game_outcome) %>%
  collect() %>% 
  ungroup()

# pivot_wider game version & add total matches played
data = data %>% 
  mutate(n = as.numeric(n)) %>% 
  pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0) %>% 
  {if (!'tie' %in% colnames(.)) mutate(., tie = 0) else .}

# add patch info to data
data = data %>% 
  mutate(game_version = str_replace(game_version, pattern = "[^_]*$", replacement = "")) %>% 
  left_join(patches, by = c('game_version' = 'patch_regex')) %>% 
  mutate(patch = if_else((day < patch_day) | (day == patch_day & pre_post_patch == "pre"), previous_patch, patch))

# summarise results
data = data %>% 
  group_by(day, patch, archetype, region, is_master) %>% 
  summarise(across(c(win, loss, tie), sum), .groups = 'drop') %>% 
  mutate(match = win + loss + tie)

# 6. save to MySQL db ----

# eliminare da ranked_daily_archetypes i dati degli ultimi 3 giorni
delete_query <- paste0("DELETE FROM ranked_daily_archetypes WHERE day >= '", Sys.Date()-days(3), "';")

DBI::dbExecute(con, delete_query)

if(nrow(data) >  0){
  
  data %>% 
    DBI::dbWriteTable(conn = con, name = "ranked_daily_archetypes", value = ., append = TRUE, row.names = FALSE) 
  
  # time of the update
  upd_time <- tibble(
    table_name = 'ranked_daily_archetypes',
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
