# Creates a MySQL table with matchup informations for the current patch (from data of MySQL databases)

# This task is performed with daily frequency at 16.30 UTC

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package

# 2. parameters ----

# load mysql db credentials
db_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# date of 3/7 days ago
last7_date <- (Sys.time() - lubridate::days(7))
last3_date <- (Sys.time() - lubridate::days(3))

# 3. functions ----

# 4. connect to db & load data ----

# close previous connections to MySQL database (if any)
if(exists("con")){ DBI::dbDisconnect(con) }

# create connection to MySQL database
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  db_host = "127.0.0.1",
  user = db_creds$uid,
  password = db_creds$pwd,
  dbname = db_creds$dbs
)

# 5. prepare table ----

current_patch <- tbl(con, "utils_patch_history") %>% 
  collect() %>% 
  arrange(desc(release_date)) %>% 
  mutate(new_change = lag(change)) %>% 
  replace_na(list(new_change = 0)) %>% 
  mutate(cum_change = cumsum(new_change)) %>% 
  filter(cum_change == min(cum_change)) %>% 
  pull(patch_regex) %>% 
  paste0(collapse = "|")

# start collecting matches only 24 hours after the patch
min_date <- tbl(con, "lor_match_info_v2") %>% 
  filter(str_detect(game_version, current_patch)) %>%
  select(game_start_time_utc) %>% 
  mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATETIME)")) %>% 
  summarise(min_date = min(game_start_time_utc, na.rm = TRUE)) %>% 
  collect() %>% 
  mutate(min_date = min_date + lubridate::days(1)) %>% 
  pull()
#min_date <- as.POSIXct("2021-12-14 18:00:00 UTC") # hotfix date

# archetypes aggregation mapping
archetypes_map <- tbl(con, 'utils_archetype_aggregation') %>% 
  collect()

# 5.3 table v2 ----

# extract data from MySQL
data_v2 <- tbl(con, "lor_match_info_v2") %>%
  filter(str_detect(game_version, current_patch)) %>% 
  mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATETIME)")) %>% 
  filter(game_start_time_utc >= min_date) %>% 
  mutate(time_frame = case_when(game_start_time_utc >= last3_date ~ 2, game_start_time_utc >= last7_date ~ 1, TRUE ~ 0)) %>% 
  select(match_id, game_outcome, archetype, time_frame, is_master) %>% 
  collect()

data_matchup_v2 <- data_v2 %>%
  left_join(archetypes_map, by = c("archetype" = "old_name")) %>%
  mutate(archetype = ifelse(!is.na(new_name), new_name, archetype)) %>%
  select(-new_name)

data_matchup_v2 <- data_matchup_v2 %>% 
  arrange(match_id, archetype) %>% 
  group_by(match_id) %>% 
  mutate(id = row_number()) %>% 
  ungroup()

opponent_data <- data_matchup_v2 %>% 
  mutate(id = case_when(id == 1 ~ 2, id == 2 ~ 1, TRUE ~ 0)) %>% 
  select(match_id, id, archetype_2 = archetype)

data_matchup_v2 <- data_matchup_v2 %>% 
  left_join(opponent_data, by = c("match_id", "id")) %>% 
  select(-id) %>% 
  rename(archetype_1 = archetype)

n_match <- data_matchup_v2 %>% 
  count(archetype_1, archetype_2, time_frame, is_master)

data_matchup_v2 <- data_matchup_v2 %>%
  group_by(archetype_1, archetype_2, time_frame, is_master) %>% 
  summarise(wins = sum(game_outcome == "win"), .groups = "drop") %>% 
  left_join(n_match, by = c("archetype_1", "archetype_2", "time_frame", "is_master")) %>% 
  mutate(winrate = ifelse(archetype_1 == archetype_2, 0.5, wins / n)) %>% 
  select(-wins)

# 6. save to MySQL db ----

if(nrow(data_matchup_v2) >  0){
  
  data_matchup_v2 %>% 
    DBI::dbWriteTable(conn = con, name = "lor_matchup_table_v2", value = ., overwrite = TRUE, row.names = FALSE) 

  # time of the update
  upd_time <- tibble(
    table_name = 'lor_matchup_table_v2',
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
