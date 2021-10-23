# Creates a MySQL table with matchup informations for the current patch (from data of MySQL databases)

# This task is performed with daily frequency at 16.30 UTC

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package

# 2. parameters ----

# load mysql db credentials
db_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

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
  dbname = "db_prova"
)

# 5. prepare table ----

patches_to_analyze <- tbl(con, "lor_patch_history") %>% 
  collect() %>% 
  arrange(-last_patch) %>% 
  mutate(new_change = lag(change)) %>% 
  replace_na(list(new_change = 0)) %>% 
  mutate(cum_change = cumsum(new_change)) %>% 
  filter(cum_change == min(cum_change)) %>%
  pull(value) %>% 
  str_replace_all(pattern = "\\.", replacement = "_")

current_patch <- patches_to_analyze %>% 
  paste0("live_", ., "_") %>% 
  paste0(collapse = "|")

# start collecting matches only 24 hours after the patch
min_date <- tbl(con, "lor_match_info_na") %>% 
  union_all(tbl(con, "lor_match_info")) %>% 
  union_all(tbl(con, "lor_match_info_asia")) %>% 
  filter(str_detect(game_version, current_patch)) %>%
  select(game_start_time_utc) %>% 
  mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATETIME)")) %>% 
  summarise(min_date = min(game_start_time_utc, na.rm = TRUE)) %>% 
  collect() %>% 
  mutate(min_date = min_date + lubridate::days(1)) %>% 
  pull()

# extract data from MySQL
data <- tbl(con, "lor_match_info_na") %>%
  union_all(tbl(con, "lor_match_info")) %>% 
  union_all(tbl(con, "lor_match_info_asia")) %>% 
  filter(str_detect(game_version, current_patch)) %>% 
  mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATETIME)")) %>% 
  filter(game_start_time_utc >= min_date) %>% 
  select(match_id, game_outcome, archetype) %>% 
  collect()

# merge archetypes according to mapping
archetypes_map <- readr::read_csv("/home/balco/dev/lor-meta-report/templates/archetypes_map.csv", col_types = "cc")

data <- data %>%
  left_join(archetypes_map, by = c("archetype" = "old_name")) %>%
  mutate(archetype = ifelse(!is.na(new_name), new_name, archetype)) %>%
  select(-new_name)

# calculate matchup information
data_matchup <- data %>%
  group_by(match_id) %>%
  arrange(match_id, archetype) %>% 
  mutate(id = row_number()) %>% 
  mutate(winner = case_when(id == 1 & game_outcome == "win" ~ 1, id == 2 & game_outcome == "win" ~ 2, TRUE ~ 0)) %>% 
  pivot_wider(names_from = id, values_from = archetype, names_prefix = "archetype_") %>% 
  fill(starts_with("archetype_"), .direction = "updown") %>% 
  ungroup() %>% 
  filter(winner != 0) %>%
  group_by(across(starts_with("archetype_"))) %>% 
  summarise(n = n(), wins = sum(winner == 1), .groups = "drop") %>% 
  mutate(winrate = ifelse(archetype_1 == archetype_2, 0.5, wins / n)) 

# also calculate the matchup information from the opponent's POV
data_matchup2 <- tibble(
  archetype_1 = data_matchup$archetype_2,
  archetype_2 = data_matchup$archetype_1,
  n = data_matchup$n,
  winrate = 1- data_matchup$winrate
)

data_matchup <- data_matchup %>% 
  select(-wins) %>% 
  bind_rows(data_matchup2)

# 6. save to MySQL db ----

# first initialization of database
#DBI::dbWriteTable(conn = con, name = "lor_matchup_table", value = data_matchup, row.names = FALSE)

# save matches to db
if(nrow(data_matchup) >  0){
  
  data_matchup %>% 
    DBI::dbWriteTable(conn = con, name = "lor_matchup_table", value = ., overwrite = TRUE, row.names = FALSE) 

  Sys.time() %>%
    as_tibble() %>% 
    DBI::dbWriteTable(conn = con, name = "lor_update_time", value = ., overwrite = TRUE, row.names = FALSE) 
  
}

DBI::dbDisconnect(con)
