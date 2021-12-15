# Creates a MySQL table with decklists information for the current patch (from data of MySQL databases)

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(googlesheets4)) # working with google spreadsheets

# 2. parameters ----

options(gargle_oauth_email = "Balco21@outlook.it")
options(googlesheets4_quiet = TRUE)

# date of 3/7 days ago
last3_date <- (Sys.time() - lubridate::days(3))
last7_date <- (Sys.time() - lubridate::days(7))

# 3. functions ----

# 4. connect to db & load data ----

# load mysql db credentials
db_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

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
#min_date <- tbl(con, "lor_match_info_v2") %>% 
#  filter(str_detect(game_version, current_patch)) %>%
#  select(game_start_time_utc) %>% 
#  mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATETIME)")) %>% 
#  summarise(min_date = min(game_start_time_utc, na.rm = TRUE)) %>% 
#  collect() %>% 
#  mutate(min_date = min_date + lubridate::days(1)) %>% 
#  pull()
min_date <- as.POSIXct("2021-12-14 18:00:00 UTC") # hotfix date

# merge archetypes according to mapping
archetypes_map <- with_gs4_quiet(read_sheet(ss = "1Xlh2kg7gLzvqugqGPpI4PidAdM5snggbJ44aRLuik5E", sheet = 'Archetypes Mapping'))

# games played by archetype (needed for next step)
archetypes <- tbl(con, "lor_match_info_v2") %>% 
  filter(str_detect(game_version, current_patch)) %>%
  mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATETIME)")) %>% 
  filter(game_start_time_utc >= min_date) %>% 
  count(archetype) %>% 
  filter(n >= 5) %>% 
  collect()
  
# archetype need at least 950 match played to be imported
archetypes <- archetypes %>% 
  left_join(archetypes_map, by = c('archetype' = 'old_name')) %>% 
  mutate(new_name = coalesce(new_name, archetype)) %>% 
  group_by(new_name) %>% 
  mutate(n = sum(n, na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(n >= 950) %>% 
  pull(archetype)

# 5.2 table of current patch v2 ----

# extract data from MySQL
data_v2 <- tbl(con, "lor_match_info_v2") %>%
  filter(str_detect(game_version, current_patch), archetype %in% archetypes) %>% 
  mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATETIME)")) %>% 
  filter(game_start_time_utc >= min_date) %>% 
  mutate(time_frame = case_when(game_start_time_utc >= last3_date ~ 2, game_start_time_utc >= last7_date ~ 1, TRUE ~ 0)) %>% 
  count(game_outcome, archetype, deck_code, time_frame, is_master) %>% 
  collect()

data_v2 <- data_v2 %>%
  left_join(archetypes_map, by = c("archetype" = "old_name")) %>%
  mutate(archetype = ifelse(!is.na(new_name), new_name, archetype)) %>%
  select(-new_name)

# calculate information
data_decks_v2 <- data_v2 %>% 
  pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0)

data_decks_v2 <- data_decks_v2 %>%
  rowwise() %>% 
  mutate(match = sum(c_across(c(win, loss, matches("tie"))))) %>% 
  ungroup() %>% 
  filter(match >= 5) %>% 
  {if(nrow(.)>0) mutate(., winrate = win / match) else . } %>% 
  {if(nrow(.)>0) select(., archetype, deck_code, match, winrate, time_frame, is_master) else . }

# 6. save to MySQL db ----

if(nrow(data_decks_v2) >  0){
  
  data_decks_v2 %>% 
    DBI::dbWriteTable(conn = con, name = "lor_decklists_v2", value = ., overwrite = TRUE, row.names = FALSE) 
  
}

DBI::dbDisconnect(con)
