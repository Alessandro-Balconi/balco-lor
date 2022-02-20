# Creates a MySQL table with decklists information for the current patch (from data of MySQL databases)

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package

# 2. parameters ----

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
  dbname = db_creds$dbs
)

# 5. prepare table ----

# patches to analyze
df_patch <- tbl(con, "utils_patch_history") %>% 
  collect() %>% 
  arrange(desc(release_date)) %>% 
  mutate(new_change = lag(change)) %>% 
  replace_na(list(new_change = 0)) %>% 
  mutate(cum_change = cumsum(new_change)) %>% 
  filter(cum_change == min(cum_change)) %>%
  collect()

# pull regex to match in lor_match_info_v2
current_patch <- df_patch %>% 
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

# games played by archetype (needed for next step)
archetypes <- tbl(con, "ranked_daily_archetypes") %>% 
  filter(patch %in% local(df_patch$patch), day >= lubridate::as_date(min_date)) %>%
  left_join(tbl(con, 'utils_archetype_aggregation'), by = c('archetype' = 'old_name')) %>% 
  mutate(archetype = coalesce(new_name, archetype)) %>% 
  group_by(archetype) %>% 
  summarise(n = sum(match, na.rm = TRUE), .groups = 'drop') %>% 
  filter(n >= 950) %>% 
  collect() %>% 
  pull(archetype)

# 5.2 table of current patch v2 ----

# extract data from MySQL
data_v2 <- tbl(con, "lor_match_info_v2") %>%
  filter(str_detect(game_version, current_patch)) %>% 
  left_join(tbl(con, 'utils_archetype_aggregation'), by = c('archetype' = 'old_name')) %>% 
  mutate(archetype = coalesce(new_name, archetype)) %>% 
  filter(archetype %in% archetypes) %>% 
  mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATETIME)")) %>% 
  filter(game_start_time_utc >= min_date) %>% 
  mutate(time_frame = case_when(game_start_time_utc >= last3_date ~ 2, game_start_time_utc >= last7_date ~ 1, TRUE ~ 0)) %>% 
  count(game_outcome, archetype, deck_code, time_frame, is_master) %>% 
  collect() %>% 
  ungroup()

# calculate information
data_decks_v2 <- data_v2 %>% 
  pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0)

data_decks_v2 <- data_decks_v2 %>%
  {if(!'tie' %in% colnames(.)) mutate(., tie = 0) else . } %>% 
  mutate(match = win + loss + tie) %>% 
  filter(match >= 5) %>% 
  {if(nrow(.)>0) mutate(., winrate = win / match) else . } %>% 
  {if(nrow(.)>0) select(., archetype, deck_code, match, winrate, time_frame, is_master) else . }

# 6. save to MySQL db ----

if(nrow(data_decks_v2) >  0){
  
  data_decks_v2 %>% 
    DBI::dbWriteTable(conn = con, name = "ranked_patch_decklists", value = ., overwrite = TRUE, row.names = FALSE) 
  
}

DBI::dbDisconnect(con)
