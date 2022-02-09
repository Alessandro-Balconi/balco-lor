# Creates a MySQL table with daily archetype informations (from data of MySQL databases)

# This task is performed with daily frequency at 16.30 UTC

# 1. libraries ----

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(lubridate))

# 2. parameters ----

# load mysql db credentials
db_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# 4. connect to db & load data ----

# close previous connections to MySQL database (if any)
if(exists("con")){ DBI::dbDisconnect(con) }

# create connection to MySQL database
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  db_host  = "127.0.0.1",
  user     = db_creds$uid,
  password = db_creds$pwd,
  dbname   = db_creds$dbs
)

# 5. prepare table ----

# get patch data
patches = tbl(con, 'utils_patch_history') %>% 
  collect() %>% 
  mutate(patch_day = as_date(release_date)) %>% 
  mutate(previous_patch = lag(patch)) %>% 
  select(patch, patch_regex, previous_patch, patch_day)

# count values per day
data = tbl(con, 'lor_match_info_v2') %>%
  mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATETIME)")) %>%
  mutate(day = sql("CAST(game_start_time_utc AS DATE)")) %>%
  filter(day >= local(Sys.Date()-days(3))) %>% 
  mutate(hour = hour(game_start_time_utc)) %>% 
  mutate(pre_post_patch = if_else(hour < 16, 'pre', 'post')) %>% 
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
  
}

DBI::dbDisconnect(con)
