# Creates a MySQL table with decklists information for the current patch (from data of MySQL databases)

# This task is performed with daily frequency at 16.30 UTC

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package

# 2. parameters ----

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

archetypes <- tbl(con, "lor_match_info_na") %>% 
  union_all(tbl(con, "lor_match_info")) %>% 
  union_all(tbl(con, "lor_match_info_asia")) %>% 
  filter(str_detect(game_version, current_patch)) %>%
  mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATETIME)")) %>% 
  filter(game_start_time_utc >= min_date) %>% 
  count(archetype) %>% 
  filter(n >= 950) %>% 
  collect() %>% 
  pull(archetype)
  
# extract data from MySQL
data <- tbl(con, "lor_match_info_na") %>%
  union_all(tbl(con, "lor_match_info")) %>% 
  union_all(tbl(con, "lor_match_info_asia")) %>% 
  filter(str_detect(game_version, current_patch), archetype %in% archetypes) %>% 
  mutate(game_start_time_utc = sql("CAST(game_start_time_utc AS DATETIME)")) %>% 
  filter(game_start_time_utc >= min_date) %>% 
  count(game_outcome, archetype, deck_code) %>% 
  collect()

# # merge archetypes according to mapping
# archetypes_map <- readr::read_csv("/home/balco/dev/lor-meta-report/templates/archetypes_map.csv")
# 
# data <- data %>% 
#   left_join(archetypes_map, by = c("archetype" = "old_name")) %>% 
#   mutate(archetype = ifelse(!is.na(new_name), new_name, archetype)) %>% 
#   select(-new_name)

# calculate information
data_decks <- data %>% 
  pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0)

data_decks <- data_decks %>% 
  rowwise() %>% 
  mutate(match = sum(c_across(where(is.numeric)))) %>% 
  ungroup() %>% 
  filter(match >= 5) %>% 
  {if(nrow(.)>0) mutate(., winrate = win / match) else . } %>% 
  {if(nrow(.)>0) select(., archetype, deck_code, match, winrate) else . }

# 6. save to MySQL db ----

# first initialization of database
#DBI::dbWriteTable(conn = con, name = "lor_decklists", value = data_decks, row.names = FALSE)

# save matches to db
if(nrow(data_decks) >  0){
  
  data_decks %>% 
    DBI::dbWriteTable(conn = con, name = "lor_decklists", value = ., overwrite = TRUE, row.names = FALSE) 
  
}

DBI::dbDisconnect(con)
