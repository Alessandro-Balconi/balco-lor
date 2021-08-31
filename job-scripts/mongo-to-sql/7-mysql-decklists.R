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
  RMySQL::MySQL(),
  db_host = "127.0.0.1",
  user = db_creds$uid,
  password = db_creds$pwd,
  dbname = "db_prova"
)

# 5. prepare table ----

# latest patch with data available (with at least 2k games in NA)
current_patch <- tbl(con, "lor_match_info_na") %>% 
  count(game_version) %>%
  collect() %>% 
  mutate(across(game_version, ~word(string = ., start = 2, end = -2, sep = "_"))) %>%
  group_by(game_version) %>% 
  summarise(n = sum(n), .groups = "drop") %>%
  {if(max(.$n)>2000) filter(., n > 5000) else .} %>% 
  separate(col = game_version, into = c("version", "patch"), sep = "\\_", convert = TRUE) %>% 
  mutate(last_patch = version*100+patch) %>% 
  slice_max(n = 1, order_by = last_patch, with_ties = FALSE) %>% 
  unite(col = value, version, patch, sep = "_") %>%
  pull(value)

patches_to_analyze <- tbl(con, "lor_patch_history") %>% 
  collect() %>% 
  arrange(-last_patch) %>% 
  mutate(cum_change = cumsum(change)) %>% 
  filter(cum_change == min(cum_change)) %>% 
  pull(value) %>% 
  str_replace_all(pattern = "\\.", replacement = "_")

if(current_patch %in% patches_to_analyze){
  
  current_patch <- patches_to_analyze %>% 
    paste0("live_", ., "_") %>% 
    paste0(collapse = "|")
  
} else {
  
  current_patch <- current_patch %>% 
    paste0("live_", ., "_")
  
}

# extract data from MySQL
data_na <- tbl(con, "lor_match_info_na") %>%
  filter(str_detect(game_version, current_patch)) %>% 
  count(game_outcome, archetype, deck_code) %>% 
  collect()

# extract data from MySQL
data_eu <- tbl(con, "lor_match_info") %>%
  filter(str_detect(game_version, current_patch)) %>% 
  count(game_outcome, archetype, deck_code) %>% 
  collect()

# extract data from MySQL
data_asia <- tbl(con, "lor_match_info_asia") %>%
  filter(str_detect(game_version, current_patch)) %>% 
  count(game_outcome, archetype, deck_code) %>% 
  collect()

# bind data from different servers together
data <- bind_rows(data_na, data_eu, data_asia) %>% ungroup()

# calculate information & keep only relevant deckcodes (deck_code at least 5 matches, archetype at least 800 matches)
data_decks <- data %>% 
  group_by(archetype) %>% 
  summarise(tot_n = sum(n, na.rm = TRUE), .groups = "drop") %>% 
  filter(tot_n >= 800) %>% 
  select(-tot_n) %>% 
  left_join(data, by = "archetype") %>% 
  group_by(game_outcome, archetype, deck_code) %>% 
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = game_outcome, values_from = n) %>% 
  mutate(across(where(is.numeric), replace_na, 0)) %>% 
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
