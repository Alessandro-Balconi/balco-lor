# Creates a MySQL table with decklists information for the current patch (from data of MySQL databases)

# This task is performed with daily frequency at 16.30 UTC

tictoc::tic()

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package

# 2. parameters ----

# 3. functions ----

# 4. connect to db & load data ----

# close previous connections to MySQL database (if any)
if(exists("con")){ DBI::dbDisconnect(con) }

# create connection to MySQL database
con <- DBI::dbConnect(
  RMySQL::MySQL(),
  db_host = "127.0.0.1",
  user = "balco",
  password = "Macosanes0!",
  dbname = "db_prova"
)

# 5. prepare table ----

# latest patch with data available
current_patch <- tbl(con, "lor_match_info_na") %>% 
  distinct(game_version) %>%
  collect() %>% 
  mutate(across(game_version, ~word(string = ., start = 2, end = -2, sep = "_"))) %>% 
  separate(col = game_version, into = c("version", "patch"), sep = "\\_", convert = TRUE) %>% 
  mutate(last_patch = version*100+patch) %>% 
  slice_max(n = 1, order_by = last_patch, with_ties = FALSE) %>% 
  unite(col = value, version, patch, sep = "_") %>%
  pull(value) %>% 
  paste0("live_", ., "_")

# extract data from MySQL
data_na <- tbl(con, "lor_match_info_na") %>%
  filter(game_version %like% paste0(current_patch, "%")) %>% 
  count(game_outcome, archetype, deck_code) %>% 
  collect()

# extract data from MySQL
data_eu <- tbl(con, "lor_match_info") %>%
  filter(game_version %like% paste0(current_patch, "%")) %>% 
  count(game_outcome, archetype, deck_code) %>% 
  collect()

# extract data from MySQL
data_asia <- tbl(con, "lor_match_info_asia") %>%
  filter(game_version %like% paste0(current_patch, "%")) %>% 
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
  mutate(winrate = win / match) %>% 
  select(archetype, deck_code, match, winrate)

# 6. save to MySQL db ----

# first initialization of database
#DBI::dbWriteTable(conn = con, name = "lor_decklists", value = data_decks, row.names = FALSE)

# save matches to db
if(nrow(data_decks) >  0){
  
  data_decks %>% 
    DBI::dbWriteTable(conn = con, name = "lor_decklists", value = ., overwrite = TRUE, row.names = FALSE) 
  
}

tictoc::toc()
