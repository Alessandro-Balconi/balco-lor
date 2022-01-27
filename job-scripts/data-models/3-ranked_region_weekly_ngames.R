# Creates a MySQL table with historical region play-rates (from data of MySQL databases)

# This task is performed weekly, Wednesday morning

tictoc::tic()

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(lubridate)) # working with dates

# 2. parameters ----

# date from which weeks are counted in the report
start_date <- as_datetime(sprintf("%sT16:50:00", Sys.Date()))

# mysql date to filter
mysql_start_date <- as.character(Sys.time()-days(22))
mysql_end_date <- as.character(Sys.time()-days(13))

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

data <- tbl(con, "lor_match_info_v2") %>%
  select(match_id, game_start_time_utc, starts_with("faction_")) %>%
  filter(game_start_time_utc <= mysql_end_date, game_start_time_utc >= mysql_start_date) %>% 
  collect()

# prepare data
data <- data %>% 
  pivot_longer(cols = starts_with("faction_")) %>%
  drop_na(value) %>% 
  mutate(
    start_time = hour(start_date) + (minute(start_date)/60),
    time = hour(game_start_time_utc) + (minute(game_start_time_utc)/60), 
    game_start_time_utc = as_date(game_start_time_utc)
  ) %>% 
  mutate(week = ceiling_date(game_start_time_utc, unit = "week", week_start = 4)-days(1)) %>%
  mutate(week = if_else(game_start_time_utc == week & start_time < time, week+weeks(1), week))

games_by_week <- data %>% 
  distinct(match_id, week) %>% 
  count(week, name = "tot_games") %>% 
  mutate(tot_games = tot_games * 2) # number of decks = number of games * 2

data <- data %>% 
  count(week, value) %>% 
  left_join(games_by_week, by = "week")

# keep only value of 3 weeks ago (older ones already in db, newer ones might change so not saved)
data <- data %>% 
  filter(week == Sys.Date() - days(14))

# 6. save to MySQL db ----

# save matches to db
if(nrow(data) >  0){
  
  data %>% 
    DBI::dbWriteTable(conn = con, name = "ranked_region_weekly_ngames", value = ., append = TRUE, row.names = FALSE) 
  
}

DBI::dbDisconnect(con)

tictoc::toc()
