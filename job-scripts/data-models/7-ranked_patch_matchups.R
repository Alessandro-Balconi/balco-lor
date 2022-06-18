# Creates a MySQL table with matchup informations for the current patch (from data of MySQL databases)

# This task is performed with daily frequency at 16.30 UTC

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package

# 2. parameters ----

# load mysql db credentials
db_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# 3. functions ----

# function to get data from query
db_get_query <- function(conn, qry, limit = -1, print_text = TRUE, print_df = FALSE, convert_int64 = TRUE){
  
  # print query if needed
  if(print_text){ cat(stringr::str_glue(qry)) }
  
  # get data & convert to tibble
  df <- DBI::dbGetQuery(conn = con, statement = stringr::str_glue(qry), n = limit) %>% 
    tibble::as_tibble()
  
  # print first rows of df if needed
  if(print_df){ head(df) }
  
  # fix column format
  if(convert_int64){ df <- df %>% mutate(across(where(bit64::is.integer64), as.numeric)) }
  
  # return df
  return(df)
  
}

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
min_date <- tbl(con, 'ranked_match_metadata_30d') %>% 
  filter(str_detect(game_version, current_patch)) %>%
  summarise(min_date = min(game_start_time_utc, na.rm = TRUE)) %>% 
  collect() %>% 
  mutate(min_date = min_date + lubridate::days(1)) %>% 
  pull()
#min_date <- as.POSIXct("2021-12-14 18:00:00 UTC") # hotfix date

# 5.3 table v2 ----

df <- db_get_query(
  conn = con,
  qry = "
  WITH 
  metadata AS (
    SELECT
      match_id,
      CASE 
        WHEN game_start_time_utc >= '{Sys.time()-lubridate::days(3)}' THEN 2
        WHEN game_start_time_utc >= '{Sys.time()-lubridate::days(7)}' THEN 1
        ELSE 0 END AS time_frame,
      region
    FROM ranked_match_metadata_30d
    WHERE game_start_time_utc >= '{min_date}'
  ),
  info AS (
    SELECT
      match_id, 
      game_outcome,
      COALESCE(new_name, archetype) AS archetype,
      time_frame,
      CASE WHEN player_rank = 2 THEN 1 ELSE 0 END AS is_master,
      region,
      ROW_NUMBER() OVER (PARTITION BY match_id ORDER BY archetype) AS id
    FROM metadata
    LEFT JOIN ranked_match_info_30d rmi
    USING(match_id)
    LEFT JOIN utils_archetype_aggregation uaa
    ON rmi.archetype = uaa.old_name
  ),
  opponent_data AS (
    SELECT
      match_id,
      CASE WHEN id = 2 THEN 1 WHEN id = 1 THEN 2 ELSE 0 END AS id,
      archetype AS archetype_2
    FROM info
  )
  
  SELECT 
    archetype AS archetype_1,
    archetype_2,
    time_frame, 
    is_master,
    region,
    SUM(game_outcome = 'win') AS win,
    COUNT(*) AS n
  FROM info
  LEFT JOIN opponent_data
  USING(match_id, id)
  GROUP BY 1, 2, 3, 4, 5
  ORDER BY 1, 2, 3, 4, 5
  ",
  print_text = FALSE
)

# add winrate (default to 0.5 if mirror match)
df <- df %>% 
  mutate(winrate = ifelse(archetype_1 == archetype_2, 0.5, win / n))

# 6. save to MySQL db ----

if(nrow(df) >  0){
  
  df %>% 
    DBI::dbWriteTable(conn = con, name = "ranked_patch_matchups", value = ., overwrite = TRUE, row.names = FALSE) 

  # time of the update
  upd_time <- tibble(
    table_name = 'ranked_patch_matchups',
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
