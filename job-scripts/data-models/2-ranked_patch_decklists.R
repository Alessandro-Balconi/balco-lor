# Creates a MySQL table with decklists information for the current patch (from data of MySQL databases)

# 1. libraries & functions ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package

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

# 2. connect to db & load data ----

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

# 3. prepare table ----

# patches to analyze
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

# extract data from MySQL
df <- db_get_query(
  conn = con,
  qry = "
  WITH 
  decklists AS (
    SELECT deck_code, COUNT(*) AS n
    FROM ranked_match_metadata_30d meta
    JOIN ranked_match_info_30d info
    USING(match_id)
    WHERE game_start_time_utc >= '{min_date}'
    GROUP BY deck_code
    HAVING n >= 5
  )
  
  SELECT 
    COALESCE(aa.new_name, info.archetype) AS archetype, 
    deck_code,
    COUNT(*) AS \"match\",
    SUM(game_outcome = 'win') AS win,
    (1.0 * SUM(game_outcome = 'win') / COUNT(*)) AS winrate,
    CASE 
      WHEN game_start_time_utc >= '{Sys.time()-lubridate::days(3)}' THEN 2
      WHEN game_start_time_utc >= '{Sys.time()-lubridate::days(7)}' THEN 1
      ELSE 0 END AS time_frame,
    CASE WHEN player_rank = 2 THEN 1 ELSE 0 END AS is_master,
    region
  FROM decklists
  JOIN ranked_match_info_30d info
  USING(deck_code)
  JOIN ranked_match_metadata_30d meta
  USING(match_id)
  LEFT JOIN utils_archetype_aggregation aa
  ON info.archetype = aa.old_name
  GROUP BY 1, 2, 6, 7, 8
  ",
  print_text = FALSE
)


# 4. save to MySQL db ----

if(nrow(df) >  0){
  
  df %>% 
    DBI::dbWriteTable(conn = con, name = "ranked_patch_decklists", value = ., overwrite = TRUE, row.names = FALSE) 
  
}

DBI::dbDisconnect(con)
