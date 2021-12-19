# create a MySQL player db merging the 3 MongoDB dbs

# steps:
# 1. stop all api calls
# 2. merge mongodbs toghether (add region info!)
# 3. fix columns type accessing mysql from terminal
# 4. set primary key (puuid)

# parameters ----

# load db credentials
mongo_creds <- config::get("mongodb", file = "/home/balco/my_rconfig.yml")
mysql_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# db connections ----

# MongoDB
m_pl <- mongolite::mongo(url = sprintf("mongodb://%s:%s@localhost:27017/admin", mongo_creds$uid, mongo_creds$pwd), collection = "lor_player_na")

# create connection to MySQL database
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  db_host = "127.0.0.1",
  user = mysql_creds$uid,
  password = mysql_creds$pwd,
  dbname = mysql_creds$dbs
)

# load data ----

# import mongodb data
pl <- m_pl$find() %>% as_tibble() %>% mutate(region = 'americas')

# user already in db
pl_old <- tbl(con, 'lor_players') %>% collect()

# check duplicates
dup <- bind_rows(pl, pl_old) %>% count(puuid) %>% filter(n > 1) %>% pull(puuid)

# print duplicates
bind_rows(pl, pl_old) %>% filter(puuid %in% dup) %>% arrange(puuid, region)

# remove duplicates (not perfect but whatever, I'm gonna lose a couple of players...)
pl <- pl %>% filter(!puuid %in% dup)

# add to mysql
DBI::dbWriteTable(conn = con, name = "lor_players", value = pl, append = TRUE, row.names = FALSE)

# check
tbl(con, 'lor_players') %>% count(region)

# JUST THE VERY FIRST TIME:
# GO TO TERMINAL:
# mysql -u balco -p db_prova
# insert password
# ALTER TABLE fixing columns

# repeat process for all 3 regions

#set primary key
DBI::dbExecute(
  conn = con, 
  statement = "ALTER TABLE lor_players
  ADD PRIMARY KEY (`puuid`);"
)

# ----
# old garbage ----

x = tbl(con, 'test') %>% 
  collect()

x %>% head(10) %>% DBI::dbWriteTable(conn = con, name = "test", value = ., overwrite = TRUE, row.names = FALSE)

test = tibble(
  puuid = c(
    'IqkzuA_I8NiiTDihS6q9EbmXZmNgieZvpJd2HnOjBllx1KlwWZ382zpIVJF-UadFfmDGBO1ZToQHzQ', 
    'QaIhNOpw2Whvy8EMzg5QUXvxwN_XL1c4GOa18peAoeYea6TJO7WZWqAspa9OHl8nX_XRPQCymAg6ig',
    'test'
  ),
  gameName = c('VK YoRHa', 'test', 'test'),
  tagLine = c('VKG', 'test', 'test')
)

mysql_player_upsert <- function(puuid, game_name, tag_line, region){
  
  DBI::dbExecute(
    conn = con,
    statement = sprintf(
      "REPLACE INTO test
    (puuid, gameName, tagLine, region)
    VALUES
    (%s);",
      paste0("'", paste0(c(puuid, game_name, tag_line, region), collapse = "', '"), "'")
    )
  )
  
}

#set primary key
DBI::dbExecute(
  conn = con, 
  statement = "ALTER TABLE test
  ADD PRIMARY KEY (`puuid`);"
)

# this should do nothing
mysql_player_upsert(
  puuid = test$puuid[1],
  game_name = test$gameName[1],
  tag_line = test$tagLine[1]
)

# this should change an existing row
mysql_player_upsert(
  puuid = test$puuid[2],
  game_name = test$gameName[2],
  tag_line = test$tagLine[2]
)

# this should add a row
mysql_player_upsert(
  puuid = test$puuid[3],
  game_name = test$gameName[3],
  tag_line = test$tagLine[3]
)

DBI::dbDisconnect(con)