library(dplyr)

# load mysql db credentials
db_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# create connection to MySQL database
con <- pool::dbPool(
  RMariaDB::MariaDB(),
  db_host  = "127.0.0.1",
  user     = db_creds$uid,
  password = db_creds$pwd,
  dbname   = db_creds$dbs,
  minSize = 0,
  idleTimeout = 0
)

#* LoR-Meta Tier List
#* @get /player/<region>/<name>
function(region = '', name = '') {

    tbl(con, 'utils_players') %>% 
      filter(region == local(region), gameName == local(URLdecode(name))) %>% 
      pull(tagLine)

}
