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

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg="") {
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot a histogram
#* @serializer png
#* @get /plot
function() {
  rand <- rnorm(100)
  hist(rand)
}

#* LoR-Meta Tier List
#* @get /tables
function() { as.list(DBI::dbListTables(con)) }

#* LoR-Meta Tier List
#* @get /player/<region>/<name>
function(region = '', name = '') {

    tbl(con, 'utils_players') %>% 
      filter(region == local(region), gameName == local(URLdecode(name))) %>% 
      pull(tagLine)

}
