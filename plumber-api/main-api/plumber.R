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
#* @get /matchup
#* @param deck_1
#* @param deck_2
function() {

    # tbl(con, 'ranked_patch_matchups') %>% 
    # filter(archetype_1 == deck_1, archetype_2 == deck_2) %>% 
    # summarise(across(c(win, n), sum, na.rm = TRUE), .groups = 'drop') %>% 
    # collect()
  
  data.frame(x = c(1, 2), y = c('a', 'b'))

}
