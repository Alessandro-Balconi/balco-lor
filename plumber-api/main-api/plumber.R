library(dplyr)

#* LoR-Meta Tier List
#* @get /player/<region>/<name>
function(region = '', name = '') {
  
  # load mysql db credentials
  db_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")
  
  # create connection to MySQL database
  con <- DBI::dbConnect(
    RMariaDB::MariaDB(),
    db_host  = "127.0.0.1",
    user     = db_creds$uid,
    password = db_creds$pwd,
    dbname   = db_creds$dbs
  )
  
  tag <- tbl(con, 'utils_players') %>% 
    filter(region == local(region), gameName == local(URLdecode(name))) %>% 
    pull(tagLine)
  
  DBI::dbDisconnect(con)
    
  return(tag)

}
