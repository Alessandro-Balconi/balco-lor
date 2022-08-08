library(dplyr)

#* LoR-Meta Tier List
#* @get /player/<region>/<name>
function(region = '', name = '') {
  
  # create connection to MySQL database
  con <- lorr::create_db_con()
  
  tag <- tbl(con, 'utils_players') %>% 
    filter(region == local(region), gameName == local(URLdecode(name))) %>% 
    pull(tagLine)
  
  DBI::dbDisconnect(con)
    
  return(tag)

}
