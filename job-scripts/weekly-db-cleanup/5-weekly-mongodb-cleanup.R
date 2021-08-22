# Weekly database cleanup

# Removes matches older than 1 month from the standard MongoDB and stores them in the "db_old" version of those databases
# Runs weekly on Wednesday Morning (why? idk)

tictoc::tic()

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(mongolite)) # connect to MongoDB

# 2. connect to database ----

# current dbs
m_db1 <- mongo(url = "mongodb://balco:n0nLadimentico@localhost:27017/admin", collection = "lor_match_info")
m_db2 <- mongo(url = "mongodb://balco:n0nLadimentico@localhost:27017/admin", collection = "lor_match_info_na")
m_db3 <- mongo(url = "mongodb://balco:n0nLadimentico@localhost:27017/admin", collection = "lor_match_info_asia")

# "old" dbs
m_db1_old <- mongo(url = "mongodb://balco:n0nLadimentico@localhost:27017/admin", collection = "lor_match_info_old")
m_db2_old <- mongo(url = "mongodb://balco:n0nLadimentico@localhost:27017/admin", collection = "lor_match_info_na_old")
m_db3_old <- mongo(url = "mongodb://balco:n0nLadimentico@localhost:27017/admin", collection = "lor_match_info_asia_old")

# 3. extract old matches from dbs ----

old_eu   <- m_db1$find(sprintf('{"info.game_start_time_utc": { "$lt" : { "$date" : "%sT00:00:00Z" }}}', Sys.Date() - lubridate::days(30)))
old_na   <- m_db2$find(sprintf('{"info.game_start_time_utc": { "$lt" : { "$date" : "%sT00:00:00Z" }}}', Sys.Date() - lubridate::days(30)))
old_asia <- m_db3$find(sprintf('{"info.game_start_time_utc": { "$lt" : { "$date" : "%sT00:00:00Z" }}}', Sys.Date() - lubridate::days(30)))

# 4. add those matches to "old_db" & remove them from current dbs ----

if(nrow(old_eu) > 0){
  
  m_db1_old$insert(old_eu)
  m_db1$remove(sprintf('{"info.game_start_time_utc": { "$lt" : { "$date" : "%sT00:00:00Z" }}}', Sys.Date() - lubridate::days(30)))
  
}

if(nrow(old_na) > 0){
  
  m_db2_old$insert(old_na)
  m_db2$remove(sprintf('{"info.game_start_time_utc": { "$lt" : { "$date" : "%sT00:00:00Z" }}}', Sys.Date() - lubridate::days(30)))
  
}

if(nrow(old_asia) > 0){
  
  m_db3_old$insert(old_asia)
  m_db3$remove(sprintf('{"info.game_start_time_utc": { "$lt" : { "$date" : "%sT00:00:00Z" }}}', Sys.Date() - lubridate::days(30)))
  
}

tictoc::toc()

# send notification
RPushbullet::pbPost(
  "note", 
  title = "Weekly database cleanup", 
  body = sprintf("The weekly MongoDB cleanup was performed correctly. Removed %s games from Europe, %s from Americas, %s from Asia.", 
                 nrow(old_eu), nrow(old_na), nrow(old_asia))
)