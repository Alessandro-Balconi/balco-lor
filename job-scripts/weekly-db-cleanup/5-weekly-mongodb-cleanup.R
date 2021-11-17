# Weekly database cleanup

# Removes matches older than 1 month from the standard MongoDB and stores them in the "db_old" version of those databases
# Runs weekly on Wednesday Morning (why? idk)

tictoc::tic()

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(mongolite)) # connect to MongoDB

# 2. connect to database ----

# load mongodb credentials
db_creds <- config::get("mongodb", file = "/home/balco/my_rconfig.yml")

# current dbs
m_db1 <- mongo(url = sprintf("mongodb://%s:%s@localhost:27017/admin", db_creds$uid, db_creds$pwd), collection = "lor_match_info")
m_db2 <- mongo(url = sprintf("mongodb://%s:%s@localhost:27017/admin", db_creds$uid, db_creds$pwd), collection = "lor_match_info_na")
m_db3 <- mongo(url = sprintf("mongodb://%s:%s@localhost:27017/admin", db_creds$uid, db_creds$pwd), collection = "lor_match_info_asia")

# "old" dbs
m_db1_old <- mongo(url = sprintf("mongodb://%s:%s@localhost:27017/admin", db_creds$uid, db_creds$pwd), collection = "lor_match_info_old")
m_db2_old <- mongo(url = sprintf("mongodb://%s:%s@localhost:27017/admin", db_creds$uid, db_creds$pwd), collection = "lor_match_info_na_old")
m_db3_old <- mongo(url = sprintf("mongodb://%s:%s@localhost:27017/admin", db_creds$uid, db_creds$pwd), collection = "lor_match_info_asia_old")

# 3. extract old matches from dbs & add them to "old_db" & remove them from current dbs----

old_eu   <- m_db1$find(sprintf('{"info.game_start_time_utc": { "$lt" : { "$date" : "%sT00:00:00Z" }}}', Sys.Date() - lubridate::days(30)))

if(nrow(old_eu) > 0){
  
  m_db1_old$insert(old_eu)
  m_db1$remove(sprintf('{"info.game_start_time_utc": { "$lt" : { "$date" : "%sT00:00:00Z" }}}', Sys.Date() - lubridate::days(30)))
  
}

rm(old_eu)
old_na   <- m_db2$find(sprintf('{"info.game_start_time_utc": { "$lt" : { "$date" : "%sT00:00:00Z" }}}', Sys.Date() - lubridate::days(30)))

if(nrow(old_na) > 0){
  
  m_db2_old$insert(old_na)
  m_db2$remove(sprintf('{"info.game_start_time_utc": { "$lt" : { "$date" : "%sT00:00:00Z" }}}', Sys.Date() - lubridate::days(30)))
  
}

rm(old_na)
old_asia <- m_db3$find(sprintf('{"info.game_start_time_utc": { "$lt" : { "$date" : "%sT00:00:00Z" }}}', Sys.Date() - lubridate::days(30)))

if(nrow(old_asia) > 0){
  
  m_db3_old$insert(old_asia)
  m_db3$remove(sprintf('{"info.game_start_time_utc": { "$lt" : { "$date" : "%sT00:00:00Z" }}}', Sys.Date() - lubridate::days(30)))
  
}

tictoc::toc()
