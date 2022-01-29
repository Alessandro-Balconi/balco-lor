# Weekly database cleanup

# Removes matches older than 1 month from the standard MongoD
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

# 3. extract old matches from dbs & add them to "old_db" & remove them from current dbs----

m_db1$update(query  = '{}', update = '[{"$set":{"info.game_start_time_utc": { "$toDate": "$info.game_start_time_utc" }}}]', multiple = TRUE)
m_db1$remove(sprintf('{"info.game_start_time_utc": { "$lt" : { "$date" : "%sT00:00:00Z" }}}', Sys.Date() - lubridate::days(30)))

m_db2$update(query  = '{}', update = '[{"$set":{"info.game_start_time_utc": { "$toDate": "$info.game_start_time_utc" }}}]', multiple = TRUE)
m_db2$remove(sprintf('{"info.game_start_time_utc": { "$lt" : { "$date" : "%sT00:00:00Z" }}}', Sys.Date() - lubridate::days(30)))

m_db3$update(query  = '{}', update = '[{"$set":{"info.game_start_time_utc": { "$toDate": "$info.game_start_time_utc" }}}]', multiple = TRUE)
m_db3$remove(sprintf('{"info.game_start_time_utc": { "$lt" : { "$date" : "%sT00:00:00Z" }}}', Sys.Date() - lubridate::days(30)))

tictoc::toc()
