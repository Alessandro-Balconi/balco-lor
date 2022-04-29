# Creates a MySQL table with archetype aggregations

# This task is performed with daily frequency at 16.30 UTC

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(googlesheets4)) # working with google spreadsheets

# 2. parameters ----

options(gargle_oauth_email = "Balco21@outlook.it")
options(googlesheets4_quiet = TRUE)

# load mysql db credentials
db_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# 4. connect to db & load data ----

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

# 5. prepare table ----

# archetypes aggregation mapping
archetypes_map <- with_gs4_quiet(read_sheet(ss = "1Xlh2kg7gLzvqugqGPpI4PidAdM5snggbJ44aRLuik5E", sheet = 'Archetypes Mapping'))

# 6. save to MySQL db ----

if(nrow(archetypes_map) >  0){
  
  archetypes_map %>% 
    DBI::dbWriteTable(conn = con, name = "utils_archetype_aggregation", value = ., overwrite = TRUE, row.names = FALSE) 
  
}

DBI::dbDisconnect(con)
