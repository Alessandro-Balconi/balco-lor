# Creates a MySQL table with archetype aggregations

# This task is performed with daily frequency at 16.30 UTC

# 1. libraries ----

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(googlesheets4))

# 2. parameters ----

options(gargle_oauth_email = "Balco21@outlook.it")
options(googlesheets4_quiet = TRUE)

# 4. connect to db & load data ----

# create connection to MySQL database
con <- lorr::create_db_con()

# 5. prepare table ----

# archetypes aggregation mapping
suppressMessages(
  archetypes_map <- read_sheet(
    ss = "1Xlh2kg7gLzvqugqGPpI4PidAdM5snggbJ44aRLuik5E", 
    sheet = 'Archetypes Mapping'
  ) 
)

# 6. save to MySQL db ----

if(nrow(archetypes_map) >  0){
  
  archetypes_map %>% 
    DBI::dbWriteTable(
      conn = con, 
      name = "utils_archetype_aggregation", 
      value = ., 
      overwrite = TRUE, 
      row.names = FALSE
    ) 
  
}

DBI::dbDisconnect(con)
