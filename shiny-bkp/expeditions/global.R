# Expedition Viewer LoR

# I.   libraries -----------------------------------------------------------------

library(dplyr)
library(stringr)
library(lubridate)
library(httr)
library(jsonlite)
library(DT)
library(shiny)

# II.  functions ---------------------------------------------------------------

# III. parameters --------------------------------------------------------------

# standard options for DT objects 
dt_options <- list(
  deferRender = TRUE,
  paging      = TRUE,
  scroller    = TRUE,
  columnDefs  = list(list(className = 'dt-center', targets = "all")),
  info        = FALSE,
  dom = 'Bfrtip',
  buttons = c('csv')
)

# load mysql db credentials
db_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# create connection to MySQL database
con <- pool::dbPool(
  RMariaDB::MariaDB(),
  db_host = "127.0.0.1",
  user = db_creds$uid,
  password = db_creds$pwd,
  dbname = db_creds$dbs
)