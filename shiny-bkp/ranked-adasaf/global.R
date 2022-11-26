# load libraries
library(shiny)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(DT)
library(lubridate)

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

# standard options for DT objects 
dt_options <- list(
  deferRender = TRUE,
  paging      = TRUE,
  scroller    = TRUE,
  scrollX     = TRUE,
  info        = FALSE,
  dom         = 'frtip',
  columnDefs  = list(list(
    className = 'dt-center', 
    targets   = "_all",
    render    = JS(
      "function(data, type, row, meta) {",
      "return type === 'display' && data != null && data.length > 15 ?",
      "'<span title=\"' + data + '\">' + data.substr(0, 15) + '...</span>' : data;",
      "}")
  ))
)