# RUN THIS SCRIPT BEFORE THE SEASONAL TO SETUP THE PARAMETRS

# SETUP ------------------------------------------------------------------------

# import must have packages
library(lorr)
library(dplyr)
library(googlesheets4)
library(httr)

# day of the ladder cutoff (if not known, leave Sys.Date()-3)
cutoff_day <- "2022-10-06"

# set google API Key & Oauth credentials
options(gargle_oauth_email = "Balco21@outlook.it")

# google spreadsheet id with the parameters 
params_ss_id <- "1pCixyJwIRkcceX3W9cwsd6tNNpOK4E75i88K1pXCN8A"

# create database connection
con <- create_db_con()

# GIOCATORI ITALIANI -----------------------------------------------------------

# names of italian players from runeterra.ar italian leaderboard
it_name <- httr::GET("runeterra.ar/main/lor/rank/europe?country=it") %>% 
  httr::content(encoding = "UTF-8") %>% 
  .[['players']] %>% 
  bind_rows() %>% 
  filter(rank <= 700) %>% 
  pull(name)

# extract gameName + tagLine for these players from my db
it <- tbl(con, 'utils_players') |> 
  filter(region == "europe", gameName %in% it_name) |> 
  transmute(player = paste0(gameName, tagLine, sep = "#")) |> 
  arrange(player) |> 
  collect()

# overwrite sheet, remove header
write_sheet(data = it, ss = params_ss_id, sheet = "Giocatori Italiani")
range_delete(ss = params_ss_id, sheet = "Giocatori Italiani", range = "1")

# OTHER PLAYERS ----------------------------------------------------------------

# top eu players names for cutoff day
other_name <- tbl(con, 'leaderboard_daily') |> 
  filter(region == "europe", day == cutoff_day, rank <= 150) |> 
  pull(name)

# extract gameName + tagLine for these players from my db
other <- tbl(con, 'utils_players') |> 
  filter(
    region == "europe", 
    gameName %in% local(setdiff(other_name, it_name))
  ) |> 
  transmute(player = paste0(gameName, tagLine, sep = "#")) |> 
  arrange(player) |> 
  collect()

# overwrite sheet, remove header
write_sheet(data = other, ss = params_ss_id, sheet = "Other Players")
range_delete(ss = params_ss_id, sheet = "Other Players", range = "1")

# MATCH RESULTS ----------------------------------------------------------------

# clear sheet (first few rows are examples)
range_delete(
  ss = params_ss_id, 
  sheet = 'Match Results', 
  range = "A5:D1000", 
  shift = "left"
)

# DISCONNECT FROM DB AFTER FINISHING -------------------------------------------
DBI::dbDisconnect(con)
