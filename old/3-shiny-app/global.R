# Shiny App to Analyze EU Master players

# I. libraries ----

library(rvest)
library(tidyverse)
library(lubridate)
library(reticulate)
library(magrittr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)

# II.  functions ----

# get champion name from card id
get_name_from_champ <- function(champ){
  
  if(is.na(champ)){ return(NA_character_) }
  
  champ_codes %>% 
    filter(X == champ) %>% 
    pull(Name)
  
}

# III. parameters ----

# install / update miniconda and "lor_deckcodes" (TO DO EVERY NEW EXPANSION)
#reticulate::install_miniconda()
#py_install("lor_deckcodes", pip = TRUE)

# load python modules
lor_deckcodes <- reticulate::import("lor_deckcodes")
deck <- lor_deckcodes$LoRDeck$from_deckcode

# IV.  connect to db & load data ----

# close previous connections to MySQL database if any
if(exists("con")) { dbDisconnect(con) }

# create connection to database
con <- DBI::dbConnect(
  RMySQL::MySQL(),
  db_host = "127.0.0.1",
  user = "balco",
  password = "Macosanes0!",
  dbname = "db_prova"
)

# load complete database
data <- tbl(con, "eu_master_players") %>% 
  collect()

# mapping with players
eum_players <- read_csv("/home/scraper2/dev/lor-meta-report/old/eum_players.csv", col_types = "cccc")

# list of codes of all runeterra champions
champ_codes <- read_html("https://leagueoflegends.fandom.com/wiki/Champion_(Legends_of_Runeterra)") %>% 
  html_table(fill = TRUE) %>% 
  extract2(2) %>% 
  as_tibble(.name_repair = make.names) %>% 
  select(1, 2, 5) %>% 
  filter(nchar(X) <= 8) %>% 
  mutate(Region = case_when( # same region names as API
    Region == "Piltover & Zaun" ~ "Piltover",
    Region == "Shadow Isles" ~ "ShadowIsles",
    Region == "Targon" ~ "MtTargon", 
    TRUE ~ Region
  ))