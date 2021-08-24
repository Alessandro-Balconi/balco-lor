
# libraries
library(RMySQL)
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(magrittr)

# name / nationality of qualified players
qualified_players <- tribble(
  ~name, ~country,
  "Teddy314", "Germany",
  "Nawatix", "Germany",
  "Ghosterdriver", "Germany",
  "jimmylihui", "United Kingdom",
  "Fizzical", "United Kingdom",
  "Spaiikz", "United Kingdom",
  "Ye Xiu 叶修", "Czech Republic",
  "pokrovac", "Czech Republic",
  "BaJAtak", "Czech Republic",
  "Ultraman1996", "Belgium",
  "ShuKee", "Belgium",
  "IrvineArtifact", "Belgium",
  "SaltySimonSE", "Sweden",
  "lm So Humble", "Sweden",
  "PyRoKz", "Sweden",
  "Alanzq", "Poland",
  "ShogoPASS", "Poland",
  "Szychu", "Poland",
  "FirstPinkBeaver", "Ukraine",
  "QuickWell", "Ukraine",
  "evildeaad", "Ukraine",
  "Akarey", "Latvia",
  "Drobene", "Latvia",
  "PCS Elyx", "France",
  "PCS Flaxeau", "France",
  "Haze", "France",
  "ragnarosich", "Russia",
  "kono jorno da", "Russia",
  "Pettish", "Russia",
  "Sergi2Vamos", "Spain",
  "Cdsfaul", "Spain",
  "Cosimo", "Spain",
  "GRANRODEO", "Turkey",
  "Uçan Protein", "Turkey",
  "Fight4RevengeLoR", "Turkey",
  "Sorry", "Lebanon",
  "Reaper9972", "Lebanon",
  "Bülat", "Italy",
  "Meliador0", "Italy",
  "Exonarf", "Italy",
  "pespscola", "Portugal",
  "pipamachado", "Portugal",
  "Bool Asha", "Portugal",
  "StratosGOD", "Belarus",
  "GrinexXx", "Belarus"
)

# API path
base.url <- "https://europe.api.riotgames.com/" # americas, asia, europe, sea

# API Key [DEVO USARE QUELLA DI SCRAPER, NON DI SCRAPER2]
api_key <- "RGAPI-cf2504e1-ec27-4878-8ae5-d82400f74c08"

# get player tag from PUUID
get_tag_from_id <- function(puuid){
  
  # API Call
  call <- GET(base.url, path = paste0("riot/account/v1/accounts/by-puuid/", puuid), add_headers("X-Riot-Token" = api_key)) %>% 
    content()
  
  # tag
  tag <- call %>% extract2("tagLine")
  
  # wait to avoid API rate limit
  Sys.sleep(1.2)
  
  return(tag)
  
}

# create connection to database
con <- dbConnect(
  MySQL(),
  db_host = "127.0.0.1",
  user = "X",
  password = "X",
  dbname = "db_prova"
)

# load complete database
data <- tbl(con, "lor_match") %>%
  select(matchid, date, matches("player_[1-2]{1}_id"), matches("player_[1-2]{1}_name")) %>% 
  collect()

# pivot longer
data <- data %>% 
  pivot_longer(cols = -c(matchid, date), names_to = "name", values_to = "value")

# separate key column & remove useless information
data <- data %>% 
  separate(col = "name", into = c("useless", "number", "what"), sep = "_") %>% 
  select(-useless)

# quality check
data <- data %>% 
  distinct()

# pivot wider to uniquely identify players
data <- data %>% 
  pivot_wider(names_from = what, values_from = value)

# get most recent name from a player
data <- data %>% 
  mutate(date = ymd(date)) %>% 
  group_by(id) %>% 
  slice_max(n = 1, order_by = date, with_ties = FALSE) %>% 
  ungroup()

# keep only important columns
data <- data %>% 
  select(id, name)

# keep only 48 qualified players (hopefully i have all of them)
data <- data %>% 
  filter(name %in% qualified_players$name)

# extract tag from puuid
data <- data %>% 
  mutate(tag = map_chr(id, get_tag_from_id))

# add nationality info
data <- data %>% 
  left_join(qualified_players, by = "name")

# remove useless id column (it contains the "scraper" puuid, but we need the "scraper2" puuid)
data <- data %>% 
  select(-id)

# save csv
write_csv(data, file = "/home/balco/dev/lor-meta-report/tmp.csv")
