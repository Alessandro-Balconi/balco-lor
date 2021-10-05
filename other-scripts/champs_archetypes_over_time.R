# Viego decks over time

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse))   # all purposes package
suppressPackageStartupMessages(library(jsonlite))    # convert JSON to R objects
suppressPackageStartupMessages(library(httr))        # http requests
suppressPackageStartupMessages(library(lubridate))   # working with dates
suppressPackageStartupMessages(library(ggimage))     # add images to ggplot

# 2. set parameters ----

# meme champs to add
focus_card <- c("Viego")

# date from which extract matches
start_date <- as_datetime(sprintf("%sT16:50:00", Sys.Date() - days(1000)))
comparison_date <- start_date - days(7)
end_date <- as_datetime("3000-01-01T00:00:00")
#end_date <- start_time + days(7)

# color of Runeterra regions for plots
region_colors <- c(
  "ShadowIsles" = "#04825d",
  "Noxus" = "#c24a47",
  "Shurima" = "#eee22f",
  "Freljord" = "#a9e6f6",
  "MtTargon" = "#8b83e9",
  "Piltover" = "#f4a864",
  "Ionia" = "#d89baf",
  "Demacia" = "#e2d6af",
  "Bilgewater" = "#c66c22"
)

# install / update "lor_deckcodes" (TO DO EVERY NEW EXPANSION maybe?)
#reticulate::py_install("lor_deckcodes", pip = TRUE)

# import python deck decoder
lor_deckcodes <- reticulate::import("lor_deckcodes")

# 3. connect to db & load data ----

# close previous connections to MySQL database (if any)
if(exists("con")){ DBI::dbDisconnect(con) }

# connect to db
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  db_host = "127.0.0.1",
  user = "X",
  password = "X",
  dbname = "db_prova"
)

# import match data (only from ranked games)
data_eu   <- tbl(con, "lor_match_info") %>% collect()
data_na   <- tbl(con, "lor_match_info_na") %>% collect()
data_asia <- tbl(con, "lor_match_info_asia") %>% collect()

data_tot <- bind_rows("europe" = data_eu, "americas" = data_na, "asia" = data_asia, .id = "shard")

# get most recent set number (to read sets JSONs)
last_set <- "https://dd.b.pvp.net/latest/core/en_us/data/globals-en_us.json" %>% 
  GET() %>% 
  content(encoding = "UTF-8") %>% 
  fromJSON() %>% 
  .[["sets"]] %>% 
  mutate(set = str_extract(nameRef, pattern = "[0-9]+")) %>% 
  mutate(set = as.numeric(set)) %>% 
  summarise(max(set, na.rm = TRUE)) %>% 
  pull()

# champions names / codes / images from set JSONs
data_champs <- map_dfr(
  .x = 1:last_set,
  .f = function(x) {
    sprintf("https://dd.b.pvp.net/latest/set%1$s/en_us/data/set%1$s-en_us.json", x) %>% 
      GET() %>%  
      content(encoding = "UTF-8") %>% 
      fromJSON() %>% 
      as_tibble()
  },
  .id = "set"
) %>% 
  filter(rarity == "Champion") %>% 
  select(assets, name, cardCode) %>%
  unnest(col = assets) %>% 
  filter(nchar(cardCode) <= 8) # additional check because sometimes Riot messes up

# regions names / abbreviations / logos from global JSON
data_regions <- "https://dd.b.pvp.net/latest/core/en_us/data/globals-en_us.json" %>% 
  GET() %>% 
  content(encoding = "UTF-8") %>% 
  fromJSON() %>% 
  .[["regions"]] %>% 
  mutate(nameRef = case_when(
    nameRef == "PiltoverZaun" ~ "Piltover",
    nameRef == "Targon" ~ "MtTargon",
    TRUE ~ nameRef
  ))

# 4. define functions ----

# same as scales::percent, but has a nicer format
scales_percent_plus <- function(x, accuracy = .1){
  
  sapply(
    X = x,
    FUN = function(x) case_when(
      is.na(x) ~ paste0("‼ NEW"),
      abs(x)<=(accuracy/200) ~ paste0("↔ ", scales::percent(abs(x), accuracy = accuracy)),
      x>0 ~ paste0("↑ +", scales::percent(x, accuracy = accuracy)),
      x<0 ~ paste0("↓ ", scales::percent(x, accuracy = accuracy)),
      TRUE ~ NA_character_ # should never happen; just an additional check
    ),
    USE.NAMES = FALSE
  )
  
}

# 5. make report ----

# codes we are interested in
focus_codes <- data_champs %>% 
  filter(name %in% focus_card) %>% 
  pull(cardCode)

# keep only decks having that code
data_tot <- data_tot %>% 
  filter(grepl(focus_codes, champs))

# keep only relevant columns
data_tot <- data_tot %>% 
  select(shard, match_id, puuid, game_start_time_utc, starts_with("faction_"), game_outcome, deck_code, champs, archetype)

# group by day
data <- data_tot %>% 
  mutate(date = as_date(game_start_time_utc)) %>% 
  count(date, archetype, faction_1, faction_2) %>% 
  group_by(archetype) %>% 
  complete(date = seq.Date(from = min(.$date), to = max(.$date), by = "day")) %>% 
  fill(faction_1, faction_2, .direction = "down") %>% 
  replace_na(list(n = 0)) %>% 
  mutate(cumn = cumsum(n)) %>% 
  ungroup() %>% 
  unite(col = factions, starts_with("faction_"), sep = " ", na.rm = TRUE) %>% 
  mutate(factions = str_remove_all(factions, pattern = "ShadowIsles| "))
  mutate(factions = ifelse(factions == "", "ShadowIsles", factions))

data %>% 
  group_by(date) %>% 
  mutate(n_g = sum(n)) %>% 
  ungroup() %>% 
  group_by(archetype) %>% 
  mutate(cumn_g = cumsum(n_g)) %>% 
  ungroup() %>% 
  mutate(pr = cumn/cumn_g) %>% 
  ggplot(aes(x = date, y = pr, group = archetype, color = factions)) +
  geom_line(size = 1) +
  theme_light() +
  theme(legend.position = "bottom") +
  labs(x = "Date", y = "Playrate", title = "Playrate of Viego archetypes over time", color = "Other Faction",
       subtitle = "Share of games played by that Viego archetype over all of Viego's games") +
  scale_color_manual(values = region_colors) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
