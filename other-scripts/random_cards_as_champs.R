# Meme script with random cards as champions

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse))   # all purposes package
suppressPackageStartupMessages(library(jsonlite))    # convert JSON to R objects
suppressPackageStartupMessages(library(httr))        # http requests
suppressPackageStartupMessages(library(lubridate))   # working with dates
suppressPackageStartupMessages(library(ggimage))     # add images to ggplot

# 2. set parameters ----

# meme champs to add
meme_champs <- c("Ruin Runner", "Merciless Hunter", "Eye of the Dragon")

# date from which extract matches
start_date <- as_datetime(sprintf("%sT16:50:00", Sys.Date() - days(7)))
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
  RMySQL::MySQL(),
  db_host = "127.0.0.1",
  user = "balco",
  password = "Macosanes0!",
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
  filter(rarity == "Champion" | name %in% meme_champs) %>% 
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

# keep only relevant columns
data_tot <- data_tot %>% 
  select(shard, match_id, puuid, game_start_time_utc, starts_with("faction_"), game_outcome, deck_code, archetype)

# keep only recent data (this week + past one)
data <- data_tot %>%
  mutate(game_start_time_utc = as_datetime(game_start_time_utc)) %>% 
  filter(game_start_time_utc >= start_date-days(7) & game_start_time_utc <= end_date)

# add flag for "current" or "last" week
data <- data %>% 
  mutate(week = ifelse(game_start_time_utc >= start_date, "current", "last"))

# extract card codes from deck code
data <- data %>% 
  distinct(deck_code) %>% 
  mutate(cards_list = map(.x = deck_code, .f = lor_deckcodes$decode$decode_deck)) %>% 
  left_join(x = data, y = ., by = "deck_code")

# add champions info
data <- data %>%
  distinct(across(c(starts_with("faction_"), cards_list))) %>% 
  mutate(
    cards = map_chr(cards_list, str_flatten, collapse = " "),
    champs = str_extract_all(cards, pattern = paste(data_champs$cardCode, collapse = "|")),
    champs = map_chr(champs, str_flatten, collapse = " ")) %>% 
  mutate(across(champs,  function(x) unname(sapply(x, function(x) { paste(sort(trimws(strsplit(x[1], ' ')[[1]])), collapse=' ')} )))) %>% 
  left_join(data, ., by = c("faction_1", "faction_2", "cards_list")) %>% 
  select(-cards_list)

# 6. images to save ----

games_by_week <- data %>% 
  count(week, name = "tot")

data_pr <- data %>% 
  select(week, champs) %>% 
  separate(col = champs, into = sprintf("champ_%s", 1:(6+length(meme_champs))), sep = " ", fill = "right") %>% 
  pivot_longer(cols = -week) %>% 
  drop_na(value) %>% 
  count(week, value, sort = TRUE) %>% 
  left_join(games_by_week, by = "week") %>% 
  mutate(playrate = n / tot) %>%
  left_join(data_champs %>% select(name, cardCode), by = c("value" = "cardCode")) %>% 
  drop_na(name)

p <- data_pr %>% 
  select(-c(n, tot)) %>% 
  pivot_wider(names_from = week, values_from = playrate) %>% 
  mutate(change = current - last) %>% 
  slice_max(n = 10, order_by = current, with_ties = FALSE) %>%
  left_join(data_champs %>% select(cardCode, img = gameAbsolutePath), by = c("value" = "cardCode")) %>% 
  mutate(region = str_remove_all(value, "[0-9]")) %>% 
  left_join(data_regions %>% select(abbreviation, nameRef, logo = iconAbsolutePath), by = c("region" = "abbreviation")) %>% 
  ggplot(aes(x = reorder(name, current))) +
  geom_col(aes(y = current, fill = nameRef), color = "grey30", alpha = 0.8) +
  geom_text(aes(label = "", y = current*1.1), size = 6) +
  geom_text(aes(label = scales::percent(current, accuracy = .1), y = current), size = 6, hjust = -0.5, vjust = -0.5) +
  geom_text(size = 5, hjust = -0.375, vjust = 1.5, aes(label = scales_percent_plus(change, accuracy = .1), y = current, 
                                                       color = case_when(is.na(change) ~ "na", abs(change)<=(0.1/200) ~ "same", change>0 ~ "pos", change<0 ~ "neg"))) +
  geom_image(aes(image = img, y = -max(current)/5), size = 0.045, asp = 1.5) +
  ggfittext::geom_fit_text(aes(label = name, ymin = -(max(current)/5.5), ymax = 0), size = 20, padding.x = grid::unit(5, "mm")) +
  coord_flip() +
  theme_classic(base_size = 18) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = NULL) +
  scale_x_discrete(labels = element_blank()) +
  scale_fill_manual(values = region_colors) +
  scale_color_manual(values = c("pos" = "#149414", "neg" = "coral2", "na" = "steelblue", "same" = "#EFB700")) +
  theme(legend.position = "none") +
  labs(x = "Champion", y = "% of decks containing the champion", title = "Champion Playrate")

ggsave(filename = "/home/balco/dev/lor-meta-report/meme_champs_pr.png", plot = p, width = 12, height = 8, dpi = 180)
