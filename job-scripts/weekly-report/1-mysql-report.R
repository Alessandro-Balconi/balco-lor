# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse))   # all purposes package
suppressPackageStartupMessages(library(jsonlite))    # convert JSON to R objects
suppressPackageStartupMessages(library(httr))        # http requests
suppressPackageStartupMessages(library(lubridate))   # working with dates
suppressPackageStartupMessages(library(ggimage))     # add images to ggplot
suppressPackageStartupMessages(library(gridExtra))   # display tables as pictures
suppressPackageStartupMessages(library(ggrepel))     # repel geom_text
suppressPackageStartupMessages(library(DT))          # display nice tables
suppressPackageStartupMessages(library(reactable))   # display nice tables
suppressPackageStartupMessages(library(htmlwidgets)) # save tables
suppressPackageStartupMessages(library(htmltools))   # use HTML functions in R
suppressPackageStartupMessages(library(googlesheets4)) # working with google spreadsheets

# 2. set parameters ----

options(gargle_oauth_email = "Balco21@outlook.it")
options(googlesheets4_quiet = TRUE)

# date from which extract matches
start_date <- as_datetime(sprintf("%sT16:50:00", Sys.Date() - days(7)))
start_date_char <- as.character(start_date)
mysql_start_date <- (start_date - days(7)) %>% as.character()

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
  "BandleCity" = "#93a908", #"#bbcb1c",
  "Bilgewater" = "#c66c22"
)

# 3. connect to db & load data ----

# load mysql db credentials
db_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# close previous connections to MySQL database (if any)
if(exists("con")){ DBI::dbDisconnect(con) }

# connect to db
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  db_host = "127.0.0.1",
  user = db_creds$uid,
  password = db_creds$pwd,
  dbname = "db_prova"
)

# number of master matches in the last 7 days
count_master_match <- tbl(con, "lor_match_info") %>% 
  union_all(tbl(con, "lor_match_info_na")) %>% 
  union_all(tbl(con, "lor_match_info_asia")) %>%
  filter(game_start_time_utc >= start_date_char) %>% 
  filter(is_master == 1) %>% 
  distinct(match_id) %>%
  count() %>% 
  collect() %>% 
  pull(n)

# if I have at least 10k master matches, collect only master matches
if(count_master_match >= 10000){
  
  # matchid with at least 1 master player (this week + last week)
  master_matchids <- tbl(con, "lor_match_info") %>% 
    union_all(tbl(con, "lor_match_info_na")) %>% 
    union_all(tbl(con, "lor_match_info_asia")) %>%
    filter(game_start_time_utc >= mysql_start_date) %>% 
    filter(is_master == 1) %>% 
    distinct(match_id) %>%
    collect() %>% 
    pull(match_id)
  
}

# import match data (only from ranked games)
data_eu <- tbl(con, "lor_match_info") %>%
  filter(game_start_time_utc >= mysql_start_date) %>% 
  {if(count_master_match >= 10000) filter(., match_id %in% master_matchids) else . } %>% 
  select(-c(game_mode, game_type, game_version, order_of_play, total_turn_count, cards, is_master)) %>% 
  collect()

data_na <- tbl(con, "lor_match_info_na") %>% 
  filter(game_start_time_utc >= mysql_start_date) %>% 
  {if(count_master_match >= 10000) filter(., match_id %in% master_matchids) else . } %>% 
  select(-c(game_mode, game_type, game_version, order_of_play, total_turn_count, cards, is_master)) %>% 
  collect()

data_asia <- tbl(con, "lor_match_info_asia") %>%
  filter(game_start_time_utc >= mysql_start_date) %>% 
  {if(count_master_match >= 10000) filter(., match_id %in% master_matchids) else . } %>% 
  select(-c(game_mode, game_type, game_version, order_of_play, total_turn_count, cards, is_master)) %>% 
  collect()

data <- bind_rows("europe" = data_eu, "americas" = data_na, "asia" = data_asia, .id = "shard")

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

# mapping champion-region combinations to archetypes aggregations
archetypes_map <- with_gs4_quiet(read_sheet(ss = "1Xlh2kg7gLzvqugqGPpI4PidAdM5snggbJ44aRLuik5E", sheet = 'Archetypes Mapping'))

# 4. define functions ----

# Render a bar chart with a label on the left
bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}

# create a nice date from  date object
nice_date <- function(date, short_month = TRUE){
  
  paste(month(date, label = TRUE, abbr = short_month), day(date), sep = " ")
  
}

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

# fill matchup table
fill_matchup_table <- function(tbl){
  
  for(i in 1:nrow(tbl)){
    
    for(j in 1:ncol(tbl)){
      
      if(i>j){
        
        tbl[i,j] <- 1 - tbl[j,i]
        
      }
      
    }
    
  }
  
  return(tbl)
  
}

# function that extracts name from player puuid
# this is really not efficient since it calls mongoDB everytime; but I don't care it's just for a handful of players
from_puuid_to_riotid <- function(puuid, shard){
  
  db_collection <- switch(
    shard,
    "europe"   = "lor_player",
    "americas" = "lor_player_na",
    "asia"     = "lor_player_asia"
  )
  
  m_player  <- mongolite::mongo(url = "mongodb://balco:n0nLadimentico@localhost:27017/admin", collection = db_collection)
  
  player <- m_player$find(sprintf('{"puuid" : "%s" }', puuid))
  
  player$gameName
  
}

# perform GET calls but with a delay in them
get_slowly <- function(..., delay = 0.1){
  
  Sys.sleep(delay)
  
  GET(...)
  
}

# convert a local image to URI (used to display local images in DT objects)
img_uri <- function(x, size = 1, custom_size = FALSE){ 
  
  if(custom_size){
    
    sprintf('<img width="%s" src="%s"/>', scales::percent(size, accuracy = 1), knitr::image_uri(x))

  } else{
    
    sprintf('<img src="%s"/>', knitr::image_uri(x))

  }

}

# 5. make report ----

# keep only recent data (this week + past one)
data <- data %>%
  mutate(game_start_time_utc = as_datetime(game_start_time_utc)) %>% 
  filter(game_start_time_utc >= start_date-days(7))

# add flag for "current" or "last" week
data <- data %>% 
  mutate(week = ifelse(game_start_time_utc >= start_date, "current", "last"))

# # merge archetypes according to mapping
data <- data %>%
  left_join(archetypes_map, by = c("archetype" = "old_name")) %>%
  mutate(new_name = coalesce(new_name, archetype))

# 6. images to save ----

# 6.0 data info ----

p1 <- data %>%
  filter(week == "current") %>%
  group_by(shard) %>% 
  summarise(
    match_analyzed = n_distinct(match_id),
    unique_players = n_distinct(puuid),
    unique_decklists = n_distinct(deck_code),
    tied_match = sum(game_outcome == "tie") / 2
  ) %>%
  mutate(tied_match = sprintf("%s (%s)", tied_match, scales::percent(tied_match/match_analyzed, accuracy = .01))) %>%
  rename_with(~str_replace_all(., pattern = "_", replacement = " ")) %>% 
  rename_with(str_to_title) %>% 
  mutate(across(!where(is.character), as.character)) %>% 
  pivot_longer(cols = -Shard, names_to = " ") %>%
  pivot_wider(names_from = Shard, values_from = value) %>% 
  mutate(` ` = paste0(` `, ": "))

p2 <- data %>%
  filter(week == "current") %>%
  summarise(
    match_analyzed = n_distinct(match_id),
    unique_players = n_distinct(puuid),
    unique_decklists = n_distinct(deck_code),
    tied_match = ceiling(sum(game_outcome == "tie") / 2)
  ) %>%
  mutate(tied_match = sprintf("%s (%s)", tied_match, scales::percent(tied_match/match_analyzed, accuracy = .01))) %>%
  rename_with(~str_replace_all(., pattern = "_", replacement = " ")) %>% 
  rename_with(str_to_title) %>% 
  mutate(across(!where(is.character), as.character)) %>%
  pivot_longer(cols = everything(), names_to = " ", values_to = "total") %>% 
  mutate(` ` = paste0(` `, ": "))

p <- left_join(p1, p2, by = " ") %>% 
  rename_with(str_to_title) %>% 
  tableGrob(rows = NULL)

h <- grid::convertHeight(sum(p$heights), "in", TRUE)
w <- grid::convertWidth(sum(p$widths), "in", TRUE)

ggsave(filename = "/home/balco/dev/lor-meta-report/output/data.png", plot = p, width = w+0.01, height = h+0.01)

# 6.1. playrate by region ----

games_by_week <- data %>% 
  count(week, name = "tot")

p <- data %>% 
  select(week, starts_with("faction_")) %>% 
  pivot_longer(cols = -week) %>% 
  count(week, value, sort = TRUE) %>% 
  drop_na() %>%
  left_join(games_by_week, by = "week") %>% 
  mutate(playrate = n / tot) %>%
  select(-c(n, tot)) %>% 
  pivot_wider(names_from = week, values_from = playrate) %>% 
  mutate(change = current - last) %>%
  left_join(data_regions %>% select(logo = iconAbsolutePath, nameRef), by = c("value" = "nameRef")) %>%
  ggplot(aes(x = reorder(value, current))) +
  geom_col(aes(y = current, fill = value), color = "grey30", alpha = 0.8) +
  geom_text(aes(label = scales::percent(current, accuracy = .1), y = current), size = 6, hjust = -0.5, vjust = -0.5) +
  geom_text(size = 5, hjust = -0.375, vjust = 1.5, aes(label = scales_percent_plus(change, accuracy = .1), y = current, 
                                                       color = case_when(is.na(change) ~ "na", abs(change)<=(0.1/200) ~ "same", change>0 ~ "pos", change<0 ~ "neg"))) +
  geom_text(aes(label = "", y = 1.1*current), size = 6) +
  geom_image(aes(image = ifelse(current>0.0495, logo, NA), y = current-0.045*max(current)), size = 0.06, asp = 1.5, na.rm = TRUE) +
  coord_flip() +
  theme_classic(base_size = 18) +
  scale_fill_manual(values = region_colors) +
  scale_color_manual(values = c("pos" = "#149414", "neg" = "coral2", "na" = "steelblue", "same" = "#EFB700")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "none") +
  labs(x = "Region", y = "% of decks containing the region", title = "Region Playrate")

ggsave(filename = "/home/balco/dev/lor-meta-report/output/region_pr.png", plot = p, width = 12, height = 8, dpi = 180)

# 6.2. champions playrate ----

data_pr <- data %>% 
  select(week, champs) %>% 
  separate(col = champs, into = sprintf("champ_%s", 1:6), sep = " ", fill = "right") %>% 
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

ggsave(filename = "/home/balco/dev/lor-meta-report/output/champs_pr.png", plot = p, width = 12, height = 8, dpi = 180)

tbl <- data_pr %>%
  filter(week == "current") %>% 
  select(name, match = n, playrate) %>% 
  arrange(-match) %>% 
  rename_with(str_to_title) %>% 
  datatable(rownames = FALSE, options = list(pageLength = 10, lengthChange = FALSE)) %>% 
  formatPercentage(columns = "Playrate", digits = 1)

tbl$width<-"100%"
tbl$height <- "500px"
tbl$sizingPolicy$browser$padding <- 10

saveWidget(tbl, "/home/balco/dev/lor-meta-report/output/champs_pr.html", background = "inherit")

# 6.3. historical play-rate ----

data_history <- tbl(con, "lor_region_history") %>% 
  collect()

weekly_decks <- data %>% 
  distinct(match_id, week) %>% 
  count(week, name = "tot_games") %>% 
  mutate(tot_games = tot_games * 2) %>% # number of decks = number of games * 2
  mutate(week = if_else(week == "last", date(start_date), date(start_date) + days(7)))

data_history_new <- data %>% 
  select(week, game_start_time_utc, starts_with("faction_")) %>% 
  pivot_longer(cols = starts_with("faction_")) %>%
  drop_na(value) %>% 
  count(week, value) %>% 
  mutate(week = if_else(week == "last", date(start_date), date(start_date) + days(7))) %>% 
  left_join(weekly_decks, by = "week") %>% 
  arrange(week)

data_history <- data_history %>% 
  mutate(week = ymd(week)) %>% 
  bind_rows(data_history_new)

p <- data_history %>% 
  mutate(playrate = n / tot_games) %>% 
  ggplot(aes(x = week)) +
  geom_line(aes(y = playrate, group = value, color = value), size = 2) +
  geom_segment(x = ymd("2021-06-30")+1, xend = ymd("2021-06-30")+1, y = 0, yend = 100, color = "steelblue", linetype = "dotted") +
  geom_label(x = ymd("2021-06-30")+1, y = 0, label = "RotU \n Patch 2.11", size = 4) +
  geom_segment(x = ymd("2021-07-14")+1, xend = ymd("2021-07-14")+1, y = 0, yend = 100, color = "steelblue", linetype = "dotted") +
  geom_label(x = ymd("2021-07-14")+1, y = 0, label = "SoL Event \n Patch 2.12", size = 4) +
  geom_segment(x = ymd("2021-08-25")+1, xend = ymd("2021-08-25")+1, y = 0, yend = 100, color = "steelblue", linetype = "dotted") +
  geom_label(x = ymd("2021-08-25")+1, y = 0, label = "BtB \n Patch 2.14", size = 4) +
  geom_segment(x = ymd("2021-10-20")+1, xend = ymd("2021-10-20")+1, y = 0, yend = 100, color = "steelblue", linetype = "dotted") +
  geom_label(x = ymd("2021-10-20")+1, y = 0, label = "Balance Change \n Patch 2.18", size = 4) +
  geom_segment(x = ymd("2021-11-10")+1, xend = ymd("2021-10-11")+1, y = 0, yend = 100, color = "steelblue", linetype = "dotted") +
  geom_label(x = ymd("2021-11-10")+1, y = 0, label = "PoC Event \n Patch 2.19", size = 4) +
  geom_point(aes(x = week, y = playrate, color = value, group = value), size = 5) +
  theme_bw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
  expand_limits(y = 0) +
  scale_x_continuous(breaks = unique(data_history$week), labels = nice_date) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  scale_color_manual(values = region_colors) +
  theme(legend.position = "bottom") +
  labs(x = "Week", y = "Playrate", title = "Weekly Region Playrate", color = "Region") +
  guides(colour = guide_legend(nrow = 2))

ggsave(filename = "/home/balco/dev/lor-meta-report/output/region_hist.png", plot = p, width = 12, height = 8, dpi = 180)

# 6.4 archetype playrate ----

most_played_version <- data %>% 
  count(new_name, archetype) %>% 
  group_by(new_name) %>% 
  slice_max(n = 1, order_by = n, with_ties = FALSE) %>% 
  ungroup() %>% 
  select(new_name, archetype)

data_archetype_pr <- data %>% 
  count(week, new_name, sort = "TRUE") %>%
  left_join(games_by_week, by = "week") %>%
  mutate(playrate = n / tot) %>%
  select(-c(n, tot)) %>% 
  pivot_wider(names_from = week, values_from = playrate) %>% 
  slice_max(n = 10, order_by = current, with_ties = FALSE) %>%
  mutate(change = current - last) %>%
  left_join(most_played_version, by = 'new_name') %>% 
  mutate(image = archetype) %>% 
  mutate(image = str_replace_all(image, set_names(data_champs$cardCode, paste0(data_champs$name, "\\b")))) %>% 
  separate(image, into = c("tmp1", "tmp2"), sep = "\\(", fill = "right") %>% 
  mutate(tmp2 = str_replace_all(tmp2, pattern = " ", replacement = "x_x")) %>% 
  mutate(tmp2 = ifelse(is.na(tmp2), tmp2, paste0("(", tmp2))) %>% 
  unite(col = image, tmp1, tmp2, sep = "", na.rm = TRUE) %>% 
  mutate(image = str_replace_all(image, pattern = " ", replacement = "_")) %>%
  mutate(image = ifelse(grepl("[A-Z]{4}", image), paste0(str_sub(image, 2, 4), "_", str_sub(image, 5, 7)), image)) %>% 
  mutate(image = str_replace_all(image, pattern = "\\(|\\)", replacement = "x")) %>%
  separate(col = image, into = sprintf("image_%s", 1:7), fill = "right", sep = "_") %>% 
  select(new_name, last, current, change, where(function(x) any(!is.na(x)))) %>% 
  mutate(across(starts_with("image_"), ~str_replace_all(., set_names(data_champs$gameAbsolutePath, data_champs$cardCode)))) %>% 
  mutate(across(starts_with("image_"), ~str_replace_all(., set_names(data_regions$iconAbsolutePath, paste0("x", data_regions$abbreviation, "x"))))) 

max_champs_play <- ncol(data_archetype_pr) - 5

p <- data_archetype_pr %>% 
  rowwise() %>% 
  mutate(disp_images = sum(!is.na(c_across(starts_with("image_"))))) %>%
  ungroup() %>% 
  ggplot(aes(x = reorder(new_name, current))) +
  geom_col(aes(y = current), fill = "#13294b", color = "steelblue", alpha = 0.9) +
  geom_text(aes(label = "", y = 1.1*current), size = 6) +
  geom_text(aes(label = scales::percent(current, accuracy = .1), y = current), size = 6, hjust = -0.5, vjust = -0.5) +
  geom_text(size = 5, hjust = -0.375, vjust = 1.5, aes(label = scales_percent_plus(change, accuracy = .1), y = current, 
                                                       color = case_when(is.na(change) ~ "na", abs(change)<=(0.1/200) ~ "same", change>0 ~ "pos", change<0 ~ "neg"))) +
  {if(max_champs_play > 0) geom_image(aes(image = image_1, y = -(max_champs_play+2)*max(current)/10), size = 0.045, asp = 1.5) } +
  {if(max_champs_play > 1) geom_image(aes(image = image_2, y = -(max_champs_play+1)*max(current)/10), size = 0.045, asp = 1.5, na.rm = TRUE) } +
  {if(max_champs_play > 2) geom_image(aes(image = image_3, y = -(max_champs_play-0)*max(current)/10), size = 0.045, asp = 1.5, na.rm = TRUE) } +
  {if(max_champs_play > 3) geom_image(aes(image = image_4, y = -(max_champs_play-1)*max(current)/10), size = 0.045, asp = 1.5, na.rm = TRUE) } +
  {if(max_champs_play > 4) geom_image(aes(image = image_5, y = -(max_champs_play-2)*max(current)/10), size = 0.045, asp = 1.5, na.rm = TRUE) } +
  {if(max_champs_play > 5) geom_image(aes(image = image_6, y = -(max_champs_play-3)*max(current)/10), size = 0.045, asp = 1.5, na.rm = TRUE) } +
  {if(max_champs_play > 6) geom_image(aes(image = image_7, y = -(max_champs_play-4)*max(current)/10), size = 0.045, asp = 1.5, na.rm = TRUE) } +
  ggfittext::geom_fit_text(aes(label = new_name, ymin = -((max(current)/4)+((max_champs_play-disp_images)*max(current)/10)), ymax = 0), 
                           size = 18, reflow = TRUE, padding.x = grid::unit(3, "mm"), padding.y = grid::unit(3, "mm"), place = "right") +
  theme_classic(base_size = 18) +
  coord_flip() +
  scale_x_discrete(labels = element_blank()) +
  scale_y_continuous(breaks = NULL) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("pos" = "#149414", "neg" = "coral2", "na" = "steelblue", "same" = "#EFB700")) +
  labs(
    x = "Archetype", 
    y = "Playrate", 
    title = "TOP 10 Archetype Playrate"
  )

ggsave(filename = "/home/balco/dev/lor-meta-report/output/arch_pr.png", plot = p, width = 12, height = 8, dpi = 180)

# 6.5 archetype winrate ----

data_archetype_wr <- data %>% 
  filter(week == "current") %>%
  left_join(games_by_week, by = "week") %>% 
  count(new_name, game_outcome, tot, name = "nn") %>%
  group_by(new_name, tot) %>% 
  mutate(n = sum(nn)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = game_outcome, values_from = nn) %>%
  mutate(across(where(is.numeric), function(x) ifelse(is.na(x), 0, x))) %>% 
  mutate(winrate = win / n) %>%
  left_join(most_played_version, by = 'new_name') %>% 
  mutate(image = archetype) %>% 
  mutate(image = str_replace_all(image, set_names(data_champs$cardCode, paste0(data_champs$name, "\\b")))) %>% 
  mutate(image = str_trim(image, side = "left")) %>% 
  separate(image, into = c("tmp1", "tmp2"), sep = "\\(", fill = "right") %>% 
  mutate(tmp2 = str_replace_all(tmp2, pattern = " ", replacement = "_")) %>% 
  mutate(tmp2 = ifelse(is.na(tmp2), tmp2, paste0("(", tmp2))) %>% 
  unite(col = image, tmp1, tmp2, sep = "", na.rm = TRUE) %>%
  mutate(image = str_replace_all(image, pattern = " ", replacement = "_")) %>%
  mutate(image = ifelse(grepl("[A-Z]{4}", image), paste0(str_sub(image, 2, 4), "_", str_sub(image, 5, 7)), image)) %>% 
  mutate(image = str_replace_all(image, pattern = "\\(|\\)", replacement = "")) %>% 
  mutate(playrate = n / tot) %>%
  separate(col = image, into = sprintf("image_%s", 1:8), fill = "right", sep ="_") %>% 
  select(where(function(x) any(!is.na(x))), -tot) %>%
  mutate(across(starts_with("image_"), function(x) ifelse(nchar(x) < 3, paste0("x", x, "x"), x))) %>% 
  mutate(across(starts_with("image_"), ~str_replace_all(., set_names(data_champs$gameAbsolutePath, data_champs$cardCode)))) %>% 
  mutate(across(starts_with("image_"), ~str_replace_all(., set_names(data_regions$iconAbsolutePath, paste0("x", data_regions$abbreviation, "x"))))) 

max_champs_win <- data_archetype_wr %>% 
  filter(playrate >= 0.01) %>% 
  slice_max(n = 10, order_by = winrate, with_ties = FALSE) %>%
  select(starts_with("image_")) %>% 
  select(where(function(x) any(!is.na(x)))) %>% 
  ncol()

p <- data_archetype_wr %>%
  filter(playrate >= 0.01) %>% 
  slice_max(n = 10, order_by = winrate, with_ties = FALSE) %>% 
  rowwise() %>% 
  mutate(disp_images = sum(!is.na(c_across(starts_with("image_"))))) %>%
  ungroup() %>% 
  ggplot(aes(x = reorder(new_name, winrate))) +
  geom_col(aes(y = winrate), fill = "#13294b", color = "steelblue", alpha = 0.9) +
  geom_text(aes(label = "", y = 1.1*winrate), size = 6) +
  geom_text(aes(label = scales::percent(winrate, accuracy = .1), y = winrate), size = 6, hjust = -0.5) +
  {if(max_champs_win > 0) geom_image(aes(image = image_1, y = -(max_champs_win+2)*max(winrate)/10), size = 0.045, asp = 1.5) } +
  {if(max_champs_win > 1) geom_image(aes(image = image_2, y = -(max_champs_win+1)*max(winrate)/10), size = 0.045, asp = 1.5, na.rm = TRUE) } +
  {if(max_champs_win > 2) geom_image(aes(image = image_3, y = -(max_champs_win-0)*max(winrate)/10), size = 0.045, asp = 1.5, na.rm = TRUE) } +
  {if(max_champs_win > 3) geom_image(aes(image = image_4, y = -(max_champs_win-1)*max(winrate)/10), size = 0.045, asp = 1.5, na.rm = TRUE) } +
  {if(max_champs_win > 4) geom_image(aes(image = image_5, y = -(max_champs_win-2)*max(winrate)/10), size = 0.045, asp = 1.5, na.rm = TRUE) } +
  {if(max_champs_win > 5) geom_image(aes(image = image_6, y = -(max_champs_win-3)*max(winrate)/10), size = 0.045, asp = 1.5, na.rm = TRUE) } +
  {if(max_champs_win > 6) geom_image(aes(image = image_7, y = -(max_champs_win-4)*max(winrate)/10), size = 0.045, asp = 1.5, na.rm = TRUE) } +
  ggfittext::geom_fit_text(aes(label = new_name, ymin = -((max(winrate)/4)+((max_champs_win-disp_images)*max(winrate)/10)), ymax = 0), 
                           size = 18, reflow = TRUE, padding.x = grid::unit(3, "mm"), padding.y = grid::unit(3, "mm"), place = "right") +
  theme_classic(base_size = 18) +
  coord_flip() +
  scale_x_discrete(labels = element_blank()) +
  scale_y_continuous(breaks = NULL) +
  labs(
    x = "Archetype", 
    y = "Winrate", 
    title = "TOP 10 Archetype Winrate",
    subtitle = "Only archtypes with a playrate >1% are considered."
  )

ggsave(filename = "/home/balco/dev/lor-meta-report/output/arch_wr.png", plot = p, width = 12, height = 8, dpi = 180)

tbl <- data_archetype_wr %>%  
  select(archetype = new_name, match = n, win, playrate, winrate) %>%
  arrange(-playrate, -winrate) %>% 
  rename_with(str_to_title) %>% 
  datatable(rownames = FALSE, options = list(pageLength = 10, lengthChange = FALSE)) %>% 
  formatPercentage(columns = c("Playrate", "Winrate"), digits = 1)

tbl$width  <- "100%"
tbl$height <- "500px"
tbl$sizingPolicy$browser$padding <- 10

saveWidget(tbl, "/home/balco/dev/lor-meta-report/output/arch_wr.html", background = "inherit")

# 6.6 archetype matchup ----

data_matchup <- data %>%
  filter(week == "current") %>% 
  select(match_id, game_outcome, new_name) %>%
  group_by(match_id) %>%
  arrange(match_id, new_name) %>% 
  mutate(id = row_number()) %>% 
  mutate(winner = case_when(id == 1 & game_outcome == "win" ~ 1, id == 2 & game_outcome == "win" ~ 2, TRUE ~ 0)) %>% 
  pivot_wider(names_from = id, values_from = new_name, names_prefix = "archetype_") %>% 
  fill(starts_with("archetype_"), .direction = "updown") %>% 
  ungroup() %>% 
  filter(winner != 0) %>%
  group_by(across(starts_with("archetype_"))) %>% 
  summarise(n = n(), wins = sum(winner == 1), .groups = "drop") %>% 
  mutate(a1_wr = ifelse(archetype_1 == archetype_2, NA_real_, wins / n)) 

p <- data_matchup %>% 
  filter(archetype_1 %in% data_archetype_pr$new_name & archetype_2 %in% data_archetype_pr$new_name) %>% 
  select(-c(n, wins)) %>% 
  pivot_wider(names_from = archetype_2, values_from = a1_wr) %>%
  column_to_rownames(var = "archetype_1") %>%
  fill_matchup_table() %>%
  rownames_to_column(var = "archetype_1") %>% 
  pivot_longer(cols = -archetype_1, names_to = "archetype_2", values_to = "a1_wr") %>%
  mutate(bin = cut(a1_wr, c(0, 0.4, 0.45, 0.55, 0.6, 1), include.lowest = TRUE)) %>%
  mutate(across(c(archetype_1, archetype_2), ~factor(., levels = data_archetype_pr$new_name, ordered = TRUE))) %>%
  ggplot(aes(y = reorder(archetype_1, desc(archetype_1)), x = archetype_2)) +
  geom_tile(aes(fill = bin), color = "grey90", size = 1, stat = "identity") +
  shadowtext::geom_shadowtext(aes(label = scales::percent(a1_wr, accuracy = .1)), color = "white", size = 6, na.rm = TRUE) +
  theme_minimal(base_size = 15) +
  labs(x = element_blank(), y = element_blank()) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10), position = "top") +
  theme(legend.position = "none") +
  scale_fill_manual(
    values = c("[0,0.4]" = "#B81D13", "(0.4,0.45]" = "coral2", "(0.45,0.55]" = "#EFB700", "(0.55,0.6]" = "#149414", "(0.6,1]" = "#046507"), 
    na.value = "grey90"
  )

ggsave(filename = "/home/balco/dev/lor-meta-report/output/matchup_tbl.png", plot = p, width = 12, height = 8, dpi = 180)

data_matchup1 <- data_matchup %>% 
  filter(archetype_1 != archetype_2) %>% 
  select(starts_with("archetype_"), n, winrate = a1_wr)

data_matchup2 <- tibble(
  archetype_1 = data_matchup1$archetype_2,
  archetype_2 = data_matchup1$archetype_1,
  n = data_matchup1$n,
  winrate = 1- data_matchup1$winrate
)

tbl <- data_matchup1 %>% 
  bind_rows(data_matchup2) %>%
  group_by(archetype_1) %>% 
  mutate(tot_match = sum(n)) %>% 
  ungroup() %>% 
  arrange(-tot_match, archetype_1, -n) %>% 
  select(-tot_match) %>%
  filter(n >= 5) %>% # FILTERING ONLY MATCHUPS WITH AT LEAST 5 GAMES
  mutate(across(starts_with("archetype_"), as.factor)) %>%
  mutate(winrate = round(winrate*100, digits = 1)) %>% # nicer filter
  rename(player = archetype_1, opponent = archetype_2, match = n) %>% 
  rename_with(str_to_title) %>% 
  datatable(rownames = FALSE, filter = 'top', options = list(pageLength = 10, lengthChange = FALSE)) %>% 
  formatCurrency(columns = "Winrate", currency = "%", before = FALSE, digits = 1)

tbl$width  <- "100%"
tbl$height <- "500px"
tbl$sizingPolicy$browser$padding <- 10

saveWidget(tbl, "/home/balco/dev/lor-meta-report/output/matchup_tbl.html", background = "inherit")

# 6.7 meta score ----

data_score1 <- data %>%
  filter(week == "current") %>% 
  select(match_id, game_outcome, new_name) %>%
  mutate(new_name = ifelse(new_name %in% data_archetype_pr$new_name, new_name, "Other")) %>% 
  arrange(match_id, new_name) %>% 
  group_by(match_id) %>%
  mutate(id = row_number()) %>% 
  mutate(winner = case_when(id == 1 & game_outcome == "win" ~ 1, id == 2 & game_outcome == "win" ~ 2, TRUE ~ 0)) %>% 
  pivot_wider(names_from = id, values_from = new_name, names_prefix = "archetype_") %>% 
  fill(starts_with("archetype_"), .direction = "updown") %>% 
  ungroup() %>% 
  filter(winner != 0) %>% 
  group_by(across(starts_with("archetype_"))) %>% 
  summarise(n = n(), wins = sum(winner == 1), .groups = "drop") %>% 
  mutate(a1_wr = ifelse(archetype_1 == archetype_2, 0.5, wins / n))

data_score2 <- tibble(
  archetype_1 = data_score1$archetype_2,
  archetype_2 = data_score1$archetype_1,
  n = data_score1$n,
  wins = data_score1$n - data_score1$wins,
  a1_wr = 1 - data_score1$a1_wr
)

data_score <- data_score1 %>% 
  filter(archetype_1 != archetype_2) %>% 
  bind_rows(data_score2) %>% 
  left_join(data_archetype_pr %>% select(new_name, playrate = current), by = c("archetype_2" = "new_name")) %>%
  group_by(archetype_1) %>% 
  mutate(playrate = ifelse(is.na(playrate), 1 - sum(playrate, na.rm = TRUE), playrate)) %>% 
  ungroup() %>% 
  replace_na(list(ai_wr = 0.5)) %>% # if i have 0 games of a particular matchup, assume its 50-50
  mutate(deck_power = a1_wr*playrate) %>%
  group_by(archetype_1) %>% 
  summarise(deck_power = sum(deck_power), .groups = "drop") %>% 
  filter(archetype_1 != "Other") %>% 
  arrange(-deck_power) %>% 
  left_join(data_archetype_pr %>% select(new_name, playrate = current), by = c("archetype_1" = "new_name")) %>% 
  mutate(freq_score = playrate*100 / max(playrate)) %>% 
  select(-playrate) %>% 
  mutate(power_score = ((deck_power + max(deck_power) - 1) * 100) / (2*max(deck_power) - 1)) %>% 
  mutate(meta_score = (power_score + freq_score) / 2) 

p <- data_score %>% 
  mutate(across(ends_with("_score"), scales::comma, accuracy = .1)) %>% 
  mutate(across(deck_power, scales::comma, accuracy = .001)) %>% 
  rename_with(~str_replace_all(., pattern = "_|[0-9]", replacement = " ")) %>% 
  rename_with(str_to_title) %>%
  tableGrob(rows = NULL)

h <- grid::convertHeight(sum(p$heights), "in", TRUE)
w <- grid::convertWidth(sum(p$widths), "in", TRUE)

ggsave(filename = "/home/balco/dev/lor-meta-report/output/meta_score1.png", plot = p, width = w+0.01, height = h+0.01)

p <- data_score %>% 
  ggplot(aes(x = power_score, y = freq_score)) +
  geom_point(aes(size = meta_score), fill = "steelblue", color = "grey30", pch = 21) +
  geom_text_repel(aes(label = paste(scales::comma(meta_score, accuracy = .1), archetype_1, sep = " \n ")), point.padding = 25, min.segment.length = 1) +
  geom_segment(aes(x = 7.5, y = 100, xend = 100, yend = 100), arrow = arrow(length = unit(0.03, "npc"))) +
  geom_label(aes(x = 7.5, y = 100), label = "Meta Peak \n Theoretical best deck whose \n frequency and power are \n both the highest.", size = 4, fill = "#FDCE2A") +
  theme_bw(base_size = 18) +
  expand_limits(x = c(0,100), y = c(0,105)) +
  theme(legend.position = "none") +
  scale_size_continuous(range = c(10,25)) +
  labs(x = "Power Score", y = "Frequency Score", title = "Meta Score", subtitle = "This chart shows only the 10 most played archetypes")

ggsave(filename = "/home/balco/dev/lor-meta-report/output/meta_score2.png", plot = p, width = 12, height = 8, dpi = 180)

# 6.8 player leaderboard ----

top_players <- data %>% 
  filter(week == "current") %>% 
  count(puuid, shard, game_outcome) %>% 
  pivot_wider(names_from = game_outcome, values_from = n) %>% 
  mutate(across(where(is.numeric), replace_na, 0)) %>% 
  rowwise() %>% 
  mutate(match = sum(c_across(where(is.numeric)))) %>% 
  ungroup() %>%
  filter(match >= 70) %>% 
  mutate(winrate = win / match) %>% 
  filter(winrate >= 0.6) %>% 
  select(shard, player = puuid, match, winrate)

favorite_deck <- data %>% 
  filter(week == "current", puuid %in% top_players$player) %>% 
  count(puuid, new_name) %>% 
  group_by(puuid) %>% 
  slice_max(n = 1, order_by = n, with_ties = FALSE) %>% 
  select(-n)

tbl <- top_players %>% 
  left_join(favorite_deck, by = c("player" = "puuid")) %>%
  arrange(-winrate, -match) %>% 
  mutate(player = map2_chr(.x = player, .y = shard, .f = ~from_puuid_to_riotid(puuid = .x, shard = .y))) %>% 
  mutate(get_call = sprintf("https://runeterra.ar/Users/get/country/%s/%s", shard, sub('#[^#]*$', '', player))) %>% 
  mutate(get_call = utils::URLencode(get_call)) %>% 
  mutate(get = map(.x = get_call, .f = get_slowly)) %>% 
  mutate(status = map_int(.x = get, .f = status_code)) %>% 
  mutate(content = map(.x = get, .f = content)) %>% 
  mutate(country = ifelse(status == 200, content, NA_character_)) %>% 
  mutate(country = map_chr(country, str_flatten, collapse = " ")) %>% 
  select(player, region = shard, country, most_played_deck = new_name, match, winrate) %>% 
  mutate(region = str_to_title(region)) %>% 
  mutate(country = ifelse(!is.na(country), sprintf("<img src='https://flagcdn.com/32x24/%s.png'></img>", country), country)) %>% 
  mutate(pos = row_number()) %>% 
  relocate(pos, .before = everything()) %>% 
  mutate(pos = case_when(pos == 1 ~ "gold.png", pos == 2 ~ "silver.png", pos == 3 ~ "bronze.png", TRUE ~ NA_character_)) %>%
  mutate(size = case_when(pos == "gold.png" ~ 0.6, pos == "silver.png" ~ 0.45, pos == "bronze.png" ~ 0.3, TRUE ~ NA_real_)) %>% 
  mutate(pos = ifelse(!is.na(pos), sprintf("/home/balco/dev/lor-meta-report/templates/medals/%s", pos), pos)) %>%
  mutate(pos = map2_chr(.x = pos, .y = size, .f = ~ifelse(!is.na(.x), img_uri(x = .x, size = .y), .x))) %>% 
  select(-size) %>% 
  rename_with(~str_replace_all(., pattern = "_", replacement = " ")) %>%
  rename(" " = pos) %>% 
  rename_with(str_to_title) %>% 
  datatable(
    escape = FALSE, 
    rownames = FALSE,  
    options = list(pageLength = 10, lengthChange = FALSE, columnDefs = list(list(className = 'dt-center', targets = "_all")))
  ) %>% 
  formatPercentage(col = "Winrate", digits = 1)

tbl$width  <- "100%"
tbl$height <- "500px"
tbl$sizingPolicy$browser$padding <- 10

saveWidget(tbl, "/home/balco/dev/lor-meta-report/output/player_leaderboard.html", background = "inherit")

# 6.9 best players ----

tbl <- data %>% 
  filter(week == "current") %>% 
  count(new_name, deck_code, puuid, game_outcome, shard) %>% 
  pivot_wider(names_from = game_outcome, values_from = n) %>% 
  mutate(across(where(is.numeric), replace_na, 0)) %>% 
  rowwise() %>% 
  mutate(match = sum(c_across(where(is.numeric)))) %>% 
  ungroup() %>%
  filter(match >= 30) %>% 
  mutate(winrate = win / match) %>% 
  filter(winrate >= 0.7) %>% 
  select(shard, player = puuid, archetype = new_name, deck_code, match, winrate) %>%
  arrange(-winrate, -match, archetype) %>% 
  mutate(player = map2_chr(.x = player, .y = shard, .f = ~from_puuid_to_riotid(puuid = .x, shard = .y))) %>% 
  mutate(deck_code = sprintf('<a href="https://runeterra.ar/decks/code/%s" target="_blank">%s</a>', deck_code, str_trunc(deck_code, width = 18))) %>% 
  mutate(get_call = sprintf("https://runeterra.ar/Users/get/country/%s/%s", shard, sub('#[^#]*$', '', player))) %>% 
  mutate(get_call = utils::URLencode(get_call)) %>% 
  mutate(get = map(.x = get_call, .f = get_slowly)) %>% 
  mutate(status = map_int(.x = get, .f = status_code)) %>% 
  mutate(content = map(.x = get, .f = content)) %>% 
  mutate(country = ifelse(status == 200, content, NA_character_)) %>% 
  mutate(country = map_chr(country, str_flatten, collapse = " ")) %>% 
  select(player, region = shard, country, archetype, deck_code, match, winrate) %>% 
  mutate(region = str_to_title(region)) %>% 
  mutate(country = ifelse(!is.na(country), sprintf("<img src='https://flagcdn.com/32x24/%s.png'></img>", country), country)) %>% 
  rename_with(~str_replace_all(., pattern = "_", replacement = " ")) %>% 
  rename_with(str_to_title) %>% 
  datatable(
    escape = FALSE, 
    rownames = FALSE,  
    options = list(pageLength = 10, lengthChange = FALSE, columnDefs = list(list(className = 'dt-center', targets = "_all")))
  ) %>% 
  formatPercentage(col = "Winrate", digits = 1)

tbl$width  <- "100%"
tbl$height <- "500px"
tbl$sizingPolicy$browser$padding <- 10

saveWidget(tbl, "/home/balco/dev/lor-meta-report/output/best_players.html", background = "inherit")

# 6.10 best decklists ----

tbl <- data %>% 
  filter(week == "current") %>% 
  count(new_name, deck_code, game_outcome) %>% 
  group_by(deck_code) %>% 
  mutate(match = sum(n)) %>% 
  ungroup() %>% 
  filter(match >= 50) %>% 
  pivot_wider(names_from = game_outcome, values_from = n) %>% 
  mutate(across(where(is.numeric), function(x) ifelse(is.na(x), 0, x))) %>%
  mutate(winrate = win / match) %>% 
  arrange(-match) %>%
  select(archetype = new_name, deck_code, match, winrate) %>% 
  reactable(
    columns = list(
      archetype = colDef(
        name = "Archetype"
      ),
      winrate = colDef(
        name = "Winrate",
        defaultSortOrder = "desc",
        cell = function(value) {
          value <- paste0(format(value * 100, digits = 3, nsmall = 1), "%")
          bar_chart(value, width = value, fill = "#fc5185", background = "#e1e1e1")
        },
        align = "left"
      ),
      match = colDef(
        name = "Match",
        defaultSortOrder = "desc",
        cell = function(value) {
          width <- paste0(value * 100 / max(.$match), "%")
          value <- format(value, big.mark = ",")
          bar_chart(value, width = width, fill = "#3fc1c9")
        },
        align = "left"
      ),
      deck_code = colDef(
        name = "Deck Code",
        html = TRUE, 
        cell = function(value) {
          sprintf('<a href="https://runeterra.ar/decks/code/%s" target="_blank">%s</a>', value, str_trunc(value, width = 18))
        })
    ),
    resizable = TRUE,
    searchable = TRUE, 
    minRows = 10
  )

tbl$width  <- "100%"
tbl$height <- "500px"
tbl$sizingPolicy$browser$padding <- 10

saveWidget(tbl, "/home/balco/dev/lor-meta-report/output/deck_codes.html", background = "inherit")

# 7. export charts and tables to "lor-meta.com" ----

reports <- system('ssh balco@lor-meta.com "ls -d /home/balco/www/_posts/*"', intern = TRUE)

latest <- reports %>% 
  str_extract(pattern = "\\#[0-9]+") %>% 
  parse_number() %>% 
  max()

system(sprintf('ssh balco@lor-meta.com "mkdir -p /home/balco/www/assets/meta-report/mr%s"', latest+1))
system(sprintf("scp -r /home/balco/dev/lor-meta-report/output/* balco@lor-meta.com:/home/balco/www/assets/meta-report/mr%s", latest+1))
