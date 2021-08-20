# 1. libraries ----

library(tidyverse)
library(mongolite)
library(lubridate)
library(lordecks)
library(rvest)
library(ggimage)
library(gridExtra)
library(ggrepel)
library(DT)
library(htmlwidgets)
library(reticulate)

# 2. connect to db & load data ----

# connect to db
m_match <- mongo(url = "mongodb://balco:n0nLadimentico@localhost:27017/admin", collection = "lor_match_info")

# import match data (only from ranked games)
data_tot <- m_match$find(
  #query = sprintf('{"info.game_mode":"Constructed", "info.game_type":"Ranked", "info.game_start_time_utc": { "$gt" : { "$date" : "%sT00:00:00Z" }}}', start_date),
  query = '{"info.game_type":"Ranked"}',
  fields = '{"metadata.match_id" : true, "info.game_start_time_utc" : true, "info.players" : true, "_id": false}'
  ) %>% 
  map(as_tibble) %>% 
  bind_cols()

# list of codes of all runeterra champions
champ_codes <- read_html("https://leagueoflegends.fandom.com/wiki/Champion_(Legends_of_Runeterra)") %>% 
  html_table(fill = TRUE) %>% 
  .[[2]] %>% 
  as_tibble(.name_repair = make.names) %>% 
  select(1, 2, 5) %>% 
  filter(nchar(X) <= 8) %>% 
  mutate(Region = case_when( # same region names as API
    Region == "Piltover & Zaun" ~ "Piltover",
    Region == "Shadow Isles" ~ "ShadowIsles",
    Region == "Targon" ~ "MtTargon", 
    TRUE ~ Region
  ))

new_champs <- tibble(
  X = c("04SI055", "04SH130"),
  Name = c("Viego", "Akshan"),
  Region = c("ShadowIsles", "Shurima")
)

champ_codes <- full_join(champ_codes, new_champs, by = colnames(champ_codes))

# 3. set parameters ----

# date from which extract matches
#start_date <- Sys.Date() %>% ceiling_date(unit = "week", week_start = 1) - days(7)
start_date <- as_datetime("2021-07-14T17:00:00")
end_date <- as_datetime("3000-01-01T00:00:00")
#end_date <- as_datetime("2021-06-30T17:00:00") + days(7)

# color of runeterra regions for plots
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

# regions name+abbreviation
region_abb <- c(
  "ShadowIsles" = "SI",
  "Noxus" = "NX",
  "Shurima" = "SH",
  "Freljord" = "FR",
  "MtTargon" = "MT",
  "Piltover" = "PZ",
  "Ionia" = "IO",
  "Demacia" = "DE",
  "Bilgewater" = "BW"
)

# path where region logos (& other images) are stored
path_logo <- "./images/"

# install / update "lor_deckcodes" (TO DO EVERY NEW EXPANSION)
#py_install("lor_deckcodes", pip = TRUE)

# import python deck decoder
lor_deckcodes <- import("lor_deckcodes")

# 4. define functions ----

# create a nice date from  date object
nice_date <- function(date, short_month = TRUE){
  
  paste(month(date, label = TRUE, abbr = short_month), day(date), sep = " ")
  
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

# 5. make report ----

# extract information
data_tot <- data_tot %>% 
  unnest(cols = c(players), keep_empty = TRUE) %>% 
  unnest(cols = c(factions), keep_empty = TRUE)

# keep only relevant columns
data_tot <- data_tot %>% 
  select(match_id, puuid, game_start_time_utc, deck_code, factions, game_outcome)

# make faction column nicer
data_tot <- data_tot %>% 
  mutate(factions = str_extract(factions, pattern = "(?<=_)(.+)(?=\\_)"))

# reshape (keeping only 1 row per deck)
data_tot <- data_tot %>% 
  group_by(match_id, puuid, deck_code, game_outcome) %>%
  mutate(id = row_number()) %>% 
  ungroup() %>% 
  {if(max(.$id) > 2) error("Something's wrong; manual fix required.") else .} %>% 
  pivot_wider(names_from = id, values_from = factions, names_prefix = "faction_")

# keep only recent data
data <- data_tot %>% 
  filter(game_start_time_utc >= start_date & game_start_time_utc <= end_date)

# extract card codes from deck code [~1500 decks / minute]
data <- data %>% 
  distinct(deck_code) %>% 
  mutate(cards = map(.x = deck_code, .f = lor_deckcodes$decode$decode_deck)) %>% 
  left_join(x = data, y = ., by = "deck_code")

# get deck champions & archetype
data <- data %>% 
  mutate(cards = map_chr(cards, str_flatten, collapse = " ")) %>%
  mutate(champs = str_extract_all(cards, pattern = paste(champ_codes$X, collapse = "|"))) %>% 
  mutate(champs = map_chr(champs, str_flatten, collapse = " ")) %>%
  mutate(champs_factions = str_remove_all(champs, "[0-9]")) %>% 
  mutate(factions = paste(region_abb[faction_1], region_abb[faction_2])) %>%
  mutate(factions = str_remove_all(factions, pattern = " NA|NA ")) %>% 
  mutate(across(c(champs, champs_factions, factions),  function(x) unname(sapply(x, function(x) { paste(sort(trimws(strsplit(x[1], ' ')[[1]])), collapse=' ')} )))) %>%
  mutate(no_fix = map2_lgl(.x = factions, .y = champs_factions, .f = ~grepl(pattern = .x, x = .y))) %>% 
  mutate(champs_factions = str_split(champs_factions, " ")) %>% 
  mutate(champs_factions = map_chr(champs_factions, str_flatten, collapse = "|")) %>%
  mutate(champs_factions = paste(" ", champs_factions, sep = "|")) %>% 
  mutate(factions_to_add = str_remove_all(factions, pattern = champs_factions)) %>% 
  mutate(archetype = if_else(no_fix, champs, sprintf("%s (%s)", champs, factions_to_add))) %>% 
  select(-c(champs_factions, factions, no_fix, factions_to_add))

# 6. images to save ----

# 6.0 data info ----

p <- data %>% 
  summarise(
    match_analyzed = n_distinct(match_id),
    from  = nice_date(min(game_start_time_utc)),
    to    = nice_date(max(game_start_time_utc)),
    unique_players = n_distinct(puuid),
    unique_decklists = n_distinct(deck_code),
    tied_match = sum(game_outcome == "tie") / 2
  ) %>%
  mutate(tied_match = sprintf("%s (%s)", tied_match, scales::percent(tied_match/match_analyzed, accuracy = .01))) %>%
  rename_with(~str_replace_all(., pattern = "_", replacement = " ")) %>% 
  rename_with(str_to_title) %>% 
  mutate(across(!where(is.character), as.character)) %>% 
  pivot_longer(cols = everything()) %>%
  mutate(name = paste0(name, ": ")) %>% 
  tableGrob(rows = NULL, cols = NULL)

h <- grid::convertHeight(sum(p$heights), "in", TRUE)
w <- grid::convertWidth(sum(p$widths), "in", TRUE)

ggsave(filename = "./images/output/data.png", plot = p, width = w+0.01, height = h+0.01)

# 6.1. playrate by region ----

p <- data %>% 
  select(starts_with("faction_")) %>% 
  pivot_longer(cols = everything()) %>% 
  count(value, sort = TRUE) %>% 
  drop_na() %>% 
  mutate(playrate = n / nrow(data)) %>% 
  mutate(logo = paste0(path_logo, value, ".png")) %>% 
  ggplot(aes(x = reorder(value, playrate))) +
  geom_col(aes(y = playrate, fill = value), color = "grey30", alpha = 0.8) +
  geom_text(aes(label = scales::percent(playrate, accuracy = .1), y = playrate), size = 6, hjust = -0.5) +
  geom_text(aes(label = "", y = 1.1*playrate), size = 6) +
  geom_image(aes(image = ifelse(playrate>0.05, logo, NA), y = playrate-0.05*max(playrate)), size = 0.06, asp = 1.5, na.rm = TRUE) +
  coord_flip() +
  theme_bw(base_size = 18) +
  scale_fill_manual(values = region_colors) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "none") +
  labs(x = "Region", y = "% of decks containing the region", title = "Playrate by Region")

ggsave(filename = "./images/output/region_pr.png", plot = p, width = 12, height = 8)

# 6.2. champions playrate ----

data_pr <- data %>% 
  select(champs) %>% 
  separate(col = champs, into = sprintf("champ_%s", 1:6), sep = " ", fill = "right") %>% 
  pivot_longer(cols = everything()) %>% 
  drop_na(value) %>% 
  count(value, sort = TRUE) %>% 
  mutate(playrate = n / nrow(data)) %>%
  left_join(champ_codes, by = c("value" = "X")) %>% 
  drop_na(Name)

p <- data_pr %>% 
  slice_max(n = 10, order_by = n, with_ties = FALSE) %>%
  mutate(img = paste0("./images/cards/", value, ".png")) %>%
  mutate(logo = paste0(path_logo, Region, ".png")) %>%
  ggplot(aes(x = reorder(Name, playrate))) +
  geom_col(aes(y = playrate, fill = Region), color = "grey30", alpha = 0.8) +
  geom_text(aes(label = "", y = playrate*1.1), size = 6) +
  geom_text(aes(label = scales::percent(playrate, accuracy = .1), y = playrate), size = 6, hjust = -0.5) +
  geom_image(aes(image = img, y = -max(playrate)/10), size = 0.045, asp = 1.5) +
  geom_image(aes(image = logo, y = -max(playrate)/5), size = 0.06, asp = 1.5) +
  coord_flip() +
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = NULL) +
  scale_x_discrete(labels = element_blank()) +
  scale_fill_manual(values = region_colors) +
  theme(legend.position = "none") +
  labs(x = "Champions", y = "% of decks containing the champions", title = "Playrate by Champion")

ggsave(filename = "./images/output/champs_pr.png", plot = p, width = 12, height = 8)

tbl <- data_pr %>% 
  select(Name, n, playrate) %>% 
  arrange(-n) %>% 
  rename_with(str_to_title) %>% 
  datatable(rownames = FALSE, options = list(pageLength = 10, lengthChange = FALSE)) %>% 
  formatPercentage(columns = "Playrate", digits = 1)

tbl$width<-"100%"
tbl$height <- "500px"
tbl$sizingPolicy$browser$padding <- 10

saveWidget(tbl, "./images/output/champs_pr.html", background = "inherit")

# 6.3. historical play-rate ----

data_history <- data_tot %>% 
  select(game_start_time_utc, starts_with("faction_")) %>%
  filter(game_start_time_utc <= end_date) %>% 
  pivot_longer(cols = starts_with("faction_")) %>%
  drop_na(value) %>% 
  mutate(game_start_time_utc = as_date(game_start_time_utc)) %>% 
  mutate(week = ceiling_date(game_start_time_utc, unit = "week", week_start = 4)-days(1)) %>%
  count(week, value)

weekly_decks <- data_tot %>% 
  filter(game_start_time_utc <= end_date) %>% 
  mutate(game_start_time_utc = as_date(game_start_time_utc)) %>% 
  mutate(week = ceiling_date(game_start_time_utc, unit = "week", week_start = 4)-days(1)) %>%
  count(week, name = "tot_decks") %>% 
  filter(tot_decks > 100)

data_history <- data_history %>% 
  filter(week %in% weekly_decks$week)
  
p <- data_history %>% 
  left_join(weekly_decks, by = "week") %>%
  mutate(playrate = n / tot_decks) %>% 
  ggplot(aes(x = week)) +
  geom_line(aes(y = playrate, group = value, color = value), size = 2) +
  geom_segment(x = ymd("2021-06-30")+1, xend = ymd("2021-06-30")+1, y = 0, yend = 100, color = "steelblue", linetype = "dotted") +
  geom_label(x = ymd("2021-06-30")+1, y = 0, label = "Rise of the Underworlds \n Patch 2.11", size = 5) +
  geom_segment(x = ymd("2021-07-14")+1, xend = ymd("2021-07-14")+1, y = 0, yend = 100, color = "steelblue", linetype = "dotted") +
  geom_label(x = ymd("2021-07-14")+1, y = 0, label = "Sentinels of Light \n Patch 2.12", size = 5) +
  geom_point(aes(x = week, y = playrate, color = value, group = value), size = 5) +
  theme_bw(base_size = 15) +
  expand_limits(y = 0) +
  scale_x_continuous(breaks = ymd(unique(weekly_decks$week)), labels = nice_date) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  scale_color_manual(values = region_colors) +
  theme(legend.position = "bottom") +
  labs(x = "Week", y = "Playrate", title = "Weekly Playrate by Region", color = "Region") +
  guides(colour = guide_legend(nrow = 1))

ggsave(filename = "./images/output/region_hist.png", plot = p, width = 12, height = 8)

# 6.4 archetype playrate ----

data_archetype_pr <- data %>% 
  count(archetype, sort = "TRUE") %>% 
  slice_max(n = 10, order_by = n, with_ties = FALSE) %>% 
  mutate(image = archetype) %>% 
  mutate(image = str_replace_all(image, pattern = " ", replacement = "_")) %>%
  mutate(image = ifelse(grepl("[A-Z]{4}", image), paste0(str_sub(image, 2, 4), "_", str_sub(image, 5, 7)), image)) %>% 
  mutate(image = str_replace_all(image, pattern = "\\(|\\)", replacement = "")) %>%
  mutate(archetype = str_replace_all(archetype, set_names(champ_codes$Name, champ_codes$X))) %>% 
  mutate(playrate = n / nrow(data)) %>%
  separate(col = image, into = sprintf("image_%s", 1:7), fill = "right") %>% 
  select(where(function(x) any(!is.na(x)))) %>% 
  mutate(across(starts_with("image_"), ~paste0("./images/cards/", ., ".png"))) 

max_champs_play <- ncol(data_archetype_pr) - 3

p <- data_archetype_pr %>% 
  ggplot(aes(x = reorder(archetype, playrate))) +
  geom_col(aes(y = playrate), fill = "#13294b", color = "steelblue", alpha = 0.9) +
  geom_text(aes(label = "", y = 1.1*playrate), size = 6) +
  geom_text(aes(label = scales::percent(playrate, accuracy = .1), y = playrate), size = 6, hjust = -0.5) +
  {if(max_champs_play > 0) geom_image(aes(image = image_1, y = -max_champs_play*max(playrate)/10), size = 0.045, asp = 1.5) } +
  {if(max_champs_play > 1) geom_image(aes(image = image_2, y = -(max_champs_play-1)*max(playrate)/10), size = 0.045, asp = 1.5) } +
  {if(max_champs_play > 2) geom_image(aes(image = image_3, y = -(max_champs_play-2)*max(playrate)/10), size = 0.045, asp = 1.5) } +
  {if(max_champs_play > 3) geom_image(aes(image = image_4, y = -(max_champs_play-3)*max(playrate)/10), size = 0.045, asp = 1.5) } +
  {if(max_champs_play > 4) geom_image(aes(image = image_5, y = -(max_champs_play-4)*max(playrate)/10), size = 0.045, asp = 1.5) } +
  {if(max_champs_play > 5) geom_image(aes(image = image_6, y = -(max_champs_play-5)*max(playrate)/10), size = 0.045, asp = 1.5) } +
  {if(max_champs_play > 6) geom_image(aes(image = image_7, y = -(max_champs_play-6)*max(playrate)/10), size = 0.045, asp = 1.5) } +
  theme_bw(base_size = 18) +
  coord_flip() +
  scale_x_discrete(labels = element_blank()) +
  scale_y_continuous(breaks = NULL) +
  labs(
    x = "Archetype", 
    y = "Playrate", 
    title = "Playrate by Archetype", 
    subtitle = "Only the 10 archetypes with the highest playrate are shown."
  )

ggsave(filename = "./images/output/arch_pr.png", plot = p, width = 12, height = 8)

# 6.5 archetype winrate ----

data_archetype_wr <- data %>% 
  count(archetype, game_outcome, name = "nn") %>%
  group_by(archetype) %>% 
  mutate(n = sum(nn)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = game_outcome, values_from = nn) %>%
  mutate(across(where(is.numeric), function(x) ifelse(is.na(x), 0, x))) %>% 
  mutate(winrate = win / n) %>%
  mutate(image = archetype) %>% 
  mutate(archetype = ifelse(grepl("^( )", archetype), paste0("No Champions", archetype), archetype)) %>% 
  mutate(image = str_replace_all(image, pattern = " ", replacement = "_")) %>%
  mutate(image = ifelse(grepl("[A-Z]{4}", image), paste0(str_sub(image, 2, 4), "_", str_sub(image, 5, 7)), image)) %>% 
  mutate(image = str_replace_all(image, pattern = "\\(|\\)", replacement = "")) %>%
  mutate(archetype = str_replace_all(archetype, set_names(champ_codes$Name, champ_codes$X))) %>% 
  mutate(playrate = n / nrow(data)) %>%
  separate(col = image, into = sprintf("image_%s", 1:7), fill = "right") %>% 
  select(where(function(x) any(!is.na(x)))) %>% 
  mutate(across(starts_with("image_"), ~paste0("./images/cards/", ., ".png"))) 

max_champs_win <- data_archetype_wr %>% 
  filter(playrate >= 0.01) %>% 
  slice_max(n = 10, order_by = winrate, with_ties = FALSE) %>%
  select(starts_with("image_")) %>% 
  mutate(across(everything(), function(x) na_if(x, "./images/cards/NA.png"))) %>% 
  select(where(function(x) any(!is.na(x)))) %>% 
  ncol()

p <- data_archetype_wr %>%
  filter(playrate >= 0.01) %>% 
  slice_max(n = 10, order_by = winrate, with_ties = FALSE) %>% 
  ggplot(aes(x = reorder(archetype, winrate))) +
  geom_col(aes(y = winrate), fill = "#13294b", color = "steelblue", alpha = 0.9) +
  geom_text(aes(label = "", y = 1.1*winrate), size = 6) +
  geom_text(aes(label = scales::percent(winrate, accuracy = .1), y = winrate), size = 6, hjust = -0.5) +
  {if(max_champs_win > 0) geom_image(aes(image = image_1, y = -max_champs_win*max(winrate)/10), size = 0.045, asp = 1.5) } +
  {if(max_champs_win > 1) geom_image(aes(image = image_2, y = -(max_champs_win-1)*max(winrate)/10), size = 0.045, asp = 1.5) } +
  {if(max_champs_win > 2) geom_image(aes(image = image_3, y = -(max_champs_win-2)*max(winrate)/10), size = 0.045, asp = 1.5) } +
  {if(max_champs_win > 3) geom_image(aes(image = image_4, y = -(max_champs_win-3)*max(winrate)/10), size = 0.045, asp = 1.5) } +
  {if(max_champs_win > 4) geom_image(aes(image = image_5, y = -(max_champs_win-4)*max(winrate)/10), size = 0.045, asp = 1.5) } +
  {if(max_champs_win > 5) geom_image(aes(image = image_6, y = -(max_champs_win-5)*max(winrate)/10), size = 0.045, asp = 1.5) } +
  {if(max_champs_win > 6) geom_image(aes(image = image_7, y = -(max_champs_win-6)*max(winrate)/10), size = 0.045, asp = 1.5) } +
  theme_bw(base_size = 18) +
  coord_flip() +
  scale_x_discrete(labels = element_blank()) +
  scale_y_continuous(breaks = NULL) +
  labs(
    x = "Archetype", 
    y = "Winrate", 
    title = "Winrate by Archetype",
    subtitle = "Archetypes with a playrate < 1% are excluded."
  )

ggsave(filename = "./images/output/arch_wr.png", plot = p, width = 12, height = 8)

tbl <- data_archetype_wr %>% 
  select(archetype, n, win, playrate, winrate) %>%
  arrange(-playrate, -winrate) %>% 
  rename_with(str_to_title) %>% 
  datatable(rownames = FALSE, options = list(pageLength = 10, lengthChange = FALSE)) %>% 
  formatPercentage(columns = c("Playrate", "Winrate"), digits = 1)

tbl$width  <- "100%"
tbl$height <- "500px"
tbl$sizingPolicy$browser$padding <- 10

saveWidget(tbl, "./images/output/arch_wr.html", background = "inherit")

# 6.6 archetype matchup ----

top_10_pr <- data_archetype_pr$archetype %>% 
  str_replace_all(set_names(champ_codes$X, champ_codes$Name))

data_matchup <- data %>% 
  select(match_id, game_outcome, archetype) %>%
  group_by(match_id) %>%
  arrange(match_id, archetype) %>% 
  mutate(id = row_number()) %>% 
  mutate(winner = case_when(id == 1 & game_outcome == "win" ~ 1, id == 2 & game_outcome == "win" ~ 2, TRUE ~ 0)) %>% 
  pivot_wider(names_from = id, values_from = archetype, names_prefix = "archetype_") %>% 
  fill(starts_with("archetype_"), .direction = "updown") %>% 
  ungroup() %>% 
  filter(winner != 0) %>%
  group_by(across(starts_with("archetype_"))) %>% 
  summarise(n = n(), wins = sum(winner == 1), .groups = "drop") %>% 
  mutate(a1_wr = ifelse(archetype_1 == archetype_2, NA_real_, wins / n)) 

p <- data_matchup %>% 
  filter(archetype_1 %in% top_10_pr & archetype_2 %in% top_10_pr) %>% 
  mutate(across(starts_with("archetype_"), ~str_replace_all(., set_names(champ_codes$Name, champ_codes$X)))) %>% 
  select(-c(n, wins)) %>% 
  pivot_wider(names_from = archetype_2, values_from = a1_wr) %>%
  column_to_rownames(var = "archetype_1") %>%
  fill_matchup_table() %>%
  rownames_to_column(var = "archetype_1") %>% 
  pivot_longer(cols = -archetype_1, names_to = "archetype_2", values_to = "a1_wr") %>%
  mutate(bin = cut(a1_wr, c(0, 0.4, 0.45, 0.55, 0.6, 1), include.lowest = TRUE)) %>%
  mutate(across(c(archetype_1, archetype_2), ~factor(., levels = data_archetype_pr$archetype, ordered = TRUE))) %>%
  ggplot(aes(y = reorder(archetype_1, desc(archetype_1)), x = archetype_2)) +
  geom_tile(aes(fill = bin), color = "grey90", size = 1, stat = "identity") +
  shadowtext::geom_shadowtext(aes(label = scales::percent(a1_wr, accuracy = .1)), color = "white", size = 6, na.rm = TRUE) +
  theme_minimal(base_size = 15) +
  labs(x = element_blank(), y = element_blank()) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10), position = "top") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("[0,0.4]" = "#B81D13", "(0.4,0.45]" = "coral2", "(0.45,0.55]" = "#EFB700", "(0.55,0.6]" = "#149414", "(0.6,1]" = "#046507"), na.value = "grey90")

ggsave(filename = "./images/output/matchup_tbl.png", plot = p, width = 12, height = 8)

data_matchup1 <- data_matchup %>% 
  filter(archetype_1 != archetype_2) %>% 
  mutate(across(starts_with("archetype_"), ~str_replace_all(., set_names(champ_codes$Name, champ_codes$X)))) %>% 
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
  mutate(across(starts_with("archetype_"), function(x) ifelse(grepl("^( )", x), paste0("No Champions", x), x))) %>% 
  select(-tot_match) %>% 
  rename(player = archetype_1, opponent = archetype_2) %>% 
  rename_with(str_to_title) %>% 
  datatable(rownames = FALSE, options = list(pageLength = 10, lengthChange = FALSE)) %>% 
  formatPercentage(columns = "Winrate", digits = 1)

tbl$width  <- "100%"
tbl$height <- "500px"
tbl$sizingPolicy$browser$padding <- 10

saveWidget(tbl, "./images/output/matchup_tbl.html", background = "inherit")

# 6.7/6.8 meta score ----

data_score <- data %>% 
  select(match_id, game_outcome, archetype) %>%
  mutate(archetype = ifelse(archetype %in% top_10_pr, archetype, "Other")) %>% 
  arrange(match_id, archetype) %>% 
  group_by(match_id) %>%
  mutate(id = row_number()) %>% 
  mutate(winner = case_when(id == 1 & game_outcome == "win" ~ 1, id == 2 & game_outcome == "win" ~ 2, TRUE ~ 0)) %>% 
  pivot_wider(names_from = id, values_from = archetype, names_prefix = "archetype_") %>% 
  fill(starts_with("archetype_"), .direction = "updown") %>% 
  ungroup() %>% 
  filter(winner != 0) %>% 
  group_by(across(starts_with("archetype_"))) %>% 
  summarise(n = n(), wins = sum(winner == 1), .groups = "drop") %>% 
  mutate(across(starts_with("archetype_"), ~str_replace_all(., set_names(champ_codes$Name, champ_codes$X)))) %>% 
  mutate(a1_wr = ifelse(archetype_1 == archetype_2, 0.5, wins / n)) %>% 
  left_join(data_archetype_pr %>% select(archetype, playrate), by = c("archetype_2" = "archetype")) %>%
  group_by(archetype_1) %>% 
  mutate(playrate = ifelse(is.na(playrate), 1 - sum(playrate, na.rm = TRUE), playrate)) %>% 
  ungroup() %>% 
  replace_na(list(ai_wr = 0.5)) %>% # if i have 0 games of a particular matchup, assume its 50-50
  mutate(deck_power = a1_wr*playrate) %>% 
  group_by(archetype_1) %>% 
  summarise(deck_power = sum(deck_power), .groups = "drop") %>% 
  filter(archetype_1 != "Other") %>% 
  arrange(-deck_power) %>% 
  left_join(data_archetype_pr %>% select(archetype, playrate), by = c("archetype_1" = "archetype")) %>% 
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

ggsave(filename = "./images/output/meta_score1.png", plot = p, width = w+0.01, height = h+0.01)

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

ggsave(filename = "./images/output/meta_score2.png", plot = p, width = 12, height = 8)

# 6.9 best decklists ----

tbl <- data %>% 
  count(archetype, deck_code, game_outcome) %>% 
  group_by(deck_code) %>% 
  mutate(match = sum(n)) %>% 
  ungroup() %>% 
  filter(match >= 50) %>% 
  pivot_wider(names_from = game_outcome, values_from = n) %>% 
  mutate(across(where(is.numeric), function(x) ifelse(is.na(x), 0, x))) %>%
  mutate(winrate = win / match) %>% 
  arrange(-winrate) %>%
  mutate(archetype = str_replace_all(archetype, set_names(champ_codes$Name, champ_codes$X))) %>%
  mutate(archetype = ifelse(grepl("^( )", archetype), paste0("No Champions", archetype), archetype)) %>% 
  mutate(deck_code = sprintf('<a href="https://lor.runeterra.ar/decks/code/%s" target="_blank">%s</a>', deck_code, str_trunc(deck_code, width = 18))) %>%
  select(archetype, deck_code, match, winrate) %>% 
  rename_with(~str_replace_all(., pattern = "_", replacement = " ")) %>% 
  rename_with(str_to_title) %>% 
  datatable(rownames = FALSE, escape = FALSE, options = list(pageLength = 10, lengthChange = FALSE)) %>% 
  formatPercentage(columns = c("Winrate"), digits = 1)

tbl$width  <- "100%"
tbl$height <- "500px"
tbl$sizingPolicy$browser$padding <- 10

saveWidget(tbl, "./images/output/deck_codes.html", background = "inherit")

# tests ----

# most dedicated players

# data %>% 
#   count(puuid, archetype, game_outcome) %>%
#   group_by(puuid, archetype) %>% 
#   mutate(match = sum(n)) %>% 
#   ungroup() %>%
#   filter(match >= 30) %>% 
#   pivot_wider(names_from = game_outcome, values_from = n) %>% 
#   left_join(m_pl$find(), by = "puuid") %>% 
#   unite(col = player, gameName, tagLine, sep = "#") %>% 
#   mutate(winrate = win / match) %>% 
#   mutate(archetype = str_replace_all(archetype, set_names(champ_codes$Name, champ_codes$X))) %>%
#   select(archetype, player, match, winrate) %>% 
#   arrange(-match)