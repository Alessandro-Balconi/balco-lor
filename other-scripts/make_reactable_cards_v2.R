# reactable cards

library(tidyverse)
library(reactable)

i_patch     <- 'live_3_06'
i_archetype <- 'Gnar Ziggs (BC NX)'
i_min_rank  <- 2 #minimum rank of the player

x <- tbl(con, 'ranked_match_metadata_30d') %>% 
  filter(str_detect(game_version, local(i_patch))) %>% 
  left_join(tbl(con, 'ranked_match_info_30d'), by = 'match_id') %>% 
  filter(player_rank >= local(i_min_rank), archetype == local(i_archetype)) %>% 
  count(cards, game_outcome) %>% 
  collect() %>% 
  ungroup() %>% 
  mutate(n = as.numeric(n))

x <- x %>% 
  pivot_wider(names_from = game_outcome, values_from = n, values_fill = 0) %>% 
  {if('tie' %in% colnames(.)) . else mutate(., tie = 0)} %>% 
  mutate(n = win + loss + tie) %>% 
  arrange(-n)

xx = x %>% 
  separate(col = cards, into = sprintf('card_%s', 1:40), sep = " ", fill = 'right') %>% 
  pivot_longer(cols = starts_with('card_'), values_drop_na = TRUE, names_to = character()) %>% 
  separate(col = value, into = c("count", "card"), sep = ":") %>% 
  group_by(card, count) %>% # summarise relevant info
  summarise(across(c(n, win), sum, na.rm = TRUE), .groups = 'drop')

xx <- xx %>% 
  mutate(winrate = win / n) %>%
  select(card, count, winrate, match = n) %>% 
  pivot_wider(names_from = count, values_from = c(winrate, match)) %>% # reshape and fill NAs with zeros
  mutate(across(starts_with("match_"), replace_na, 0))

xx <- xx %>% 
  rowwise() %>% 
  mutate(match_overall = sum(c_across(starts_with("match_")))) %>% 
  ungroup() %>% 
  mutate(match_0 = max(match_overall) - match_overall) %>% 
  mutate(across(starts_with("match_"), function(x) x / max(.$match_overall)))

# get most recent set number (to read sets JSONs)
last_set <- "https://dd.b.pvp.net/latest/core/en_us/data/globals-en_us.json" %>% 
  httr::GET() %>% 
  httr::content(encoding = "UTF-8") %>% 
  jsonlite::fromJSON() %>% 
  .[["sets"]] %>% 
  mutate(set = str_extract(nameRef, pattern = "[0-9]+")) %>% 
  mutate(set = as.numeric(set)) %>% 
  summarise(max(set, na.rm = TRUE)) %>% 
  pull()

# cards names / codes / rarity from set JSONs
data_cards <- map_dfr(
  .x = 1:last_set,
  .f = function(x) {
    sprintf("https://dd.b.pvp.net/latest/set%1$s/en_us/data/set%1$s-en_us.json", x) %>% 
      httr::GET() %>%  
      httr::content(encoding = "UTF-8") %>% 
      jsonlite::fromJSON() %>% 
      as_tibble()
  },
  .id = "set"
) %>% 
  select(name, cardCode, rarity) %>%
  filter(nchar(cardCode) <= 8)

xx <- xx %>% 
  left_join(data_cards, by = c("card" = "cardCode")) %>%
  mutate(rarity = ifelse(rarity == 'Champion', 1, 0)) %>% 
  filter(match_overall >= 0.05*max(match_overall) | rarity == 1) %>%
  mutate(name = coalesce(name, card)) %>% 
  select(-card) %>% 
  arrange(-rarity, -match_overall, -match_3, -match_2, -match_1) %>% 
  relocate(name, match_overall, match_0, match_1, match_2, match_3, winrate_1, winrate_2, winrate_3)

tot_ngames <- x %>%
  summarise(n = sum(n, na.rm = TRUE), .groups = 'drop') %>% 
  mutate(n = scales::comma(n, accuracy = 1)) %>% 
  pull(n)

tot_wr <- x %>%
  summarise(across(c(n, win), sum, na.rm = TRUE), .groups = 'drop') %>% 
  mutate(wr = win / n) %>% 
  pull(wr)

# add "winrate_0" column (winrate when the card is not played)
xx <- xx %>%
  rowwise() %>%
  mutate(winrate_pl = sum(match_1*winrate_1, match_2*winrate_2, match_3*winrate_3, na.rm = TRUE) / match_overall) %>%
  ungroup() %>%
  mutate(winrate_0 = ifelse(match_0>0.0001, (tot_wr - winrate_pl*match_overall) / match_0, NA_real_)) %>% 
  relocate(winrate_0, .before = winrate_1)

row_style = htmlwidgets::JS("
          function(rowInfo, state) {
          // Ignore padding rows
          if (!rowInfo) return

          // Add horizontal separators between groups when sorting by school
          var firstSorted = state.sorted[0]
          if (firstSorted && firstSorted.id === 'rarity') {
            var nextRow = state.pageRows[rowInfo.viewIndex + 1]
            if (nextRow && rowInfo.row['rarity'] !== nextRow['rarity']) {
              // Use box-shadow to add a 2px border without taking extra space
              return { boxShadow: 'inset 0 -3px 0 rgba(0, 0, 0, 0.2)' }
            }
          }
        }
      ")

# color palettes used in the table
col_pal <- function(x, col_1 = "#FFFFFF", col_2 = "#000000") rgb(colorRamp(c(col_1, col_2))(x), maxColorValue = 255)

# make table nicer
xx %>% 
  reactable::reactable(
    columns = list(
      rarity = colDef(show = FALSE),
      name = colDef(name = "", minWidth = 200, style = list(fontWeight = "bold", borderRight = "1px solid rgba(0, 0, 0, 0.55)")),
      match_overall = colDef(name = "Overall", defaultSortOrder = "desc", format = colFormat(percent = TRUE, digits = 1), align = "center",
                             style = function(value) {
                               color <- col_pal(value, col_1 = "#fcfcff", col_2 = "#9bc2e6")
                               list(background = color)
                             }),
      match_0 = colDef(name = "0", defaultSortOrder = "desc", format = colFormat(percent = TRUE, digits = 1), align = "center",
                       style = function(value) {
                         color <- col_pal(value, col_1 = "#fcfcff", col_2 = "#9bc2e6")
                         list(background = color)
                       }),
      match_1 = colDef(name = "1", defaultSortOrder = "desc", format = colFormat(percent = TRUE, digits = 1), align = "center",
                       style = function(value) {
                         color <- col_pal(value, col_1 = "#fcfcff", col_2 = "#9bc2e6")
                         list(background = color)
                       }),
      match_2 = colDef(name = "2", defaultSortOrder = "desc", format = colFormat(percent = TRUE, digits = 1), align = "center",
                       style = function(value) {
                         color <- col_pal(value, col_1 = "#fcfcff", col_2 = "#9bc2e6")
                         list(background = color)
                       }),
      match_3 = colDef(name = "3", defaultSortOrder = "desc", format = colFormat(percent = TRUE, digits = 1), align = "center",
                       style = function(value) {
                         color <- col_pal(value, col_1 = "#fcfcff", col_2 = "#9bc2e6")
                         list(background = color, borderRight = "1px solid rgba(0, 0, 0, 0.5)")
                       }),
      winrate_0 = colDef(name = "0", defaultSortOrder = "desc", format = colFormat(percent = TRUE, digits = 1), align = "center",
                         style = function(value, index){
                           txt_col <- if(xx$match_0[index] < 0.1){ "#b5b5b5" } else { "#000000" }
                           bg_col <- if(xx$match_0[index] < 0.1){
                             "#FFFFFF"
                           } else if(value > tot_wr){
                             x <- min((value - tot_wr)*33, 1)
                             col_pal(x, col_1 = "#FFFFFF", col_2 = "#7ec992")
                           } else{
                             x <- min((tot_wr - value)*33, 1)
                             col_pal(x, col_1 = "#FFFFFF", col_2 = "#f8696b")
                           }
                           list(color = txt_col, background = bg_col)
                         }),
      winrate_1 = colDef(name = "1", defaultSortOrder = "desc", format = colFormat(percent = TRUE, digits = 1), align = "center",
                         style = function(value, index){
                           txt_col <- if(xx$match_1[index] < 0.1){ "#b5b5b5" } else { "#000000" }
                           bg_col <- if(xx$match_1[index] < 0.1){
                             "#FFFFFF"
                           } else if(value > tot_wr){
                             x <- min((value - tot_wr)*33, 1)
                             col_pal(x, col_1 = "#FFFFFF", col_2 = "#7ec992")
                           } else{
                             x <- min((tot_wr - value)*33, 1)
                             col_pal(x, col_1 = "#FFFFFF", col_2 = "#f8696b")
                           }
                           list(color = txt_col, background = bg_col)
                         }),
      winrate_2 = colDef(name = "2", defaultSortOrder = "desc", format = colFormat(percent = TRUE, digits = 1), align = "center",
                         style = function(value, index){
                           txt_col <- if(xx$match_2[index] < 0.1){ "#b5b5b5" } else { "#000000" }
                           bg_col <- if(xx$match_2[index] < 0.1){
                             "#FFFFFF"
                           } else if(value > tot_wr){
                             x <- min((value - tot_wr)*33, 1)
                             col_pal(x, col_1 = "#FFFFFF", col_2 = "#7ec992")
                           } else{
                             x <- min((tot_wr - value)*33, 1)
                             col_pal(x, col_1 = "#FFFFFF", col_2 = "#f8696b")
                           }
                           list(color = txt_col, background = bg_col)
                         }),
      winrate_3 = colDef(name = "3", defaultSortOrder = "desc", format = colFormat(percent = TRUE, digits = 1), align = "center",
                         style = function(value, index){
                           txt_col <- if(xx$match_3[index] < 0.1){ "#b5b5b5" } else { "#000000" }
                           bg_col <- if(xx$match_3[index] < 0.1){
                             "#FFFFFF"
                           } else if(value > tot_wr){
                             x <- min((value - tot_wr)*50, 1)
                             col_pal(x, col_1 = "#FFFFFF", col_2 = "#7ec992")
                           } else{
                             x <- min((tot_wr - value)*50, 1)
                             col_pal(x, col_1 = "#FFFFFF", col_2 = "#f8696b")
                           }
                           list(color = txt_col, background = bg_col)
                         }),
      winrate_pl = colDef(show = FALSE)
    ),
    columnGroups = list(
      colGroup(name = i_archetype, columns = "name"),
      colGroup(name = sprintf("Playrate (N: %s)", tot_ngames), columns = c("match_overall", "match_0", "match_1", "match_2", "match_3")),
      colGroup(name = sprintf("Winrate (Avg.: %s)", scales::percent(tot_wr, accuracy = .1)), columns = c("winrate_1", "winrate_2", "winrate_3"))
    ),
    defaultPageSize = 50,
    bordered = TRUE,
    highlight = TRUE,
    sortable = FALSE,
    defaultSorted = list(rarity = 'desc', match_overall = 'desc', match_3 = 'desc', match_2 = 'desc', match_1 = 'desc'),
    rowStyle = row_style
  )
