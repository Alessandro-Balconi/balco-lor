# cards info from data dragon

suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(googlesheets4)) # manage google sheets API
suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library(jsonlite))

# id of the spreadsheet 
ss_id <- "1eWTbWBmyYYO2Qwj_q0xEHgZTi3NvtL3oOfz8sxC92Hs"

# get most recent set number (to read sets JSONs)
last_set <- lorr::last_set()

# fetcjh info from set JSONs
data <- map_dfr(
  .x = 1:last_set,
  .f = function(x) {
    sprintf("https://dd.b.pvp.net/latest/set%1$s/en_us/data/set%1$s-en_us.json", x) %>% 
      GET() %>%  
      content(encoding = "UTF-8") %>% 
      fromJSON() %>% 
      as_tibble()
  },
  .id = "set"
)

# flatten list columns, using "-" as a separator; also arrange by cardcode
data <- data %>% 
  mutate(across(where(is.list), map_chr, str_flatten, collapse = ' - ')) %>% 
  arrange(cardCode)

# keep only relevant info
data <- data %>% 
  select(
    cardCode, 
    name, 
    set,
    regions,
    type,
    rarityRef,
    collectible,
    cost,
    attack,
    health,
    spellSpeed,
    subtypes,
    supertype,
    keywords,
    associatedCardRefs,
    assets,
    descriptionRaw,
    levelupDescriptionRaw,
    flavorText,
    artistName
  )

# update spreadsheet
with_gs4_quiet(sheet_write(data = data, ss = ss_id, sheet = "Cards Info"))

# additional information
update <- sprintf("Last update: %s UTC", Sys.time())
info <- tibble(" " = c(update))
with_gs4_quiet(sheet_write(data = info,   ss = ss_id, sheet = "Data Information"))

# names of the spreadsheet to update
ss_names <- sheet_names(ss_id)

# adjust spacing of columns in the spreadsheet
map(
  .x = ss_names,
  .f = ~with_gs4_quiet(range_autofit(ss = ss_id, sheet = ., dimension = "columns"))
)
