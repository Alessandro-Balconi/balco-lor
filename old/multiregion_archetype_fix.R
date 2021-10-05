# update all older archetypes on mysql

suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(mongolite)) # connect to MongoDB
suppressPackageStartupMessages(library(jsonlite))  # convert JSON to R objects
suppressPackageStartupMessages(library(httr))      # http requests

# import python deck decoder
lor_deckcodes <- reticulate::import("lor_deckcodes")

# load db credentials
mysql_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# extract region from monoregion champs
get_monoregion <- function(champs){
  
  # split champions
  champs <- str_split(champs, pattern = " ") %>% unlist()
  
  # get monoregions
  data_champs %>% 
    filter(cardCode %in% champs) %>% 
    mutate(n_regions = map_int(regionRefs, length)) %>% 
    filter(n_regions == 1) %>% 
    pull(cardCode) %>%
    paste0(collapse = " ") %>% 
    str_remove_all(pattern = "[0-9]")
  
}

# close previous connections to MySQL database (if any)
if(exists("con")){ DBI::dbDisconnect(con) }

# create connection to MySQL database
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  db_host = "127.0.0.1",
  user = mysql_creds$uid,
  password = mysql_creds$pwd,
  dbname = "db_prova"
)

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

# champions names / codes / regions from set JSONs
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
  select(name, cardCode, regionRefs) %>%
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

data_eu <- tbl(con, "lor_match_info_asia") %>% 
  collect()

data_neu <- data_eu %>%
  distinct(champs) %>% 
  mutate(champs_factions = map_chr(champs, get_monoregion)) %>% 
  left_join(data_eu, ., by = "champs") %>% 
  left_join(data_regions %>% select(faction_abb1 = abbreviation, nameRef), by = c("faction_1" = "nameRef")) %>% 
  left_join(data_regions %>% select(faction_abb2 = abbreviation, nameRef), by = c("faction_2" = "nameRef")) %>%
  unite(col = factions, faction_abb1, faction_abb2, sep = " ") %>% 
  mutate(
    factions = str_remove_all(factions, pattern = " NA|NA "),
    across(c(champs, champs_factions, factions),  function(x) unname(sapply(x, function(x) { paste(sort(trimws(strsplit(x[1], ' ')[[1]])), collapse=' ')} ))),
    no_fix = map2_lgl(.x = factions, .y = champs_factions, .f = ~grepl(pattern = .x, x = .y)),
    champs_factions = str_replace_all(champs_factions, pattern = " ", replacement = "|"),
    champs_factions = paste0(champs_factions, "| "),
    factions_to_add = str_remove_all(factions, pattern = champs_factions),
    archetype = if_else(no_fix, champs, sprintf("%s (%s)", champs, factions_to_add))
  ) %>% 
  select(-c(champs_factions, factions, no_fix, factions_to_add))
  
# make archetype name nicer
data_neu <- data_neu %>% 
  mutate(archetype = str_replace_all(archetype, set_names(data_champs$name, data_champs$cardCode))) %>% 
  mutate(across(archetype, function(x) ifelse(grepl("^( )", x), paste0("No Champions", x), x))) 

identical(data_eu %>% select(-archetype), data_neu %>% select(-archetype)) # must be TRUE
identical(data_eu , data_neu) # must be FALSE

tibble(old = data_eu$archetype, new = data_neu$archetype) %>% 
  mutate(change = ifelse(old == new, "no" , "yes")) %>% 
  count(change)

tibble(old = data_eu$archetype, new = data_neu$archetype) %>% 
  distinct() %>% 
  mutate(change = ifelse(old == new, "no" , "yes")) %>% 
  filter(change == "yes")

data_neu %>% 
  DBI::dbWriteTable(conn = con, name = "lor_match_info_asia", value = ., overwrite = TRUE, row.names = FALSE) 
