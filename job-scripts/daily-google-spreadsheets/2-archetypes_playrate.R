suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(googlesheets4)) # manage google sheets API
suppressPackageStartupMessages(library(lubridate)) # working with dates

# load mysql db credentials
db_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# create connection to MySQL database
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  db_host = "127.0.0.1",
  user = db_creds$uid,
  password = db_creds$pwd,
  dbname = db_creds$dbs
)

# patches to analyze
patches <- tbl(con, "utils_patch_history") %>% 
  collect() %>% 
  arrange(desc(release_date)) %>% 
  mutate(new_change = lag(change)) %>% 
  replace_na(list(new_change = 0)) %>% 
  mutate(cum_change = cumsum(new_change)) %>% 
  filter(cum_change == min(cum_change)) %>%
  collect() %>% 
  pull(patch)

# functions ----

# get most 50 played archetypes
get_top_played_decks <- function(patches, start_date = '2000-01-01', only_master = FALSE, n_top = 50){
  
  top = tbl(con, 'ranked_daily_archetypes') %>% 
    filter(patch %in% patches) %>%
    {if (only_master) filter(., is_master == 1) else . } %>% 
    filter(day >= local(ymd(start_date))) %>% 
    left_join(tbl(con, 'utils_archetype_aggregation'), by = c('archetype' = 'old_name')) %>% 
    mutate(archetype = coalesce(new_name, archetype)) %>% 
    group_by(archetype) %>% 
    summarise(across(c(match),  sum, na.rm = TRUE), .groups = 'drop') %>% 
    slice_max(n = n_top, order_by = match, with_ties = FALSE) %>% 
    collect() %>% 
    pull(archetype)
  
  return(top)
  
}

# number of daily games by region
daily_region_games <- function(patches, start_date = '2000-01-01', only_master = FALSE){
  
  ngames = tbl(con, 'ranked_daily_archetypes') %>% 
    filter(patch %in% patches) %>% 
    {if (only_master) filter(., is_master == 1) else . } %>% 
    filter(day >= local(ymd(start_date))) %>% 
    group_by(region, day) %>% 
    summarise(tot_match = sum(match, na.rm = TRUE), .groups = 'drop') %>% 
    collect()
  
  return(ngames)
}

# daily data for top archetypes by region
get_daily_data <- function(patches, archetypes, start_date = '2000-01-01', only_master = FALSE){
  
  df = tbl(con, 'ranked_daily_archetypes') %>% 
    {if (only_master) filter(., is_master == 1) else . } %>% 
    filter(patch %in% patches, day >= local(ymd(start_date))) %>% 
    left_join(tbl(con, 'utils_archetype_aggregation'), by = c('archetype' = 'old_name')) %>% 
    mutate(archetype = coalesce(new_name, archetype)) %>% 
    filter(archetype %in% archetypes) %>% 
    group_by(archetype, day, region) %>% 
    summarise(across(c(match, win),  sum, na.rm = TRUE), .groups = 'drop') %>% 
    collect()
  
  return(df)
  
}

# create spreadsheet page with info needed
create_spreadsheet_page <- function(patches, start_date = '2000-01-01', only_master = FALSE, n_top = 50){
  
  #most played archetypes
  top50 = get_top_played_decks(patches = patches, start_date = start_date, only_master = only_master, n_top = n_top)
  
  # number of decks by shard, day
  daily_region_ngames = daily_region_games(patches = patches, start_date = start_date, only_master = only_master)
  
  # get daily data and add number of daily decks analyzed
  df = get_daily_data(patches = patches, archetypes = top50, start_date = start_date, only_master = only_master) %>% 
    left_join(daily_region_ngames, by = c('day', 'region'))
  
  # region numbers
  df_patch_region = df %>% 
    group_by(archetype, region) %>% 
    summarise(across(where(is.numeric), sum), .groups = 'drop') %>% 
    mutate(pr = match / tot_match, wr = win / match) %>%
    select(archetype, region, match, pr, wr)
  
  # calculate total numbers and join with region numbers
  df = df %>% 
    group_by(archetype) %>% 
    summarise(across(where(is.numeric), sum), .groups = 'drop') %>% 
    mutate(all = match / tot_match) %>% 
    mutate(pr = match / tot_match, wr = win / match, region = 'all') %>%
    select(archetype, region, match, pr, wr) %>% 
    arrange(desc(match)) %>% 
    bind_rows(df_patch_region) %>% 
    pivot_wider(names_from = region, values_from = c(match, pr, wr), values_fill = 0)
  
  # prettify numbers
  df = df %>% 
    mutate(
      across(starts_with('match_'), scales::comma, accuracy = 1),
      across(c(starts_with('pr_'), starts_with('wr_')), scales::percent, accuracy = .1)
    )
  
  #return result
  return(df)
  
}

# create pages ----

# date of 7 days ago
last_week = as.character(Sys.Date()-days(7))

# plat+, all patch
df1 = create_spreadsheet_page(patches = patches, only_master = FALSE)
df2 = create_spreadsheet_page(patches = patches, only_master = FALSE, start_date = last_week)
df3 = create_spreadsheet_page(patches = patches, only_master = TRUE )
df4 = create_spreadsheet_page(patches = patches, only_master = TRUE,  start_date = last_week)

# edit spreadsheet ----

# additional information
update <- sprintf("Last update: %s UTC", Sys.time())
patches_data <- sprintf("Patch Analyzed: %s", paste0(patches, collapse = ', '))

info <- tibble(
  " " = c(update, patches_data)
)

# id of the spreadsheet 
ss_id <- "1itUlBIY0gz0kZUnl2zBVz0x-fAjQrvTfCcDqxPLFURQ"

# update all sheets of the spreadsheet
with_gs4_quiet(sheet_write(data = info, ss = ss_id, sheet = "Data Information"      ))
with_gs4_quiet(sheet_write(data = df1,  ss = ss_id, sheet = "Plat+ - Current Patch" ))
with_gs4_quiet(sheet_write(data = df2,  ss = ss_id, sheet = "Plat+ - Last 7 Days"   ))
with_gs4_quiet(sheet_write(data = df3,  ss = ss_id, sheet = "Master - Current Patch"))
with_gs4_quiet(sheet_write(data = df4,  ss = ss_id, sheet = "Master - Last 7 Days"  ))

# names of the spreadsheet to update
ss_names <- sheet_names(ss_id)

# adjust spacing of columns in the spreadsheet
map(
  .x = ss_names,
  .f = ~with_gs4_quiet(range_autofit(ss = ss_id, sheet = ., dimension = "columns"))
)

DBI::dbDisconnect(con)
