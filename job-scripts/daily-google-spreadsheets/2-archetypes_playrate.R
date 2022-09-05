suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(googlesheets4)) # manage google sheets API
suppressPackageStartupMessages(library(lubridate)) # working with dates

# create connection to MySQL database
con <- lorr::create_db_con()

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
get_top_played_decks <- function(patches, time_frame = 0, only_master = FALSE, n_top = 50){
  
  top = tbl(con, 'ranked_patch_archetypes') %>% 
    {if (only_master) filter(., is_master == 1) else . } %>% 
    filter(time_frame >= local(time_frame)) %>% 
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
daily_region_games <- function(patches, time_frame = 0, only_master = FALSE){
  
  ngames = tbl(con, 'ranked_patch_archetypes') %>% 
    {if (only_master) filter(., is_master == 1) else . } %>% 
    filter(time_frame >= local(time_frame)) %>% 
    group_by(region) %>% 
    summarise(tot_match = sum(match, na.rm = TRUE), .groups = 'drop') %>% 
    collect()
  
  return(ngames)
}

# daily data for top archetypes by region
get_daily_data <- function(patches, archetypes, time_frame = 0, only_master = FALSE){
  
  df = tbl(con, 'ranked_patch_archetypes') %>% 
    {if (only_master) filter(., is_master == 1) else . } %>% 
    filter(time_frame >= local(time_frame)) %>% 
    left_join(tbl(con, 'utils_archetype_aggregation'), by = c('archetype' = 'old_name')) %>% 
    mutate(archetype = coalesce(new_name, archetype)) %>% 
    filter(archetype %in% archetypes) %>% 
    group_by(archetype, region) %>% 
    summarise(across(c(match, win),  sum, na.rm = TRUE), .groups = 'drop') %>% 
    collect()
  
  return(df)
  
}

# create spreadsheet page with info needed
create_spreadsheet_page <- function(patches, time_frame = 0, only_master = FALSE, n_top = 50){
  
  #most played archetypes
  top50 = get_top_played_decks(patches = patches, time_frame = time_frame, only_master = only_master, n_top = n_top)
  
  # number of decks by shard, day
  daily_region_ngames = daily_region_games(patches = patches, time_frame = time_frame, only_master = only_master)
  
  # get daily data and add number of daily decks analyzed
  df = get_daily_data(patches = patches, archetypes = top50, time_frame = time_frame, only_master = only_master) %>% 
    left_join(daily_region_ngames, by = c('region'))
  
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
    {if(nrow(.)> 0) pivot_wider(., names_from = region, values_from = c(match, pr, wr), values_fill = 0) else .}
  
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

# plat+, all patch
df1 = create_spreadsheet_page(patches = patches, only_master = FALSE)
df2 = create_spreadsheet_page(patches = patches, only_master = FALSE, time_frame = 1)
df3 = create_spreadsheet_page(patches = patches, only_master = TRUE )
df4 = create_spreadsheet_page(patches = patches, only_master = TRUE,  time_frame = 1)

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
sheet_write(data = info, ss = ss_id, sheet = "Data Information"      )
sheet_write(data = df1,  ss = ss_id, sheet = "Plat+ - Current Patch" )
sheet_write(data = df2,  ss = ss_id, sheet = "Plat+ - Last 7 Days"   )
sheet_write(data = df3,  ss = ss_id, sheet = "Master - Current Patch")
sheet_write(data = df4,  ss = ss_id, sheet = "Master - Last 7 Days"  )

# names of the spreadsheet to update
ss_names <- sheet_names(ss_id)

# adjust spacing of columns in the spreadsheet
walk(.x = ss_names, .f = ~range_autofit(ss = ss_id, sheet = .))

DBI::dbDisconnect(con)
