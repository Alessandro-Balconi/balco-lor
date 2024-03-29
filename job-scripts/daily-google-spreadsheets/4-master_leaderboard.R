# daily leaderboard for hogwarts

suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(googlesheets4)) # manage google sheets API

# create connection to MySQL database
con <- lorr::create_db_con()

# id of the spreadsheet 
ss_id <- "1srxnNQ-cSM3npsGgYd9eCyIujMVQ1A5xrvOB5FScF2s"

# days with at least 100 master players
days <- tbl(con, 'leaderboard_daily') %>% 
  filter(
    region == 'europe', 
    rank == 50, 
    day >= local(Sys.Date()-lubridate::days(180))
  ) %>%
  select(day) %>% 
  collect()

# day of the season start
season_start <- days %>% 
  mutate(has_100 = 1) %>% 
  complete(day = seq.Date(
    from = min(days$day), 
    to = Sys.Date()-1, 
    by = 'day'
  )) %>%
  filter(is.na(has_100)) %>% 
  mutate(days_diff = (day - lag(day)) %>% as.numeric()) %>% 
  filter(days_diff != 1) %>% 
  mutate(day = day + lubridate::days(1)) %>% # add 1 day just to be safe
  slice_max(n = 1, order_by = day, with_ties = FALSE) %>% 
  pull(day)

# update daily pages of the region
update_sheet_region <- function(input_region){
  
  # fetch data for this season' masters
  data <- tbl(con, 'leaderboard_daily') %>% 
    filter(region == input_region, day >= season_start) %>% 
    select(name, rank, lp, day) %>% 
    collect()
  
  # today's ranks
  today_ranks <- data %>%
    filter(day == max(day)) %>% 
    select(name, rank)
  
  # master join date
  join_date <- data %>% 
    with_groups(.groups = name, .f = summarise, join_date = min(day))
  
  # main df with daily lps
  daily_lps <- data %>%
    with_groups(.groups = c(day, name), .f = summarise, lp = max(lp)) %>% 
    mutate(lp = scales::comma(lp, accuracy = 1)) %>% 
    pivot_wider(names_from = day, values_from = lp, values_fill = '')
  
  # merge all dfs with infos
  df <- today_ranks %>% 
    left_join(join_date, by = 'name') %>% 
    left_join(daily_lps, by = 'name')
  
  # same but with lps
  df_lps <- data %>% 
    select(rank, lp, day) %>% 
    mutate(lp = scales::comma(lp, accuracy = 1)) %>% 
    pivot_wider(names_from = day, values_from = lp, values_fill = '')
  
  # name of the region but nicer
  nice_region <- switch(
    input_region,
    'europe' = 'Europe',
    'americas' = 'Americas',
    'asia' = 'APAC'
  )
  
  # update all sheets of the spreadsheet
  sheet_write(
    data = df, 
    ss = ss_id, 
    sheet = sprintf('%s - Players', nice_region)
  )
  
  sheet_write(
    data = df_lps, 
    ss = ss_id, 
    sheet = sprintf('%s - LP', nice_region)
  )
  
}

update_sheet_region('europe')
update_sheet_region('americas')
update_sheet_region('asia')

# additional information
info <- tibble(
  " " = c(
    sprintf("Last update: %s UTC", Sys.time()),
    "The daily snapshots are taken at the following hours:",
    "Europe: 00:00 UTC (2:00 CEST)",
    "Americas: 08:00 UTC (1:00 PDT)",
    "APAC: 16:00 UTC (22:00 CST)"
  )
)

# update data information sheet
sheet_write(data = info, ss = ss_id, sheet = "Data Information")

# names of the spreadsheet to update
ss_names <- sheet_names(ss_id)

# adjust spacing of columns in the spreadsheet
walk(.x = ss_names, .f = ~range_autofit(ss = ss_id, sheet = .))

DBI::dbDisconnect(con)
