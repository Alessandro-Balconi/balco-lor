# AGGIUGERE CHECK SU TABLE OLD VS TABLE NONOLD
library(tidyverse)
library(lubridate)

# filter master only? or all players
masters_only = TRUE

# remove bandle from plots (it was with the old limit & master logic)
no_bandle = TRUE

# calculate number of matches played by day
match_by_day <- function(name, start_date, end_date, masters_only){
  
  df1 = tibble(ts = ymd(), n = bit64::as.integer64())
  df2 = tibble(ts = ymd(), n = bit64::as.integer64())
  
  # collect number of matches by day between start_date and end_date
  if(ymd(start_date) <= Sys.Date()-days(30)){
    
    df1 = tbl(con, 'ranked_match_metadata') %>% 
      filter(game_start_time_utc >= local(ymd(start_date)) & game_start_time_utc <= local(ymd(end_date))) %>%
      left_join(tbl(con, 'ranked_match_info'), by = 'match_id') %>% 
      { if(masters_only) filter(., player_rank == 2) else . } %>% 
      mutate(ts = sql('CAST(game_start_time_utc AS DATE)')) %>% 
      distinct(ts, match_id) %>% 
      count(ts) %>%
      arrange(ts) %>% 
      collect()
    
  }
  
  if(ymd(end_date) >= Sys.Date()-days(40)){
    
    df2 = tbl(con, 'lor_match_info_v2') %>% 
      { if(masters_only) filter(., is_master == 1) else . } %>% 
      mutate(ts = sql('CAST(game_start_time_utc AS DATE)')) %>% 
      filter(ts >= local(ymd(start_date)) & ts <= local(ymd(end_date))) %>%
      distinct(ts, match_id) %>% 
      count(ts) %>%
      arrange(ts) %>% 
      collect()
    
  }
  
  # add season name, row number (day of the season)
  df = bind_rows(df1, df2) %>% 
    group_by(ts) %>% 
    summarise(n = sum(n), .groups = "drop") %>% 
    mutate(season = name, day = row_number())
  
  # return df
  return(df)
  
}

# collect data for all seasons
today = as.character(Sys.Date()-lubridate::days(1))

if(!no_bandle) { s1 = match_by_day(name = 'Beyond the Bandlewood', start_date = '2021-08-27', end_date = '2021-10-19', masters_only = masters_only) }
s2 = match_by_day(name = 'Between Worlds',      start_date = '2021-10-21', end_date = '2021-12-07', masters_only = masters_only)
s3 = match_by_day(name = 'Magic Misadventures', start_date = '2021-12-09', end_date = '2022-02-15', masters_only = masters_only)
s4 = match_by_day(name = 'A Curious Journey',   start_date = '2022-02-17', end_date = today,        masters_only = masters_only)

# bind rows and fix column class
z = { if(!no_bandle) bind_rows(s1, s2, s3, s4) else bind_rows(s2, s3, s4) } %>% mutate(n = as.numeric(n))

# make plot
ggplot(z, aes(x = day, y = n, color = season)) +
  geom_line(size = 1.5) +
  geom_point(alpha = 0.3, size = 3) +
  expand_limits(y = 0) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma_format()) +
  theme(legend.position = 'bottom') +
  labs(
    x = 'Day of the Season', 
    y = '# of Matches', 
    title = 'Matches collected by day of the season', 
    color = 'Season',
    subtitle = { if(masters_only) 'Master data' else 'Plat+ data'}
  )
