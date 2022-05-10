library(tidyverse)
library(lubridate)

# parameter: rank to analyze
focus_rank<- 700

# load mysql db credentials
db_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# create connection to MySQL database
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  db_host  = "127.0.0.1",
  user     = db_creds$uid,
  password = db_creds$pwd,
  dbname   = db_creds$dbs
)

# remove credentials
rm(db_creds)

x <- tbl(con, 'leaderboard_daily') %>% 
  filter(rank == local(focus_rank)) %>% 
  select(day, region, lp) %>% 
  collect()

season_start = ymd('2022-02-18')
season_end = ymd('2022-05-12') # ranked cutoff day

x = x %>% 
  filter(day >= season_start)


make_forecast <- function(df, ts_col, h){
  
  mdl <- forecast::auto.arima(df[ts_col])
  
  fcst <- forecast::forecast(mdl, h = h, level = 95)
  
  res <- tibble(
    day = seq.Date(from = max(df$day)+1, to = max(df$day)+h, by = 'day'),
    mean = as.numeric(fcst$mean),
    low = as.numeric(fcst$lower),
    high = as.numeric(fcst$upper)
  )
  
}

xx = x %>% 
  arrange(region, day) %>% 
  split(.$region) %>% 
  map_dfr(.f = make_forecast, ts_col = 'lp', h = as.numeric(season_end - Sys.Date())+1, .id = 'region')

x %>% 
  bind_rows(xx) %>% 
  ggplot(aes(x = day, color = region)) +
  geom_line(aes(y = lp), size = 2, na.rm = TRUE) +
  geom_point(aes(y = lp), size = 4, na.rm = TRUE) +
  geom_line(aes(y = mean), size = 1.5, linetype = 'dotdash', na.rm = TRUE) +
  geom_ribbon(aes(ymin = low, ymax = high, fill = region), alpha = 0.2, na.rm = TRUE, size = 0, show.legend = FALSE) +
  geom_vline(xintercept = season_end, color = 'red', linetype = 'dashed') +
  geom_label(y = 0, x = season_end, label = 'Ranked \nCutoff', show.legend = FALSE, color = 'red', size = 6) +
  geom_label(aes(label = round(mean, digits = 0), y = mean), x = season_end, show.legend = FALSE, na.rm = TRUE, fontface = 'bold', size = 8, data = xx %>% filter(day == season_end)) +
  theme_bw(base_size = 22) +
  theme(legend.position = 'bottom') +
  expand_limits(x = season_end+1) +
  scale_color_discrete(labels = c('asia' = 'APAC', 'americas' = 'Americas', 'europe' = 'Europe')) +
  labs(x = 'Day', y = 'LP', title = sprintf('Rank %s Master LP by Day', focus_rank), color = 'Region')
