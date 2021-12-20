# import libraries
library(tidyverse)
library(lubridate)

# create a nice date from  date object
nice_date <- function(date, short_month = TRUE){
  
  paste(day(date), month(date, label = TRUE, abbr = short_month), year(date), sep = " ")
  
}

# same as scales::comma, but has a nicer format
scales_comma_plus <- function(x, accuracy = 1){
  
  sapply(
    X = x,
    FUN = function(x) case_when(
      is.na(x) ~ paste0("‼ NEW"),
      x==0 ~ paste0("↔ ", scales::comma(x, accuracy = accuracy)),
      x>0 ~ paste0("↑ +", scales::comma(x, accuracy = accuracy)),
      x<0 ~ paste0("↓ ", scales::comma(x, accuracy = accuracy)),
      TRUE ~ NA_character_ # should never happen; just an additional check
    ),
    USE.NAMES = FALSE
  )
  
}

# load db credentials
mysql_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# create connection to MySQL database
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  db_host = "127.0.0.1",
  user = mysql_creds$uid,
  password = mysql_creds$pwd,
  dbname = mysql_creds$dbs
)

# check today's top 10 players and how they were doing yesteday
new <- tbl(con, 'leaderboard_eu') %>% filter(rank <= 10) %>% collect()
old <- tbl(con, 'leaderboard_eu_daily') %>% filter(name %in% local(new$name)) %>% collect()

# remove duplicate names 
old <- old %>% 
  group_by(name) %>% 
  slice_min(n = 1, order_by = rank, with_ties = FALSE) %>% 
  ungroup()

# join and calculate changes
diff <- left_join(new, old, by = c('name')) %>% 
  mutate(rank_gain = rank.y - rank.x, lp_gain = lp.x - lp.y)

# make plot
p <- ggplot(diff, aes(x = reorder(name, lp.x))) +
  geom_col(aes(y = lp.x), fill = "#13294b", color = "steelblue", alpha = 1) +
  geom_text(aes(label = "", y = 1.1*lp.x), size = 6) +
  geom_text(aes(label = scales::comma(lp.x, accuracy = 1), y = lp.x), size = 6, hjust = -0.5, vjust = -0.5) +
  geom_text(
    size = 5, hjust = -0.375, vjust = 1.5, 
    aes(label = scales_comma_plus(lp_gain, accuracy = 1), y = lp.x,
        color = case_when(is.na(lp_gain) ~ "na", lp_gain==0 ~ "same", lp_gain>0 ~ "pos", lp_gain<0 ~ "neg")
    )
  ) +
  ggfittext::geom_fit_text(
    aes(label = scales::comma(rank.x, accuracy = 1), ymin = -max(lp.x)/10, ymax = 0), hjust = -0.5, vjust = -0.5,
    size = 18, reflow = TRUE, padding.x = grid::unit(3, "mm"), padding.y = grid::unit(3, "mm"), place = "top") +
  ggfittext::geom_fit_text(
    aes(label = scales_comma_plus(rank_gain, accuracy = 1), ymin = -max(lp.x)/10, ymax = 0,
        color = case_when(is.na(rank_gain) ~ "na", rank_gain==0 ~ "same", rank_gain>0 ~ "pos", rank_gain<0 ~ "neg")
    ), size = 18, reflow = FALSE, padding.x = grid::unit(3, "mm"), padding.y = grid::unit(3, "mm"), place = "bottom"
  ) +
  ggfittext::geom_fit_text(
    aes(label = name, ymin = -((max(lp.x)/4)+(max(lp.x)/10)), ymax = -max(lp.x)/10),
    size = 18, reflow = TRUE, padding.x = grid::unit(3, "mm"), padding.y = grid::unit(3, "mm"), place = "center") +
  theme_classic(base_size = 18) +
  scale_x_discrete(labels = element_blank()) +
  scale_y_continuous(breaks = NULL, labels = scales::comma_format(accuracy = 1)) +
  scale_color_manual(values = c("pos" = "#149414", "neg" = "coral2", "na" = "steelblue", "same" = "#EFB700")) +
  coord_flip() +
  theme(legend.position = "none") +
  labs(
    x = "Player", 
    y = 'LP', 
    title = 'TOP 10 - EU Master Leadeboard', 
    subtitle = sprintf('Date: %s - Rank & LP change in the past 24 hours', nice_date(Sys.Date()-days(1))))

# save plot
ggsave(filename = "/home/balco/dev/lor-meta-report/templates/tweet-plots/leaderboard.png", plot = p, width = 12, height = 8, dpi = 180)