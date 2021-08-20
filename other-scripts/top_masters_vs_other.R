# Check Top masters vs Other masters for a specific matchup

# 1. libraries ----

library(rvest)

# 2. functions ----

# 3. parameters ----

# decks to focus on
deck_focus <- "Anivia (SI)"
deck_oppo  <- c("Zed Sivir Akshan", "Sivir Akshan (IO)")

# file where there are the names of top masters
top_file <- "/home/balco/dev/lor-meta-report/old/top_masters.html"

# 4. connect to db & load data ----

# close previous connections to MySQL database (if any)
if(exists("con")){ DBI::dbDisconnect(con) }

# create connection to MySQL database
con <- DBI::dbConnect(
  RMySQL::MySQL(),
  db_host = "127.0.0.1",
  user = "balco",
  password = "Macosanes0!",
  dbname = "db_prova"
)

# extract names of top masters
top_names_list <- top_file %>% 
  read_html(encoding = "utf8") %>% 
  html_table()

# player dbs
db_eu <- mongolite::mongo(url = "mongodb://balco:n0nLadimentico@localhost:27017/admin", collection = "lor_player")
db_na <- mongolite::mongo(url = "mongodb://balco:n0nLadimentico@localhost:27017/admin", collection = "lor_player_na")
db_as <- mongolite::mongo(url = "mongodb://balco:n0nLadimentico@localhost:27017/admin", collection = "lor_player_asia")

# 5. prepare data ----

# get names of top masters from list of tables
top_names <- top_names_list %>% 
  bind_rows() %>% 
  pivot_longer(cols = everything()) %>% 
  filter(nchar(value)>=3) %>% 
  distinct(value) %>% 
  pull()

# db of players
db_player <- bind_rows(db_eu$find(), db_na$find(), db_as$find())

# db of matches
x1 <- tbl(con, "lor_match_info_na") %>% 
  filter(archetype == deck_focus) %>% 
  distinct(match_id) %>% 
  collect() %>% 
  pull()

y1 <- tbl(con, "lor_match_info_na") %>% 
  filter(archetype %in% deck_oppo) %>% 
  distinct(match_id) %>% 
  collect() %>% 
  pull()

m1 <- intersect(x1, y1)

data_1 <- tbl(con, "lor_match_info_na") %>% 
  filter(match_id %in% m1) %>% 
  collect()

x2 <- tbl(con, "lor_match_info") %>% 
  filter(archetype == deck_focus) %>% 
  distinct(match_id) %>% 
  collect() %>% 
  pull()

y2 <- tbl(con, "lor_match_info") %>% 
  filter(archetype %in% deck_oppo) %>% 
  distinct(match_id) %>% 
  collect() %>% 
  pull()

m2 <- intersect(x2, y2)

data_2 <- tbl(con, "lor_match_info") %>% 
  filter(match_id %in% m2) %>% 
  collect()

x3 <- tbl(con, "lor_match_info_asia") %>% 
  filter(archetype == deck_focus) %>% 
  distinct(match_id) %>% 
  collect() %>% 
  pull()

y3 <- tbl(con, "lor_match_info_asia") %>% 
  filter(archetype %in% deck_oppo) %>% 
  distinct(match_id) %>% 
  collect() %>% 
  pull()

m3 <- intersect(x3, y3)

data_3 <- tbl(con, "lor_match_info_asia") %>% 
  filter(match_id %in% m3) %>% 
  collect()

data <- bind_rows(data_1, data_2, data_3)

# 6. generate plot ----

data <- data %>% 
  select(match_id, game_outcome, archetype, puuid) %>% 
  left_join(db_player, by = "puuid") %>% 
  select(-c(puuid, tagLine))

data <- data %>% 
  mutate(is_top = ifelse(gameName %in% top_names, 1, 0))

data %>% 
  filter(archetype == deck_focus) %>% 
  count(game_outcome, is_top) %>% 
  pivot_wider(names_from = game_outcome, values_from = n) %>% 
  mutate(is_top = ifelse(is_top == 1, "Top Masters", "Other Masters")) %>%
  mutate(is_top = factor(is_top, levels = c("Top Masters", "Other Masters"), ordered = TRUE)) %>% 
  mutate(winrate = win / (win + loss)) %>%
  mutate(low = map2_dbl(.x = win, .y = loss, .f = ~binom.test(x = .x, n = .x+.y)$conf.int[1])) %>% 
  mutate(hi = map2_dbl(.x = win, .y = loss, .f = ~binom.test(x = .x, n = .x+.y)$conf.int[2])) %>% 
  ggplot(aes(x = as.numeric(is_top), y = winrate)) +
  geom_segment(aes(x = as.numeric(is_top), xend = as.numeric(is_top), y = low, yend = hi), color = "grey30", size = 1) +
  geom_label(aes(y = hi, label = sprintf("High 95%%: %s", scales::percent(hi, accuracy = .1))), color = "forestgreen", size = 7) +
  geom_label(aes(y = low, label = sprintf("Low 95%%: %s", scales::percent(low, accuracy = .1))), color = "coral", size = 7) +               
  geom_label(aes(label = sprintf("Observed WR: %s \n #Games: %s", scales::percent(winrate, accuracy = .1), loss+win)), color = "steelblue", size = 7) +
  theme_light(base_size = 15) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(limits = c("1", "2"), labels = c("Top Masters", "Other Masters")) +
  theme(legend.position = "none") +
  expand_limits(y = c(0.3,0.7)) +
  labs(x = "Group", y = "Winrate", title = "Winrate as Anivia vs Sivir Akshan (+Zed) in Master",
       subtitle = "Top Master are players that appear in Riot's World Championship Standings")
