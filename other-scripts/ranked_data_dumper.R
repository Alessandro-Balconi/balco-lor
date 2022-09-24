# setup ------------------------------------------------------------------------
library(dplyr)
library(purrr)
library(googlesheets4)
library(lorr)

# set google API Key & Oauth credentials
google_creds <- config::get("google", file = "/home/balco/my_rconfig.yml")
gargle::oauth_app_from_json(google_creds$client_secret)
gs4_auth_configure(api_key = google_creds$api_key)
gs4_auth(path = google_creds$auth_path)
options(googlesheets4_quiet = TRUE)

data_patches <- get_db_query("SELECT * FROM utils_patch_history")

data_patches <- data_patches %>% 
  mutate(end_date = lead(release_date)) %>% 
  filter(end_date <= Sys.Date() - 45)

patches <- data_patches %>%
  pull(patch_regex)

# remove already deployed patch data
patches <- setdiff(patches, c(
  'live_2_14_',
  'live_2_15_',
  'live_2_16_',
  'live_2_17_',
  'live_2_18_',
  'live_2_19_',
  'live_2_20_',
  'live_2_21_',
  'live_3_00_',
  'live_3_01_',
  'live_3_02_',
  'live_3_03_',
  'live_3_04_',
  'live_3_05_',
  'live_3_06_',
  'live_3_07_',
  'live_3_08_',
  'live_3_09_',
  'live_3_10_'
))

query <- "
  SELECT 
    match_id,
    game_start_time_utc,
    region,
    puuid,
    deck_code,
    game_outcome,
    order_of_play,
    archetype,
    CASE
      WHEN player_rank IS NULL THEN 1
      WHEN player_rank = 2 THEN 1
      ELSE 0
    END AS is_master
  FROM 
    ranked_match_metadata md
  INNER JOIN
    ranked_match_info i
  USING(match_id)
  WHERE
    game_version LIKE '{patch}%'
  ORDER BY
    game_start_time_utc,
    order_of_play
"

queries <- map_chr(
  .x = set_names(patches, patches), 
  .f = ~glue::glue(query, patch = .x)
)

db_players <- get_db_query(
  query = "
  SELECT 
    puuid, 
    CONCAT(gameName, '#', tagLine) AS player
  FROM utils_players
  "
)

# fetch data -----------------------------------------------------------------

ss_id <- ''

i <- 1

print(names(queries[i]))

# get match data
data <- get_db_query(query = queries[[i]])
  
# replace puuid with player name
data <- data %>% 
  left_join(db_players, by = "puuid") %>% 
  relocate(player, .after = puuid) %>% 
  select(-puuid)

# check if there are duplicates
check <- data %>% 
  count(match_id) %>% 
  filter(n != 2) %>% 
  nrow()

# if there are, remove them
if(check != 0){ 
  
  print('There are duplicated matches!')
  
  # first remove duplicate rows
  data <- data %>% 
    distinct()
  
  # then remove errors due to "is_master" field
  data <- data %>% 
    group_by(across(all_of(setdiff(colnames(data), 'is_master')))) %>% 
    summarise(is_master = max(is_master), .groups = 'drop') %>% 
    arrange(game_start_time_utc, match_id, player)
  
  print(sprintf('Fixed %s matches!', check))
  
  # check again (there should be no more duplicates)
  check <- data %>% 
    count(match_id) %>% 
    filter(n != 2) %>% 
    nrow()
  
  if(check != 0){ stop("There are still duplicates...STACCA STACCA!") }
  
  
}

# create sheet using first row as column info --------------------------------

print(sprintf('Rows to add: %s', nrow(data)))
  
write_sheet(data = data[1, ], ss = ss_id, sheet = names(queries[i]))
  
# add 10000 rows at a time to the spreadhseet --------------------------------
  
row <- 2
  
while(row < nrow(data)){
    
  print(row)
    
  sheet_append(
    data = data[row:min(row+10000, nrow(data)), ],
    ss = ss_id,
    sheet = names(queries[i])
  )
    
  row <- row + 10001
    
}
