# Saves information about patches (had new cards - balance changes?)

# This task is performed weekly on Thursday morning

tictoc::tic()

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(jsonlite))  # convert JSON to R objects
suppressPackageStartupMessages(library(httr))      # http requests

# 2. parameters ----

# load db credentials
mysql_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# 3. functions ----
# 4. connect to db & load data ----

# close previous connections to MySQL database (if any)
if(exists("con")){ DBI::dbDisconnect(con) }

# create connection to MySQL database
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  db_host = "127.0.0.1",
  user = mysql_creds$uid,
  password = mysql_creds$pwd,
  dbname = mysql_creds$dbs
)

# extract list of patches from sql data
patch_list <- tbl(con, "lor_match_info_v2") %>% 
  distinct(game_version) %>% 
  collect()

# quick preprocess
patch_list <- patch_list %>% 
  mutate(across(game_version, ~word(string = ., start = 2, end = -2, sep = "_"))) %>% 
  distinct() %>% 
  separate(col = game_version, into = c("version", "patch"), sep = "\\_", convert = TRUE) %>% 
  mutate(last_patch = version*100+patch) %>% 
  unite(col = value, version, patch, sep = ".")

# check if most recent patch is already in db (which means this week there was no patch)
last_patch <- patch_list %>% 
  slice_max(n = 1, order_by = last_patch, with_ties = FALSE) %>% 
  pull(value)

# latest saved patch
mysql_patch <- tbl(con, "utils_patch_history") %>% 
  filter(release_date == max(release_date, na.rm = TRUE)) %>% 
  distinct(patch) %>% # should be just one anyway...
  collect() %>% 
  pull()

# 5. perform calculations ----
if(mysql_patch == last_patch){
  
  RPushbullet::pbPost(
    "note", 
    title = "Weekly patch history table update", 
    body = "No update performed. Table was already up to date."
  )
  
} else {
  
  # patches are different! check if there is any difference
  patch_list <- patch_list %>% 
    slice_max(n = 2, order_by = last_patch, with_ties = FALSE) %>% 
    mutate(live_patch = paste0(str_replace(value, pattern = "\\.", replacement = "_"), "_0")) %>% 
    mutate(data_json = sprintf("https://dd.b.pvp.net/%s/core/en_us/data/globals-en_us.json", live_patch))
  
  # call "globals.json" for both patches
  patch_list <- patch_list %>% 
    mutate(get_json = map(.x = data_json, .f = GET)) %>% 
    mutate(status_code = get_json %>% map_int("status_code"))
  
  # if all returned code 200, check differences
  if(length(setdiff(unique(patch_list$status_code), 200)) != 0){
    
    stop(sprintf("There was an error in the GET. Received status codes: %s", paste0(unique(patch_list$status_code), collapse = ", ")))
    
  } else {
    
    # number of sets in every patch
    n_sets <- patch_list %>% 
      mutate(content = map(.x = get_json, .f = ~content(., encoding = "UTF-8"))) %>% 
      mutate(content = map(.x = content, .f = fromJSON)) %>% 
      mutate(sets = map(.x = content, .f = "sets")) %>% 
      mutate(nsets = map_dbl(sets, nrow)) %>% 
      distinct(nsets)
    
    # if they have a different number of sets, it means there is a change in the data
    if(nrow(n_sets) > 1){
      
      data <- patch_list %>% 
        select(value, last_patch) %>% 
        slice_max(n = 1, order_by = last_patch, with_ties = FALSE) %>% 
        mutate(change = 1)
      
    # if they have the same number of sets, check if they have a different number of cards  
    } else{
      
      latest_set <- patch_list$get_json[[1]] %>% 
        content(encoding = "UTF-8") %>% 
        fromJSON() %>% 
        .[["sets"]] %>% 
        mutate(set = str_extract(nameRef, pattern = "[0-9]+")) %>% 
        mutate(set = as.numeric(set)) %>% 
        summarise(max(set, na.rm = TRUE)) %>% 
        pull()
      
      patch_list <- patch_list %>% 
        mutate(data_json = sprintf("https://dd.b.pvp.net/%s/set%2$s/en_us/data/set%2$s-en_us.json", live_patch, latest_set)) %>% 
        mutate(get_json = map(.x = data_json, .f = GET)) %>% 
        mutate(status_code = get_json %>% map_int("status_code"))
      
      # if all returned code 200, check differences
      if(length(setdiff(unique(patch_list$status_code), 200)) != 0){
        
        stop(sprintf("There was an error in the GET. Received status codes: %s", paste0(unique(patch_list$status_code), collapse = ", ")))
        
      } else {
       
        # number of cards in every patch
        n_cards <- patch_list %>% 
          mutate(content = map(.x = get_json, .f = ~content(., encoding = "UTF-8"))) %>% 
          mutate(content = map(.x = content, .f = fromJSON)) %>% 
          mutate(ncards = map_dbl(content, nrow)) %>% 
          distinct(ncards)
        
        # if they have a different number of sets, it means there is a change in the data
        if(nrow(n_cards) > 1){
          
          data <- patch_list %>% 
            select(value, last_patch) %>% 
            slice_max(n = 1, order_by = last_patch, with_ties = FALSE) %>% 
            mutate(change = 1)
          
        # if they have the same number of cards, check if there was a balance change  
        } else{
         
          # this returns 1 if no balance change, 2 if balance change
          balance_change <- patch_list %>% 
            mutate(all_data_json = map(
              .x = live_patch, 
              .f = ~sprintf("https://dd.b.pvp.net/%1$s/set%2$s/en_us/data/set%2$s-en_us.json", rep(.x, latest_set) , 1:latest_set))
            ) %>% 
            mutate(all_get_json = map(.x = all_data_json, .f = ~map(.x = ., .f = GET))) %>% 
            mutate(content = map(.x = all_get_json, .f = ~map(.x = ., .f = ~content(., encoding = "UTF-8")))) %>% 
            mutate(content = map(.x = content, .f = ~map_dfr(.x = ., .f = fromJSON))) %>% 
            select(value, last_patch, content) %>% 
            unnest(col = content) %>% 
            select(value, last_patch, cardCode, attack, cost, health, spellSpeed) %>% 
            group_by(cardCode) %>% 
            summarise(n = n_distinct(attack, cost, health, spellSpeed), .groups = "drop") %>% 
            summarise(max = max(n)) %>% 
            pull()
          
          # if there was a balance change, it means there is a change in the data
          if(balance_change > 1){
            
            data <- patch_list %>% 
              select(value, last_patch) %>% 
              slice_max(n = 1, order_by = last_patch, with_ties = FALSE) %>% 
              mutate(change = 1)
            
            # if they there was no balance change, it means the patches are the same
          } else{
            
            data <- patch_list %>% 
              select(value, last_patch) %>% 
              slice_max(n = 1, order_by = last_patch, with_ties = FALSE) %>% 
              mutate(change = 0)
            
          }
            
        }
         
      }
      
    }
    
  }
    
  
  # 6. save to MySQL db ----
  
  # save matches to db
  if(nrow(data) >  0){
    
    data %>% 
      DBI::dbWriteTable(conn = con, name = "lor_patch_history", value = ., append = TRUE, row.names = FALSE)
    
    data %>%
      separate(value, into = c('main', 'sub'), sep = "\\.", remove = FALSE) %>% 
      mutate(sub = str_pad(sub, width = 2, pad = "0", side = 'left')) %>% 
      mutate(patch_regex = paste0("live_", main, "_", sub, "_")) %>% 
      mutate(release_date = lubridate::as_datetime(paste(Sys.Date() - lubridate::days(1), "16:00:00"))) %>% 
      select(patch = value, patch_regex, release_date, change) %>% 
      DBI::dbWriteTable(conn = con, name = "utils_patch_history", value = ., append = TRUE, row.names = FALSE) 

  }
  
  RPushbullet::pbPost(
    "note", 
    title = "Weekly patch history table update", 
    body = sprintf("The weekly update was performed correctly; added patch %s", last_patch)
  )

}

tictoc::toc()

DBI::dbDisconnect(con)
