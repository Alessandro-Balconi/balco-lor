# google spreadshett update optimization ----
suppressPackageStartupMessages(library(tidyverse)) # all purposes package
suppressPackageStartupMessages(library(googlesheets4)) # manage google sheets API

# create connection to MySQL database
con <- lorr::create_db_con()

update_spreadsheet <- function(n, time_frame, is_master, ss_id){
  
  # ndays to use to check for patches
  patch_ndays <- switch(as.character(time_frame), "0" = 1e3, "1" = 7, "2" = 3)
  
  # patches from which data is analyzed
  patches <- tbl(con, "utils_patch_history") %>%
    collect() %>% 
    arrange(desc(release_date)) %>%
    mutate(release_date = lag(release_date)) %>% 
    filter(is.na(release_date) | release_date >= Sys.Date() - lubridate::days(patch_ndays)) %>% 
    mutate(new_change = lag(change)) %>% 
    replace_na(list(new_change = 0)) %>% 
    mutate(cum_change = cumsum(new_change)) %>% 
    filter(cum_change == min(cum_change)) %>%
    arrange(release_date) %>% 
    pull(patch) %>% 
    paste0(collapse = ", ")
  
  # most played archetypes
  top <- tbl(con, "ranked_patch_matchups") %>% 
    filter(time_frame >= local(time_frame), is_master >= local(is_master)) %>% 
    count(archetype_1, wt = n, sort = TRUE) %>% 
    head(n = local(n)) %>% 
    pull(archetype_1)
  
  # same but wrapped (nicer looking in spreadsheets)
  top_wrap <- str_wrap(top, width = 8)
  
  # collect matchups data
  x <- tbl(con, "ranked_patch_matchups") %>%
    filter(time_frame >= local(time_frame), is_master >= local(is_master)) %>% 
    mutate(
      archetype_1 = if_else(archetype_1 %in% top, archetype_1, 'Other'), 
      archetype_2 = if_else(archetype_2 %in% top, archetype_2, 'Other')
    ) %>% 
    group_by(archetype_1, archetype_2) %>% 
    summarise(across(c(n, win), sum, na.rm = TRUE), .groups = 'drop') %>% 
    mutate(winrate = win / n) %>% 
    select(archetype_1, archetype_2, n, winrate) %>% 
    collect()
  
  # adjust winrate (so that the sum is 100%)
  x <- x %>% 
    left_join(x, by = c('archetype_1' = 'archetype_2', "archetype_2" = "archetype_1")) %>% 
    mutate(across(starts_with("winrate."), .fns = list(function(x) x / (winrate.x + winrate.y)), .names = "{col}_adj")) %>% 
    mutate(n_adj = round((n.x + n.y) / 2, digits = 0)) %>% 
    select(archetype_1, archetype_2, winrate = winrate.x_adj, n = n_adj)
  
  # total number of games played
  total_n <- tbl(con, "ranked_patch_matchups") %>% 
    filter(time_frame >= local(time_frame), is_master >= local(is_master)) %>% 
    summarise(full_n = sum(n, na.rm = TRUE)) %>% 
    pull()
  
  # top decks info (playrate)
  y <- tbl(con, "ranked_patch_matchups") %>% 
    filter(
      time_frame >= local(time_frame), 
      is_master >= local(is_master), 
      archetype_1 %in% top
    ) %>%
    group_by(archetype_1) %>% 
    summarise(games_played = sum(n, na.rm = TRUE), .groups = "drop") %>% 
    collect() %>% 
    mutate(playrate = (games_played / total_n)*100) %>% 
    mutate(playrate = round(playrate, digits = 2)) %>% 
    arrange(-playrate)
  
  # top decks info (winrate)
  yy_wr <- tbl(con, "ranked_patch_matchups") %>% 
    filter(
      time_frame >= local(time_frame), 
      is_master >= local(is_master), 
      archetype_1 %in% top
    ) %>%
    group_by(archetype_1) %>% 
    summarise(across(c(n, win), sum, na.rm = TRUE), .groups = 'drop') %>% 
    mutate(winrate = win / n) %>% 
    select(archetype_1, winrate) %>% 
    collect()

  # prepare matchup table ----
  
  # remove duplicate rows (mirror matches)
  x <- x %>% 
    distinct() 
  
  # only for Flixus, keep number of mirror matches played
  if(ss_id == "1Oe5-neLTksWlldS1lc1viiCwjKE_L3GbcQAC0T-l5N0"){
    
    x <- x %>% 
      mutate(
        winrate = ifelse(archetype_1 == archetype_2, 0.5, winrate),
        n = ifelse(archetype_1 == archetype_2, n / 2, n)
      )
    
  } else {
    
    x <- x %>% 
      mutate(across(c(winrate, n), function(x) ifelse(archetype_1 == archetype_2, NA, x))) 
    
  }

  x <- x %>% 
    mutate(across(archetype_2, str_wrap, width = 8)) %>% 
    mutate(across(archetype_1, factor, levels = c(top, 'Other'), ordered = TRUE)) %>% 
    mutate(across(archetype_2, factor, levels = c(top_wrap, 'Other'), ordered = TRUE)) %>% 
    arrange(archetype_1, archetype_2)
  
  # generate table with winrates    
  x_wr <- x %>% 
    select(-n) %>%
    mutate(winrate = round(winrate*100, digits = 1)) %>% 
    pivot_wider(names_from = archetype_2, values_from = winrate) %>% 
    rename(" " = archetype_1)
  
  # generate table with number of games
  x_n <- x %>% 
    select(-winrate) %>% 
    pivot_wider(names_from = archetype_2, values_from = n) %>% 
    rename(" " = archetype_1)
  
  # additional information
  update <- sprintf("Last update: %s UTC", format(Sys.time(), '%d %b, %Y %X'))
  patches <- sprintf("Patch Analyzed: %s", patches)
  
  info <- tibble(
    " " = c(update, patches)
  )
  
  # deck information
  deck_info <- y %>% 
    left_join(yy_wr, by = "archetype_1") %>% 
    mutate(winrate = round(winrate*100, digits = 2)) %>% 
    rename(" " = archetype_1)
  
  # update all sheets of the spreadsheet (this is for all but Flixus's SS)
  if(ss_id != '1Oe5-neLTksWlldS1lc1viiCwjKE_L3GbcQAC0T-l5N0'){
    
    range_write(data = x_wr,      ss = ss_id, sheet = "Winrate")
    range_write(data = x_n,       ss = ss_id, sheet = "Number of Games")
    range_write(data = deck_info, ss = ss_id, sheet = "Decks Information")
    range_write(data = info,      ss = ss_id, sheet = "Data Information")
    
    # names of the spreadsheet to update
    ss_names <- sheet_names(ss_id)
    
    # adjust spacing of columns in the spreadsheet
    walk(.x = ss_names, .f = ~range_autofit(ss = ss_id, sheet = .))
    
  }
  
  # for Flixus's SS do it a bit differently
  if(ss_id == '1Oe5-neLTksWlldS1lc1viiCwjKE_L3GbcQAC0T-l5N0'){

    range_write(data = x_wr,      ss = ss_id, sheet = "Matchup Table")
    range_write(data = x_n,       ss = ss_id, sheet = "Number of Games")
    
    range_write(
      data = info, 
      ss = ss_id, 
      sheet = "important information",
      range = "E3"
    )
    
  }

}

# update all tables
update_spreadsheet(n = 25, time_frame = 0, is_master = 0, ss_id = "1Xlh2kg7gLzvqugqGPpI4PidAdM5snggbJ44aRLuik5E")
update_spreadsheet(n = 40, time_frame = 0, is_master = 0, ss_id = "1b2sgPXcgV7zTmOVmotoVnUfGTeuK_PIuYsx6zA46rks")
update_spreadsheet(n = 60, time_frame = 0, is_master = 0, ss_id = "1Y30Rng9sA7WVZLjRDs2OBnpJGkHLgtjDMdY8mjkKBZg")
update_spreadsheet(n = 40, time_frame = 1, is_master = 0, ss_id = "1K_V50pebtLj9nM-lKcFZ7louo27VBVL4zLY9AVRdMnE")
update_spreadsheet(n = 40, time_frame = 2, is_master = 0, ss_id = "1BHK_ZsZadjD7WhqmmKXReBcWe_Rok9UkqE8y_ukTdmc")
update_spreadsheet(n = 25, time_frame = 0, is_master = 1, ss_id = "1v2OtHSvmhYIvtUvCW7whvkuU1NIQa0teFyDtVxr-rnM")
update_spreadsheet(n = 40, time_frame = 0, is_master = 1, ss_id = "1x2RhoT45Wdow6g8C3ZuatrxniqLrwjIOk4yn240G0GA")

# this updates the spreadsheet of the Flixus Guy
update_spreadsheet(n = 60, time_frame = 0, is_master = 0, ss_id = "1Oe5-neLTksWlldS1lc1viiCwjKE_L3GbcQAC0T-l5N0")

DBI::dbDisconnect(con)
