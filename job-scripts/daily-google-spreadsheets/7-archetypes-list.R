suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(googlesheets4))

archetypes <- lorr::get_db_query(
  "SELECT DISTINCT archetype FROM ranked_patch_archetypes"
)

# additional information
info <- tibble(" " = sprintf("Last update: %s UTC", Sys.time()))

# id of the spreadsheet 
ss_id <- "1A1ELDzcZ_7VN6ci3bRDKyGnWUIJ7eNKC_KFb89ekXPw"

# update all sheets of the spreadsheet
sheet_write(data = info, ss = ss_id, sheet = "Data Information")
sheet_write(data = archetypes, ss = ss_id, sheet = "Archetypes" )

# names of the spreadsheet to update
ss_names <- sheet_names(ss_id)

# adjust spacing of columns in the spreadsheet
purrr::walk(.x = ss_names, .f = ~range_autofit(ss = ss_id, sheet = .))
