# imoprt libraries
library(lorr)
library(dplyr)
library(purrr)

# get info for the different game versions
game_versions <- get_db_query(
  "
  SELECT
    game_version,
    MIN(game_start_time_utc) AS mints,
    MAX(game_start_time_utc) AS maxts,
    COUNT(*) AS n
  FROM 
    ranked_match_metadata
  GROUP BY
    game_version
  ORDER BY
    mints
  "
)

# main function to delete data
# 1. first deletes the data from the ranked_match_info table
# 2. then deletes the data from the ranked_match_metadata table
delete_data <- function(game_version){
  
  cat(sprintf("%s UTC - Deleting info for patch %s... \n", Sys.time(), game_version))

  execute_db_query(
    "
  DELETE FROM
    ranked_match_info
  WHERE
    match_id IN (
      SELECT
        match_id
      FROM
        ranked_match_metadata
      WHERE
        game_version = '{game_version}'
    )
  "
  )
  
  cat(sprintf("%s UTC - Deleted info for patch %s! \n", Sys.time(), game_version))
  
  cat(sprintf("%s UTC - Deleting metadata for patch %s... \n", Sys.time(), game_version))

  execute_db_query(
    "
  DELETE FROM
    ranked_match_metadata
  WHERE
    game_version = '{game_version}'
  "
  )
  
  cat(sprintf("%s UTC - Deleted metadata for patch %s! \n", Sys.time(), game_version))
  
}

# get game_versions older than 6 months (the ones to delete)
game_versions_to_delete <- game_versions |>
  filter(maxts <= Sys.Date()-180) |> 
  pull(game_version)

# delete data for those
walk(.x = game_versions_to_delete, .f = delete_data)