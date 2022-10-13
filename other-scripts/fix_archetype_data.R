library(dplyr)

data <- lorr::get_db_query(
  "
  WITH
  codes_to_fix AS (
    SELECT
      deck_code,
      COUNT(DISTINCT archetype) AS n
    FROM
      ranked_match_info_30d
    GROUP BY
      deck_code
    HAVING
      n > 1
  ),
  info AS (
    SELECT i.*
    FROM
      codes_to_fix
    INNER JOIN
      ranked_match_info_30d i
    USING(deck_code)
  )
  
  SELECT
    deck_code,
    archetype,
    MIN(game_start_time_utc) AS mints
  FROM
    ranked_match_metadata_30d
  INNER JOIN
    info
  USING(match_id)
  GROUP BY
    deck_code,
    archetype
  ORDER BY
    deck_code,
    mints
  "
)

data_to_fix <- data |> 
  with_groups(deck_code, mutate, id = row_number()) |>   #View() 
  mutate(check = nchar(archetype)) |> 
  group_by(deck_code) |> 
  slice_max(n = 1, order_by = check) |> 
  ungroup() |> 
  mutate(archetype = ifelse(archetype == "No Champions (FR Jax)", "Jax (FR)", archetype)) |> 
  #filter(id == 2) |>
  select(deck_code, archetype) |>
  arrange(archetype, deck_code) |> 
  # fix Kai'Sa
  mutate(archetype = gsub("'", "\\\\'", archetype))

purrr::walk2(
  .x = data_to_fix$archetype,
  .y = data_to_fix$deck_code,
  .f = ~lorr::execute_db_query(
    query = "
  UPDATE ranked_match_info_30d 
  SET archetype = '{correct_name}' 
  WHERE deck_code = '{deck_code}'
  ;
  ",
  correct_name = .x,
  deck_code = .y
  )
)
