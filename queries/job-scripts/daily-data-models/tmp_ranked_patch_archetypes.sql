CREATE TABLE tmp_ranked_patch_archetypes AS (
  WITH 
  data AS (
    SELECT
      md.region,
      i.game_outcome,
      COALESCE(aa.new_name, i.archetype) AS archetype,
      CASE 
        WHEN md.game_start_time_utc >= '{update_time - 1*86400}' THEN 3
        WHEN md.game_start_time_utc >= '{update_time - 3*86400}' THEN 2
        WHEN md.game_start_time_utc >= '{update_time - 7*86400}' THEN 1
        ELSE 0 
      END AS time_frame,
      CASE 
        WHEN i.player_rank = 2 THEN 1 
        ELSE 0 
      END AS is_master
    FROM 
      ranked_match_metadata_30d md
    JOIN
      ranked_match_info_30d i
    ON
      md.match_id = i.match_id
    LEFT JOIN
      utils_archetype_aggregation aa
    ON
      i.archetype = aa.old_name
    WHERE 
      md.game_start_time_utc >= '{patch_release_date}'
  )
  
  SELECT 
    archetype,
    region,
    time_frame,
    is_master,
    1.0 * COUNT(*) + 0E0 AS `match`,
    1.0 * SUM(game_outcome = 'win') + 0E0 AS `win`,
    1.0 * SUM(game_outcome = 'win') / COUNT(*) + 0E0 AS winrate
  FROM 
    data
  GROUP BY
    archetype,
    region,
    time_frame,
    is_master
  ORDER BY
    archetype,
    region,
    time_frame,
    is_master
)
