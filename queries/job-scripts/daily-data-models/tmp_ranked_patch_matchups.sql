CREATE TABLE tmp_ranked_patch_matchups AS (
  WITH 
  metadata AS (
    SELECT
      match_id,
      CASE 
        WHEN game_start_time_utc >= '{update_time - 1*86400}' THEN 3
        WHEN game_start_time_utc >= '{update_time - 3*86400}' THEN 2
        WHEN game_start_time_utc >= '{update_time - 7*86400}' THEN 1
        ELSE 0 
      END AS time_frame,
      region
    FROM 
      ranked_match_metadata_30d
    WHERE 
      game_start_time_utc >= '{patch_release_date}'
  ),
  info AS (
    SELECT
      md.match_id, 
      md.time_frame,
      md.region,
      i.game_outcome,
      COALESCE(aa.new_name, i.archetype) AS archetype_1,
      CASE 
        WHEN i.player_rank = 2 THEN 1 
        ELSE 0 
      END AS is_master,
      ROW_NUMBER() OVER (
        PARTITION BY i.match_id 
        ORDER BY COALESCE(aa.new_name, i.archetype)
      ) AS id
    FROM 
      metadata md
    LEFT JOIN 
      ranked_match_info_30d i
    ON
      md.match_id = i.match_id
    LEFT JOIN 
      utils_archetype_aggregation aa
    ON 
      i.archetype = aa.old_name
  ),
  opponent_data AS (
    SELECT
      match_id,
      CASE 
        WHEN id = 2 THEN 1 
        WHEN id = 1 THEN 2 
        ELSE 0 
      END AS id,
      archetype_1 AS archetype_2
    FROM 
      info
  )
  
  SELECT 
    i.archetype_1,
    od.archetype_2,
    i.time_frame, 
    i.is_master,
    i.region,
    1.0 * SUM(i.game_outcome = 'win') + 0E0 AS win,
    1.0 * COUNT(*) + 0E0 AS n,
    CASE
      WHEN i.archetype_1 = od.archetype_2 THEN 0.5
      ELSE (1.0 * SUM(game_outcome = 'win') / COUNT(*)) 
    END AS winrate
  FROM 
    info i
  LEFT JOIN 
    opponent_data od
  ON
    i.match_id = od.match_id
    AND i.id = od.id
  GROUP BY
    i.archetype_1,
    od.archetype_2,
    i.time_frame, 
    i.is_master,
    i.region
  ORDER BY
    i.archetype_1,
    od.archetype_2,
    i.time_frame, 
    i.is_master,
    i.region
)
