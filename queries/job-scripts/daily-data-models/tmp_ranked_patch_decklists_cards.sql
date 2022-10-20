CREATE TABLE tmp_ranked_patch_decklists_cards AS (
  SELECT 
    rpd.archetype, 
    region, 
    time_frame, 
    is_master, 
    card_code, 
    `count`,
    1.0 * SUM(`match`) + 0E0 AS `match`,
    1.0 * SUM(`win`) + 0E0 AS `win`
  FROM 
    ranked_patch_decklists rpd
  INNER JOIN
    utils_ranked_patch_decklists_cards urpdc
  ON
    rpd.archetype = urpdc.archetype
    AND rpd.deck_code = urpdc.deck_code
  GROUP BY
    archetype, 
    region, 
    time_frame, 
    is_master, 
    card_code, 
    `count`
  ORDER BY
    archetype, 
    region, 
    time_frame, 
    is_master, 
    card_code, 
    `count`
)
