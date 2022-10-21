CREATE TABLE tmp_ranked_patch_decklists_cards AS (
  WITH 
  decklists AS (
    SELECT DISTINCT
      rpd.archetype,
      rpd.deck_code,
      rmi.cards
    FROM 
      ranked_patch_decklists rpd
    JOIN
      ranked_match_info_30d rmi
    USING(deck_code)
  ),
  data_cards AS (
    SELECT
      d.archetype,
      d.deck_code,
      SUBSTRING_INDEX(SUBSTRING_INDEX(d.cards, ' ', un.n), ' ', -1) AS cards
    from
      utils_numbers un
    inner join 
      decklists d
    on 
      CHAR_LENGTH(d.cards) - CHAR_LENGTH(REPLACE(d.cards, ' ', '')) >= un.n - 1
    ORDER BY
      archetype,
      deck_code,
      n
  ),
  utils_ranked_patch_decklists_cards AS (
    SELECT 
      archetype,
      deck_code,
      SUBSTRING_INDEX(cards, ':', 1) AS `count`,
      SUBSTRING_INDEX(cards, ':', -1) AS card_code
    FROM
      data_cards
  )
    
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
