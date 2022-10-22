CREATE TABLE tmp_utils_ranked_patch_decklists_cards AS (
  WITH 
  decklists AS (
    SELECT DISTINCT
      COALESCE(aa.new_name, i.archetype) AS archetype,
      i.deck_code,
      i.cards
    FROM 
      ranked_match_metadata_30d md
    JOIN
      ranked_match_info_30d i
    USING(match_id)
    LEFT JOIN
      utils_archetype_aggregation aa
    ON
      i.archetype = aa.old_name
    WHERE
      md.game_start_time_utc >= '{patch_release_date}'
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
    )
    
  SELECT 
    archetype,
    deck_code,
    SUBSTRING_INDEX(cards, ':', 1) AS `count`,
    SUBSTRING_INDEX(cards, ':', -1) AS card_code
  FROM
    data_cards
)
