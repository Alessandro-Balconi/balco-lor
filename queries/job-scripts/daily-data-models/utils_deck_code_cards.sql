INSERT INTO 
  utils_deck_code_cards
WITH
missing AS (
  SELECT DISTINCT
    rpd.archetype,
    rpd.deck_code
  FROM
    ranked_patch_decklists rpd
  LEFT JOIN
    utils_deck_code_cards udcc
  ON
    rpd.archetype = udcc.archetype
    AND rpd.deck_code = udcc.deck_code
  WHERE
    udcc.card_code IS NULL
),
decklists AS (
  SELECT DISTINCT
    archetype,
    deck_code,
    cards
  FROM
    missing
  INNER JOIN
    ranked_match_info_30d
  USING(archetype, deck_code)
),
data_cards AS (
  SELECT
    d.archetype,
    d.deck_code,
    SUBSTRING_INDEX(SUBSTRING_INDEX(d.cards, ' ', un.n), ' ', -1) AS cards
  FROM
    utils_numbers un
  INNER JOIN 
    decklists d
  ON 
    CHAR_LENGTH(d.cards) - CHAR_LENGTH(REPLACE(d.cards, ' ', '')) >= un.n - 1
  ORDER BY
    archetype,
    deck_code,
    n
),
to_add AS (
  SELECT 
    archetype,
    deck_code,
    SUBSTRING_INDEX(cards, ':', 1) AS `count`,
    SUBSTRING_INDEX(cards, ':', -1) AS card_code
  FROM
    data_cards
)
SELECT 
  * 
FROM 
  to_add
