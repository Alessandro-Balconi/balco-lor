SELECT
  CONCAT_WS('#', gameName, tagLine) AS player,
  puuid
FROM
  utils_players
WHERE
  region = 'europe'
  AND CONCAT_WS('#', gameName, tagLine) IN ('{mysql_player_filter}')
