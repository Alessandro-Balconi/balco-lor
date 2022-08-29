SELECT
  LOWER(name) AS name,
  MIN(`rank`) AS ladder_rank
FROM
  leaderboard_daily
WHERE
  region = 'europe'
  AND day = '{cutoff_day}'
GROUP BY
  name
