#Runs analysis + shows error if anything happens
tryCatch({
  source("/home/balco/dev/lor-meta-report/job-scripts/update-leaderboard/update_leaderboard.R")
}, error = function(e) {
  RPushbullet::pbPost(
    "note",
    title = "Hourly leaderboard update",
    body = "There was an error during the leaderboard update. Manual intervention required."
  )
})