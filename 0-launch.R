#Runs analysis + shows error if anything happens
tryCatch({
  if(lubridate::hour(Sys.time()) == 0) { cat(sprintf("--- %s --- \n", Sys.Date())) }
  tictoc::tic()
  source("/home/balco/dev/lor-meta-report/job-scripts/mongo-to-sql/4-ASIA-mongo-to-mysql.R")
  tictoc::toc()
  tictoc::tic()
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/mongo-to-sql/3-AMERICAS-mongo-to-mysql.R")
  tictoc::toc()
  tictoc::tic()
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/mongo-to-sql/2-mongo-to-mysql.R")
  tictoc::toc()
  tictoc::tic()
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/mongo-to-sql/5-mysql-matchup-table.R")
  tictoc::toc()
  tictoc::tic()
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/mongo-to-sql/7-mysql-decklists.R")
  tictoc::toc()
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/gs4-report/gs4_report.R")
}, error = function(e) {
  RPushbullet::pbPost(
    "note",
    title = "Daily database update",
    body = "There was an error during the database update. Manual intervention required."
  )
  print(e)
})