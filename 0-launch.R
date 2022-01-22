#Runs analysis + shows error if anything happens
tryCatch({
  cat(sprintf("--- %s --- \n", Sys.time()))
  tictoc::tic()
  source("/home/balco/dev/lor-meta-report/job-scripts/mongo-to-sql/4-ASIA-mongo-to-mysql.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/mongo-to-sql/3-AMERICAS-mongo-to-mysql.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/mongo-to-sql/2-mongo-to-mysql.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/data-models/2-ranked_archetypes.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/mongo-to-sql/5-mysql-matchup-table.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/mongo-to-sql/7-mysql-decklists.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/google-spreadsheets/1-base_table.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/google-spreadsheets/2-table_master.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/google-spreadsheets/3-table_40decks.R")
  #rm(list = ls(all.names = TRUE))
  #Sys.setenv(RSTUDIO_PANDOC = "/usr/lib/rstudio-server/bin/pandoc")
  #source("/home/balco/dev/lor-meta-report/job-scripts/mongo-to-sql/10-masteringruneterra_tierlist.R")
  tictoc::toc()
}, error = function(e) {
  RPushbullet::pbPost(
    "note",
    title = "Daily database update",
    body = "There was an error during the database update. Manual intervention required."
  )
  print(e)
})