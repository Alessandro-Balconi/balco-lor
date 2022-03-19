#Runs analysis + shows error if anything happens
tryCatch({
  
  # update time
  cat(sprintf("--- %s --- \n", Sys.time()))

  # update mysql raw tables
  tictoc::tic()
  source("/home/balco/dev/lor-meta-report/job-scripts/mongo-to-sql/4-ASIA-mongo-to-mysql.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/mongo-to-sql/3-AMERICAS-mongo-to-mysql.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/mongo-to-sql/2-mongo-to-mysql.R")
  cat("MongoDB to MySQL: ") tictoc::toc()
  
  # update data models
  tictoc::tic()
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/data-models/6-utils_archetype_aggregation.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/data-models/5-ranked_daily_archetypes.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/data-models/2-ranked_patch_decklists.R")
  rm(list = ls(all.names = TRUE))
  source("/home/balco/dev/lor-meta-report/job-scripts/mongo-to-sql/5-mysql-matchup-table.R")
  cat("Data Models: ") tictoc::toc()
  
  # update google spreadsheets
  tictoc::tic()
  
  # list of files to update
  gs_list <- list.files('/home/balco/dev/lor-meta-report/job-scripts/google-spreadsheets')
  
  # function to update spreadsheet
  update_ss <- function(ss){
    rm(list = ls(all.names = TRUE))
    source(paste0("/home/balco/dev/lor-meta-report/job-scripts/google-spreadsheets/", ss))
  }
  
  # apply function to all spreadsheets
  lapply(X = gs_list, FUN = update_ss)
  
  # log message after finishing updates
  cat("Google Spreadsheets: ") tictoc::toc()
  
}, error = function(e) {
  RPushbullet::pbPost(
    "note",
    title = "Daily database update",
    body = "There was an error during the database update. Manual intervention required."
  )
  print(e)
})