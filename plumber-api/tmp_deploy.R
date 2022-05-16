# This script temporarily deploys the api at balco-lor.com/api

library(plumber)

# 'plumber.R' is the location of the file shown above
pr("/home/balco/dev/lor-meta-report/plumber-api/plumber.R") %>%
  pr_run(port=8000)
