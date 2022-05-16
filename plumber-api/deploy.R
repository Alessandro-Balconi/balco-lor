# This script temporarily deploys the api at balco-lor.com/api

# library(plumber)
# 
# 'plumber.R' is the location of the file shown above
# pr("/home/balco/dev/lor-meta-report/plumber-api/plumber.R") %>%
#   pr_run(port=8000)


# PERMANENT DEPLOY ----

# DROPLET IDs:
# - droplet-number-one : 242745676
# - secondo-droplet : 251801753

# if it asks for password, it's the same used to login on the RStudio server

plumberDeploy::do_deploy_api(
  droplet   = analogsea::droplet(id = 242745676),
  path      = "api",
  localPath = "/home/balco/dev/lor-meta-report/plumber-api/main-api",
  port      = 8000,
  overwrite = TRUE
)
