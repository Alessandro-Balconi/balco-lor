**This project is ded. RIP (2020-2022)**

# balco-lor

Files in this directory:

- README: this file.

- folder "data-dumps": contains dumps of data that I keep for archive reasons.

- folder "job-launcher": contains scripts that launch the cron jobs.

- folder "job-scripts": contains all the scripts to perform activities that are run periodically. These scripts are called by the scripts inside the 'job-launcher' folder. Inside this folder there is another "README" file with additional info on all the activities performed.

- folder "old": contains old scripts / analysis that are no more used, but the code might come in handy for future projects.

- folder "other-scripts": contains scripts that are not regularly launched but might be useful in the future. (e.g scripts for seasonal tournament)

- folder "output": contains the images used in the report. These are updated weekly.

- folder "plumber-api": contains the functions that expose the data as API.

- folder "queries": contains the SQL queries used in this project; the internal structure of this folder mimics the project folder structure.

- folder "templates": contains markdown templates and images used in the report. There's also a "output" folder that should not be here but can't be removed or it breaks the markdown.

- folder "tests": contains scripts that are work in progress.

#####################
###               ###
###   IMPORTANT   ###
###               ###
#####################

99% of the job is automated. The only manual intervention required is the following:

- After "publish-report.R" job finishes, it is necessary to perform "cd ./www" and "bundle exec jekyll build" to publish the website.

- Once the report is published, make a tweet and a reddit post about it (this could be automated, I'll look into it).

- If a push notification notifies of an error, manual intervention is usually required to fix those and relaunch whatever it was that stopped because of the error.
