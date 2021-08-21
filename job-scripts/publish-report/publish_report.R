# Publish weekly report on lor-meta.com

tictoc::tic()

# 1. libraries ----

# 2. parameters ----

template_file_1 <- "/home/balco/dev/lor-meta-report/templates/report_pt1.md" # this should not be changed
template_file_2 <- "/home/balco/dev/lor-meta-report/templates/report_pt2.md" # this should not be changed

# this should be changed every week
p_report_number <- "8"
p_full_art      <- "05BC001T1"
p_emote         <- "taric"
p_subtitle      <- "Patch 2.13 - Week 4 - Bandle City Waiting Room"

# 3. functions ----

make_report <- function(template, report_number = "0", full_art = "03IO006", emote = "lulu", subtitle = "If you see this, I forgot to add a subtitle."){
  
  # THE ORDER OF THE PARAMETERS IS IMPORTANT !!! DO NOT CHANGE
  sprintf(template, report_number, subtitle, full_art, emote)
  
}

# 4. publish report ----

# read template
template <- map(
  .x = list(template_file_1, template_file_2),
  .f = ~readChar(., file.info(.)$size)
)

# add this week's parameter to the report
suppressWarnings(
  weekly_report <- map(
    .x = template,
    .f = ~make_report(template = .x, report_number = p_report_number, full_art = p_full_art, emote = p_emote, subtitle = p_subtitle)
  )
)

weekly_report <- str_flatten(weekly_report)

# name of the report
file_name <- sprintf("%s-meta-report-#%s.md", Sys.Date(), p_report_number)

# remove previously stored reports
do.call(file.remove, list(list.files("/home/balco/dev/lor-meta-report/templates/output/", full.names = TRUE)))

# save report locally
writeLines(weekly_report, sprintf("/home/balco/dev/lor-meta-report/templates/output/%s", file_name))

# push to "lor-meta.com" and bundle exec jekyll build
system("scp -r /home/balco/dev/lor-meta-report/templates/output/* balco@lor-meta.com:/home/balco/www/_posts/")

tictoc::toc()

# send notification
RPushbullet::pbPost(
  "note", 
  title = "Weekly Report Uploaded", 
  body = "The weekly report was uploaded correctly."
)

