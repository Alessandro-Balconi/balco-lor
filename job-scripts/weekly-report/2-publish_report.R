# Publish weekly report on lor-meta.com

# 1. libraries ----

suppressPackageStartupMessages(library(tidyverse))   # all purposes package

# 2. parameters ----

template_file_1 <- "/home/balco/dev/lor-meta-report/templates/report_pt1.md" # this should not be changed
template_file_2 <- "/home/balco/dev/lor-meta-report/templates/report_pt2.md" # this should not be changed

# check which report is the last available (to make sure we updated the parameters this week)
reports <- system('ssh balco@lor-meta.com "ls -d /home/balco/www/_posts/*"', intern = TRUE)

latest <- reports %>% 
  str_extract(pattern = "\\#[0-9]+") %>% 
  parse_number() %>% 
  max()

# this should be changed every week
p_report_number <- as.character(latest+1)
p_full_art      <- "03IO006"
p_emote         <- "lulu"
p_subtitle      <- "Patch 2.18 - Week 3"

# 3. functions ----

make_report <- function(template, report_number = "0", full_art = "03IO006", emote = "lulu", subtitle = "If you see this, I forgot to add a subtitle."){
  
  # THE ORDER OF THE PARAMETERS IS IMPORTANT !!! DO NOT CHANGE
  sprintf(template, report_number, subtitle, full_art, emote)
  
}

# 4. publish report ----

if(as.numeric(p_report_number) != latest+1){
  
  RPushbullet::pbPost(
    "note", 
    title = "Weekly Report Publishing", 
    body = "Report parameters where not updated this week. Failed to publish the report."
  )
  
} else {
  
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
  
  RPushbullet::pbPost(
    "note", 
    title = "Weekly Report Uploaded", 
    body = "The weekly report was uploaded correctly."
  )
  
}
