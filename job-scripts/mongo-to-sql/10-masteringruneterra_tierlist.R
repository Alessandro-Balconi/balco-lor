suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(reactable))
suppressPackageStartupMessages(library(htmltools))

# Render a bar chart with a label on the left
bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}

# Precalculate donut chart score colors to be used for custom cell rendering
get_score_color <- function(score) {
  blue_pal <- function(x) rgb(colorRamp(c("#9fc7df", "#416ea4"))(x), maxColorValue = 255)
  normalized <- (score - min(score)) / (max(score) - min(score))
  blue_pal(normalized)
}

# function to generate donut charts
cell_func <- JS("function(cellInfo) {
       const sliceColor = cellInfo.row['Score Color']
       const sliceLength = 2 * Math.PI * 24
       const sliceOffset = sliceLength * (1 - cellInfo.value / 100)
       const donutChart = (
         '<svg width=60 height=60 style=\"transform: rotate(-90deg)\" focusable=false>' +
           '<circle cx=30 cy=30 r=24 fill=none stroke-width=4 stroke=rgba(0,0,0,0.1)></circle>' +
           '<circle cx=30 cy=30 r=24 fill=none stroke-width=4 stroke=' + sliceColor +
           ' stroke-dasharray=' + sliceLength + ' stroke-dashoffset=' + sliceOffset + '></circle>' +
         '</svg>'
       )
       const label = '<div style=\"position: absolute; top: 50%; left: 50%; ' +
         'transform: translate(-50%, -50%)\">' + cellInfo.value.toFixed(1) + '</div>'
       return '<div style=\"display: inline-flex; position: relative\">' + donutChart + label + '</div>'
      }")

# load mysql db credentials
db_creds <- config::get("mysql", file = "/home/balco/my_rconfig.yml")

# close previous connections to MySQL database (if any)
if(exists("con")){ DBI::dbDisconnect(con) }

# create connection to MySQL database
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  db_host = "127.0.0.1",
  user = db_creds$uid,
  password = db_creds$pwd,
  dbname = "db_prova"
)

input_data <- tbl(con, "lor_matchup_table") %>%
  collect()

tbl <- input_data %>% 
  group_by(archetype_1) %>% 
  summarise(winrate = weighted.mean(winrate, w = n, na.rm = TRUE), n = sum(n, na.rm = TRUE), .groups = "drop") %>% 
  mutate(playrate = n / sum(n, na.rm = TRUE)) %>% 
  filter(playrate >= 0.01) %>% 
  select(-n) %>% 
  mutate(freq_score = playrate*100 / max(playrate)) %>% 
  mutate(power_score = ((winrate + max(winrate) - 1) * 100) / (2*max(winrate) - 1)) %>% 
  mutate(meta_score = (power_score + freq_score) / 2) %>%
  mutate(meta_score = ifelse(meta_score < 0, 0.001, meta_score)) %>% 
  arrange(-meta_score) %>% 
  mutate(tier = case_when(
    meta_score >= 75 ~ "S",
    meta_score >= 50 ~ "A",
    meta_score >= 25 ~ "B",
    TRUE ~ "C"
  )) %>% 
  select(-c(freq_score, power_score))

reactbl <- reactable(
  data = tbl %>%
    relocate(tier, .before = everything()) %>%
    mutate(score_color = get_score_color(meta_score)) %>% 
    rename_with(~str_remove(., pattern = "_1")) %>% 
    rename_with(~str_replace_all(., pattern = "_", replacement = " ")) %>% 
    rename_with(str_to_title),
  columns = list(
    Winrate = colDef(
      defaultSortOrder = "desc",
      cell = function(value) {
        value <- paste0(format(value * 100, digits = 3, nsmall = 1), "%")
        bar_chart(value, width = value, fill = "#fc5185", background = "#e1e1e1")
      },
      align = "left"
    ),
    Playrate = colDef(
      defaultSortOrder = "desc",
      cell = function(value) {
        width <- paste0(value * 100 / max(tbl$playrate), "%")
        value <- paste0(format(value * 100, digits = 3, nsmall = 1), "%")
        bar_chart(value, width = width, fill = "#3fc1c9")
      },
      align = "left"
    ),
    `Meta Score` = colDef(
      defaultSortOrder = "desc",
      cell = cell_func,
      html = TRUE,
      width = 140,
      align = "center"
    ),
    Tier = colDef(
      cell = function(value) {
        image_src <- switch(
          tolower(value),
          "s" = "https://cdn-icons-png.flaticon.com/512/3600/3600950.png",
          "a" = "https://cdn-icons-png.flaticon.com/512/3595/3595030.png",
          "b" = "https://cdn-icons-png.flaticon.com/512/3600/3600910.png",
          "c" = "https://cdn-icons-png.flaticon.com/512/3600/3600912.png",
          "d" = "https://cdn-icons-png.flaticon.com/512/3600/3600914.png"
        )
        image <- img(src = image_src, height = "32px", alt = paste0("Tier ", value))
        tagList(
          div(style = list(display = "inline-block", width = "32px"), image)
        )
      },
      sortable = FALSE,
      align = "center"
    ),
    `Score Color` = colDef(show = FALSE)
  ),
  defaultPageSize = min(30, nrow(tbl))
)

reactbl$width  <- "100%"
reactbl$sizingPolicy$browser$padding <- 10

htmlwidgets::saveWidget(reactbl, "/home/balco/dev/lor-meta-report/templates/tierlist.html", background = "inherit")

system('ssh balco@lor-meta.com "mkdir -p /home/balco/www/assets/tierlist"')
system("scp -r /home/balco/dev/lor-meta-report/templates/tierlist.html balco@lor-meta.com:/home/balco/www/assets/tierlist")
system("scp -r /home/balco/dev/lor-meta-report/templates/tierlist.html balco@lor-meta.com:/home/balco/www/_site/assets/tierlist")
