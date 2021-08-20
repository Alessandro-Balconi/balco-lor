library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(shiny)
library(reactable)
library(htmltools)
library(httr)
library(jsonlite)

# connect to db
con <- DBI::dbConnect(
  RMySQL::MySQL(),
  db_host = "127.0.0.1",
  user = "balco",
  password = "Macosanes0!",
  dbname = "db_prova"
)

# Render a bar chart with a label on the left
bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}

# latest LoL patch (needed to extract LoL champion icons)
lol_patch <- GET("https://ddragon.leagueoflegends.com/api/versions.json") %>% 
  content() %>% 
  .[[1]]

# path where latest LoL champion icons are stored
lol_icon_path <- sprintf("https://ddragon.leagueoflegends.com/cdn/%s/img/champion/", lol_patch)



# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      uiOutput(outputId = "input_p1", label = "Select Archetype:"),
      uiOutput(outputId = "input_btn")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      reactableOutput(outputId = "reactbl_matchup"),
      reactableOutput(outputId = "reactbl_decks")
    )
  )
)

server <- function(input, output, session) {
  
  # # close previous connections to MySQL database (if any)
  # if(exists("con")){ DBI::dbDisconnect(con) }
  
  # # create connection to MySQL database
  # con <- DBI::dbConnect(
  #   RMySQL::MySQL(),
  #   host = "167.71.42.85",
  #   port = 3306,
  #   user = "remotebalco",
  #   password = "Macosanes0!",
  #   dbname = "db_prova"
  # )
  
  # # when app is closed, disconnect from server
  # onStop(function() {
  #   DBI::dbDisconnect(con)
  # })
  
  # define reactive values used in the dashboard
  input_data <- reactiveValues(
    decks = tibble(),
    decklists = tibble(),
    n_games = 0
  )
  
  # input choices at first landing
  input_choices <- reactive({
    
    suppressWarnings(
      archetypes <- tbl(con, "lor_matchup_table") %>% 
        group_by(archetype_1) %>% 
        summarise(match = sum(n, na.rm = TRUE), .groups = "drop") %>%
        filter(match >= 1000) %>% 
        collect() %>% 
        arrange(-match) %>%
        pull(archetype_1)
    )
    
    suppressWarnings(
      input_data$n_games <- tbl(con, "lor_matchup_table") %>% 
        group_by("tmp") %>% 
        summarise(rows = sum(n, na.rm = TRUE), .groups = "drop") %>% 
        collect() %>%
        mutate(rows = rows / 2) %>% # I was counting games twice
        pull(rows)
    )
    
    
    return(
      list(
        archetypes = archetypes
      )
    )
    
  })
  
  # update inputs when one changes ----
  
  # update all inputs when data loads from server
  observeEvent(input_data$n_games > 0, {
    updateSelectizeInput(inputId = "i_arch", choices = c("", input_choices()$archetypes))
  })
  
  # extract data when button is pressed ----
  
  observeEvent(input$button, {
    
    input_data$decks <- tbl(con, "lor_matchup_table") %>%
      filter(archetype_1 == local(input$i_arch), n > 10) %>%
      select(-archetype_1) %>% 
      collect() %>% 
      distinct() # remove duplicate rows for mirror matchups
    
    input_data$decklists <- tbl(con, "lor_decklists") %>% 
      filter(archetype == local(input$i_arch)) %>%
      select(-archetype) %>% 
      collect()
      
  })
  
  # render outputs ----
  
  # data loaded?
  output$data_loading <- reactive({ return(input_data$n_games > 0) })
  outputOptions(output, 'data_loading', suspendWhenHidden = FALSE)
  
  # input decks
  output$input_p1 <- renderUI({ selectInput(inputId = "i_arch", label = "Select Archetype:", choices = "") })

  # Button to launch analysis
  output$input_btn <- renderUI({
    
    actionButton(inputId = "button", label = "Show Stats!", icon("search"), 
                 style="color: #000; background-color: #ffffff; border-color: #2e6da4")
    
  })
  
  # Main table with matchups
  output$reactbl_matchup <- renderReactable({
    
    req(input$button > 0)
    
    tbl <- reactable(
      data = input_data$decks %>% 
        arrange(-n) %>%
        rename(match = n, opponent = archetype_2) %>% 
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
        Match = colDef(
          defaultSortOrder = "desc",
          cell = function(value) {
            width <- paste0(value * 100 / max(input_data$decks$n), "%")
            value <- format(value, big.mark = ",")
            bar_chart(value, width = width, fill = "#3fc1c9")
          },
          align = "left"
        )
      )
    )
    
  })

  # Main table with decklists
  output$reactbl_decks <- renderReactable({
    
    req(input$button > 0)
    
    tbl <- reactable(
      data = input_data$decklists %>% 
        arrange(-match) %>%
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
        Match = colDef(
          defaultSortOrder = "desc",
          cell = function(value) {
            width <- paste0(value * 100 / max(input_data$decks$n), "%")
            value <- format(value, big.mark = ",")
            bar_chart(value, width = width, fill = "#3fc1c9")
          },
          align = "left"
        )
      )
    )
    
  })
  
}

shinyApp(ui, server)

# # images test ----
# 
# lol_patch <- GET("https://ddragon.leagueoflegends.com/api/versions.json") %>% 
#   content() %>% 
#   .[[1]]
# 
# lol_icon_path <- sprintf("https://ddragon.leagueoflegends.com/cdn/%s/img/champion/", lol_patch)
# 
# # get most recent set number (to read sets JSONs)
# last_set <- "https://dd.b.pvp.net/latest/core/en_us/data/globals-en_us.json" %>% 
#   GET() %>% 
#   content(encoding = "UTF-8") %>% 
#   fromJSON() %>% 
#   .[["sets"]] %>% 
#   mutate(set = str_extract(nameRef, pattern = "[0-9]+")) %>% 
#   mutate(set = as.numeric(set)) %>% 
#   summarise(max(set, na.rm = TRUE)) %>% 
#   pull()
# 
# # champions names / codes / images from set JSONs
# champ_names <- map_dfr(
#   .x = 1:last_set,
#   .f = function(x) {
#     sprintf("https://dd.b.pvp.net/latest/set%1$s/en_us/data/set%1$s-en_us.json", x) %>% 
#       GET() %>%  
#       content(encoding = "UTF-8") %>% 
#       fromJSON() %>% 
#       as_tibble()
#   },
#   .id = "set"
# ) %>% 
#   filter(rarity == "Champion") %>% 
#   distinct(name) %>% 
#   pull()
# 
# champ_img_names <- champ_names %>% 
#   sprintf(paste0(lol_icon_path, "%s.png"), .) %>%
#   str_remove_all(pattern = " ") %>% 
#   setNames(., nm = champ_names)
# 
# x <- input_data$decks %>% 
#   mutate(img = str_remove(archetype_2, pattern = "\\ \\(.*")) %>% 
#   mutate(img = str_replace_all(img, champ_img_names))
# 
# 
# reactable(x, columns = list(
#   img = colDef(cell = function(value) {
#     
#     image <- value %>% 
#       str_split(pattern = " ") %>% 
#       .[[1]] %>% 
#       map(.x = ., .f = ~img(src = .x, height = "24px", style = "margin-right=5px"))
#     
#     tagList(
#       div(style = list(display = "inline-block", width = "50px"), image[[1]], { if(length(image)>1) image[[2]] })
#     )
#   })
# )) 
# 
# a <- value %>% 
#   str_split(pattern = " ") %>% 
#   .[[1]] %>% 
#   map(.x = ., .f = ~img(src = .x, height = "24px")) %>% 
#   map_chr(~paste0(., collapse = " ")) %>% 
#   paste0(collapse = " ")
# 
