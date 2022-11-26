# UI ---------------------------------------------------------------------------
ui <- fluidPage(
  
  # App title ----
  titlePanel("LoR - Expeditions Data Viewer"),
  
  sidebarPanel(
    width = 4,
    "Wait until you see a slider with dates to choose from at the bottom of this panel!",
    selectInput(inputId = "is_master", label = "Player Rank:", choices = list('All' = 0, 'Plat+' = 1, 'Master' = 2)),
    checkboxInput("include_bots", "Include games with bots", value = FALSE),
    checkboxInput("only_champions", "Show only champions", value = FALSE),
    uiOutput(outputId = 'filter_dates'),
    actionButton("get_info", "Load data!"),
    conditionalPanel(
      condition = "output.render_tbl && input.get_info > 0",
      textOutput('ngames_msg')
    )
  ),
  
  mainPanel(
    width = 8,
    conditionalPanel(
      condition = "output.render_tbl && input.get_info > 0",
      DT::DTOutput("main_table")
    ),
    conditionalPanel(
      condition = "!output.render_tbl && input.get_info > 0",
      textOutput("text_msg")
    )
  )
)
