# UI ---------------------------------------------------------------------------
ui <- fluidPage(
  
  # App title ----
  titlePanel("LoR - Database Viewer"),
  
  sidebarPanel(
    width = 4,
    uiOutput(outputId = 'list_tables'),
    conditionalPanel(
      condition = "output.input_loading > 0",
      br(), 
      actionButton("get_info", "Check sample of data!")
    ),
    conditionalPanel(
      condition = "(output.data_loading == 0 && input.get_info > 0)",
      br(),
      img(src = "loading.gif", align = "left", height = '18px', style = "margin-right:3px"),
      "Loading..."
    )
  ),
  
  mainPanel(
    width = 8,
    DT::DTOutput("main_table")
  )
)

# Wrap your UI with secure_app
ui <- secure_app(ui)