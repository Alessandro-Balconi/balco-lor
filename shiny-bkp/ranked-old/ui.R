# UI ---------------------------------------------------------------------------
ui <- fluidPage(
  
  # App title ----
  titlePanel("LoR - Ranked Data Viewer"),
  
  sidebarPanel(
    width = 4,
    strong("Please wait until you see the 'Load data' button at the bottom of this panel!"), 
    br(), 
    selectInput(
      inputId = "region", 
      label = "Region:", 
      choices = list(
        'All' = 'all',
        'Europe' = 'europe', 
        'Americas' = 'americas', 
        'Asia-Pacific' = 'asia'
    )),
    uiOutput(outputId = 'filter_patches'),
    conditionalPanel(
      condition = "output.input_loading > 0",
      br(), 
      actionButton("get_info", "Load data!")
    ),
    conditionalPanel(
      condition = "(output.data_loading == 0 && input.get_info > 0)",
      br(),
      img(
        src = "loading.gif", 
        align = "left", 
        height = '18px', 
        style = "margin-right:3px"
      ),
      "Loading..."
    ),
    conditionalPanel(
      condition = "output.render_tbl && input.get_info > 0",
      br(), br(), 
      uiOutput(outputId = 'dl_btn'), br(), 
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
