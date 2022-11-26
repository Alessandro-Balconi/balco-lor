# UI ---------------------------------------------------------------------------
ui <- fluidPage(
  
  # App title ----
  titlePanel("LoR - Ranked Data Viewer"),
  
  sidebarPanel(
    width = 4,
    strong("Please wait until you see the 'Load data' button at the bottom of this panel!"), br(), 
    #strong("In the next weeks I might replace the 'is_master' field with a 'player_rank' one since I'm planning to start collecting data of people below plat; maybe I'll do it, maybe I won't; just saying that it might change in the future."), br(), 
    "Only last 30 days of ranked data are available to collect.", br(), br(),
    #"I've temporarily removed the 'All' choice from regions to prevent crashes due to loading too much data at once.", br(), br(),
    selectInput(inputId = "region", label = "Region:", choices = list('Europe' = 'europe', 'Americas' = 'americas', 'Asia-Pacific' = 'asia')),
    #selectInput(inputId = "region", label = "Region:", choices = list('All' = 'all', 'Europe' = 'europe', 'Americas' = 'americas', 'Asia-Pacific' = 'asia')),
    radioButtons("range_choice", label = "Filter data by:", choices = list("Dates" = 1, "Patches" = 2), selected = 1),
    conditionalPanel(condition = "input.range_choice == 1", uiOutput(outputId = 'filter_dates')),
    conditionalPanel(condition = "input.range_choice == 2", uiOutput(outputId = 'filter_patches')),
    conditionalPanel(
      condition = "output.input_loading > 0",
      br(), 
      actionButton("get_info", "Load data!")
    ),
    conditionalPanel(
      condition = "(output.data_loading == 0 && input.get_info > 0)",
      br(),
      img(src = "loading.gif", align = "left", height = '18px', style = "margin-right:3px"),
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
