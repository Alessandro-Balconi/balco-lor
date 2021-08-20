#source("./global.R")

fluidPage(
  
  titlePanel("EU Masters Player Analytics"),
  useShinydashboard(),
  
  sidebarPanel(
    width = 12,
    
    fluidRow(
      
      column(
        width = 3,
        selectInput(inputId = "select_country", label = "Select Country:", choices = unique(eum_players$country), selected = "Italy"),
        conditionalPanel(
          condition = "input.button_push == 0",
          actionBttn(inputId = "button_push", label = "Check Country", icon = icon("globe-europe"), style = "simple", color = "success")
        )
      ),
      column(
        width = 6,
        "Data collected from ranked games.", br(),
        "Players are taken from LoR Masters Europe leaderboard and may be wrong; colpa della small indie company.", br(),
        "There are probably a lot of bugs. I'll fix them when I've time."
      )
    )
  ),
  
  mainPanel(
    
    width = 12,
    
    conditionalPanel(
      condition = "input.button_push != 0",
      
      fluidRow(
        column(
          width = 4,
          uiOutput("box1")
        ),
        column(
          width = 4,
          uiOutput("box2")
        ),
        column(
          width = 4,
          uiOutput("box3")
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          box(
            title = "Country Playrate", 
            width = NULL, 
            plotOutput("plot_country"), 
            solidHeader = TRUE, 
            collapsible = TRUE, 
            status = "primary"
          )
        ),
        column(
          width = 6,
          box(
            title = "Winrate",
            width = NULL,
            plotOutput("plot_country2"),
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "info"
          )
        )
      )
    )
  )
)