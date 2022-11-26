# UI ---------------------------------------------------------------------------
ui <- fluidPage(
  
  # App title ----
  titlePanel("A very awesome deckroll"),
  br(),
  
  mainPanel(
    width = 12,
    
    sidebarPanel(
      width = 4,
      
      conditionalPanel(
        condition = 'output.advanced_options != 1 && output.advanced_options != 0',
        img(src = "loading.gif", align = "left", height = '20px', style = 'margin-left:3px;margin-top:7px'),
        p('Loading, please wait...', style = 'padding-left:30px;padding-top:7px')
      ),
      
      column(
        width = 3,
        conditionalPanel(
          condition = 'output.advanced_options == 1 || output.advanced_options == 0',
          actionButton("button", "Roll!", icon = icon('dice'))
        )
      ),
      
      column(
        width = 9,
        conditionalPanel(
          condition = 'output.advanced_options == 1',
          actionButton("hide_options", "Hide Advanced Options", icon = icon('minus'))
        ),
        conditionalPanel(
          condition = 'output.advanced_options == 0',
          actionButton("show_options", "Show Advanced Options", icon = icon('plus'))
        ),
        align = 'right'
      ),
      
      br(), br(),
      conditionalPanel(
        condition = 'output.advanced_options == 1',
        br(),
        column(
          width = 4,
          numericInput("proba1", "Prob. of 1x (%)", min = 0, max = 100, value = 10, step = 10)
        ),
        column(
          width = 4,
          numericInput("proba2", "Prob. of 2x (%)", min = 0, max = 100, value = 30, step = 10)
        ),
        column(
          width = 4,
          numericInput("proba3", "Prob. of 3x (%)", min = 0, max = 100, value = 60, step = 10)
        ),
        conditionalPanel(
          condition = 'input.proba1 + input.proba2 + input.proba3 != 100',
          strong("WARNING: sum of probabilities is not 100%; values will be rescaled."),
          br(), br(),
        ),
        fluidRow(
          column(
            width = 6,
            sliderInput("nchamps", "Champions in Deck", min = 1, max = 6, value = 6, step = 1)
          ),
          column(
            width = 6,
            numericInput("ncards",  "Other Cards in Deck", min = 0, max = 100, value = 34, step = 1),
          )
        )
      )
    ),
    
    mainPanel(
      width = 8,
      textOutput("text_msg"),
      br(),
      uiOutput('text_link')
    )
  )
  
)
