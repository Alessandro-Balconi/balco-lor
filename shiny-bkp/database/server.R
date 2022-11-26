# Server
server <- function(input, output, session){
  
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  # empty container of values
  data <- reactiveValues(
    tbl       = tibble(),
    counter_1 = 0,
    counter_2 = 0
  )
  
  # has inputs finished loading?
  output$input_loading <- reactive({ return(data$counter_1) })
  outputOptions(output, 'input_loading', suspendWhenHidden = FALSE)
  
  # has data finished loading?
  output$data_loading <- reactive({ return(data$counter_2) })
  outputOptions(output, 'data_loading', suspendWhenHidden = FALSE)
  
  # list of tables to use as input  
  output$list_tables <- renderUI({ selectInput(inputId = "i_tbl", label = 'Choose table:', choices = DBI::dbListTables(con)) })
  
  # has inputs finished loading?
  finish_inputs <- observeEvent(input$i_tbl, { data$counter_1 <- data$counter_1 + 1 })
  
  # get info from user inputs
  get_info <- observeEvent(input$get_info, {
    
    data$tbl = tbl(con, input$i_tbl) %>% 
      head(10) %>% 
      collect()
    
    data$counter_2 <- data$counter_2 + 1
    
  })
  
  # output
  output$main_table <- renderDT({
    
    req(input$get_info)
    
    datatable(
      data = data$tbl,
      options = dt_options,
      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;', htmltools::em('Example of data:')),
      callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
      rownames = FALSE
    )
    
  })
  
}