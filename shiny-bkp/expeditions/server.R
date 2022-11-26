# Server
server <- function(input, output, session){
  
  # empty list to fill with infos
  data <- reactiveValues(
    error_msg = "",
    cards = tibble(),
    counter_1 = 0
  )
  
  # has inputs finished loading?
  output$input_loading <- reactive({ return(data$counter_1) })
  outputOptions(output, 'input_loading', suspendWhenHidden = FALSE)
  
  output$filter_dates <- renderUI({
    
    # dates to filter between
    suppressWarnings(
      date_range <- tbl(con, 'expedition_cards') %>% 
        group_by(1) %>% 
        summarise(mints = min(day, na.rm = TRUE), maxts = max(day, na.rm = TRUE)) %>% 
        collect()
    )
    
    start_date = max(Sys.Date()-days(60), ymd(date_range$mints))
    end_date   = ymd(date_range$maxts)
    
    sliderInput(
      inputId = "dates",
      label   = 'Choose dates:',
      min     = start_date,
      max     = end_date,
      value   = c(start_date, end_date)
    )
    
  })
  
  finish_inputs <- observeEvent(input$dates, {
    
    data$counter_1 <- data$counter_1 + 1
    
  })
  
  # get info from player name and tag
  get_info <- observeEvent(input$get_info, {
    
    # error message
    data$error_msg <- paste0("No data found. Check that you are using the correct filters, or contact me on twitter if you believe there is an error: @Balco21")
    
    # get cards info using user-inputed filters
    suppressWarnings(
      data$cards <- tbl(con, 'expedition_cards') %>%
        filter(is_master >= local(input$is_master)) %>%
        {if(!input$include_bots) filter(., opponent == "human") else .} %>%
        {if(input$only_champions) filter(., rarity == "Champion") else .} %>%
        filter(day >= local(input$dates[1]), day <= local(input$dates[2])) %>% 
        group_by(name) %>%
        summarise(
          across(c(n, win, loss, tie), sum),
          .groups = "drop"
        ) %>%
        mutate(winrate = win / n) %>% 
        collect()
    )
    
    suppressWarnings(
      data$n_tot <- tbl(con, 'expedition_ngames') %>%
        filter(is_master >= local(input$is_master)) %>%
        {if(!input$include_bots) filter(., opponent == "human") else .} %>%
        filter(day >= local(input$dates[1]), day <= local(input$dates[2])) %>%
        group_by(1) %>% 
        summarise(ntot = sum(n)) %>%
        collect() %>% 
        pull()
    )
    
    data$cards <- data$cards %>%
      mutate(playrate = n / data$n_tot) %>%
      arrange(desc(n))
    
  })
  
  # check if inputed player exists
  output$render_tbl <- reactive({ return(nrow(data$cards) > 0) })
  outputOptions(output, "render_tbl", suspendWhenHidden = FALSE)
  
  # error message if player not found
  output$text_msg <- renderText({ 
    
    req(input$get_info)
    
    data$error_msg 
    
  })
  
  # n of games
  output$ngames_msg <- renderText({ 
    
    req(input$get_info)
    
    paste0('# of Decks: ', scales::comma(data$n_tot)) 
    
  })
  
  # output
  output$main_table <- renderDT({
    
    req(input$get_info)
    
    datatable(
      data = data$cards %>% 
        rename_with(str_to_title),
      options = dt_options,
      extensions = 'Buttons',
      callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
      rownames = FALSE) %>% 
      formatPercentage(columns = c('Winrate', 'Playrate'), digits = 2) %>% 
      formatRound(columns = c('N', 'Win', 'Loss', 'Tie'), digits = 0)
    
  }, server = FALSE)
  
}
