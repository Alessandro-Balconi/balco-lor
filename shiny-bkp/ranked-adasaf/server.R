# Server
server <- function(input, output, session){
  
  # empty list to fill with infos
  data <- reactiveValues(
    error_msg = "",
    cards = tibble(),
    counter_1 = 0,
    counter_2 = 0,
    db_patch = tibble()
  )
  
  # check if there is data for the user inputs
  output$render_tbl <- reactive({ return(nrow(data$cards) > 0) })
  outputOptions(output, "render_tbl", suspendWhenHidden = FALSE)
  
  # has inputs finished loading?
  output$input_loading <- reactive({ return(data$counter_1) })
  outputOptions(output, 'input_loading', suspendWhenHidden = FALSE)
  
  # has data finished loading?
  output$data_loading <- reactive({ return(data$counter_2) })
  outputOptions(output, 'data_loading', suspendWhenHidden = FALSE)
  
  output$filter_dates <- renderUI({
    
    # dates to filter between
    start_date = Sys.Date()-days(15)
    end_date   = Sys.Date()
    
    sliderInput(
      inputId = "dates",
      label   = 'Choose dates:',
      min     = start_date,
      max     = end_date,
      value   = c(start_date, end_date)
    )
    
  })
  
  output$filter_patches <- renderUI({
    
    # dates to filter between
    data$db_patch <- tbl(con, 'utils_patch_history') %>% 
      mutate(last_date = lead(release_date)) %>% 
      filter(last_date >= local(Sys.Date() - lubridate::days(35)) | is.na(last_date)) %>% 
      arrange(desc(release_date)) %>% 
      select(patch, patch_regex) %>% 
      collect()

    selectInput(
      inputId  = "patches",
      label    = 'Choose patches:',
      choices  = data$db_patch %>% pull(patch),
      multiple = TRUE
    )
    
  })
  
  finish_inputs <- observeEvent(input$dates, {
    
    data$counter_1 <- data$counter_1 + 1
    
  })
  
  # get info from user inputs
  get_info <- observeEvent(input$get_info, {
    
    # error message
    data$error_msg <- paste0("No data found. Check that you are using the correct filters, or contact me on twitter if you believe there is an error: @Balco21")
    
    # if user chose to filter by patch, prepare patch format  
    if(input$range_choice == 2){
      
      current_patch <- data$db_patch %>% 
        filter(patch %in% input$patches) %>%
        pull(patch_regex) %>% 
        paste0(collapse = "|")
      
    }

    # get cards info using user-inputed filters
    suppressWarnings(
      data$cards <- tbl(con, 'ranked_match_metadata_30d') %>% 
        {if(input$region != 'all') filter(., region == local(input$region)) else . } %>% 
        {if(input$range_choice == 1) filter(., game_start_time_utc >= local(input$dates[1]), game_start_time_utc <= local(input$dates[2])) else .} %>% 
        {if(input$range_choice == 2) filter(., str_detect(game_version, current_patch)) else .} %>% 
        left_join(tbl(con, 'ranked_match_info_30d'), by = 'match_id') %>% 
        mutate(is_master = if_else(player_rank == 2, 1, 0)) %>% 
        select(-c(cards, player_rank, match_rank)) %>% 
        collect()
    )
    
    players <- tbl(con, 'utils_players') %>%
      {if(input$region != 'all') filter(., region == local(input$region)) else . } %>% 
      collect() %>% 
      unite(col = 'player', gameName, tagLine, sep = "#")
    
    data$cards <- data$cards %>% 
      left_join(players, by = c('puuid', 'region')) %>% 
      select(-puuid) %>% 
      relocate(player, .after = game_version)
    
    suppressWarnings(
      data$n_tot <- nrow(data$cards) / 2
    )
    
    data$counter_2 <- data$counter_2 + 1
    
  })

  # error message if data not found
  output$text_msg <- renderText({ 
    
    req(input$get_info)
    
    data$error_msg 
    
  })
  
  # n of games
  output$ngames_msg <- renderText({ 
    
    req(input$get_info)
    
    paste0('# of Matches: ', scales::comma(data$n_tot)) 
    
  })
  
  # output
  output$main_table <- renderDT({
    
    req(input$get_info)
    
    datatable(
      data = data$cards %>% 
        {if(nrow(.) > 100) sample_n(., size = 100) else . } %>% 
        rename_with(str_to_title),
      options = dt_options,
      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;', htmltools::em('Example of data:')),
      callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
      rownames = FALSE)
    
  })
  
  # If button is pressed at least once, also show a button to download the data
  output$dl_btn <- renderUI({
    
    req(input$get_info)
    
    downloadButton(
      outputId = "dl_csv", 
      label = " Download CSV", 
      icon("download"), 
      style="color: #000; background-color: #ffffff; border-color: #2e6da4"
    )
    
  })
  
  # Download data
  output$dl_csv <- downloadHandler(
    filename = function() {
      paste0("lor_ranked_data_", input$region, ".csv")
    },
    content = function(file) {
      write.csv(data$cards, file, row.names = FALSE)
    }
  )
  
}