server <- function(input, output, session) {
  
  # define reactive values used in the dashboard
  input_data <- reactiveValues(
    decks = tibble(),
    check_decks = numeric(),
    n_games = 0
  )
  
  # input choices at first landing
  input_choices <- reactive({
    
    req(input$i_rank)
    
    patch <- tbl(con, "utils_patch_history") %>% 
      collect() %>% 
      arrange(desc(release_date)) %>% 
      mutate(new_change = lag(change)) %>% 
      replace_na(list(new_change = 0)) %>% 
      mutate(cum_change = cumsum(new_change)) %>% 
      filter(cum_change == min(cum_change)) %>%
      arrange(release_date) %>% 
      pull(patch) %>% 
      paste0(collapse = ", ")

    suppressWarnings(
      archetypes <- tbl(con, 'ranked_patch_archetypes') %>% 
        {if(input$i_rank == "master") filter(., is_master == 1) else . } %>%
        group_by(archetype) %>% 
        summarise(match = sum(match, na.rm = TRUE), .groups = "drop") %>%
        filter(match >= 100) %>% 
        arrange(-match) %>%
        pull(archetype)
    )

    suppressWarnings(
      input_data$n_games <- tbl(con, "ranked_patch_archetypes") %>% 
        {if(input$i_rank == "master") filter(., is_master == 1) else . } %>%
        group_by("tmp") %>% 
        summarise(rows = sum(match, na.rm = TRUE), .groups = "drop") %>% 
        mutate(rows = rows / 2) %>% # I was counting games twice
        pull(rows)
    )

    return(
      list(
        archetypes = archetypes,
        patch = patch
      )
    )
    
  })
  
  # update inputs when one changes ----
  
  # update all inputs when data loads from server
  observeEvent(input_data$n_games > 0, {
    updateSelectizeInput(inputId = "player_1", choices = c("", input_choices()$archetypes))
    updateSelectizeInput(inputId = "player_2", choices = c("", input_choices()$archetypes))
    updateSelectizeInput(inputId = "player_3", choices = c("", input_choices()$archetypes))
    updateSelectizeInput(inputId = "oppone_1", choices = c("", input_choices()$archetypes))
    updateSelectizeInput(inputId = "oppone_2", choices = c("", input_choices()$archetypes))
    updateSelectizeInput(inputId = "oppone_3", choices = c("", input_choices()$archetypes))
  })
  
  # update player decks 2 and 3 when 1 is updated
  observeEvent(input$player_1, {
    updateSelectizeInput(
      inputId = "player_2",
      choices = setdiff(c("", input_choices()$archetypes), c(input$player_1, input$player_3)),
      selected = {if(input$player_2 %in% setdiff(c("", input_choices()$archetypes), c(input$player_1, input$player_3))) input$player_2 else ""}
    )
    updateSelectizeInput(
      inputId = "player_3",
      choices = setdiff(c("", input_choices()$archetypes), c(input$player_1, input$player_2)),
      selected = {if(input$player_3 %in% setdiff(c("", input_choices()$archetypes), c(input$player_1, input$player_2))) input$player_3 else ""}
    )
    
  })
  
  # update player decks 1 and 3 when 2 is updated
  observeEvent(input$player_2, {
    updateSelectizeInput(
      inputId = "player_1",
      choices = setdiff(c("", input_choices()$archetypes), c(input$player_2, input$player_3)),
      selected = {if(input$player_1 %in% setdiff(c("", input_choices()$archetypes), c(input$player_2, input$player_3))) input$player_1 else ""}
    )
    updateSelectizeInput(
      inputId = "player_3",
      choices = setdiff(c("", input_choices()$archetypes), c(input$player_1, input$player_2)),
      selected = {if(input$player_3 %in% setdiff(c("", input_choices()$archetypes), c(input$player_1, input$player_2))) input$player_3 else ""}
    )
    
  })
  
  # update player decks 1 and 2 when 3 is updated
  observeEvent(input$player_3, {
    updateSelectizeInput(
      inputId = "player_1",
      choices = setdiff(c("", input_choices()$archetypes), c(input$player_2, input$player_3)),
      selected = {if(input$player_1 %in% setdiff(c("", input_choices()$archetypes), c(input$player_2, input$player_3))) input$player_1 else ""}
    )
    updateSelectizeInput(
      inputId = "player_2",
      choices = setdiff(c("", input_choices()$archetypes), c(input$player_1, input$player_3)),
      selected = {if(input$player_2 %in% setdiff(c("", input_choices()$archetypes), c(input$player_1, input$player_3))) input$player_2 else ""}
    )
    
  })
  
  # update opponent decks 2 and 3 when 1 is updated
  observeEvent(input$oppone_1, {
    updateSelectizeInput(
      inputId = "oppone_2",
      choices = setdiff(c("", input_choices()$archetypes), c(input$oppone_1, input$oppone_3)),
      selected = {if(input$oppone_2 %in% setdiff(c("", input_choices()$archetypes), c(input$oppone_1, input$oppone_3))) input$oppone_2 else ""}
    )
    updateSelectizeInput(
      inputId = "oppone_3",
      choices = setdiff(c("", input_choices()$archetypes), c(input$oppone_1, input$oppone_2)),
      selected = {if(input$oppone_3 %in% setdiff(c("", input_choices()$archetypes), c(input$oppone_1, input$oppone_2))) input$oppone_3 else ""}
    )
    
  })
  
  # update opponent decks 1 and 3 when 2 is updated
  observeEvent(input$oppone_2, {
    updateSelectizeInput(
      inputId = "oppone_1",
      choices = setdiff(c("", input_choices()$archetypes), c(input$oppone_2, input$oppone_3)),
      selected = {if(input$oppone_1 %in% setdiff(c("", input_choices()$archetypes), c(input$oppone_2, input$oppone_3))) input$oppone_1 else ""}
    )
    updateSelectizeInput(
      inputId = "oppone_3",
      choices = setdiff(c("", input_choices()$archetypes), c(input$oppone_1, input$oppone_2)),
      selected = {if(input$oppone_3 %in% setdiff(c("", input_choices()$archetypes), c(input$oppone_1, input$oppone_2))) input$oppone_3 else ""}
    )
    
  })
  
  # update opponent decks 1 and 2 when 3 is updated
  observeEvent(input$oppone_3, {
    updateSelectizeInput(
      inputId = "oppone_1",
      choices = setdiff(c("", input_choices()$archetypes), c(input$oppone_2, input$oppone_3)),
      selected = {if(input$oppone_1 %in% setdiff(c("", input_choices()$archetypes), c(input$oppone_2, input$oppone_3))) input$oppone_1 else ""}
    )
    updateSelectizeInput(
      inputId = "oppone_2",
      choices = setdiff(c("", input_choices()$archetypes), c(input$oppone_1, input$oppone_3)),
      selected = {if(input$oppone_2 %in% setdiff(c("", input_choices()$archetypes), c(input$oppone_1, input$oppone_3))) input$oppone_2 else ""}
    )
    
  })
  
  # extract data when button is pressed ----
  
  observeEvent(input$button, {
    
    inputs <- c(input$player_1, input$player_2, input$player_3, input$oppone_1, input$oppone_2, input$oppone_3)
    
    input_data$check_decks = sum(inputs == "")
    
    input_data$decks <- tbl(con, "ranked_patch_matchups") %>%
      {if(input$i_rank == "master") filter(., is_master == 1) else .} %>%
      filter(archetype_1 %in% c(local(input$player_1), local(input$player_2), local(input$player_3)),
             archetype_2 %in% c(local(input$oppone_1), local(input$oppone_2), local(input$oppone_3))) %>% 
      collect() %>% 
      distinct() # remove duplicate rows for mirror matchups
    
    input_data$decks <- input_data$decks %>% 
      group_by(archetype_1, archetype_2) %>% 
      summarise(
        winrate = weighted.mean(winrate, w = n),
        n = sum(n, na.rm = TRUE), 
        .groups = "drop"
      )

  })
  
  # render outputs ----
  
  # data loaded?
  output$data_loading <- reactive({ return(input_data$n_games > 0) })
  outputOptions(output, 'data_loading', suspendWhenHidden = FALSE)

  # all inputs selected?
  output$quality_check <- reactive({ return(input_data$check_decks) })
  outputOptions(output, 'quality_check', suspendWhenHidden = FALSE)
  
  # input decks
  output$input_p1 <- renderUI({ selectInput(inputId = "player_1", label = "Player Deck #1:", choices = "") })
  output$input_p2 <- renderUI({ selectInput(inputId = "player_2", label = "Player Deck #2:", choices = "") })
  output$input_p3 <- renderUI({ selectInput(inputId = "player_3", label = "Player Deck #3:", choices = "") })
  output$input_o1 <- renderUI({ selectInput(inputId = "oppone_1", label = "Opponent Deck #1:", choices = "") })
  output$input_o2 <- renderUI({ selectInput(inputId = "oppone_2", label = "Opponent Deck #2:", choices = "") })
  output$input_o3 <- renderUI({ selectInput(inputId = "oppone_3", label = "Opponent Deck #3:", choices = "") })
  
  # Button to launch analysis
  output$input_btn <- renderUI({

    actionButton(inputId = "button", label = "Show Stats!", icon("search"), 
                 style="color: #000; background-color: #ffffff; border-color: #2e6da4")
    
  })
  
  # text output with a couple of info
  output$caption <- renderText({
    
    sprintf("Patch %s - Total number of ranked games collected: %s", input_choices()$patch, scales::comma(input_data$n_games, accuracy = 1))
    
  })
  
  # plot output
  output$matchups <- renderImage({
    
    req(input$button, input_data$check_decks == 0)
    
    # dimensions
    width  <- session$clientData$output_matchups_width
    
    # temp file to save the output (deleted later).
    outfile <- tempfile(fileext = '.png')
    
    # Generate the PNG
    png(outfile, width = 1080, height = 1080*9/16)
    
    p <- input_data$decks %>% 
      mutate(bin = cut(winrate, c(0, 0.4, 0.45, 0.55, 0.6, 1), include.lowest = TRUE)) %>%
      ggplot(aes(x = archetype_2, y = archetype_1)) +
      geom_tile(aes(fill = bin), color = "grey90", size = 1, stat = "identity") +
      shadowtext::geom_shadowtext(aes(label = scales::percent(winrate, accuracy = .1)), color = "white", size = 20, na.rm = TRUE) +
      shadowtext::geom_shadowtext(aes(label = ifelse(n<50, "⚠️ Low sample size ⚠️", "")), color = "white", size = 8, vjust = 3, na.rm = TRUE) +
      theme_classic(base_size = 30) +
      labs(x = element_blank(), y = element_blank()) +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10), position = "top") +
      theme(legend.position = "none") +
      scale_fill_manual(values = c("[0,0.4]" = "#B81D13", "(0.4,0.45]" = "coral2", "(0.45,0.55]" = "#EFB700", "(0.55,0.6]" = "#149414", "(0.6,1]" = "#046507"), na.value = "grey90")
    
    plot(p)
    
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = width,
         height = width*9/16)
  }, deleteFile = TRUE)

  # suggested ban
  output$best_ban <- renderText({
    
    req(input$button, input_data$check_decks == 0)
    
    low_wr <- input_data$decks %>% 
      group_by(archetype_2) %>% 
      summarise(winrate = mean(winrate), .groups = "drop") %>% 
      slice_min(n = 1, order_by = winrate) %>% 
      pull(archetype_2) %>% 
      paste(collapse = ", ")
    
    sprintf("Suggested ban: %s", low_wr)
    
  })
  
}