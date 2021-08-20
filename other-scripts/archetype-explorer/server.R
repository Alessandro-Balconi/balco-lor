server <- function(input, output, session) {
  
  # # close previous connections to MySQL database (if any)
  # if(exists("con")){ DBI::dbDisconnect(con) }
  # 
  # # create connection to MySQL database
  # con <- DBI::dbConnect(
  #   RMySQL::MySQL(),
  #   db_host = "127.0.0.1",
  #   user = "balco",
  #   password = "Macosanes0!",
  #   dbname = "db_prova"
  # )
  # 
  # # when app is closed, disconnect from server
  # onStop(function() {
  #   DBI::dbDisconnect(con)
  # })
  # 
  # define reactive values used in the dashboard
  input_data <- reactiveValues(
    decks = tibble(),
    decklists = tibble(),
    n_games = 0,
    n_button = 0
  )
  
  # input choices
  input_choices <- reactive({
    
    suppressWarnings(
      archetypes <- tbl(con, "lor_matchup_table") %>% 
        group_by(archetype_1) %>% 
        summarise(match = sum(n, na.rm = TRUE), .groups = "drop") %>%
        filter(match >= 1000) %>% 
        collect() %>% 
        arrange(-match) %>%
        pull(archetype_1)
    )
    
    suppressWarnings(
      input_data$n_games <- tbl(con, "lor_matchup_table") %>% 
        group_by("tmp") %>% 
        summarise(rows = sum(n, na.rm = TRUE), .groups = "drop") %>% 
        collect() %>%
        mutate(rows = rows / 2) %>% # I was counting games twice
        pull(rows)
    )
    
    patch <- tbl(con, "lor_match_info_na") %>% 
      distinct(game_version) %>%
      collect() %>% 
      mutate(across(game_version, ~word(string = ., start = 2, end = -2, sep = "_"))) %>% 
      separate(col = game_version, into = c("version", "patch"), sep = "\\_", convert = TRUE) %>% 
      mutate(last_patch = version*100+patch) %>% 
      slice_max(n = 1, order_by = last_patch, with_ties = FALSE) %>% 
      unite(col = value, version, patch, sep = ".") %>%
      pull(value)
    
    return(
      list(
        archetypes = archetypes,
        patch = patch
      )
    )
    
  })
  
  # update all inputs when data loads from server
  observeEvent(input_data$n_games > 0, {
    updateSelectizeInput(inputId = "i_arch", choices = c("", input_choices()$archetypes))
  })
  
  # extract data when button is pressed ----
  
  observeEvent(input$button, {
    
    input_data$decks <- tbl(con, "lor_matchup_table") %>%
      filter(archetype_1 == local(input$i_arch)) %>%
      select(-archetype_1) %>% 
      collect() %>% 
      distinct() # remove duplicate rows for mirror matchups
    
    input_data$decklists <- tbl(con, "lor_decklists") %>% 
      filter(archetype == local(input$i_arch)) %>%
      select(-archetype) %>% 
      collect()
    
    input_data$n_button <- input_data$n_button + 1
    
  })
  
  # data loaded?
  output$data_loading <- reactive({ return(input_data$n_games > 0) })
  outputOptions(output, 'data_loading', suspendWhenHidden = FALSE)
  
  # how many times was the button pressed?
  output$times_btn <- reactive({ return(input_data$n_button) })
  outputOptions(output, 'times_btn', suspendWhenHidden = FALSE)
  
  # input decks
  output$input_p1 <- renderUI({ selectInput(inputId = "i_arch", label = "Select Archetype:", choices = "") })
  
  # Button to launch analysis
  output$input_btn <- renderUI({
    
    actionButton(inputId = "button", label = "Show Stats!", icon("search"), 
                 style="color: #000; background-color: #ffffff; border-color: #2e6da4")
    
  })
  
  # number of games collected
  output$num_games <- renderText({
    
    req(input$button > 0)
    
    n_games <- input_data$decks %>% 
      summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>% 
      mutate(n = scales::comma(n, accuracy = 1)) %>% 
      pull(n)
    
    mean_wr <- input_data$decks %>% 
      summarise(wr = weighted.mean(winrate, w = n, na.rm = TRUE)) %>%
      mutate(wr = scales::percent(wr, accuracy = .1)) %>% 
      pull(wr)
    
    paste0("Patch: ", input_choices()$patch, " - Games: ", n_games, " - Winrate: ", meanwr)
    
  })
  
  # Main table with matchups
  output$reactbl_matchup <- renderReactable({
    
    req(input$button > 0)
    
    tbl <- reactable(
      data = input_data$decks %>% 
        filter(n > 10) %>%
        arrange(-n) %>%
        rename(match = n, opponent = archetype_2) %>% 
        rename_with(str_to_title),
      columns = list(
        Winrate = colDef(
          defaultSortOrder = "desc",
          cell = function(value) {
            value <- paste0(format(value * 100, digits = 3, nsmall = 1), "%")
            bar_chart(value, width = value, fill = "#fc5185", background = "#e1e1e1")
          },
          align = "left"
        ),
        Match = colDef(
          defaultSortOrder = "desc",
          cell = function(value) {
            width <- paste0(value * 100 / max(input_data$decks$n), "%")
            value <- format(value, big.mark = ",")
            bar_chart(value, width = width, fill = "#3fc1c9")
          },
          align = "left"
        )
      )
    )
    
  })
  
  # Main table with decklists
  output$reactbl_decks <- renderReactable({
    
    req(input$button > 0)
    
    tbl <- reactable(
      data = input_data$decklists %>% 
        arrange(-match) %>%
        rename_with(~str_replace_all(., pattern = "_", replacement = " ")) %>% 
        rename_with(str_to_title),
      columns = list(
        Winrate = colDef(
          defaultSortOrder = "desc",
          cell = function(value) {
            value <- paste0(format(value * 100, digits = 3, nsmall = 1), "%")
            bar_chart(value, width = value, fill = "#fc5185", background = "#e1e1e1")
          },
          align = "left"
        ),
        Match = colDef(
          defaultSortOrder = "desc",
          cell = function(value) {
            width <- paste0(value * 100 / max(input_data$decklists$match), "%")
            value <- format(value, big.mark = ",")
            bar_chart(value, width = width, fill = "#3fc1c9")
          },
          align = "left"
        ),
        `Deck Code` = colDef(html = TRUE, cell = function(value) {
          sprintf('<a href="https://lor.runeterra.ar/decks/code/%s" target="_blank">%s</a>', value, str_trunc(value, width = 18))
        })
      )
    )
    
  })
  
}