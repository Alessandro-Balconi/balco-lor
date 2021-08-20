function(input, output) {
  
  get_data <- reactive({
    
    req(input$button_push != 0)
    
    # get players from the selected country
    focus_players <- eum_players %>% 
      filter(country == input$select_country) %>% 
      pull(name)
    
    # extract match of those players
    focus_data <- data %>% 
      filter(player_1_name %in% focus_players | player_2_name %in% focus_players)
    
    # prepare data
    focus_data <- focus_data %>% 
      mutate(focus      = if_else(player_1_name %in% focus_players, 1, 2)) %>% 
      mutate(player     = if_else(focus == 1, player_1_name, player_2_name)) %>% 
      mutate(winner     = if_else(winner == player_1_id, 1, 2)) %>% 
      mutate(won        = if_else(winner == focus, 1, 0)) %>%
      mutate(faction_1  = if_else(focus  == 1, faction_1_1, faction_2_1)) %>% 
      mutate(faction_2  = if_else(focus  == 1, faction_1_2, faction_2_2)) %>% 
      mutate(deck       = if_else(focus  == 1, deck_1, deck_2)) %>% 
      mutate(deck_cards = if_else(focus  == 1, deck_1_cards, deck_2_cards)) %>% 
      select(matchid, player, won, faction_1, faction_2, deck, deck_cards)
    
    # prepare deck_cards
    focus_data <- focus_data %>% 
      mutate(across(deck_cards, ~map(., .f = str_split, pattern = " "))) %>%
      mutate(across(deck_cards, ~map(., .f = 1)))
    
    # add champions info
    champs_data <- focus_data$deck_cards %>% 
      set_names(value = focus_data$matchid) %>%
      map_dfr(as_tibble, .id = "matchid") %>%
      filter(value %in% champ_codes$X) %>% 
      group_by(matchid) %>%
      arrange(value) %>% 
      mutate(id = row_number()) %>%
      ungroup() %>%
      pivot_wider(names_from = id, values_from = value, names_prefix = "champ_") %>% 
      mutate(across(starts_with("champ_"), function(x) map_chr(x, get_name_from_champ)))
    
    # finish preparing data
    focus_data <- focus_data %>% 
      left_join(champs_data, by = "matchid")
    
    # players
    players_data <- focus_data %>% 
      group_by(across(c(player, starts_with("champ_")))) %>% 
      summarise(match = n(), wins = sum(won), .groups = "drop") %>% 
      mutate(across(starts_with("champ_"), function(x) ifelse(is.na(x), "", x)))
    
    return(
      list(
        data         = focus_data,
        players_data = players_data
      )
    )
  })
  
  # render box 1
  output$box1 <- renderUI({
    
    req(input$button_push != 0)
    
    box(
      title = eum_players %>% filter(country == input$select_country) %>% pull(name) %>% .[1], 
      width = NULL, 
      plotOutput("plot_player1"), 
      solidHeader = TRUE, 
      collapsible = TRUE, 
      status = "primary"
    )
    
  })
  
  # render box 2
  output$box2 <- renderUI({
    
    req(input$button_push != 0)
    
    box(
      title = eum_players %>% filter(country == input$select_country) %>% pull(name) %>% .[2], 
      width = NULL, 
      plotOutput("plot_player2"), 
      solidHeader = TRUE, 
      collapsible = TRUE, 
      status = "primary"
    )
    
  })
  
  # render box 3
  output$box3 <- renderUI({
    
    req(input$button_push != 0)
    
    box(
      title = eum_players %>% filter(country == input$select_country) %>% pull(name) %>% .[3], 
      width = NULL, 
      plotOutput("plot_player3"), 
      solidHeader = TRUE, 
      collapsible = TRUE, 
      status = "primary"
    )
    
  })
  
  # plot player 1 info
  output$plot_player1 <- renderPlot({
    
    data_plot <- get_data()$players_data %>% 
      filter(player == (eum_players %>% filter(country == input$select_country) %>% pull(name) %>% .[1])) %>% 
      unite(col = "deck", starts_with("champ_"), sep = " ") %>% 
      mutate(deck = ifelse(match < 0.1*sum(match), "Other", deck)) %>%
      group_by(deck) %>% 
      summarise(match = sum(match), wins = sum(wins), .groups = "drop")
    
    data_plot %>% 
      ggplot(aes(x = "", y = match, fill = deck)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0) +
      geom_text(aes(label = str_wrap(deck, width = 15)), position = position_stack(vjust = 0.5), color = "white", size = 5) +
      theme_void() +
      theme(legend.position="none") +
      labs(
        title = paste0("Number of Ranked Games: ", sum(data_plot$match)), 
        subtitle = paste0("Overall Winrate: ", scales::percent(sum(data_plot$wins) / sum(data_plot$match)))
      ) +
      scale_fill_brewer(palette="Set1")
    
  })
  
  # plot player 2 info
  output$plot_player2 <- renderPlot({
    
    data_plot <- get_data()$players_data %>% 
      filter(player == (eum_players %>% filter(country == input$select_country) %>% pull(name) %>% .[2])) %>% 
      unite(col = "deck", starts_with("champ_"), sep = " ") %>% 
      mutate(deck = ifelse(match < 0.1*sum(match), "Other", deck)) %>%
      group_by(deck) %>% 
      summarise(match = sum(match), wins = sum(wins), .groups = "drop") 
    
    data_plot %>% 
      ggplot(aes(x = "", y = match, fill = deck)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0) +
      geom_text(aes(label = str_wrap(deck, width = 15)), position = position_stack(vjust = 0.5), color = "white", size = 5) +
      theme_void() +
      theme(legend.position="none") +
      labs(
        title = paste0("Number of Ranked Games: ", sum(data_plot$match)), 
        subtitle = paste0("Overall Winrate: ", scales::percent(sum(data_plot$wins) / sum(data_plot$match)))
      ) +
      scale_fill_brewer(palette="Set1")
    
  })
  
  # plot player 3 info
  output$plot_player3 <- renderPlot({
    
    data_plot <- get_data()$players_data %>% 
      filter(player == (eum_players %>% filter(country == input$select_country) %>% pull(name) %>% .[3])) %>% 
      unite(col = "deck", starts_with("champ_"), sep = " ") %>% 
      mutate(deck = ifelse(match < 0.1*sum(match), "Other", deck)) %>%
      group_by(deck) %>% 
      summarise(match = sum(match), wins = sum(wins), .groups = "drop") 
    
    data_plot %>% 
      ggplot(aes(x = "", y = match, fill = deck)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0) +
      geom_text(aes(label = str_wrap(deck, width = 15)), position = position_stack(vjust = 0.5), color = "white", size = 5) +
      theme_void() +
      theme(legend.position="none") +
      labs(
        title = paste0("Number of Ranked Games: ", sum(data_plot$match)), 
        subtitle = paste0("Overall Winrate: ", scales::percent(sum(data_plot$wins) / sum(data_plot$match)))
      ) +
      scale_fill_brewer(palette="Set1")
    
  })
  
  # overall country info
  output$plot_country <- renderPlot({
    
    data_plot <- get_data()$players_data %>% 
      unite(col = "deck", starts_with("champ_"), sep = " ") %>%
      group_by(player) %>% 
      mutate(deck = ifelse(match < 0.1*sum(match), "Other", deck)) %>%
      ungroup() %>% 
      group_by(deck, player) %>% 
      summarise(across(where(is.numeric), sum), .groups = "drop") %>% 
      group_by(deck) %>%
      mutate(tot_match = sum(match)) %>% 
      ungroup() 
    
    tot_match <- data_plot %>% 
      group_by(deck) %>% 
      summarise(tot_match = max(tot_match), .groups = "drop")
    
    data_plot %>%
      ggplot(aes(x = reorder(deck, tot_match))) +
      geom_col(aes(fill = player, y = match), color = "white") +
      geom_text(aes(label = "", y = tot_match*1.1), size = 5) +
      geom_text(aes(label = tot_match, y = tot_match), hjust = -0.5, size = 5, data = tot_match) +
      coord_flip() +
      theme_bw() +
      theme(legend.position = "bottom") +
      labs(y = "# of Match", x = "Deck", fill = "Player")
    
  })
  # overall country info
  output$plot_country2 <- renderPlot({
    
    data_plot <- get_data()$players_data %>% 
      unite(col = "deck", starts_with("champ_"), sep = " ") %>%
      group_by(player) %>% 
      mutate(deck = ifelse(match < 0.1*sum(match), "Other", deck)) %>%
      ungroup() %>% 
      group_by(deck) %>% 
      summarise(across(where(is.numeric), sum), .groups = "keep") %>% 
      mutate(tot_match = sum(match), tot_wins = sum(wins)) %>%
      ungroup() %>% 
      mutate(winrate = tot_wins / tot_match)
    
    tot_match <- data_plot %>% 
      group_by(deck) %>% 
      summarise(winrate = max(winrate), .groups = "drop")
    
    data_plot %>%
      ggplot(aes(x = reorder(deck, winrate))) +
      geom_col(aes(y = winrate), color = "white", fill = "steelblue") +
      geom_text(aes(label = "", y = winrate*1.1), size = 5) +
      geom_text(aes(label = scales::percent(winrate, accuracy = .1), y = winrate), hjust = -0.5, size = 5, data = tot_match) +
      coord_flip() +
      theme_bw() +
      theme(legend.position = "bottom") +
      labs(y = "Winrate", x = "Deck", title = "Highest Winrate Decks")
    
  })
  
}