library(dplyr)
library(tidyr)
library(stringr)

# import list of cards
cards <- lorr::get_cards_data(
  select = c(
    "regionRefs", 
    "name",
    "cardCode", 
    "supertype", 
    "collectible",
    "set",
    "subtypes",
    "associatedCardRefs",
    "descriptionRaw"
  )
) %>% 
  filter(collectible) %>% 
  select(-collectible) %>% 
  mutate(across(
    where(is.list), 
    function(x) purrr::map_chr(.x = x, .f = str_flatten, collapse = "__")
  )) %>% 
  separate(
    col = regionRefs, 
    into = c('region_1', "region_2"), 
    sep = "__", 
    fill = 'right'
  ) %>% 
  as_tibble()

server <- function(input, output, session) {
  
  # initialize server ----------------------------------------------------------
  
  # define reactive values used in the dashboard
  input_data <- reactiveValues(
    deck_code = as.character(),
    show_advanced_options = 0,
    champs = cards %>% 
      filter(supertype == "Champion")
  )
  
  # initially hide advanced options by default
  output$advanced_options <- reactive({ return(input_data$show_advanced_options) })
  outputOptions(output, 'advanced_options', suspendWhenHidden = FALSE)

  # show or hide advanced options based on value
  observeEvent(input$show_options, { input_data$show_advanced_options <- 1 })
  observeEvent(input$hide_options, { input_data$show_advanced_options <- 0 })
  
  observeEvent(input$nchamps, {
    updateNumericInput(inputId = 'ncards', value = 40 - input$nchamps)
  })
  
  # extract data when button is pressed ----------------------------------------
  
  observeEvent(input$button, {
    
    # probabilities of 1ofs, 2ofs, 3 ofs
    probas = c(
      max(input$proba1, 1e-9), # this can't be 0 for mathematicl reasons
      input$proba2,
      input$proba3
    )
    
     force_champs <- input_data$champs %>%
       filter(name %in% c('Varus', 'Seraphine', 'Vayne'))
    
    # initialize deck with one random champion
    deck <- tibble(
      card = sample(input_data$champs$cardCode, size = 1), 
      #card = sample(force_champs$cardCode, size = 1), 
      count = sample(
        1:(min(3, input$nchamps)),
        size = 1,
        prob = probas[1:(min(3, input$nchamps))]
      )
      #count = 3
    )
    
    # check the regions already set
    allocated_regions <- input_data$champs %>% 
      filter(cardCode %in% deck$card) %>% 
      filter(is.na(region_2)) %>% 
      pull(region_1) %>% 
      unique()
    
    # check the regions "semi-set" (those of double region champs)
    semialloc_regions <- input_data$champs %>% 
      filter(cardCode %in% deck$card) %>% 
      pivot_longer(
        cols = c(region_1, region_2), 
        names_to = NULL, 
        values_drop_na = TRUE
      ) %>% 
      pull(value) %>% 
      setdiff(allocated_regions) %>% 
      unique()
    
    # add other champs until we reach 6 --------------------------------------------
    while(sum(deck$count) < input$nchamps){
      
      # keep only allowed champions based on restrictions above
      allowed_champs <- input_data$champs %>% 
        # remove already picked champs
        filter(!cardCode %in% deck$card) %>%
        # if we already have a runeterra champ, remove the others
        {if("Runeterra" %in% allocated_regions){
          filter(., str_detect(region_1, "Runeterra", negate = TRUE))
        }  else {. } } %>% 
        # if we already have set both regions, keep only champs from those regions
        {if(length(allocated_regions) == 2){
          filter(., region_1 %in% allocated_regions | 
                   region_2 %in% allocated_regions)
        }  else {. } } %>% 
        # if one region is set, and the other is semi-set, keep only these regions
        {if(length(allocated_regions) == 1 & length(semialloc_regions) > 0){
          filter(., region_1 %in% c(allocated_regions, semialloc_regions) & 
                   is.na(region_2))
        }  else {. } } %>% 
        # this is not perfect, but helps me avoiding too many cases
        {if(length(allocated_regions) == 0 & length(semialloc_regions) > 0){
          filter(., region_1 %in% semialloc_regions &
                   is.na(region_2)
                   )
        }  else {. } }
      
      
      # sample one champ among the allowed ones
      add_c <- tibble(
        card = sample(allowed_champs$cardCode, 1),
        count = sample(
          1:(min(3, input$nchamps-sum(deck$count))), 
          size = 1, 
          prob = probas[1:(min(3, input$nchamps-sum(deck$count)))]
        )
      )
      
      # add it to deck
      deck <- bind_rows(deck, add_c)
      
      # update allocated regions
      allocated_regions <- input_data$champs %>% 
        filter(cardCode %in% deck$card) %>% 
        filter(is.na(region_2)) %>% 
        pull(region_1) %>% 
        unique()
      
      # update semi allocated regions
      semialloc_regions <- input_data$champs %>% 
        filter(cardCode %in% deck$card) %>% 
        pivot_longer(
          cols = c(region_1, region_2), 
          names_to = NULL, 
          values_drop_na = TRUE
        ) %>% 
        pull(value) %>% 
        setdiff(allocated_regions) %>% 
        unique()
      
    }
    
    # after adding al champs, define deck regions (if still needed) ----------------
    if(length(allocated_regions) != 2){
      
      # if we already have regions semiallocated, pick one of these
      if(length(semialloc_regions) > 0){
        add_r <- sample(semialloc_regions, size = 1)
        # else pick one at random (NO runeterra cause it gets more complicated)
      } else {
        add_r <- cards %>% 
          select(region_1, region_2) %>% 
          pivot_longer(
            cols = c(region_1, region_2), 
            names_to = NULL, 
            values_to = "nameRef",
            values_drop_na = TRUE
          ) %>% 
          distinct() %>% 
          filter(!nameRef %in% c(allocated_regions, 'Runeterra')) %>% 
          sample_n(size = 1) %>% 
          pull(nameRef)
      }
      
      # add to allocated regions
      allocated_regions <- c(allocated_regions, add_r)

    }
    
    # add cards til we reach 40 ----------------------------------------------------
    
    # list of cards in the allowed regions
    allowed_cards <- cards %>% 
      filter(supertype == '') %>%
      filter(region_1 %in% allocated_regions |
               region_2 %in% allocated_regions)
    
    # if we have a runeterra champion, also allow his cards
    if("Runeterra" %in% allocated_regions){
      
      # runeterra champ in the deck
      runeterra_champ <- deck %>%
        left_join(input_data$champs, by = c('card' = 'cardCode')) %>%
        filter(region_1 == 'Runeterra') %>%
        pull(name)
      
      if(runeterra_champ == "Jhin"){
       
        # for Jhin we need skill cards
        skills_cards <- lorr::get_cards_data(
          select = c(
            "cardCode",
            "keywords"
          )
        ) %>%
          mutate(keywords = purrr::map_chr(keywords, str_flatten, "__")) %>%
          filter(str_detect(keywords, "Skill")) %>%
          pull(cardCode) %>%
          substr(start = 1, stop = 7) %>%
          unique()
        
      }
      
      # these champs also allow these cards
      also_allowed_cards <- switch(
        runeterra_champ,
        "Bard" = cards %>% filter(str_detect(associatedCardRefs, '06RU001T3')),
        "Jhin" = cards %>% filter(cardCode %in% skills_cards),
        "Kayn" = cards %>% filter(str_detect(subtypes, "CULTIST")),
        "Varus" = cards %>% filter(str_detect(subtypes, "CULTIST")),
        "Jax" = cards %>% filter(str_detect(subtypes, "WEAPONMASTER")),
        "Evelynn" = cards %>% filter(str_detect(descriptionRaw, 'ummon a random Husk'))
      )
      
      also_allowed_cards <- also_allowed_cards %>% 
        pull(cardCode)
      
      # get data for these cards
      also_allowed_cards <- cards %>%
        filter(supertype == '') %>%
        filter(cardCode %in% also_allowed_cards)
      
      # bind them to the allowed cards data
      allowed_cards <- allowed_cards %>%
        bind_rows(also_allowed_cards) %>%
        distinct()
      
      # add weight to runeterra cards
      # TBD
      
    }
    
    # keep adding until we reach 40 --------------------------------------------
    allowed_cards_left <- allowed_cards %>% 
      filter(!cardCode %in% deck$card)
    
    while(sum(deck$count) < (input$nchamps+input$ncards) &
          nrow(allowed_cards_left) > 2){
      
      # remove from pool cards already in the deck
      allowed_cards_left <- allowed_cards %>% 
        filter(!cardCode %in% deck$card)
      
      # sample one card among the allowed ones
      add_c <- tibble(
        card = sample(allowed_cards_left$cardCode, 1),
        count = sample(
          1:(min(3, (input$nchamps+input$ncards)-sum(deck$count))), 
          size = 1, 
          prob = probas[1:(min(3, (input$nchamps+input$ncards)-sum(deck$count)))]
        )
      )
      
      # add it to deck
      deck <- bind_rows(deck, add_c)
      
    }
    
    #input_data$deck <- deck
    
    input_data$deck_code <- deck %>% 
      rename(cardcode = card) %>% 
      lordecks::get_code_from_decklist_df()
    
  })
  
  # render outputs ------------------------------------------------------------
  
  # suggested ban
  output$text_msg <- renderText({
    
    req(input$button)
    
    input_data$deck_code
    
  })

  # suggested ban
  output$text_link <- renderUI({
    
    req(input$button)
    
    tagList(
      '', 
      a(
        'Mobalytics', 
        href = sprintf(
          "https://lor.mobalytics.gg/decks/code/%s", 
          input_data$deck_code
        ),
        target="_blank",
        style = "font-size: 20px"
      ),
      ' - ',
      a(
        'Mastering Runeterra', 
        href = sprintf(
          "https://masteringruneterra.com/deck/%s", 
          input_data$deck_code
        ),
        target="_blank",
        style = "font-size: 20px"
      )
    )
    
  })
  
}