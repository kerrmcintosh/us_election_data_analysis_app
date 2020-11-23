library(shiny)

server <- function(input, output) {
 
   #reactive shared filtering
  filtered_year <- reactive({
    vote_share %>%
  filter(elect_year == input$year)
  })
  
  

  #-------------------TAB1 server code--------------------
  #TAB1 UsMap

  output$which_year<- renderText({
    paste0("Which election year would you like to view?")
  })
  
  output$which_state<- renderText({
    paste0("Select State to View Key Stats")
  })
  
  output$title_map<- renderText({
    paste0(input$year, " US Presidential Election Results")
  })
  
  output$us_map <- renderPlot({
    pivoted_sf %>%
      filter(election_year == input$year) %>% 
      ggplot() +
      
      geom_sf(mapping = aes(fill = result),
              color = "#ffffff", size = 0.25) + 
      scale_fill_manual(values = c("#5390D9", "#D90429")) +
      labs(fill = "") +
      coord_sf(datum = NA) +
      geom_sf_text(data = get_urbn_labels(map = "states", sf = TRUE), 
                   aes(label = state_abbv), 
                   size = 3) + 
      theme_void() 
  }) 
  
  output$vote_share<- renderText({
    paste0("Candidate Share of Overall Vote: ", input$year)
  })
  
  output$share <- renderPlot({
    filtered_year() %>%
      ggplot() +
      aes(x = candidate, y = votes_perc, fill= party) +
      geom_col(position = "dodge", width=0.75) +
      labs(x = "") +
      theme_light() +
      theme(axis.title.x = element_blank(),
            axis.text.y = element_text(size = 12,  margin=margin(0,0,0,0)),
            axis.text.x = element_blank(),
            legend.position = "none",
            line = element_blank(),
            panel.border = element_blank(),
            aspect.ratio = 3/5) + 
      scale_fill_manual(values = c("#5390D9", "#D90429")) +
      coord_flip() +
      geom_text(aes(label = paste0(votes_perc,"%")), position = position_stack(vjust = 0.8), size = 5 ,colour = "#ffffff")
  })    
  
  
  output$elec_vote<- renderText({
    paste0("Electoral Votes: ", input$year)
  })
  
  output$vote_270 <- renderText({
    paste0("270 required to win")
  })
  
  output$vote <- renderPlot({
    filtered_year() %>%
      ggplot() +
      aes(x = candidate, y = electoral_votes, fill=party ) +
      geom_col(position = "dodge", width=0.75) +
      labs(x = "") +
      theme_light() +
      theme(axis.title.x = element_blank(),
            axis.text.y = element_text(size = 12,  margin=margin(0,0,0,0)),
            axis.text.x = element_blank(),
            legend.position = "none",
            line = element_blank(),
            panel.border = element_blank(),
            aspect.ratio = 3/5) + 
      scale_fill_manual(values = c("#5390D9", "#D90429")) +
      geom_hline(yintercept =270, , colour="#002266", linetype="dashed") +
      coord_flip() +
      geom_text(aes(label = paste0(electoral_votes)), position = position_stack(vjust = 0.8), size = 5 ,colour = "#ffffff")
  })    
  
  output$state_share <- renderText({
    paste0("Share of Vote in States Won: ", input$year)
  })
  
  output$state_sh <- renderPlot({
    average_state_vote %>%
      filter(election_year == input$year) %>% 
      ggplot() +
      aes(x = candidate, y = votes, fill= result) +
      geom_col(position = "dodge", width=0.75) +
      labs(x = "") +
      theme_light() +
      theme(axis.title.x = element_blank(),
            axis.text.y = element_text(size = 12,  margin=margin(0,0,0,0)),
            axis.text.x = element_blank(),
            legend.position = "none",
            line = element_blank(),
            panel.border = element_blank(),
            aspect.ratio = 3/5) + 
      scale_fill_manual(values = c("#5390D9", "#D90429")) +
      coord_flip() +
      geom_text(aes(label = paste0(votes,"%")), position = position_stack(vjust = 0.8), size = 5 ,colour = "#ffffff")
  })  

  output$state_key_inds<- DT::renderDataTable({
    key_indicators %>% 
      filter(state == input$state_tab1) %>% 
      select(-"state")
  },options = list(
    pageLength = 16))
  
  output$compare <- renderText({
    paste0("Compare key stats in States won (2020)")
  })
  
  output$key_stat_name <- renderText({
    paste0(input$indicator_bars)
  })
  
  output$state_split <- renderPlot({
    key_indicators_bar %>%
      filter(indicator == input$indicator_bars) %>% 
      ggplot() +
      aes(x = party2020, y = mean, fill= party2020) +
      geom_col(position = "dodge", width=0.75) +
      # labs(x = "") +
      theme_light() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            legend.position = "none",
            line = element_blank(),
            panel.border = element_blank(),
            aspect.ratio = 4/5) + 
      scale_fill_manual(values = c("#D90429", "#5390D9")) +
      geom_text(aes(label = paste0(mean)), position = position_stack(vjust = 0.8), size = 5 ,colour = "#ffffff")
    
  })  
  
  
  #------------------TAB1 server code END-----------------
  
  #-------------------TAB2 server code--------------------
  #TAB2 Conor
  output$title_tab2<- renderText({
    paste0("Typical Demographic Factors and their Effect on the 2020 US Election")
  })
  
  output$sex_text <- renderText({
    paste0("Female population larger than male population?")
  })
  
  output$age_text <- renderText({
    paste0("Is the proportion of 65+ population higher or lower than average?")
  })
  
  output$age_text2 <- renderText({
    paste0("Is the proportion of 19-25 years population higher or lower than average?")
  })
  
  output$hisp_text <- renderText({
    paste0("Above average Hispanic population?")
  })
  
  output$black_text <- renderText({
    paste0("Above average Black population?")
  })
  
  output$college_text <- renderText({
    paste0("Above 10 Electoral College votes?")
  })
  
  output$sex_plot <- renderPlot({
    if(input$sex == "Yes"){
      demo[-c(52,53), ] %>%
        filter(female > male) %>%
        mutate(totalvotes2020 = joe_biden + donald_trump) %>%
        rename("Biden" = "joe_biden", "Trump" = "donald_trump") %>%
        melt(id.vars = "state",
             measure.vars = c("Biden",
                              "Trump")) %>%
        ggplot(aes(x = reorder(state, value),
                   y = value)) +
       geom_col(aes(fill = variable), width=0.6)+
        coord_flip()+
        scale_fill_manual(values = c("#5390D9", "#D90429"))+
        labs(x = "States", y = "Votes (Dem and Rep only)",
             title = "Number of Votes for Presidential Candidate by State",
             subtitle = "Democratic and Republican Parties Only")+
        scale_y_continuous(labels = paste0(ylab, "M"),
                           breaks = 10^6 * ylab)+
        theme(legend.title = element_blank(),
              legend.justification=c(0.95,0.25),
              legend.position=c(0.95,0.25))
    }else{
      demo[-c(52,53), ] %>%
        filter(male > female) %>%
        mutate(totalvotes2020 = joe_biden + donald_trump) %>%
        rename("Biden" = "joe_biden", "Trump" = "donald_trump") %>%
        melt(id.vars = "state",
             measure.vars = c("Biden",
                              "Trump")) %>%
        ggplot(aes(x = reorder(state, value),
                   y = value)) +
        geom_col(aes(fill = variable), width=0.6)+
        coord_flip()+
        scale_fill_manual(values = c("#5390D9", "#D90429")) +
        labs(x = "States", y = "Votes (Dem and Rep only)",
             title = "Number of Votes for Presidential Candidate by State",
             subtitle = "Democratic and Republican Parties Only")+
        scale_y_continuous(labels = paste0(ylab, "M"),
                           breaks = 10^6 * ylab)+
        theme(legend.title = element_blank(),
              legend.justification=c(0.95,0.25),
              legend.position=c(0.95,0.25))
    }
  })
  
  output$age_plot <- renderPlot({
    if(input$age == "Higher than average"){
      demo[-c(52,53), ] %>%
        filter(adults_65plus >= 0.16) %>%
        mutate(totalvotes2020 = joe_biden + donald_trump) %>%
        rename("Biden" = "joe_biden", "Trump" = "donald_trump") %>%
        melt(id.vars = "state",
             measure.vars = c("Biden",
                              "Trump")) %>%
        ggplot(aes(x = reorder(state, value),
                   y = value)) +
        geom_col(aes(fill = variable), width=0.6)+
        coord_flip()+
        scale_fill_manual(values = c("#5390D9", "#D90429"))+
        labs(x = "States", y = "Votes (Dem and Rep only)",
             title = "Number of Votes for Presidential Candidate by State",
             subtitle = "Democratic and Republican Parties Only")+
        scale_y_continuous(labels = paste0(ylab, "M"),
                           breaks = 10^6 * ylab)+
        theme(legend.title = element_blank(),
              legend.justification=c(0.95,0.25),
              legend.position=c(0.95,0.25))
    }else{
      demo[-c(52,53), ] %>%
        filter(adults_65plus < 0.16) %>%
        mutate(totalvotes2020 = joe_biden + donald_trump) %>%
        rename("Biden" = "joe_biden", "Trump" = "donald_trump") %>%
        melt(id.vars = "state",
             measure.vars = c("Biden",
                              "Trump")) %>%
        ggplot(aes(x = reorder(state, value),
                   y = value)) +
        geom_col(aes(fill = variable), width=0.6)+
        coord_flip()+
        scale_fill_manual(values = c("#5390D9", "#D90429"))+
        labs(x = "States", y = "Votes (Dem and Rep only)",
             title = "Number of Votes for Presidential Candidate by State",
             subtitle = "Democratic and Republican Parties Only")+
        scale_y_continuous(labels = paste0(ylab, "M"),
                           breaks = 10^6 * ylab)+
        theme(legend.title = element_blank(),
              legend.justification=c(0.95,0.25),
              legend.position=c(0.95,0.25))
    }
  })
  
  output$age2_plot <- renderPlot({
    if(input$age2 == "Higher than average"){
      demo[-c(52,53), ] %>%
        filter(adults_19_25 >= 0.086) %>%
        mutate(totalvotes2020 = joe_biden + donald_trump) %>%
        rename("Biden" = "joe_biden", "Trump" = "donald_trump") %>%
        melt(id.vars = "state",
             measure.vars = c("Biden",
                              "Trump")) %>%
        ggplot(aes(x = reorder(state, value),
                   y = value)) +
        geom_col(aes(fill = variable), width=0.6)+
        coord_flip()+
        scale_fill_manual(values = c("#5390D9", "#D90429"))+
        labs(x = "States", y = "Votes (Dem and Rep only)",
             title = "Number of Votes for Presidential Candidate by State",
             subtitle = "Democratic and Republican Parties Only")+
        scale_y_continuous(labels = paste0(ylab, "M"),
                           breaks = 10^6 * ylab)+
        theme(legend.title = element_blank(),
              legend.justification=c(0.95,0.25),
              legend.position=c(0.95,0.25))
    }else{
      demo[-c(52,53), ] %>%
        filter(adults_19_25 < 0.086) %>%
        mutate(totalvotes2020 = joe_biden + donald_trump) %>%
        rename("Biden" = "joe_biden", "Trump" = "donald_trump") %>%
        melt(id.vars = "state",
             measure.vars = c("Biden",
                              "Trump")) %>%
        ggplot(aes(x = reorder(state, value),
                   y = value)) +
        geom_col(aes(fill = variable), width=0.6)+
        coord_flip()+
        scale_fill_manual(values = c("#5390D9", "#D90429"))+
        labs(x = "States", y = "Votes (Dem and Rep only)",
             title = "Number of Votes for Presidential Candidate by State",
             subtitle = "Democratic and Republican Parties Only")+
        scale_y_continuous(labels = paste0(ylab, "M"),
                           breaks = 10^6 * ylab)+
        theme(legend.title = element_blank(),
              legend.justification=c(0.95,0.25),
              legend.position=c(0.95,0.25))
    }
  })
  
  output$hispanic_plot <- renderPlot({
    if(input$hispanic == "Yes"){
      demo[-c(52,53), ] %>%
        filter(is_hispanic >= 1187691) %>%
        mutate(totalvotes2020 = joe_biden + donald_trump) %>%
        rename("Biden" = "joe_biden", "Trump" = "donald_trump") %>%
        melt(id.vars = "state",
             measure.vars = c("Biden",
                              "Trump")) %>%
        ggplot(aes(x = reorder(state, value),
                   y = value)) +
        geom_col(aes(fill = variable), width=0.6)+
        coord_flip()+
        scale_fill_manual(values = c("#5390D9", "#D90429"))+
        labs(x = "States", y = "Votes (Dem and Rep only)",
             title = "Number of Votes for Presidential Candidate by State",
             subtitle = "Democratic and Republican Parties Only")+
        scale_y_continuous(labels = paste0(ylab, "M"),
                           breaks = 10^6 * ylab)+
        theme(legend.title = element_blank(),
              legend.justification=c(0.95,0.25),
              legend.position=c(0.95,0.25))
    }else{
      demo[-c(52,53), ] %>%
        filter(is_hispanic < 1187691) %>%
        mutate(totalvotes2020 = joe_biden + donald_trump) %>%
        rename("Biden" = "joe_biden", "Trump" = "donald_trump") %>%
        melt(id.vars = "state",
             measure.vars = c("Biden",
                              "Trump")) %>%
        ggplot(aes(x = reorder(state, value),
                   y = value)) +
        geom_col(aes(fill = variable), width=0.6)+
        coord_flip()+
        scale_fill_manual(values = c("#5390D9", "#D90429"))+
        labs(x = "States", y = "Votes (Dem and Rep only)",
             title = "Number of Votes for Presidential Candidate by State",
             subtitle = "Democratic and Republican Parties Only")+
        scale_y_continuous(labels = paste0(ylab, "M"),
                           breaks = 10^6 * ylab)+
        theme(legend.title = element_blank(),
              legend.justification=c(0.95,0.25),
              legend.position=c(0.95,0.25))
    }
  })
  
  output$black_plot <- renderPlot({
    if(input$black == "Yes"){
      demo[-c(52,53), ] %>%
        filter(black >= 864217.4) %>%
        mutate(totalvotes2020 = joe_biden + donald_trump) %>%
        rename("Biden" = "joe_biden", "Trump" = "donald_trump") %>%
        melt(id.vars = "state",
             measure.vars = c("Biden",
                              "Trump")) %>%
        ggplot(aes(x = reorder(state, value),
                   y = value)) +
        geom_col(aes(fill = variable), width=0.6)+
        coord_flip()+
        scale_fill_manual(values = c("#5390D9", "#D90429"))+
        labs(x = "States", y = "Votes (Dem and Rep only)",
             title = "Number of Votes for Presidential Candidate by State",
             subtitle = "Democratic and Republican Parties Only")+
        scale_y_continuous(labels = paste0(ylab, "M"),
                           breaks = 10^6 * ylab)+
        theme(legend.title = element_blank(),
              legend.justification=c(0.95,0.25),
              legend.position=c(0.95,0.25))
    }else{
      demo[-c(52,53), ] %>%
        filter(black < 864217.4) %>%
        mutate(totalvotes2020 = joe_biden + donald_trump) %>%
        rename("Biden" = "joe_biden", "Trump" = "donald_trump") %>%
        melt(id.vars = "state",
             measure.vars = c("Biden",
                              "Trump")) %>%
        ggplot(aes(x = reorder(state, value),
                   y = value)) +
        geom_col(aes(fill = variable), width=0.6)+
        coord_flip()+
        scale_fill_manual(values = c("#5390D9", "#D90429"))+
        labs(x = "States", y = "Votes (Dem and Rep only)",
             title = "Number of Votes for Presidential Candidate by State",
             subtitle = "Democratic and Republican Parties Only")+
        scale_y_continuous(labels = paste0(ylab, "M"),
                           breaks = 10^6 * ylab)+
        theme(legend.title = element_blank(),
              legend.justification=c(0.95,0.25),
              legend.position=c(0.95,0.25))
    }
  })
  
  output$college_plot <- renderPlot({
    if(input$college_1 == "Yes"){
      demo[-c(52,53), ] %>%
        mutate(totalvotes2020 = joe_biden + donald_trump) %>%
        filter( college_votes_available >= 10) %>%
        rename("Biden" = "joe_biden", "Trump" = "donald_trump") %>%
        melt(id.vars = "state",
             measure.vars = c("Biden",
                              "Trump")) %>%
        ggplot(aes(x = reorder(state, value),
                   y = value)) +
        geom_col(aes(fill = variable), width=0.6)+
        coord_flip()+
        scale_fill_manual(values = c("#5390D9", "#D90429"))+
        labs(x = "States", y = "Votes (Dem and Rep only)",
             title = "Number of Votes for Presidential Candidate by State",
             subtitle = "Democratic and Republican Parties Only")+
        scale_y_continuous(labels = paste0(ylab, "M"),
                           breaks = 10^6 * ylab)+
        theme(legend.title = element_blank(),
              legend.justification=c(0.95,0.25),
              legend.position=c(0.95,0.25))
    }else{
      demo[-c(52,53), ] %>%
        mutate(totalvotes2020 = joe_biden + donald_trump) %>%
        filter( college_votes_available < 10) %>%
        rename("Biden" = "joe_biden", "Trump" = "donald_trump") %>%
        melt(id.vars = "state",
             measure.vars = c("Biden",
                              "Trump")) %>%
        ggplot(aes(x = reorder(state, value),
                   y = value)) +
        geom_col(aes(fill = variable), width=0.6)+
        coord_flip()+
        scale_fill_manual(values = c("#5390D9", "#D90429"))+
        labs(x = "States", y = "Votes (Dem and Rep only)",
             title = "Number of Votes for Presidential Candidate by State",
             subtitle = "Democratic and Republican Parties Only")+
        scale_y_continuous(labels = paste0(ylab, "M"),
                           breaks = 10^6 * ylab)+
        theme(legend.title = element_blank(),
              legend.justification=c(0.95,0.25),
              legend.position=c(0.95,0.25))
    }
  })
  
  output$tab_2_map <- renderPlot({
    if(input$sex == "Yes"){
      demo2 %>%
        filter(female > male) %>%
        ggplot() +
        geom_sf(mapping = aes(fill = party2020),
                color = "#ffffff", size = 0.25) +
        scale_fill_manual(values = c("#5390D9", "#D90429")) +
        labs(fill = "") +
        coord_sf(datum = NA)+
        theme_void()
    }else{
      demo2 %>%
        filter(male > female) %>%
        ggplot() +
        geom_sf(mapping = aes(fill = party2020),
                color = "#ffffff", size = 0.25) +
        scale_fill_manual(values = c("#5390D9", "#D90429")) +
        labs(fill = "") +
        coord_sf(datum = NA)+
        theme_void()
    }
  })
  
  output$tab_2_map2 <- renderPlot({
    if(input$age == "Higher than average"){
      demo2 %>%
        filter(adults_65plus >= 0.16) %>%
        ggplot() +
        geom_sf(mapping = aes(fill = party2020),
                color = "#ffffff", size = 0.25) +
        scale_fill_manual(values = c("#5390D9", "#D90429")) +
        labs(fill = "") +
        coord_sf(datum = NA)+
        theme_void()
    }else{
      demo2 %>%
        filter(adults_65plus < 0.16) %>%
        ggplot() +
        geom_sf(mapping = aes(fill = party2020),
                color = "#ffffff", size = 0.25) +
        scale_fill_manual(values = c("#5390D9", "#D90429")) +
        labs(fill = "") +
        coord_sf(datum = NA)+
        theme_void()
    }
  })
  
  output$tab_2_map3 <- renderPlot({
    if(input$age2 == "Higher than average"){
      demo2 %>%
        filter(adults_19_25 >= 0.086) %>%
        ggplot() +
        geom_sf(mapping = aes(fill = party2020),
                color = "#ffffff", size = 0.25) +
        scale_fill_manual(values = c("#5390D9", "#D90429")) +
        labs(fill = "") +
        coord_sf(datum = NA)+
        theme_void()
    }else{
      demo2 %>%
        filter(adults_19_25 < 0.086) %>%
        ggplot() +
        geom_sf(mapping = aes(fill = party2020),
                color = "#ffffff", size = 0.25) +
        scale_fill_manual(values = c("#5390D9", "#D90429")) +
        labs(fill = "") +
        coord_sf(datum = NA)+
        theme_void()
    }
  })
  
  output$tab_2_map4 <- renderPlot({
    if(input$hispanic == "Yes"){
      demo2 %>%
        filter(is_hispanic >= 1187691) %>%
        ggplot() +
        geom_sf(mapping = aes(fill = party2020),
                color = "#ffffff", size = 0.25) +
        scale_fill_manual(values = c("#5390D9", "#D90429")) +
        labs(fill = "") +
        coord_sf(datum = NA)+
        theme_void()
    }else{
      demo2 %>%
        filter(is_hispanic < 1187691) %>%
        ggplot() +
        geom_sf(mapping = aes(fill = party2020),
                color = "#ffffff", size = 0.25) +
        scale_fill_manual(values = c("#5390D9", "#D90429")) +
        labs(fill = "") +
        coord_sf(datum = NA)+
        theme_void()
    }
  })
  
  output$tab_2_map5 <- renderPlot({
    if(input$black == "Yes"){
      demo2 %>%
        filter(black >= 864217.4) %>%
        ggplot() +
        geom_sf(mapping = aes(fill = party2020),
                color = "#ffffff", size = 0.25) +
        scale_fill_manual(values = c("#5390D9", "#D90429")) +
        labs(fill = "") +
        coord_sf(datum = NA)+
        theme_void()
    }else{
      demo2 %>%
        filter(black < 864217.4) %>%
        ggplot() +
        geom_sf(mapping = aes(fill = party2020),
                color = "#ffffff", size = 0.25) +
        scale_fill_manual(values = c("#5390D9", "#D90429")) +
        labs(fill = "") +
        coord_sf(datum = NA)+
        theme_void()
    }
  })
  
  output$tab_2_map6 <- renderPlot({
    if(input$college_1 == "Yes"){
      demo2 %>%
        filter(college_votes_available > 10) %>%
        ggplot() +
        geom_sf(mapping = aes(fill = party2020),
                color = "#ffffff", size = 0.25) +
        scale_fill_manual(values = c("#5390D9", "#D90429")) +
        labs(fill = "") +
        coord_sf(datum = NA)+
        theme_void()
    }else{
      demo2 %>%
        filter(college_votes_available < 10) %>%
        ggplot() +
        geom_sf(mapping = aes(fill = party2020),
                color = "#ffffff", size = 0.25) +
        scale_fill_manual(values = c("#5390D9", "#D90429")) +
        labs(fill = "") +
        coord_sf(datum = NA)+
        theme_void()
    }
  })
  
  
  
  #-------------------TAB2 server code END----------------- 
  
  
  #-------------------TAB3 server code--------------------
  #TAB3 David 
  
  #cases as a
    output$case_death <- renderPlot({
      key_indicators_bar %>%
        filter(indicator == input$covid_radio) %>% 
        ggplot() +
        aes(x = party2020, y = mean, fill= party2020) +
        geom_col(position = "dodge", width=0.75) +
        # labs(x = "") +
        theme_light() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              legend.position = "none",
              line = element_blank(),
              panel.border = element_blank(),
              aspect.ratio = 4/5) + 
        scale_fill_manual(values = c("#D90429", "#5390D9")) +
        geom_text(aes(label = paste0(mean)), position = position_stack(vjust = 0.8), size = 5 ,colour = "#ffffff")
    })    
  
  output$covid_chat<- renderText({
    paste0("Covid Stats (mean) by Candidate's States Won")
  })
  
  output$covid_stat<- renderText({
    ifelse(input$covid_radio == "Covid Cases as Proportion of Population (%)",  paste0("2.22%"), paste0("2.84%"))
  })
  
  output$covid_title<- renderText({
    paste0(input$covid_radio)
  })
  
  output$covid_us<- renderText({
    paste0("US average: ",input$covid_radio)
  })
  
    # covid %>%
    #   select(state, tot_cases, tot_death, party2020) %>%
    #   group_by(party2020) %>%
    #   mutate(avg_cases = mean(tot_cases)) %>%
    #   mutate(avg_deaths = mean(tot_death)) %>%
    #   ggplot() +
    #   geom_col(aes(x = avg_cases, y = party2020), fill = "green") +
    #   geom_col(aes(x = avg_deaths, y = party2020), fill = "red") +
    #   ggtitle("Mean Deaths and Cases across all States ") +
    #   labs(x = "Mean Deaths & Cases", y = "Party")

  output$select_votes_text <- renderText({
    paste0("Select number of Electoral College votes")
  })    

  output$total_cases <- renderPlot({
    if(input$select_num_votes == "3-6"){
      covid %>%
        filter(college_votes_available >= 3 & college_votes_available <= 6) %>% 
        ggplot(aes(x = tot_cases, y = reorder(state, tot_cases, fill = party2020), fill = party2020 )) +
        geom_col(width = .6) +
        geom_vline(xintercept = 176725.6, linetype = "dotted") +
        ggtitle("No. of Cases per state by the Political Party Elected (2020)") +
        theme(legend.position = c(0.8, 0.5)) +
        theme_grey(base_size = 8) +
        scale_fill_discrete(name = "Winner") +
        labs(x = "Total Cases", y = "States") +
        scale_fill_manual(values = c("#5390D9", "#D90429"))+
        theme(legend.title = element_blank(),
              legend.justification=c(0.95,0.25),
              legend.position=c(0.95,0.25))
    }else if (input$select_num_votes == "7-10"){
      covid %>%
        filter(college_votes_available >= 7 & college_votes_available <= 10) %>% 
        ggplot(aes(x = tot_cases, y = reorder(state, tot_cases, fill = party2020), fill = party2020 )) +
        geom_col(width = .6) +
        geom_vline(xintercept = 176725.6, linetype = "dotted") +
        ggtitle("No. of Cases per state by the Political Party Elected (2020)") +
        theme(legend.position = c(0.8, 0.5)) +
        theme_grey(base_size = 8) +
        scale_fill_discrete(name = "Winner") +
        labs(x = "Total Cases", y = "States") +
        scale_fill_manual(values = c("#5390D9", "#D90429"))+
        theme(legend.justification=c(0.95,0.25),
              legend.position=c(0.95,0.25))
    }else if (input$select_num_votes == "11-14"){
      covid %>%
        filter(college_votes_available >= 11 & college_votes_available <= 14) %>% 
        ggplot(aes(x = tot_cases, y = reorder(state, tot_cases, fill = party2020), fill = party2020 )) +
        geom_col(width = .6) +
        geom_vline(xintercept = 176725.6, linetype = "dotted") +
        ggtitle("No. of Cases per state by the Political Party Elected (2020)") +
        theme(legend.position = c(0.8, 0.5)) +
        theme_grey(base_size = 8) +
        scale_fill_discrete(name = "Winner") +
        labs(x = "Total Cases", y = "States") +
        scale_fill_manual(values = c("#5390D9", "#D90429"))+
        theme(legend.title = element_blank(),
              legend.justification=c(0.95,0.25),
              legend.position=c(0.95,0.25))
    }else if (input$select_num_votes == "15-19"){
      covid %>%
        filter(college_votes_available >= 15 & college_votes_available <= 19) %>% 
        ggplot(aes(x = tot_cases, y = reorder(state, tot_cases, fill = party2020), fill = party2020 )) +
        geom_col(width = .6) +
        geom_vline(xintercept = 176725.6, linetype = "dotted") +
        ggtitle("No. of Cases per state by the Political Party Elected (2020)") +
        theme(legend.position = c(0.8, 0.5)) +
        theme_grey(base_size = 8) +
        scale_fill_discrete(name = "Winner") +
        labs(x = "Total Cases", y = "States") +
        scale_fill_manual(values = c("#5390D9", "#D90429"))+
        theme(legend.title = element_blank(),
              legend.justification=c(0.95,0.25),
              legend.position=c(0.95,0.25))
    }else if (input$select_num_votes == "20+"){
      covid %>%
        filter(college_votes_available >= 20) %>% 
        ggplot(aes(x = tot_cases, y = reorder(state, tot_cases, fill = party2020), fill = party2020 )) +
        geom_col(width = .6) +
        geom_vline(xintercept = 176725.6, linetype = "dotted") +
        ggtitle("No. of Cases per state by the Political Party Elected (2020)") +
        theme(legend.position = c(0.8, 0.5)) +
        theme_grey(base_size = 8) +
        scale_fill_discrete(name = "Winner") +
        labs(x = "Total Cases", y = "States") +
        scale_fill_manual(values = c("#5390D9", "#D90429"))+
        theme(legend.title = element_blank(),
              legend.justification=c(0.95,0.25),
              legend.position=c(0.95,0.25))
    }
  })
  
  
  output$total_deaths <- renderPlot({
    if(input$select_num_votes == "3-6"){
      covid %>%
        filter(college_votes_available >= 3 & college_votes_available <= 6) %>% 
        ggplot(aes(x = tot_death, y = reorder(state, tot_death), fill = party2020)) +
        geom_col(width = .6) +
        geom_vline(xintercept = 4058.824, linetype = "dotted") +
        ggtitle("Total No. of Deaths (Mean line also noted)") +
        theme(legend.position = c(0.9, 0.4)) +
        theme_grey(base_size = 8) +
        scale_fill_discrete(name = "Winner") +
        labs(x = "Total Deaths", y = "States") +
        scale_fill_manual(values = c("#5390D9", "#D90429"))+
        theme(legend.title = element_blank(),
              legend.justification=c(0.95,0.25),
              legend.position=c(0.95,0.25))
    }else if (input$select_num_votes == "7-10"){
      covid %>%
        filter(college_votes_available >= 7 & college_votes_available <= 10) %>% 
        ggplot(aes(x = tot_death, y = reorder(state, tot_death), fill = party2020)) +
        geom_col(width = .6) +
        geom_vline(xintercept = 4058.824, linetype = "dotted") +
        ggtitle("Total No. of Deaths (Mean line also noted)") +
        theme(legend.position = c(0.9, 0.4)) +
        theme_grey(base_size = 8) +
        scale_fill_discrete(name = "Winner") +
        labs(x = "Total Deaths", y = "States") +
        scale_fill_manual(values = c("#5390D9", "#D90429"))+
        theme(legend.title = element_blank(),
              legend.justification=c(0.95,0.25),
              legend.position=c(0.95,0.25))
    }else if (input$select_num_votes == "11-14"){
      covid %>%
        filter(college_votes_available >= 11 & college_votes_available <= 14) %>% 
        ggplot(aes(x = tot_death, y = reorder(state, tot_death), fill = party2020)) +
        geom_col(width = .6) +
        geom_vline(xintercept = 4058.824, linetype = "dotted") +
        ggtitle("Total No. of Deaths (Mean line also noted)") +
        theme(legend.position = c(0.9, 0.4)) +
        theme_grey(base_size = 8) +
        scale_fill_discrete(name = "Winner") +
        labs(x = "Total Deaths", y = "States") +
        scale_fill_manual(values = c("#5390D9", "#D90429"))+
        theme(legend.title = element_blank(),
              legend.justification=c(0.95,0.25),
              legend.position=c(0.95,0.25))
    }else if (input$select_num_votes == "15-19"){
      covid %>%
        filter(college_votes_available >= 15 & college_votes_available <= 19) %>% 
        ggplot(aes(x = tot_death, y = reorder(state, tot_death), fill = party2020)) +
        geom_col(width = .6) +
        geom_vline(xintercept = 4058.824, linetype = "dotted") +
        ggtitle("Total No. of Deaths (Mean line also noted)") +
        theme(legend.position = c(0.9, 0.4)) +
        theme_grey(base_size = 8) +
        scale_fill_discrete(name = "Winner") +
        labs(x = "Total Deaths", y = "States") +
        scale_fill_manual(values = c("#5390D9", "#D90429")) +
        theme(legend.justification=c(0.95,0.25),
              legend.position=c(0.95,0.25))
    }else if (input$select_num_votes == "20+"){
      covid %>%
        filter(college_votes_available >= 20) %>% 
        ggplot(aes(x = tot_death, y = reorder(state, tot_death), fill = party2020)) +
        geom_col(width = .6) +
        geom_vline(xintercept = 4058.824, linetype = "dotted") +
        ggtitle("Total No. of Deaths (Mean line also noted)") +
        theme(legend.position = c(0.9, 0.4)) +
        theme_grey(base_size = 8) +
        scale_fill_discrete(name = "Winner") +
        labs(x = "Total Deaths", y = "States") +
        scale_fill_manual(values = c("#D90429", "#5390D9"))+
        theme(legend.title = element_blank(),
              legend.justification=c(0.95,0.25),
              legend.position=c(0.95,0.25))
    }



  })

  # output$cases_vs_deaths <- renderPlot({
  #   covid %>%
  #     ggplot() +
  #     geom_col(aes(y = reorder(state, desc(state)), x = perc_covid_cases), fill = "green")+
  #     geom_col(aes(y = states, x = perc_covid_deaths), fill = "red") +
  #     ggtitle("Cases vs Deaths per State (2020)") +
  #     theme(legend.position = c(0.8, 0.5)) +
  #     theme_grey(base_size = 8) +
  #     scale_fill_discrete(name = "Political Party") +
  #     labs(x = "Deaths vs Cases", y = "States")
  # })

  output$lm_cases_vs_deaths <- renderPlot({
    covid %>%
      ggplot(aes(x = tot_cases, y = tot_death)) +
      geom_point(aes(x = tot_cases, y = tot_death, color = party2020), size = 5) +
      theme_minimal() +
      theme(legend.position = "none") +
      geom_smooth(method = "lm", colour = "black") +
      scale_colour_manual(values = c("#D90429", "#5390D9")) +
      labs(x = "Total Cases", y = "Total Deaths") +
      ggtitle("Linear Model on Cases vs Deaths") +
      theme(legend.position = c(0.8, 0.3),
            legend.title = element_blank()) 
    
  })

  
  #-------------------TAB3 server code END----------------- 
  
  
  
}

