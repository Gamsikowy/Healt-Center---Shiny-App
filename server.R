server <- function(input, output, session) {
  
  observeEvent(input$show_desc, {
    # scraping the disease description from Wikipedia
    wikiArticle <- switch(input$illness,
                          "Diabetes" = "Diabetes",
                          "Insomnia" = "Insomnia",
                          "Depression" = "Depression_(mood)"
    )
    
    link <- paste0("https://en.wikipedia.org/wiki/", wikiArticle)
    
    page <- read_html(link)
    description <- page %>%
      html_nodes(".mw-parser-output p") %>%
      html_text()
    
    # the first element is "\n\n"
    output$show_desc <- renderText(gsub("[[0-9]+]", "", description[2:3]))
  })
  
  observeEvent(input$tabSwitch, {
    if(input$tabSwitch == "Ethnicity & Income") {
      # scraping the BMI definition from Wikipedia
      link <- "https://en.wikipedia.org/wiki/Body_mass_index"
      
      page <- read_html(link)
      description <- page %>%
        html_nodes(".mw-parser-output p") %>%
        html_text()
      
      output$bmi <- renderText(gsub("[[0-9]+]", "", description[2]))
    }
  })
  
  output$timer <- renderText({
    invalidateLater(1000, session)
    paste(Sys.time())
  })
  
  output$income <- renderPlot({
    switch(input$illness,
           "Diabetes" = { patients <- NHANES %>%
             filter(Diabetes == "Yes")
           },
           "Insomnia" = { patients <- NHANES %>%
             filter(SleepTrouble == "Yes")
           },
           "Depression" = { patients <- NHANES %>%
             filter(Depressed == "Most" | Depressed == "Several")
           }
    )
    
    patients %>%
      group_by(HHIncome) %>%
      ggplot(aes(x = HHIncome, fill = HHIncome)) +
      geom_bar() +
      scale_x_discrete(na.translate = FALSE) +
      labs(title = "Total annual gross income for the household in US dollars",
           x = "Income",
           y = "Number of people") +
      coord_flip() +
      theme(title = element_text(size = 18),
            plot.title = element_text(margin = margin(10, 0, 5, 0), hjust = .5),
            axis.title.y = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            legend.position = "none")
  })
  
  output$ethnic <- renderPlot({
    switch(input$illness,
           "Diabetes" = { patients <- NHANES %>%
             filter(Diabetes == "Yes")
           axisIllness <- "Diabetics"
           },
           "Insomnia" = { patients <- NHANES %>%
             filter(SleepTrouble == "Yes")
           axisIllness <- "Insomniacs"
           },
           "Depression" = { patients <- NHANES %>%
             filter(Depressed == "Most" | Depressed == "Several")
           axisIllness <- "Depressed people"
           }
    )
    
    patients %>%
      group_by(Race1) %>%
      ggplot(aes(x = "", fill = Race1)) +
      geom_bar(width = 1, colour = "black") +
      coord_polar(theta = "y", start = 0) +
      labs(title = paste0(axisIllness, " among human races"),
           fill = "Races") +
      theme(title = element_text(size = 18),
            plot.title = element_text(margin = margin(10, 0, 5, 0), hjust = .5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text = element_blank())
  })
  
  output$bmiage <- renderPlot({
    switch(input$illness,
           "Diabetes" = { patients <- NHANES %>%
             filter(Diabetes == "Yes")
           },
           "Insomnia" = { patients <- NHANES %>%
             filter(SleepTrouble == "Yes")
           },
           "Depression" = { patients <- NHANES %>%
             filter(Depressed == "Most" | Depressed == "Several")
           }
    )
    options(repr.plot.width = 0.3,
            repr.plot.height = 1.4,
            rep.plot.res = 1000)
    
    print(nrow(patients$Age))
    
    patients %>%
      ggplot(aes(x = Age, y = BMI)) +
      geom_hex(bins = 15) +
      labs(title = "Does BMI affect?",
           x = "Age",
           y = "Body mass index [kg/(m^2)]") +
      theme(title = element_text(size = 18),
            plot.title = element_text(margin = margin(10, 0, 5, 0), hjust = .5),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 14),
            plot.margin=unit(c(0, 4, 0, 4),"cm"))
  })
  
  output$activity <- renderPlot({
    switch(input$illness,
           "Diabetes" = { patients <- NHANES %>%
             filter(Diabetes == "Yes")
           axisIllness <- "diabetics"
           },
           "Insomnia" = { patients <- NHANES %>%
             filter(SleepTrouble == "Yes")
           axisIllness <- "insomniacs"
           },
           "Depression" = { patients <- NHANES %>%
             filter(Depressed == "Most" | Depressed == "Several")
           axisIllness <- "depressed people"
           }
    )
    
    patients %>%
      ggplot(aes(PhysActive)) +
      geom_bar(aes(fill = PhysActive), position = "identity") +
      scale_x_discrete(na.translate = FALSE) +
      ylim(0, 1200) +
      labs(title = "Do they play sports?",
           x = "",
           y = paste0("Number of ", axisIllness)) +
      theme(title = element_text(size = 18),
            plot.title = element_text(margin = margin(10, 0, 5, 0), hjust = .5),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 14),
            legend.position = "none")
  })
  
  output$TVTime <- renderPlot({
    switch(input$illness,
           "Diabetes" = { patients <- NHANES %>%
             filter(Diabetes == "Yes")
           axisIllness <- "diabetics"
           },
           "Insomnia" = { patients <- NHANES %>%
             filter(SleepTrouble == "Yes")
           axisIllness <- "insomniacs"
           },
           "Depression" = { patients <- NHANES %>%
             filter(Depressed == "Most" | Depressed == "Several")
           axisIllness <- "depressed people"
           }
    )
    
    levels(patients$TVHrsDay) <- c("0", "0+", "1", "2", "3", "4", "4+")
    
    patients %>%
      ggplot(aes(x = TVHrsDay)) +
      geom_bar(fill = "skyblue") +
      scale_x_discrete(na.translate = FALSE) +
      labs(title = "Amount of TV viewing time",
           x = "Number of hours during the day",
           y = paste0("Number of ", axisIllness)) +
      theme(title = element_text(size = 18),
            plot.title = element_text(margin = margin(10, 0, 5, 0), hjust = .5),
            axis.title.y = element_text(size = 14),
            axis.title.x = element_text(size = 14))
  })
  
  output$ComputerTime <- renderPlot({
    switch(input$illness,
           "Diabetes" = { patients <- NHANES %>%
             filter(Diabetes == "Yes")
           axisIllness <- "diabetics"
           },
           "Insomnia" = { patients <- NHANES %>%
             filter(SleepTrouble == "Yes")
           axisIllness <- "insomniacs"
           },
           "Depression" = { patients <- NHANES %>%
             filter(Depressed == "Most" | Depressed == "Several")
           axisIllness <- "depressed people"
           }
    )
    
    levels(patients$CompHrsDay) <- c("0", "0+", "1", "2", "3", "4", "4+")
    
    patients %>%
      ggplot(aes(x = CompHrsDay)) +
      geom_bar(fill = "limegreen") +
      scale_x_discrete(na.translate = FALSE) +
      labs(title = "Amount of computer games playing time",
           x = "Number of hours during the day",
           y = paste0("Number of ", axisIllness)) +
      theme(title = element_text(size = 18),
            plot.title = element_text(margin = margin(10, 0, 5, 0), hjust = .5),
            axis.title.y = element_text(size = 14),
            axis.title.x = element_text(size = 14))
  })
  
  output$testosterone <- renderPlot({
    switch(input$illness,
           "Diabetes" = { patients <- NHANES %>%
             filter(Diabetes == "Yes" & Gender == "male")
           },
           "Insomnia" = { patients <- NHANES %>%
             filter(SleepTrouble == "Yes" & Gender == "male")
           },
           "Depression" = { patients <- NHANES %>%
             filter(Depressed == "Most" | Depressed == "Several" & Gender == "male")
           }
    )
    
    patients %>%
      group_by(Age) %>%
      summarize(mean_testosterone_by_age = mean(Testosterone, na.rm = TRUE)) %>%
      ggplot(aes(x = Age, y = mean_testosterone_by_age)) +
      geom_area(color = "green", fill = "green", alpha = .3) +
      scale_x_continuous(breaks = seq(20, 80, 10)) +
      theme(legend.position = "none") +
      labs(title = "The association of average testosterone level with age in men",
           x = "Age",
           y = "Testosterone level [ng/dL]") +
      theme(title = element_text(size = 18),
            plot.title = element_text(margin = margin(10, 0, 5, 0), hjust = .5),
            axis.title.y = element_text(size = 14),
            axis.title.x = element_text(size = 14))
  })
  
  output$stimulants <- renderPlot({
    switch(input$illness,
           "Diabetes" = { patients <- NHANES %>%
             filter(Diabetes == "Yes")
           },
           "Insomnia" = { patients <- NHANES %>%
             filter(SleepTrouble == "Yes")
           },
           "Depression" = { patients <- NHANES %>%
             filter(Depressed == "Most" | Depressed == "Several")
           }
    )
    
    switch(input$select_stimulants,
           "Smoke" = {
             tmp <- patients %>%
               group_by(AgeDecade) %>%
               filter(!any(is.na(AgeDecade)))
             is_taking <- tmp$SmokeNow
           },
           "Marijuana" = {
             tmp <- patients %>%
               group_by(AgeDecade) %>%
               filter(!any(is.na(AgeDecade)))
             is_taking <- tmp$RegularMarij
           },
           "Hard Drugs" = {
             tmp <- patients %>%
               group_by(AgeDecade) %>%
               filter(!any(is.na(AgeDecade)))
             is_taking <- tmp$HardDrugs
           }
    )
    
    patients %>%
      group_by(AgeDecade) %>%
      filter(!any(is.na(AgeDecade))) %>%
      ggplot(aes(x = AgeDecade, fill = factor(is_taking))) +
      geom_bar(position = "dodge") +
      scale_fill_discrete(na.translate = FALSE) +
      scale_y_discrete(na.translate = FALSE) +
      labs(title = "Do people stimulate themselves?",
           x = "Age",
           y = "Number of people with bad habits") +
      theme(title = element_text(size = 18),
            plot.title = element_text(margin = margin(10, 0, 5, 0), hjust = .5),
            axis.title.y = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            legend.title = element_blank())
  })
}