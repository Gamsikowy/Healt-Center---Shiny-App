ui <- fluidPage(
  
  titlePanel("Health Data Center"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('illness', 'Select Illness', c("Diabetes", "Depression", "Insomnia")
      ),
      textOutput("timer"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(id = "tabSwitch",
                  tabPanel('Summary',
                           br(),
                           textOutput("show_desc"),
                           br(),
                           fluidRow(
                             align = "right",
                             actionButton("show_desc", "Show description")
                           )
                  ),
                  tabPanel('Ethnicity & Income',
                           plotOutput("ethnic"),
                           plotOutput("income")
                  ),
                  tabPanel('BMI & Age',
                           br(),
                           textOutput("bmi"),
                           plotOutput("bmiage")
                  ),
                  tabPanel('Activity',
                           plotOutput("activity"),
                           plotOutput("TVTime"),
                           plotOutput("ComputerTime"),
                           plotOutput("testosterone")
                  ),
                  tabPanel('Stimulants',
                           align = "center",
                           radioButtons("select_stimulants",
                                        "Select the stimulants",
                                        list("Smoke", "Marijuana", "Hard Drugs"),
                                        inline = TRUE
                           ),
                           plotOutput("stimulants")
                  )
      )
    )
  )
)