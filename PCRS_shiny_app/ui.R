navbarPage("Task Completion",
  tabPanel("Plot",
    sidebarLayout(
      sidebarPanel(
        # change all of these accordingly
        sliderInput("lower",
                    "lower bound (tasks per month):",
                    min = 1,
                    max = 1000,
                    value = 50,
                    step=1),
        sliderInput("upper",
                    "upper bound (tasks per month:",
                    min = 50,
                    max = 2000,
                    value = 100,
                    step=1),
        sliderInput("remaining",
                    "remaining tasks:",
                    min = 1,
                    max = 10000,
                    value = 2000,
                    step=1),
        sliderInput("months",
                    "remaining months:",
                    min = 1,
                    max = 60,
                    value = 12,
                    step=1),
        # actionButton("button", "Draw Plot")
      ),
      mainPanel(
        plotOutput("plot")

      )
    )
  ),
  # TO DO
  # tabPanel("Summary",
  #   verbatimTextOutput("summary")
  # ),

    # tabPanel("Table",
    #   DT::dataTableOutput("table")
    # )
    # ,

    
  )

