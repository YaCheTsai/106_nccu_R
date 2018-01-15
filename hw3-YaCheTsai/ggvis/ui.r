 library(shiny)
 shinyUI(fluidPage(
    titlePanel("HW3_106753028"),
    sidebarLayout(
      fluidPage(
        fluidRow(
            column(4,
  
            # Copy the line below to make a slider range 
            sliderInput("slider1", label = h3("CP Range"), min = 1, 
                max = 4, value = c(1, 4))
            )
        ),
        hr()
  
        ),
       mainPanel(
       plotOutput("distPlot"),
       hr(),
       plotOutput("distPlot2"),
       hr(),
       verbatimTextOutput("summary"),
       hr("Predict PCs"),
       tableOutput("view")
       )

    )
  ))