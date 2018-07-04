#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("Sample Size Calculator"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      numericInput(
        inputId = "p1",
        label = "Probability of Control Event:",
        value = 0.01,
        min = 0,
        max = 1,
        step = 0.001
      )
      ,
      numericInput(
        "Alpha",
        "Significance Level:",
        min = 0.01,
        max = 0.2,
        value = 0.05,
        step = 0.01
      )
      ,
      sliderInput(
        "N",
        "Range of Sample Size per Group:",
        min = 10,
        max = 5000000,
        value = c(50000,500000),
        step = 1,
        round=TRUE
      ),
      p("Sample size of test population and control population are currently set to be equal as specified above.")
      ,
      sliderInput(
        "Lift",
        "Range of Lifts to consider:",
        min = 0.01,
        max = 0.5,
        value = c(0.05,0.25),
        step = 0.01
      ),
      p("Lift is calculated as percentage of Control event Probability(p1). If p1 = 20% or 0.2, a lift of .1 would correspnd to a p2 of 22%.  5 Lift lines will be calculated evenly spread across the inputted lifts to consider.")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3(
        "INSTRUCTIONS:"
      ),
     
         tags$ul(
           tags$li("A single value is needed as input for Alpha and expected Probability of the Control event.")
          ,tags$li("For lift and sample size, a range can be input and the widget will calculate power for several values within that range.")
          ,tags$li("Power is the likelihood of identifying a difference in a test assuming the true difference in population is equal to the inputed list value.  Typically we look for power to be equal to 80% or more.
          More information is available at ", a(href = 'https://en.wikipedia.org/wiki/Power_(statistics)', 'Wikipedia'))
         )
    
      ,
      plotlyOutput("PowerPlot")
              )
    
  )
))