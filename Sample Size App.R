#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(Hmisc)
library(tidyr)
library(scales)
library(pwr)


# Define server logic required to draw a histogram
server <- function(input, output) {

############################
## Approach based on https://rpubs.com/sypark0215/223385 which is based on
## https://www.statmethods.net/stats/power.html
###################
  
  output$PowerPlot <- renderPlotly({
  #########     Input Gathering ###############
  alpha <- input$Alpha ## From UI
 
    
  MinN <- input$N[1] ## From UI
  MaxN <- input$N[2] ## From UI
  NIncrement <- (MaxN - MinN)/200
  n  <- seq(MinN, MaxN, by = NIncrement) ## List of Total N's
  n.len <- length(n)
  ######## Determine test and control N's
  testPercent <- input$testPercent ## From UI
  nTest <- n*testPercent
  nControl <- n*(1-testPercent)
    
    
  MinLift <- input$Lift[1] ## From UI
  MaxLift <- input$Lift[2] ## From UI
  LiftIncrement <- (MaxLift - MinLift)/4
  Lift  <- seq(MinLift, MaxLift, by = LiftIncrement)
  p1 <- input$p1 ## From UI
  p2 <- (Lift + 1) * p1
  p2.len <- length(p2)

  
  ############################################
  ##Initiate blank array that will be populated during for loop
  PowerArray <-
    array(numeric(n.len * p2.len), dim = c(n.len, p2.len))
  ###################################

  ####################################################
  ######  For loop to gather power for various P2 and N combinatons
  ######  PWR package supports list inputs, consider using that to simplify script
  ############################################################
  for (i in 1:p2.len) {
    ##Cohen's Effect size same as ES.h(p1, p2) 
    h = abs(2*asin(sqrt(p1))-2*asin(sqrt(p2[i])))
    for (j in 1:n.len) {
      PowerTest <-
        pwr.2p2n.test(
          h, 
          n1=nTest[j], 
          n2=nControl[j], 
          sig.level=alpha,
          alternative = "greater"
          )
## OLD METHOD - DELETE ONCE NEW METHOD WORKING     
##       power.prop.test(
#          n = n[j],
#          p1 = p1,
#          p2 = p2[i],
#          sig.level = alpha,
#          alternative = "one.sided"
#        )
      PowerArray[j, i] <- PowerTest$power ## Write specific calc to Array, can be deleted if switched to list input method
      
    }
  }
  
  PowerDF <- as.data.frame(PowerArray) ## Convert array into DF
  names(PowerDF) <- Lift
  PowerDF$n <- n
  TidyPower <- gather(PowerDF, "lift", "power", -n)
  PowerGGplot <- ggplot(data = TidyPower, aes(x = n, y = power, color = lift)) + geom_path() + scale_x_continuous(labels = comma) + theme_bw()

  Powerplot <- ggplotly(PowerGGplot)
  Powerplot

  
  
  })
  
}



# Define UI for application that draws a histogram
ui <- fluidPage(
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
        "Range of Total Sample Size (Control + Test, sets X axis limits):",
        min = 10,
        max = 5000000,
        value = c(50000,500000),
        step = 1,
        round=TRUE
      ),
      p("Sample size of test population and control population are currently set to be equal as specified above.")
      ,
      numericInput(
        "testPercent",
        "% of Sample dedicated to Test group",
        min = 0.01,
        max = 0.95,
        value = 0.1,
        step = 0.01,
      ),
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
)

shinyApp(ui = ui, server = server)
