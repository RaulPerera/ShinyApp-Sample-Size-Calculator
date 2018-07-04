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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  output$PowerPlot <- renderPlotly({
  ######From UI###############
    p1 <- input$p1
    alpha <- input$Alpha
    
    MinN <- input$N[1]
    MaxN <- input$N[2]
    NIncrement <- (MaxN - MinN)/200

    
    MinN <- input$N[1]
    MaxN <- input$N[2]
    NIncrement <- (MaxN - MinN)/200
    
    
  MinLift <- input$Lift[1]
  MaxLift <- input$Lift[2]
  LiftIncrement <- (MaxLift - MinLift)/4

  #####################
  n  <- seq(MinN, MaxN, by = NIncrement)
  Lift  <- seq(MinLift, MaxLift, by = LiftIncrement)
  p2 <- (Lift + 1) * p1
  p2.len <- length(p2)
  n.len <- length(n)
  PowerArray <-
    array(numeric(n.len * p2.len), dim = c(n.len, p2.len))
  for (i in 1:p2.len) {
    for (j in 1:n.len) {
      PowerTest <-
        power.prop.test(
          n = n[j],
          p1 = p1,
          p2 = p2[i],
          sig.level = alpha,
          alternative = "one.sided"
        )
      PowerArray[j, i] <- PowerTest$power
    }
  }
  PowerDF <- as.data.frame(PowerArray)
  names(PowerDF) <- Lift
  PowerDF$n <- n
  TidyPower <- gather(PowerDF, "lift", "power", -n)
  PowerGGplot <- ggplot(data = TidyPower,
                      aes(x = n, y = power, color = lift)) + geom_path() +
    scale_x_continuous(labels = comma) +
    theme_bw()
  #############TEST################
  Powerplot <- ggplotly(PowerGGplot)
  Powerplot
  #############################
  
  
  })
  
})
