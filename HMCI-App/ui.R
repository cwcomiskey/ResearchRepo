# ui.R

shinyUI(fluidPage(
  
  titlePanel("Dynamic Heat Map Confidence Intervals"),
  
  # title = "Dynamic Heat Map Confidence Intervals",
 
  fluidRow( 
     column(4, plotOutput("HeatMapL")),
     column(4, plotOutput("HeatMap")),
     column(4, plotOutput("HeatMapU"))
  ),

  fluidRow(
     column(6, sliderInput("alpha",
                 label = h2("alpha"),
                 min = 0.01, max = 0.99, value = 0.95)),
     column(6, textOutput("alpha_text"))
     
       )))