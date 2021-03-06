library(shiny)
library(ggplot2)

source("ShinyHMCI.R")

shinyServer(function(input, output) {

    q_a <- reactive({ 
      input$"Pct CI"
  })
  
   output$alpha_text <- renderText({            
    paste("You have selected a ", input$"Pct CI", "% confidence interval.")
  }) 
   
  output$HeatMapL <- renderPlot({ 
    with(CI_list[[input$"Pct CI" + 1]], 
         shiny_hmci_fcn(dataset = CI_list[[input$"Pct CI" + 1]], plb)) 
    })
  
  output$HeatMap <- renderPlot({
    with(CI_list[[1]], shiny_hmci_fcn(dataset = CI_list[[1]], phat))     
  })
  
  output$HeatMapU <- renderPlot({
    with(CI_list[[input$"Pct CI" + 1]], 
         shiny_hmci_fcn(dataset = CI_list[[input$"Pct CI" + 1]], pub))     
  })

})