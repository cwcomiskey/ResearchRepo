library(shiny)
library(ggplot2)

source("ShinyHMCI.R")

shinyServer(function(input, output) {

    q_a <- reactive({ # quantile: P[Z > z_(alpha/2)] = alpha/2
    round(qnorm(input$alpha/2, lower.tail = FALSE),3)
  })
  
   output$alpha_text <- renderText({            
    paste("You have selected alpha = ", input$alpha, "so q =", q_a(), "and pointwise P(non-coverage)=",  input$alpha)
  }) 
   
  output$HeatMapL <- renderPlot({ 
    ggplot(aes(px, pz, fill = p - q_a()*SE_p), 
           data = hitzone) + 
      geom_tile() + 
      geom_path(aes(x, y, fill=NULL), data = kZone, 
                lwd = 1.5, col = "blue", linetype = 2) + 
      coord_equal() + 
      xlim(-1.5, 1.5) + ylim(1, 4) +
      scale_fill_distiller(palette = "Spectral", 
                           limits = c(0, 0.170), 
            guide = guide_legend(title = expression(hat(p))))  
    })
  
  output$HeatMap <- renderPlot({
    ggplot(aes(px, pz, fill = p), 
           data = hitzone) + 
      geom_tile() + 
      geom_path(aes(x, y, fill=NULL), 
                        data = kZone, lwd = 1.5, 
                        col = "blue", linetype = 2) + 
      coord_equal() + xlim(-1.5, 1.5) + ylim(1, 4) +
      scale_fill_distiller(palette = "Spectral", 
                           limits = c(0.0, .170), trans="reverse",
                guide = guide_legend(title = expression(hat(p))))
  })
  
  output$HeatMapU <- renderPlot({
    ggplot(aes(px, pz, fill = p + q_a()*SE_p), 
           data = hitzone) + 
      geom_tile() + 
      geom_path(aes(x, y, fill=NULL), 
                data = kZone, lwd = 1.5, 
                col = "blue", linetype = 2) + 
      coord_equal() + xlim(-1.5, 1.5) + ylim(1, 4) +
      scale_fill_distiller(palette = "Spectral", 
                           limits = c(0, 0.170), 
              guide = guide_legend(title = expression(hat(p))))
  })

})