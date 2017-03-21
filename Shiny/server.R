shinyServer(
  function(input, output) {
    
    output$text1 <- renderText({ 
      paste("Selected Query : ", input$query)
    })
    
    
  }
)