shinyUI(fluidPage(
  titlePanel("Reddit data analysis"),
  
  sidebarLayout(
    sidebarPanel(
      
      
      textInput("query",label = "Enter Query : ",""),
      submitButton("Submit")
    ),
    
    mainPanel(
      textOutput("text1")
    )
  )
))