

library(shiny)
setwd("C:/Users/rohitga/Desktop/Coursera_Capstone/final/en_US")

shinyUI(
  
  pageWithSidebar(
    headerPanel("Coursera Capstone: Text Prediction"),
    
  
    sidebarPanel(
      textInput("text", label="Enter your text here"),
      submitButton("submit")
    ),
  
    mainPanel(
      
     h4("You entered"),
     verbatimTextOutput("inputValue"),
     h4("Your predicted next word is "),
     verbatimTextOutput("prediction")
    
    )  
    
  )
  
)
