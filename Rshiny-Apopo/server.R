#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)

# Define server logic required to draw a histogram
function(input, output, session){
  # Read XLSX file
  data <- reactive({
    read_excel("/Users/kutlukanwar/Desktop/STAD94/TBdetectionRats_Tanzania.xlsx", sheet = 2, col_names = TRUE)
  })
  
  # Output summary
  output$summary <- renderPrint({
    data()
  })
}
