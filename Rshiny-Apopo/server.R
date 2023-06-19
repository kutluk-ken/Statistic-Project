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
library(ggplot2)

# # Read the data file
# data <- read_excel("/Users/kutlukanwar/Desktop/STAD94/TBdetectionRats_Tanzania.xlsx", sheet = 3, col_names = TRUE)
# reused_data <- data$REUSED
# Define server logic required to draw a histogram
function(input, output, session){
  # Read the data file
  data <- read_excel("/Users/kutlukanwar/Desktop/STAD94/TBdetectionRats_Tanzania.xlsx", sheet = 3, col_names = TRUE)


  reused_sample_counts <- reactive({
    fresh_count <- nrow(data[data$REUSED == 1, ])
    reused_count <- nrow(data[data$REUSED > 1, ])
    counts <- data.frame(REUSED = c("Fresh", "Reused"), COUNT = c(fresh_count, reused_count))
    counts
  })

  output$pie_chart <- renderPlot({
    counts <- reused_sample_counts()
    ggplot(counts, aes(x = "", y = COUNT, fill = REUSED)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y") +
      labs(fill = "Sample Type") +
      theme_void() +
      theme(legend.position = "right")
  })
  

}
