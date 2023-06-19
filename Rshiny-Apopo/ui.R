#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(shinydashboard)
library(ggplot2)



# Define UI for application that draws a histogram
# ui <- fluidPage(
#   mainPanel(
#     verbatimTextOutput("summary")
#   )
# )
ui <- dashboardPage(
  dashboardHeader(title = "My Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
      menuItem("Rat Performance", tabName = "Rat_Performance", icon = icon("chart-bar")),
      menuItem("Reused Analyst", tabName = "Reused_Analyst", icon = icon("recycle")),
      menuItem("Feedback", tabName = "Feedback", icon = icon("envelope")),
      menuItem("something1", tabName = "something1", icon = icon("question")),
      menuItem("something2", tabName = "something2", icon = icon("recycle")),
      menuItem("something3", tabName = "something3", icon = icon("recycle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("Overview",
              fluidRow(
                box(plotOutput("pie_chart"), height = 300)
              )
      ),
      tabItem("Rat_Performance",
              h2("Rat Performance Tab Content")
      ),
      tabItem("Reused_Analyst",
              h2("Reused Tab Content")
      ),
      tabItem("Feedback",
              h2("Feedback Tab Content")
      ),
      tabItem("something1",
              h2("Reused Tab Content")
      ),
      tabItem("something2",
              h2("Reused Tab Content")
      ),
      tabItem("something3",
              h2("Reused Tab Content")
      )
    )
  )
)