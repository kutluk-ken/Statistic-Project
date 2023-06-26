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
      menuItem("Rat Performance", tabName = "Rat_Performance", icon = icon("chart-pie")),
      menuItem("Trainer Analysis", tabName = "Trainer_Analysis", icon = icon("chart-bar")),
      menuItem("Time Control", tabName = "Control_Time_Analysis", icon = icon("clock")),
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
              h2("Overview"),
              tabsetPanel(
                tabPanel("Total Program Results",tableOutput("TPR"))
              ),
              tabsetPanel(
                tabPanel("Program-Level Sample Details",tableOutput("PLSD_DOTs"))
              )
              
      ),
      tabItem("Rat_Performance",
              h2("Rat Performance Tab Content"),
              tabsetPanel(
                tabPanel("Sen", tableOutput("basicInformation_Sensitivity")),
                tabPanel("Spe", tableOutput("basicInformation_Specificity")),
                tabPanel("Ind per", plotOutput("Individual_rat_performance"))
              )
      ),
      tabItem("Trainer_Analysis",
              h2("Analysis Trainer by time"),
              tabsetPanel(
                tabPanel("O_Sen", tableOutput("Trainer_Sensitivity")),
                tabPanel("O_Spe", tableOutput("Trainer_Specificity")),
                tabPanel("D_Sen", tableOutput("Sensitivity_trainer_table_daily")),
                tabPanel("D_Spe", tableOutput("Specificity_trainer_table_daily")),
                tabPanel("W_Sen", tableOutput("Sensitivity_trainer_table_weekly")),
                tabPanel("W_Spe", tableOutput("Specificity_trainer_table_weekly")),
                tabPanel("M_Sen", tableOutput("Sensitivity_trainer_table_monthly")),
                tabPanel("M_Spe", tableOutput("Specificity_trainer_table_monthly"))
              )
      ),
      tabItem("Control_Time_Analysis",
              h2("Time"),
              uiOutput("valueSelect"),
              uiOutput("trainerSelect"),
              uiOutput("timeSelect"),
              plotOutput("LineChart"),
              tableOutput("trainertable")
              
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
