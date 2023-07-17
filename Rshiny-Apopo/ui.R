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
library(scales)
library(shinyWidgets)


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
      menuItem("Rat Hit Analyst", tabName = "Rat_Hit_Analyst", icon = icon("chart-pie")),
      menuItem("Rat Performance", tabName = "Rat_Performance", icon = icon("chart-pie")),
      menuItem("Rat Visualized Data", tabName = "Rat_Visualized_Data", icon = icon("paw")),
      menuItem("Trainer Analysis", tabName = "Trainer_Analysis", icon = icon("chart-bar")),
      menuItem("Time Control", tabName = "Control_Time_Analysis", icon = icon("clock")),
      menuItem("Reused Analyst", tabName = "Reused_Analyst", icon = icon("recycle")),
      menuItem("Feedback", tabName = "Feedback", icon = icon("envelope")),
      menuItem("Overall Table", tabName = "Overall_Table", icon = icon("file-excel")),
      menuItem("something2", tabName = "something2", icon = icon("recycle")),
      menuItem("something3", tabName = "something3", icon = icon("recycle"))
    )
  ),
  dashboardBody(
    tags$style(HTML("
      .custom-table th:first-child,
      .custom-table td:first-child {
        background-color: #f5f5f5;
        font-weight: bold;
      }
      
      .custom-table th,
      .custom-table td {
        padding: 8px;
        border: 1px solid #ddd;
      }
      
      .custom-table th {
        background-color: #f9f9f9;
      }
      
      .custom-table tbody tr:nth-child(even) {
        background-color: #f1f1f1;
      }
      
      .custom-table tbody tr:hover {
        background-color: #f1f1f1;
      }
    ")),
    tabItems(
      tabItem("Overview",
              h2("Overview"),
              tabsetPanel(
                id = "overviewTabs",
                tabPanel("Total Program Results",
                         fluidRow(
                           selectInput("timePeriod", "Select Time Period:",
                                       choices = c("Overall", "Day", "Week", "Month"),
                                       selected = "Overall"),
                           uiOutput("dateInput"),
                           uiOutput("weekInput"),
                           uiOutput("monthInput"),
                           column(4,DT::dataTableOutput("overviewTable")),
                           column(6, plotOutput("pieChart"))
                         )
                ),
                tabPanel("Program-Level Sample Details",
                         h3("DOTS Cases"),
                         fluidRow( 
                           column(4, DT::dataTableOutput("PLSD_DOTs")), 
                           column(6, plotOutput("barChart_Dots"))
                         )
                         ,
                         h3("New Cases"),
                         fluidRow(  column(4, DT::dataTableOutput("PLSD_newcase")), 
                                    column(6, plotOutput("barChart_newcases"))
                         )
                         
                ),
                tabPanel("Average Individual Rat Results",
                         splitLayout(
                           DT::dataTableOutput("AIRR"), plotOutput("sensitivitySpecificityPlot"))
                ),
                tabPanel("Average Rat Sample Details",
                         fluidRow(
                           column(width = 12,
                                  h4("DOTS Cases"),
                                  div(
                                    style = "overflow-x: auto; max-height: 600px;",
                                    DT::dataTableOutput("ARSD_DOTs"),
                                    style = "width: 100%; border-collapse: collapse; margin-bottom: 20px; border: 1px solid #ddd; font-size: 14px; font-family: Arial, sans-serif;",
                                    class = "custom-table"
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 12,
                                  h4("New Samples"),
                                  div(
                                    style = "overflow-x: auto; max-height: 600px;",
                                    DT::dataTableOutput("ARSD_newcase"),
                                    style = "width: 100%; border-collapse: collapse; margin-bottom: 20px; border: 1px solid #ddd; font-size: 14px; font-family: Arial, sans-serif;",
                                    class = "custom-table"
                                  )
                           )
                         )
                )
              )
      ),
      tabItem("Rat_Hit_Analyst",
              h2("Rat Hit Analyst"),
              fluidRow(
                box(
                  uiOutput("ratHitAnalystInputs")
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  box(
                    title = "Relative data",
                    style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
                    h4("Sample Number as chosen"),
                    tableOutput("Selected_Bac_Level"),
                    h4("Total Hits as chosen"),
                    tableOutput("Selected_Hits"),
                    h4("Percentage as chosen (in %)"),
                    tableOutput("Selected_Percentage")
                  )
                ),
                column(
                  width = 10,
                  box(
                    title = "Bar Chart",
                    plotOutput("barplot")
                  )
                )
              )
      ),
      tabItem("Rat_Performance",
              h2("Rat Performance Tab Content"),
              tabsetPanel(
                tabPanel("Sensitivity", DT::dataTableOutput("basicInformation_Sensitivity")),
                tabPanel("Specificity", DT::dataTableOutput("basicInformation_Specificity")),
                tabPanel("Visualization", plotOutput("Individual_rat_performance"))
              )
      ),
      tabItem("Rat_Visualized_Data",
              h2("Rat Visualized Data"),
              uiOutput("ratSelect")
      ),
      tabItem("Trainer_Analysis",
              h2("Analysis Trainer by time"),
              tabsetPanel(
                tabPanel("Overall Sen", DT::dataTableOutput("Trainer_Sensitivity")),
                tabPanel("Overall Spe", DT::dataTableOutput("Trainer_Specificity")),
                tabPanel("Daily Sen", DT::dataTableOutput("Sensitivity_trainer_table_daily")),
                tabPanel("Daily Spe", DT::dataTableOutput("Specificity_trainer_table_daily")),
                tabPanel("Weekly Sen", DT::dataTableOutput("Sensitivity_trainer_table_weekly")),
                tabPanel("Weekly Spe", DT::dataTableOutput("Specificity_trainer_table_weekly")),
                tabPanel("Monthly Sen", DT::dataTableOutput("Sensitivity_trainer_table_monthly")),
                tabPanel("Monthly Spe", DT::dataTableOutput("Specificity_trainer_table_monthly"))
              )
      ),
      tabItem("Control_Time_Analysis",
              h2("Time"),
              uiOutput("valueSelect"),
              uiOutput("trainerSelect"),
              uiOutput("timeSelect"),
              splitLayout(plotOutput("LineChart"), DT::dataTableOutput("trainertable"))
              
      ),
      tabItem("Reused_Analyst",
              h2("Reused Tab Content")
      ),
      tabItem("Feedback",
              h2("Feedback Tab Content")
      ),
      tabItem("Overall_Table",
              h2("Overall Table"),
              tabsetPanel(
                tabPanel("bac_level",
                         fluidRow(
                           column(width = 12,
                                  div(
                                    style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
                                    tableOutput("BacterialLevelTable")
                                  )
                           )
                         )
                ),
                tabPanel("Hit",
                         fluidRow(
                           column(width = 12,
                                  div(
                                    style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
                                    tableOutput("HitTable")
                                  )
                           )
                         )
                ),
                tabPanel("Pct.(%)",
                         fluidRow(
                           column(width = 12,
                                  div(
                                    style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
                                    tableOutput("PercentageTable")
                                  )
                           )
                         )
                )
              )
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

