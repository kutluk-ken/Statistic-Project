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
                           column(width = 6,
                                  div(
                                    style = "overflow-x: auto; max-height: 600px;",
                                    tableOutput("TPR"),
                                    style = "width: 100%; border-collapse: collapse; margin-bottom: 20px; border: 1px solid #ddd; font-size: 14px; font-family: Arial, sans-serif; table-layout: fixed;",
                                    class = "custom-table"
                                  )
                           ),
                           column(width = 6,
                                  plotOutput("pieChart")
                           )
                         )
                ),
                tabPanel("Program-Level Sample Details",
                         fluidRow(
                           column(width = 6,
                                  h4("DOTS Cases"),
                                  div(
                                    style = "overflow-x: auto; max-height: 600px;",
                                    tableOutput("PLSD_DOTs"),
                                    style = "width: 100%; border-collapse: collapse; margin-bottom: 20px; border: 1px solid #ddd; font-size: 14px; font-family: Arial, sans-serif;",
                                    class = "custom-table"
                                  )
                           ),
                          column(width = 6,
                                  plotOutput("barChart_Dots")
                           )
                          ),
                         fluidRow(
                           column(width = 6,
                                  h4("New Samples"),
                                  div(
                                    style = "overflow-x: auto; max-height: 600px;",
                                    tableOutput("PLSD_newcase"),
                                    style = "width: 100%; border-collapse: collapse; margin-bottom: 20px; border: 1px solid #ddd; font-size: 14px; font-family: Arial, sans-serif;",
                                    class = "custom-table"
                                  )
                           )
                         ,
                          column(width = 6,
                                plotOutput("barChart_newcases")
                           )
                         )
                ),
                tabPanel("Average Individual Rat Results",
                         fluidRow(
                           column(width = 12,
                                  div(
                                    style = "overflow-x: auto; max-height: 600px;",
                                    tableOutput("AIRR"),
                                    style = "width: 100%; border-collapse: collapse; margin-bottom: 20px; border: 1px solid #ddd; font-size: 14px; font-family: Arial, sans-serif;",
                                    class = "custom-table"
                                  )
                           )
                         ,
                          column(width = 12,
                                 plotOutput("sensitivitySpecificityPlot")
                          )
                         )
                ),
                tabPanel("Average Rat Sample Details",
                         fluidRow(
                           column(width = 12,
                                  h4("DOTS Cases"),
                                  div(
                                    style = "overflow-x: auto; max-height: 600px;",
                                    tableOutput("ARSD_DOTs"),
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
                                    tableOutput("ARSD_newcase"),
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
                  )
                ),
                fluidRow(
                  box(
                    title = "Bar Chart",
                    plotOutput("barplot")
                  )
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
      tabItem("Rat_Visualized_Data",
              h2("Rat Visualized Data"),
              uiOutput("ratSelect")
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

