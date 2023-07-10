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
    tabItems(
      tabItem("Overview",
              h2("Overview"),
              fluidRow(
                column(width = 3,
                       h3("Total Program Results"),
                       div(
                         style = "overflow-x: auto; max-height: 600px;",
                         tableOutput("TPR"),
                         class = "table-responsive"
                       )
                ),
                column(width = 9,
                       fluidRow(
                         column(width = 6,
                                h3("Program-Level Sample Details"),
                                div(
                                  style = "overflow-x: auto; max-height: 600px;",
                                  fluidRow(
                                    column(width = 12,
                                           h4("DOTS Cases"),
                                           div(
                                             style = "overflow-x: auto;",
                                             tableOutput("PLSD_DOTs"),
                                             class = "table-responsive"
                                           )
                                    )
                                  ),
                                  fluidRow(
                                    column(width = 12,
                                           h4("New Samples"),
                                           div(
                                             style = "overflow-x: auto;",
                                             tableOutput("PLSD_newcase"),
                                             class = "table-responsive"
                                           )
                                    )
                                  )
                                )
                         ),
                         column(width = 6,
                                h3("Average Individual Rat Results"),
                                div(
                                  style = "overflow-x: auto; max-height: 600px;",
                                  tableOutput("AIRR"),
                                  class = "table-responsive"
                                )
                         )
                       ),
                       fluidRow(
                         column(width = 6,
                                h3("Average Rat Sample Details"),
                                div(
                                  style = "overflow-x: auto; max-height: 600px;",
                                  fluidRow(
                                    column(width = 12,
                                           h4("DOTS Cases"),
                                           div(
                                             style = "overflow-x: auto;",
                                             tableOutput("ARSD_DOTs"),
                                             class = "table-responsive"
                                           )
                                    )
                                  ),
                                  fluidRow(
                                    column(width = 12,
                                           h4("New Samples"),
                                           div(
                                             style = "overflow-x: auto;",
                                             tableOutput("ARSD_newcase"),
                                             class = "table-responsive"
                                           )
                                    )
                                  )
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
