

################### Introduction ###########################
# This app is designed for the rat data virtualization.
# All reference is included in the reference section.
# We would give our great appreciation to Prof.Sohee and Prof. Marco for their wonderful suggestions.
# If the App is running locally, then there would be no issue for big data set.
# If the App is running online, then the max size of data set is approx. 40000 data.
# We hope this App helps the researcher to analyze the Rats data.







#################### Load Libraries #########################
library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(readxl)
library(ggplot2)
library(dplyr)
library(scales)
library(shinyWidgets)
library(plotly)
library(shinythemes)
library(dashboardthemes)
library(highcharter)

## One can set acceptable file size here.
## We set the size to be the biggest.
options(shiny.maxRequestSize=30*1024^2)
options(shiny.error = NULL)

rowCallback <- c(
  "function(row, data){",
  "  for(var i=0; i<data.length; i++){",
  "    if(data[i] === null){",
  "      $('td:eq('+i+')', row).html('NA')",
  "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
  "    }",
  "  }",
  "}"  
)

## Reference for the login page:
## https://www.listendata.com/2019/06/how-to-add-login-page-in-shiny-r.html#comment-form
## Other reference to be included.

# Main login screen
loginpage <- div(id = "loginpage", style = "width: 800px;margin: 0 auto;",
                 wellPanel(
                   tags$h2("Log In to Continue", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("password", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     shinyjs::hidden(
                       div(id = "WRONG",
                           tags$p("Invalid username or password, please try again",
                                  style = "color: black; font-weight: 600; 
                                           font-size:16px;", 
                                  class = "text-center"))),
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#000;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     br(),
                     br(),
                     h5("For access, please email Professor Sohee Kang at "),
                     h5("sohee.kang@utoronto.ca"),
                     tags$a(href="https://apopo.org/?v=3e8d115eb4b3", "APOPO website", target = "_blank")
                   ))
)

# File Upload Page
fileUploadPage <- div(id = "fileuploadpage", style = "width: 800px;margin: 0 auto;",
                 wellPanel(
                   tags$h2("Upload the Excel File", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   fileInput("fileUpload", "Choose a xlsx file", accept = ".xlsx"),
                   br(),
                   div(
                     style = "text-align: center;",
                     h5("1.Please make sure the uploaded file satisfies the prerequsite."),
                     h5("2. If you are running this App online, please make sure that the sample size is less than 40,000"),
                     h5("Please double check the with template file before uploading the required file"),
                     tags$a(href="https://docs.google.com/spreadsheets/d/11PFf5FmsA15XWFCiCJJfI9OVgIReDTIk/edit?usp=sharing&ouid=107039720820806758868&rtpof=true&sd=true", "Data template", target = "_blank"),
                     br(),
                     tags$a(href="https://youtube.com/", "Video Guide(Youtube Link here)", target = "_blank"))
                   )
)

## The username and the password can be adjusted here.
user_base <- data.frame(
  USER_ID = c("Junjie Ma", "Ken", "Huairu Chen"),
  PASSWORD   = sapply(c("STAD94", "STAD94", "STAD94"),password_store),
  permission  = c("advanced", "advanced", "advanced"), 
  stringsAsFactors = F
)

header <- dashboardHeader(title = "Analysis for Rat Data", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  login = FALSE
  fileUpload = FALSE
  USER <- reactiveValues(login = login)
  FILE <- reactiveValues(fileUpload = fileUpload)
  
  observe({ 
    if (FILE$fileUpload == FALSE) {
      if (!is.null(input$fileUpload)) {
        FILE$fileUpload <- TRUE
      }
    }    
  })
  

  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$password)
          ## We check if the username matches with the existing username
          if(length(which(user_base$USER_ID==Username)) != 0) { 
            pasmatch  <- user_base["PASSWORD"][which(user_base$USER_ID==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "WRONG", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "WRONG", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "WRONG", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "WRONG", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("lock"), "Logout", 
              href= "javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "color: #f00; background-color: #eee; border: 0;margin:3px; padding: 10px;")
  })

  output$body <- renderUI({
    if (USER$login == TRUE & FILE$fileUpload == TRUE) {
      shinyDashboardThemes(
        theme = "onenote"
      )
      HitTable <- HitTable()
      tabItems(
        ## The items of Overview page starts here
        tabItem("Overview",
                h1("Overview"),
                tabsetPanel(
                  id = "overviewTabs",
                  tabPanel("Total Program Results",
                           h2("Overall Results at Program Level"),
                           fluidRow(
                             column(
                               4,
                             selectInput("timePeriod", "Please Select Time Period:",
                                         choices = c("Overall", "Day", "Week", "Month"),
                                         selected = "Overall"),
                             uiOutput("dateInput"),
                             uiOutput("weekInput"),
                             uiOutput("monthInput"))
                             ),
                             fluidRow(
                                    box(
                                      solidHeader = TRUE,
                                      status = "primary",
                                      DT::dataTableOutput("overviewTable")
                                    ),
                                    box(
                                    title = "Value Boxes",
                                    solidHeader = TRUE,
                                    status = "primary",
                                    valueBoxOutput("BlindsSampleBox"), valueBoxOutput("DOTSNegativeBox"), valueBoxOutput("DOTSPositiveBox"), valueBoxOutput("NewcaseBox"),
                                    valueBoxOutput("NewpatientBox"), valueBoxOutput("DotsPosPatientBox"), valueBoxOutput("DotsNegPatientBox"), valueBoxOutput("TotalBlindPatientBox"),valueBoxOutput("UncomCaseBox"))
                           ),
                           br(),
                           br(),
                           br(),
                           h2("Program Results by New Cases & DOTS"),
                           fluidRow(
                             column(
                               4,
                               selectInput(
                                 "timePeriod_PLSD",
                                 "Select Time Period:",
                                 choices = c("Overall", "Day", "Week", "Month"),
                                 selected = "Overall"
                               ),
                               uiOutput("dateInput_PLSD"),
                               uiOutput("weekInput_PLSD"),
                               uiOutput("monthInput_PLSD"),
                             )), 
                           h3("DOTS:"),
                           fluidRow(
                             box(DT::dataTableOutput("PLSD_DOTs")),
                             box(highchartOutput("barChart_Dots"))),
                           h3("New Cases:"),
                           fluidRow( 
                             box(DT::dataTableOutput("PLSD_newcase")),
                             box(highchartOutput("barChart_newcases")))
                  ),
                  tabPanel("Average Individual Rat Results",
                           h2("Average Rat Sample Details"),
                           fluidRow(
                             box(DT::dataTableOutput("AIRR")), box(highchartOutput("sensitivitySpecificityPlot"))),
                           br(),
                           br(),
                           h2("Sample by Time:"),
                           fluidRow(
                             column(
                               width = 4,
                               selectInput(
                                 "timePeriod_ARSD",
                                 "Select Time Period:",
                                 choices = c("Overall", "Day", "Week", "Month"),
                                 selected = "Overall"
                               ),
                               uiOutput("dateInput_ARSD"),
                               uiOutput("weekInput_ARSD"),
                               uiOutput("monthInput_ARSD"))
                           ),
                           fluidRow(
                             box(
                                 DT::dataTableOutput("ARSD_newcase")
                             ),
                             box(
                                 style = "overflow-x: auto; max-height: 600px;",
                                 DT::dataTableOutput("ARSD_DOTs")
                             )
                           )
                  )
                )
        ),
        tabItem("Rat_Hit_Analyst",
                h2("Rat Hit Analyst"),
                fluidRow(
                  column(
                    width = 12,
                    box(
                      title = "Relative data",
                      solidHeader = TRUE,
                      status = "primary",
                      selectInput("rat", "Select Rat Name", choices = unique(HitTable$RAT_NAME)),
                      selectInput("sample_reuse", "Select Sample Reuse:", choices = c("ALL", "FRESH", "RE-USED"))
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    box(
                      title = "Relative data",
                      solidHeader = TRUE,
                      style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
                      status = "primary",
                      h4("Sample Number as chosen"),
                      tableOutput("Selected_Bac_Level"),
                      h4("Total Hits as chosen"),
                      tableOutput("Selected_Hits"),
                      h4("Percentage as chosen (in %)"),
                      tableOutput("Selected_Percentage")
                    )
                  )),
                fluidRow(
                  column(
                    width = 12,
                    box(
                      title = "Bar Chart",
                      solidHeader = TRUE,
                      status = "primary",
                      highchartOutput("barplot")
                    )
                  ))
        ),
        tabItem("Rat_Performance",
                h2("Rat Performance Tab Content"),
                tabsetPanel(
                  tabPanel("Sensitivity & Specificity", 
                           h2("Sensitivity"),
                           DT::dataTableOutput("basicInformation_Sensitivity"),
                           h2("Specificity"),
                           DT::dataTableOutput("basicInformation_Specificity")),
                  tabPanel("Visualization", 
                           h2("Interactive Scatter Plot for Rat Sensitivity and Specificity:"),
                           highchartOutput("Individual_rat_performance"),
                           h2("Interactive Bar Chart for Rats"),
                           highchartOutput("Individual_rat_bar")
                  ))
        ),
        tabItem("Trainer_Analysis",
                h2("Overall Trainer analysis"),
                fluidPage(
                  fluidRow(
                    tabsetPanel(
                      tabPanel("Overall Sen", icon = icon("o"), box(width = 12, DT::dataTableOutput("Trainer_Sensitivity"))),
                      tabPanel("Overall Spe", icon = icon("o"), box(width = 12,DT::dataTableOutput("Trainer_Specificity"))),
                      tabPanel("Daily Sen", icon = icon("d"),  box(width = 12, DT::dataTableOutput("Sensitivity_trainer_table_daily"))),
                      tabPanel("Daily Spe", icon = icon("d"), box(width = 12,DT::dataTableOutput("Specificity_trainer_table_daily"))),
                      tabPanel("Weekly Sen", icon = icon("w"), box(width = 12,DT::dataTableOutput("Sensitivity_trainer_table_weekly"))),
                      tabPanel("Weekly Spe", icon = icon("w"), box(width = 12,DT::dataTableOutput("Specificity_trainer_table_weekly"))),
                      tabPanel("Monthly Sen", icon = icon("m"), box(width = 12,DT::dataTableOutput("Sensitivity_trainer_table_monthly"))),
                      tabPanel("Monthly Spe", icon = icon("m"), box(width = 12,DT::dataTableOutput("Specificity_trainer_table_monthly")))
                    )),
                  h2("Trainer Analysis By Time:"),
                  fluidRow(
                    uiOutput("valueSelect"),
                    uiOutput("trainerSelect"),
                    uiOutput("timeSelect"),
                    h2("Virtualized Plot:")),
                  fluidRow(
                    box(
                      title = "Plot",
                      solidHeader = TRUE,
                      status = "primary",
                      highchartOutput("LineChart")
                    ),
                    box(
                      title = "Table",
                      DT::dataTableOutput("trainertable")
                    )
                  )
                )
        ),
        tabItem("About_App",
                h2("Something about this App:"),
                h3("Reference"),
                code("Main Structure"),
                p("The main structure of this App(mainly the login page) is from:", em("https://www.listendata.com/2019/06/how-to-add-login-page-in-shiny-r.html#comment-form"), 
                  " ,and we then implant our own code for the Shiny Dashboard and the corresponding data analysis."),
                p("For the structure of the shiny dashboard, see:",em("https://rstudio.github.io/shinydashboard/"),"."),
                code("Highchart Output"),
                p("The interactive charts/plots in this App are generated from the package called 'highcharter',see:", em("https://www.highcharts.com/blog/tutorials/highcharts-for-r-users/"),"."),
                code("Datatable Output"),
                p("The data tables in the app are generated from the package 'DT', for more information see: ", em("https://rstudio.github.io/DT/shiny.html"),"."),
                code("ValueBox Output"),
                p("For information about valuebox and other boxes, see:", em("https://rstudio.github.io/shinydashboard/structure.html"),"."),
                br(),
                h3("Feedback"),
                p("If you have any comments or concerns, please contact Professor Sohee Kang at:sohee.kang@utoronto.ca, or any member of our team:"),
                p("Kuteluke Ainiwaer: ainiwaer.kuteluke@mail.utoronto.ca"),
                p("Huairu Chen: huairu.chen@mail.utoronto.ca"),
                p("Junjie Ma: junjie.ma@mail.utoronto.ca"),
                p("Any suggestion will be greatly appreciated.")
        ),
        tabItem("Overall_Table",
                h2("Overall Table"),
                tabsetPanel(
                  tabPanel("By Bacterial Level",
                           fluidRow(
                             column(width = 12,
                                    div(
                                      DT::dataTableOutput("BacterialLevelTable")
                                    )
                             )
                           )
                  ),
                  tabPanel("By Hit",
                           fluidRow(
                             column(width = 12,
                                    div(
                                      DT::dataTableOutput("HitTable")
                                    )
                             )
                           )
                  ),
                  tabPanel("Pct.(%)",
                           fluidRow(
                             column(width = 12,
                                    div(
                                      DT::dataTableOutput("PercentageTable")
                                    )
                             )
                           )
                  )
                )
        )
      )
    }
    else if (USER$login == TRUE){
      fileUploadPage
    }
    else {
      loginpage
    }
  })
  
  
 
  ## To get the TB_rat
  TB_rat <- reactive({
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    data <- read_excel(file, sheet = 2, col_names = TRUE)
    data$ID_STATUS <- ifelse(!is.na(data$ID_BL_APOPO), data$ID_BL_APOPO, data$ID_BL_DOTS)
    #data$ID_STATUS <- data$ID_BL_DOTS
    # Create the week number
    data$DATE <- as.Date(data$SESSION_DATE, format = "%Y/%m/%d")
    data$week_numbers <- format(data$DATE, format = "%U")
    data$month_numbers <- format(data$DATE, format = "%m")
    data$day_of_week <- weekdays(data$DATE)
    return(data)
  })
  
  
  ## To get the TB_Adtl
  TB_Adtl <- reactive({
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    data <- read_excel(file, sheet = 3, col_names = TRUE)
    data$ID_STATUS <- ifelse(!is.na(data$ID_BL_APOPO), data$ID_BL_APOPO, data$ID_BL_DOTS)
    #data$ID_STATUS <- data$ID_BL_DOTS
    # Create the week number
    data$DATE <- as.Date(data$SESSION_DATE, format = "%Y/%m/%d")
    data$week_numbers <- format(data$DATE, format = "%U")
    data$month_numbers <- format(data$DATE, format = "%m")
    data$day_of_week <- weekdays(data$DATE)
    return(data)
  })
  
  unique_names <- reactive({
    temp <- TB_rat()
    data <- unique(temp$RAT_NAME)
    return(data)
  })
  
  
  
  
  
  
  ############### Some sensitivity and Specificity Tables ##################
  
  ## Get the specificity_table
  Specificity_table <- reactive({
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    ### Return NULL if there is no input file
    TB_rat <- TB_rat()
    Specificity_table <- data.frame()
    unique_names <- unique_names()
    # Loop over all rats' name
    for (name in unique_names) {
      # Splitting the wanted subset with specific name
      # Total Negative samples
      Neg_sample <- subset(TB_rat, RAT_NAME == name & ID_STATUS == 1)
      # Amount of Total Negative samples
      Neg_Amount <- nrow(Neg_sample)
      # HIT == FALSE
      HITS_sample <- subset(Neg_sample, HIT == "FALSE")
      # Amount of HIT == FALSE
      HITS_Amount <- nrow(HITS_sample)
      # Specificity
      Specificity_Rat <- HITS_Amount / Neg_Amount
      # Adding to existing table
      new_row <- data.frame(
        Rat_Name = name,
        HIT_FALSE = HITS_Amount,
        Total_Amount = Neg_Amount,
        Specificity = Specificity_Rat
      )
      Specificity_table <- rbind(Specificity_table, new_row)
    }
    return(Specificity_table)
  })
  
  ## Get the Specificity_table_dots
  
  Specificity_table_Dots <- reactive({
    
    ### Return NULL if there is no input file
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    TB_rat <- TB_rat()
    Specificity_table_Dots <- data.frame()
    unique_names <- unique_names()
    # Loop over all rats' name
    for (name in unique_names) {
      # Splitting the wanted subset with specific name
      # Total Negative samples
      Neg_sample <- subset(TB_rat, RAT_NAME == name & ID_BL_DOTS == 1)
      # Amount of Total Negative samples
      Neg_Amount <- nrow(Neg_sample)
      # HIT == FALSE
      HITS_sample <- subset(Neg_sample, HIT == "FALSE")
      # Amount of HIT == FALSE
      HITS_Amount <- nrow(HITS_sample)
      # Specificity
      Specificity_Rat <- HITS_Amount / Neg_Amount
      # Adding to existing table
      new_row <- data.frame(
        Rat_Name = name,
        HIT_FALSE = HITS_Amount,
        Total_Amount = Neg_Amount,
        Specificity = Specificity_Rat
      )
      Specificity_table_Dots <- rbind(Specificity_table_Dots, new_row)
    }
    return(Specificity_table_Dots)
  })
  
  
  
  ## Get the specificity_table
  Sensitivity_table <- reactive({
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    Sensitivity_table <- data.frame()
    TB_rat <- TB_rat()
    unique_names <- unique_names()
    for (name in unique_names) {
      # Splitting the wanted subset with specific name
      # Total Positive samples
      Pos_sample <- subset(TB_rat, RAT_NAME == name & ID_STATUS != 1)
      # Amount of Total Positive samples
      Pos_Amount <- nrow(Pos_sample)
      # HIT == TRUE
      HITS_sample <- subset(Pos_sample, HIT == "TRUE")
      # Amount of HIT == TRUE
      HITS_Amount <- nrow(HITS_sample)
      # Sensitivity
      Sensitivity_Rat <- HITS_Amount / Pos_Amount
      # Adding to existing table
      new_row <- data.frame(
        Rat_Name = name,
        HIT_True = HITS_Amount,
        Total_Amount = Pos_Amount,
        Sensitivity = Sensitivity_Rat
      )
      Sensitivity_table <- rbind(Sensitivity_table, new_row)
    }
    return(Sensitivity_table)
  })
  
  ## Get the Specificity_table_dots
  
  Sensitivity_table_Dots <- reactive({
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    ### Return NULL if there is no input file
    
    Sensitivity_table_Dots <- data.frame()
    TB_rat <- TB_rat()
    unique_names <- unique_names()
    # Loop over all rats' name
    for (name in unique_names) {
      # Splitting the wanted subset with specific name
      # Total Positive samples
      Pos_sample <- subset(TB_rat, RAT_NAME == name & ID_BL_DOTS != 1)
      # Amount of Total Positive samples
      Pos_Amount <- nrow(Pos_sample)
      # HIT == TRUE
      HITS_sample <- subset(Pos_sample, HIT == "TRUE")
      # Amount of HIT == TRUE
      HITS_Amount <- nrow(HITS_sample)
      # Sensitivity
      Sensitivity_Rat <- HITS_Amount / Pos_Amount
      # Adding to existing table
      new_row <- data.frame(
        Rat_Name = name,
        HIT_True = HITS_Amount,
        Total_Amount = Pos_Amount,
        Sensitivity = Sensitivity_Rat
      )
      Sensitivity_table_Dots <- rbind(Sensitivity_table_Dots, new_row)
    }
    return(Sensitivity_table_Dots)
  })
  
  
  
  
  
  
  
  
  
  
  
  ## Generate the output for the Sensitivity
  output$basicInformation_Sensitivity <-  DT::renderDataTable({
    ## Again if there is no file input we skip the render and return NULL
    data <- Sensitivity_table()
    datatable(data, options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20), rowCallback = JS(rowCallback), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
      "}")))%>% formatRound(c(4), 4)
  })
  
  ## Generate the output for the Specificity
  
  output$basicInformation_Specificity <- DT::renderDataTable({
    ## Again if there is no file input we skip the render and return NULL
    data <- Specificity_table()
    datatable(data, options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20), rowCallback = JS(rowCallback), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
      "}")))%>% formatRound(c(4), 4)
  })
  
  ## Merge the Sensitivity and Specificity table
  Individual_rat_performance <- reactive({
    ## Again if there is no file input we skip the render and return NULL
    Sensitivity_table <- Sensitivity_table()
    Specificity_table <- Specificity_table()
    return(merge(Sensitivity_table, Specificity_table, by = "Rat_Name", all.x = TRUE))
  })
  
  ## Generate highcharts for the sensitivity and specificity
  
  output$Individual_rat_performance <- renderHighchart({
    ## Again if there is no file input we skip the render and return NULL
    Individual_rat_performance <- Individual_rat_performance()
    #plot(Individual_rat_performance$Sensitivity, Individual_rat_performance$Specificity, pch = 19, col = c("red", "blue", "green", "orange", "purple"), ylab = "Specificity",xlab = "Sensitivity")
    #text(Individual_rat_performance$Sensitivity, Individual_rat_performance$Specificity,
    #labels = Individual_rat_performance$Rat_Name, pos = 4, cex = 0.8)
    #ggplot(Individual_rat_performance, aes(Sensitivity, Specificity,color = Rat_Name)) + geom_point() + labs(fill = "Rat Name") + 
    #theme(plot.background = element_rect(fill = "grey95"))
    hchart(Individual_rat_performance,type = "scatter", hcaes(x = Sensitivity, y = Specificity, group = Rat_Name),showInLegend = TRUE)
  })
  
  output$Individual_rat_bar <- renderHighchart({
    
    ## Again if there is no file input we skip the render and return NULL
    
    Specificity_table <- Specificity_table()
    Sensitivity_table <- Sensitivity_table()
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_add_series(name = "Total Negative Samples", data = Specificity_table$Total_Amount) %>%
      hc_add_series(name = "Total Positive Samples", data = Sensitivity_table$Total_Amount) %>%
      hc_add_series(name = "Hiteed Positive", data = Sensitivity_table$HIT_True) %>%
      hc_add_series(name = "Rejected Negative", data = Specificity_table$HIT_FALSE) %>%
      hc_xAxis(categories = Specificity_table$Rat_Name) %>%
      hc_yAxis(title = list(text = "Number of Hits/samples"))
  })
  
  ## Trainer 
  
  ## Trainer sensitivity table 
  Sensitivity_trainer_table <- reactive({
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    ## Again if there is no file input we skip the render and return NULL
    Sensitivity_trainer_table <- data.frame()
    TB_rat <- TB_rat()
    unique_names <- unique(TB_rat$TRAINER)
    # Loop over all trainers' name
    for (name in unique_names) {
      # Splitting the wanted subset with specific name
      # Total Positive samples
      Pos_sample <- subset(TB_rat, TRAINER == name & ID_STATUS != 1)
      # Amount of Total Positive samples
      Pos_Amount <- nrow(Pos_sample)
      # HIT == TRUE
      HITS_sample <- subset(Pos_sample, HIT == "TRUE")
      # Amount of HIT == TRUE
      HITS_Amount <- nrow(HITS_sample)
      # Sensitivity
      Sensitivity_trainer <- HITS_Amount / Pos_Amount
      # Adding to existing table
      new_row <- data.frame(
        Trainer_Name = name,
        HIT_True = HITS_Amount,
        Total_Amount = Pos_Amount,
        Sensitivity = Sensitivity_trainer
      )
      Sensitivity_trainer_table <- rbind(Sensitivity_trainer_table, new_row)
    }
    return(Sensitivity_trainer_table)
  })
  
  ## Trainer Specificity table
  
  Specificity_trainer_table <- reactive({
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    ## Again if there is no file input we skip the render and return NULL
    Specificity_trainer_table <- data.frame()
    TB_rat <- TB_rat()
    unique_names <- unique(TB_rat$TRAINER)
    # Loop over all rats' name
    for (name in unique_names) {
      # Splitting the wanted subset with specific name
      # Total Negative samples
      Neg_sample <- subset(TB_rat, TRAINER == name & TB_rat$ID_STATUS == 1)
      # Amount of Total Negative samples
      Neg_Amount <- nrow(Neg_sample)
      # HIT == FALSE
      HITS_sample <- subset(Neg_sample, HIT == "FALSE")
      # Amount of HIT == FALSE
      HITS_Amount <- nrow(HITS_sample)
      # Specificity
      Specificity_trainer <- HITS_Amount / Neg_Amount
      # Adding to existing table
      new_row <- data.frame(
        Trainer_Name = name,
        HIT_FALSE = HITS_Amount,
        Total_Amount = Neg_Amount,
        Specificity = Specificity_trainer
      )
      Specificity_trainer_table <- rbind(Specificity_trainer_table, new_row)
    }
    return(Specificity_trainer_table)
  })
  
  output$Trainer_Sensitivity <- DT::renderDataTable({
    Sensitivity_trainer_table <- Sensitivity_trainer_table()
    datatable(Sensitivity_trainer_table, options = list(pageLength = 8, lengthMenu = c(5, 10, 15, 20), rowCallback = JS(rowCallback), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
      "}"))) %>% formatRound(c(4), 4)
  })
  
  output$Trainer_Specificity <- DT::renderDataTable({
    Specificity_trainer_table <- Specificity_trainer_table()
    datatable(Specificity_trainer_table, options = list(pageLength = 8, lengthMenu = c(5, 10, 15, 20), rowCallback = JS(rowCallback), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
      "}"))) %>% formatRound(c(4), 4)
  })
  
  unique_days <- reactive({
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    TB_rat <- TB_rat()
    return(unique(TB_rat$DATE))
  })
  
  ## Daily Sensitivity Table for trainer
  
  Sensitivity_trainer_table_daily <- reactive({
    ## Again if there is no file input we skip the render and return NULL
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    unique_days <- unique_days()
    TB_rat <- TB_rat()
    # Loop over all rats' name
    Sensitivity_trainer_table_daily <- data.frame()
    unique_names <- unique_names()
    # Loop over all dates
    for (day in unique_days) {
      Daily <- subset(TB_rat, DATE == day)
      unique_names <- unique(Daily$TRAINER)
      # Loop over all rats' name
      for (name in unique_names) {
        # Splitting the wanted subset with specific name
        # Total Positive samples
        Pos_sample <- subset(Daily, TRAINER == name & ID_STATUS != 1)
        # Amount of Total Positive samples
        Pos_Amount <- nrow(Pos_sample)
        # HIT == TRUE
        HITS_sample <- subset(Pos_sample, HIT == "TRUE")
        # Amount of HIT == TRUE
        HITS_Amount <- nrow(HITS_sample)
        # Sensitivity
        Sensitivity_trainer <- HITS_Amount / Pos_Amount
        # Adding to existing table
        new_row <- data.frame(
          Trainer_Name = name,
          HIT_True = HITS_Amount,
          Total_Amount = Pos_Amount,
          Sensitivity = Sensitivity_trainer,
          Date = day
        )
        Sensitivity_trainer_table_daily <- rbind(Sensitivity_trainer_table_daily, new_row)
      }
    }
    Sensitivity_trainer_table_daily$Date <- as.Date(Sensitivity_trainer_table_daily$Date, origin = "1970-01-01", format = "%Y-%m-%d")
    return(Sensitivity_trainer_table_daily)
  })
  
  
  ## Daily Specificity Table for Trainer
  Specificity_trainer_table_daily <- reactive({
    ## Again if there is no file input we skip the render and return NULL
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    unique_days <- unique_days()
    TB_rat <- TB_rat()
    Specificity_trainer_table_daily <- data.frame()
    # Loop over all rats' name
    unique_names <- unique_names()
    # Loop over all dates
    for (day in unique_days) {
      Daily <- subset(TB_rat, DATE == day)
      unique_names <- unique(Daily$TRAINER)
      # Loop over all rats' name
      for (name in unique_names) {
        # Splitting the wanted subset with specific name
        # Total Negative samples
        Neg_sample <- subset(Daily, TRAINER == name & Daily$ID_STATUS == 1)
        # Amount of Total Negative samples
        Neg_Amount <- nrow(Neg_sample)
        # HIT == FALSE
        HITS_sample <- subset(Neg_sample, HIT == "FALSE")
        # Amount of HIT == FALSE
        HITS_Amount <- nrow(HITS_sample)
        # Specificity
        Specificity_trainer <- HITS_Amount / Neg_Amount
        # Adding to existing table
        new_row <- data.frame(
          Trainer_Name = name,
          HIT_FALSE = HITS_Amount,
          Total_Amount = Neg_Amount,
          Specificity = Specificity_trainer,
          Date = day
        )
        Specificity_trainer_table_daily <- rbind(Specificity_trainer_table_daily, new_row)
      }
    }
    Specificity_trainer_table_daily$Date <- as.Date(Specificity_trainer_table_daily$Date, origin = "1970-01-01", format = "%Y-%m-%d")
    return(Specificity_trainer_table_daily)
  })
  
  
  ## Create unique weeks
  unique_weeks <- reactive({
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    TB_rat <- TB_rat()
    return(unique(TB_rat$week_numbers))
  })
  
  ## Generate weekly tables
  
  Sensitivity_trainer_table_weekly <- reactive({
    ## Again if there is no file input we skip the render and return NULL
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    unique_weeks <- unique_weeks()
    unique_names <- unique_names()
    TB_rat <- TB_rat()
    # Loop over all rats' name
    Sensitivity_trainer_table_weekly <- data.frame()
    
    # Loop over all dates
    for (week in unique_weeks) {
      Weekly <- subset(TB_rat, week_numbers == week)
      unique_names <- unique(Weekly$TRAINER)
      # Loop over all rats' name
      for (name in unique_names) {
        # Splitting the wanted subset with specific name
        # Total Positive samples
        Pos_sample <- subset(Weekly, TRAINER == name & ID_STATUS != 1)
        # Amount of Total Positive samples
        Pos_Amount <- nrow(Pos_sample)
        # HIT == TRUE
        HITS_sample <- subset(Pos_sample, HIT == "TRUE")
        # Amount of HIT == TRUE
        HITS_Amount <- nrow(HITS_sample)
        # Sensitivity
        Sensitivity_trainer <- HITS_Amount / Pos_Amount
        # Adding to existing table
        new_row <- data.frame(
          Trainer_Name = name,
          HIT_True = HITS_Amount,
          Total_Amount = Pos_Amount,
          Sensitivity = Sensitivity_trainer,
          Week = week
        )
        Sensitivity_trainer_table_weekly <- rbind(Sensitivity_trainer_table_weekly, new_row)
      }
    }
    return(Sensitivity_trainer_table_weekly)
  })
  
  
  ## Weekly Specificity Table for Trainer
  Specificity_trainer_table_weekly <- reactive({
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    ## Again if there is no file input we skip the render and return NULL
    unique_weeks <- unique_weeks()
    unique_names <- unique_names()
    TB_rat <- TB_rat()
    Specificity_trainer_table_weekly <- data.frame()
    
    # Loop over all dates
    for (week in unique_weeks) {
      Weekly <- subset(TB_rat, week_numbers == week)
      unique_names <- unique(Weekly$TRAINER)
      # Loop over all rats' name
      for (name in unique_names) {
        # Splitting the wanted subset with specific name
        # Total Negative samples
        Neg_sample <- subset(Weekly, TRAINER == name & ID_STATUS == 1)
        # Amount of Total Negative samples
        Neg_Amount <- nrow(Neg_sample)
        # HIT == FALSE
        HITS_sample <- subset(Neg_sample, HIT == "FALSE")
        # Amount of HIT == FALSE
        HITS_Amount <- nrow(HITS_sample)
        # Specificity
        Specificity_trainer <- HITS_Amount / Neg_Amount
        # Adding to existing table
        new_row <- data.frame(
          Trainer_Name = name,
          HIT_FALSE = HITS_Amount,
          Total_Amount = Neg_Amount,
          Specificity = Specificity_trainer,
          Week = week
        )
        Specificity_trainer_table_weekly <- rbind(Specificity_trainer_table_weekly, new_row)
      }
    }
    return(Specificity_trainer_table_weekly)
  })
  
  unique_months <- reactive({
    TB_rat <- TB_rat()
    return(unique(TB_rat$month_numbers))
  })
  
  
  Sensitivity_trainer_table_monthly <- reactive({
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    ## Again if there is no file input we skip the render and return NULL
    unique_months <- unique_months()
    unique_names <- unique_names()
    TB_rat <- TB_rat()
    # Loop over all rats' name
    Sensitivity_trainer_table_monthly <- data.frame()
    
    # Loop over all dates
    for (month in unique_months) {
      Monthly <- subset(TB_rat, month_numbers == month)
      unique_names <- unique(Monthly$TRAINER)
      # Loop over all rats' name
      for (name in unique_names) {
        # Splitting the wanted subset with specific name
        # Total Positive samples
        Pos_sample <- subset(Monthly, TRAINER == name & ID_STATUS != 1)
        # Amount of Total Positive samples
        Pos_Amount <- nrow(Pos_sample)
        # HIT == TRUE
        HITS_sample <- subset(Pos_sample, HIT == "TRUE")
        # Amount of HIT == TRUE
        HITS_Amount <- nrow(HITS_sample)
        # Sensitivity
        Sensitivity_trainer <- HITS_Amount / Pos_Amount
        # Adding to existing table
        new_row <- data.frame(
          Trainer_Name = name,
          HIT_True = HITS_Amount,
          Total_Amount = Pos_Amount,
          Sensitivity = Sensitivity_trainer,
          Month = month
        )
        Sensitivity_trainer_table_monthly <- rbind(Sensitivity_trainer_table_monthly, new_row)
      }
    }
    return(Sensitivity_trainer_table_monthly)
  })
  
  
  ## Daily Specificity Table for Trainer
  Specificity_trainer_table_monthly <- reactive({
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    ## Again if there is no file input we skip the render and return NULL
    
    unique_months <- unique_months()
    unique_names <- unique_names()
    TB_rat <- TB_rat()
    Specificity_trainer_table_monthly <- data.frame()
    
    # Loop over all dates
    for (month in unique_months) {
      Monthly <- subset(TB_rat, month_numbers == month)
      unique_names <- unique(Monthly$TRAINER)
      # Loop over all rats' name
      for (name in unique_names) {
        # Splitting the wanted subset with specific name
        # Total Negative samples
        Neg_sample <- subset(Monthly, TRAINER == name & ID_STATUS == 1)
        # Amount of Total Negative samples
        Neg_Amount <- nrow(Neg_sample)
        # HIT == FALSE
        HITS_sample <- subset(Neg_sample, HIT == "FALSE")
        # Amount of HIT == FALSE
        HITS_Amount <- nrow(HITS_sample)
        # Specificity
        Specificity_trainer <- HITS_Amount / Neg_Amount
        # Adding to existing table
        new_row <- data.frame(
          Trainer_Name = name,
          HIT_FALSE = HITS_Amount,
          Total_Amount = Neg_Amount,
          Specificity = Specificity_trainer,
          Month = month
        )
        Specificity_trainer_table_monthly <- rbind(Specificity_trainer_table_monthly, new_row)
      }
    }
    return(Specificity_trainer_table_monthly)
  })
  
  ## For Blind samples 
  
  Sensitivity_blind_table <- reactive({
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    ## Again if there is no file input we skip the render and return NULL
    unique_names <- unique_names()
    TB_rat <- TB_rat()
    Sensitivity_blind_table<- data.frame() 
    for (name in unique_names) {
      # Splitting the wanted subset with specific name
      # Total Positive samples
      blind_sample <- subset(TB_rat, RAT_NAME == name & STATUS_BLINDPOS == "TRUE")
      # Amount of Total Positive samples
      blind_sample_Amount <- nrow(blind_sample)
      # HIT == TRUE
      HITS_sample <- subset(blind_sample, HIT == "TRUE")
      # Amount of HIT == TRUE
      HITS_Amount <- nrow(HITS_sample)
      # Sensitivity
      Sensitivity_Rat <- HITS_Amount / blind_sample_Amount
      # Adding to existing table
      new_row <- data.frame(
        Rat_Name = name,
        HIT_True_Blind = HITS_Amount,
        Total_Amount = blind_sample_Amount,
        Sensitivity_Blind = Sensitivity_Rat
      )
      Sensitivity_blind_table <- rbind(Sensitivity_blind_table, new_row)
    }
    return(Sensitivity_blind_table)
  })
  
  NewCase_table <- reactive({
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    ## Again if there is no file input we skip the render and return NULL
    unique_names <- unique_names()
    NewCase_table <- data.frame()
    TB_rat <- TB_rat()
    for (name in unique_names) {
      new_cases_sample <- subset(TB_rat, RAT_NAME == name & ID_BL_DOTS == 1 & ID_BL_APOPO > 1)
      # Amount of Total Positive samples
      new_cases_amount <- nrow(new_cases_sample)
      # HIT == TRUE
      HITS_sample <- subset(new_cases_sample, HIT == "TRUE")
      # Amount of HIT == TRUE
      HITS_Amount <- nrow(HITS_sample)
      
      new_row <- data.frame(
        Rat_Name = name,
        HIT_ON_NewCase = HITS_Amount,
        Total_Amount = new_cases_amount
      )
      NewCase_table <- rbind(NewCase_table, new_row)
    }
    return(NewCase_table)
  })
  
  
  
  
  
  
  
  ## Overall Program Stuff
  # User input
  output$dateInput <- renderUI({
    TB_Adtl <- TB_Adtl()
    if (input$timePeriod == "Day") {
      selectInput("date", "Select Date:", choices = unique(TB_Adtl$DATE), selected = Sys.Date())
    } else {
      NULL
    }
  })
  
  output$weekInput <- renderUI({
    TB_Adtl <- TB_Adtl()
    if (input$timePeriod == "Week") {
      selectInput("week", "Select Week:", choices = unique(TB_Adtl$week_numbers))
    } else {
      NULL
    }
  })
  
  output$monthInput <- renderUI({
    TB_Adtl <- TB_Adtl()
    if (input$timePeriod == "Month") {
      selectInput("month", "Select Month:", choices = unique(TB_Adtl$month_numbers))
    } else {
      NULL
    }
  })
  
  filteredData <- reactive({
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    TB_Adtl <- TB_Adtl()
    if (input$timePeriod == "Overall") {
      TB_Adtl
    } else if (input$timePeriod == "Day") {
      subset(TB_Adtl, as.Date(DATE) == input$date)
    } else if (input$timePeriod == "Week") {
      subset(TB_Adtl, week_numbers == input$week)
    } else if (input$timePeriod == "Month") {
      subset(TB_Adtl, month_numbers == input$month)
    }
  })
  
  # InfoBox for the Overview of rats
  output$BlindsSampleBox <- renderValueBox({
    filteredData <- filteredData()
    TotalBlindSample_Filter <- n_distinct(subset(filteredData, STATUS_BLINDPOS == "TRUE")$ID_SAMPLE)
    valueBox(
      TotalBlindSample_Filter, "Blinds Samples", icon = icon("eye"),
      color = "blue"
    )
  })
  
  output$DOTSNegativeBox <- renderValueBox({
    filteredData <- filteredData()
    TotalDotsNegative_Filter <- n_distinct(subset(filteredData, ID_BL_DOTS == 1)$ID_SAMPLE)
    valueBox(
      TotalDotsNegative_Filter,"DOTS Negative Samples",  icon = icon("n"),
      color = "green"
    )
  })
  
  output$DOTSPositiveBox <- renderValueBox({
    filteredData <- filteredData()
    TotalDOTSPos_Filter <- n_distinct(subset(filteredData, ID_BL_DOTS != 1)$ID_SAMPLE)
    valueBox(
      TotalDOTSPos_Filter, "DOTS Positive Samples",icon = icon("p"),
      color = "maroon"
    )
  })
  
  output$NewcaseBox <- renderValueBox({
    filteredData <- filteredData()
    TotalNewCase_Filter <- n_distinct(subset(filteredData, ID_BL_DOTS == 1 & ID_BL_APOPO > 1)$ID_SAMPLE)
    valueBox(
      TotalNewCase_Filter, "New cases by samples",icon = icon("t"),
      color = "purple"
    )
  })
  
  output$NewpatientBox <- renderValueBox({
    filteredData <- filteredData()
    TotalNewCasePatient_filter <- n_distinct(subset(filteredData, ID_BL_DOTS == 1 & ID_BL_APOPO > 1)$ID_PATIENT)
    valueBox(
      TotalNewCasePatient_filter, "New patients",icon = icon("t"),
      color = "yellow"
    )
  })
  
  output$DotsPosPatientBox <- renderValueBox({
    filteredData <- filteredData()
    TotalDotsPosPatient_filter <- n_distinct(subset(filteredData, ID_BL_DOTS != 1)$ID_PATIENT)
    valueBox(
      TotalDotsPosPatient_filter, "DOTS Positive Patient",icon = icon("t"),
      color = "red"
    )
  })
  
  output$DotsNegPatientBox <- renderValueBox({
    filteredData <- filteredData()
    TotalDotsNegPatient_filter <- n_distinct(subset(filteredData, ID_BL_DOTS == 1)$ID_PATIENT)
    valueBox(
      TotalDotsNegPatient_filter, "DOTS Negative Patient",icon = icon("p"),
      color = "maroon"
    )
  })
  
  output$TotalBlindPatientBox <- renderValueBox({
    filteredData <- filteredData()
    TotalBlindPatient_filter <- n_distinct(subset(filteredData, STATUS_BLINDPOS == "TRUE")$ID_PATIENT)
    valueBox(
      TotalBlindPatient_filter, "Blind Patients",icon = icon("eye"),
      color = "orange"
    )
  })
  
  output$UncomCaseBox <- renderValueBox({
    filteredData <- filteredData()
    UnconfirmedNegHit_filter <- n_distinct(subset(filteredData, RatHit > 0 & ID_BL_DOTS == 1 & ID_BL_APOPO == 1)$ID_SAMPLE)
    valueBox(
      UnconfirmedNegHit_filter, "Uncomfirm Hits",icon = icon("eye"),
      color = "navy"
    )
  })
  
  
  output$overviewTable <- renderDataTable({
    
    filteredData <- filteredData()
    
    # Calculate the required values based on the filtered data
    TotalDOTSPos_filter <- n_distinct(subset(filteredData, ID_BL_DOTS != 1)$ID_SAMPLE)
    TotalDotsPosPatient_filter <- n_distinct(subset(filteredData, ID_BL_DOTS != 1)$ID_PATIENT)
    TotalBlindSample_filter <- n_distinct(subset(filteredData, STATUS_BLINDPOS == "TRUE")$ID_SAMPLE)
    TotalBlindPatient_filter <- n_distinct(subset(filteredData, STATUS_BLINDPOS == "TRUE")$ID_PATIENT)
    TotalDotsNegative_filter <- n_distinct(subset(filteredData, ID_BL_DOTS == 1)$ID_SAMPLE)
    TotalDotsNegPatient_filter <- n_distinct(subset(filteredData, ID_BL_DOTS == 1)$ID_PATIENT)
    TotalNegativeIndicated_filter <- n_distinct(subset(filteredData, ID_BL_DOTS == 1 & RatHit > 0)$ID_SAMPLE)
    UnconfirmedNegHit_filter <- n_distinct(subset(filteredData, RatHit > 0 & ID_BL_DOTS == 1 & ID_BL_APOPO == 1)$ID_SAMPLE)
    TotalNewCase_filter <- n_distinct(subset(filteredData, ID_BL_DOTS == 1 & ID_BL_APOPO > 1)$ID_SAMPLE)
    TotalNewCasePatient_filter <- n_distinct(subset(filteredData, ID_BL_DOTS == 1 & ID_BL_APOPO > 1)$ID_PATIENT)
    TotalSampleCase_filter <- TotalDOTSPos_filter + TotalDotsNegative_filter
    TotalPatientCase_filter <- TotalDotsPosPatient_filter + TotalDotsNegPatient_filter
    
    # Create the overview table with the calculated values
    overview_TPR <- data.frame(
      Category = c("DOTS Positive", "Blinds", "DOTS Negative", "Neg samples indicated", 
                   "Unconfirmed HITS", "New Cases", "Avg #Rats HIT New Case", 
                   "Total Cases", "Increase in detection"),
      Samples = c(TotalDOTSPos_filter, TotalBlindSample_filter, TotalDotsNegative_filter, TotalNegativeIndicated_filter,
                  UnconfirmedNegHit_filter, TotalNewCase_filter, NA, TotalSampleCase_filter, NA),
      Patients = c(TotalDotsPosPatient_filter, TotalBlindPatient_filter, TotalDotsNegPatient_filter, NA, NA,
                   TotalNewCasePatient_filter, NA, TotalPatientCase_filter, NA),
      Prevalence = c(paste0(round((TotalDOTSPos_filter / TotalSampleCase_filter) * 100, 1), "%"), 
                     paste0(round((TotalBlindSample_filter / TotalDOTSPos_filter) * 100, 1), "%"), 
                     paste0(round((TotalDotsNegative_filter / TotalSampleCase_filter) * 100, 1), "%"),
                     paste0(round((TotalNegativeIndicated_filter / TotalDotsNegative_filter) * 100, 1), "%"), 
                     paste0(round((UnconfirmedNegHit_filter / TotalNegativeIndicated_filter) * 100, 1), "%"),
                     paste0(round((TotalNewCase_filter / TotalDotsNegative_filter) * 100, 1), "%"), NA, NA,
                     paste0(round(( TotalNewCase_filter / TotalDotsPosPatient_filter) * 100, 1), "%"))
    ) 
    
    datatable(overview_TPR, options = list(pageLength = 8, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
      "}")))
  })
  
  
  # Program-Level Sample Details
  filteredData_PLSD <- reactive({
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    TB_Adtl <- TB_Adtl()
    if (input$timePeriod_PLSD == "Overall") {
      TB_Adtl
    } else if (input$timePeriod_PLSD == "Day") {
      subset(TB_Adtl, as.Date(DATE) == input$date_PLSD)
    } else if (input$timePeriod_PLSD == "Week") {
      subset(TB_Adtl, week_numbers == input$week_PLSD)
    } else if (input$timePeriod_PLSD == "Month") {
      subset(TB_Adtl, month_numbers == input$month_PLSD)
    }
  })
  
  output$dateInput_PLSD <- renderUI({
    TB_Adtl <- TB_Adtl()
    if (input$timePeriod_PLSD == "Day") {
      selectInput("date_PLSD", "Select Date:", choices = unique(TB_Adtl$DATE), selected = Sys.Date())
    } else {
      NULL
    }
  })
  
  output$weekInput_PLSD <- renderUI({
    TB_Adtl <- TB_Adtl()
    if (input$timePeriod_PLSD == "Week") {
      selectInput("week_PLSD", "Select Week:", choices = unique(TB_Adtl$week_numbers))
    } else {
      NULL
    }
  })
  
  output$monthInput_PLSD <- renderUI({
    TB_Adtl <- TB_Adtl()
    if (input$timePeriod_PLSD == "Month") {
      selectInput("month_PLSD", "Select Month:", choices = unique(TB_Adtl$month_numbers))
    } else {
      NULL
    }
  })
  
  
  output$PLSD_DOTs <- renderDataTable({

    
    filteredData <- filteredData_PLSD()
    # Calculate the required values based on the filtered data
    TotalDOTSPos3Plus <- n_distinct(subset(filteredData, ID_BL_DOTS == 13)$ID_SAMPLE)
    TotalDOTSPos2Plus <- n_distinct(subset(filteredData, ID_BL_DOTS == 12)$ID_SAMPLE)
    TotalDOTSPos1Plus <- n_distinct(subset(filteredData, ID_BL_DOTS == 11)$ID_SAMPLE)
    TotalDOTSPos_filter <- n_distinct(subset(filteredData, ID_BL_DOTS != 1)$ID_SAMPLE)
    TotalDOTSPosScanty <- TotalDOTSPos_filter - TotalDOTSPos3Plus - TotalDOTSPos2Plus - TotalDOTSPos1Plus
    
    # Create the overview table with the calculated values
    overview_PLSD_DOTs <- data.frame(
      "Bact load" = c("3+", "2+", "1+", "Scanty", "Total"),
      "Total" = c(TotalDOTSPos3Plus, TotalDOTSPos2Plus, TotalDOTSPos1Plus, TotalDOTSPosScanty, TotalDOTSPos_filter),
      "Percentage" = c(
        paste0(round((TotalDOTSPos3Plus / TotalDOTSPos_filter) * 100, 1), "%"),
        paste0(round((TotalDOTSPos2Plus / TotalDOTSPos_filter) * 100, 1), "%"),
        paste0(round((TotalDOTSPos1Plus / TotalDOTSPos_filter) * 100, 1), "%"),
        paste0(round((TotalDOTSPosScanty / TotalDOTSPos_filter) * 100, 1), "%"),
        paste0(100.0, "%")
      )
    )
    
    datatable(overview_PLSD_DOTs, options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
      "}"), rowCallback = JS(rowCallback)))
  })
  
  
  output$PLSD_newcase <- renderDataTable({
    # Skip the render if there iuss no input

    filteredData <- filteredData_PLSD()
    
    # Calculate the required values based on the filtered data
    TotalNewCase3Plus <- n_distinct(subset(filteredData, (ID_BL_DOTS == 1 & ID_BL_APOPO > 0) & ID_STATUS == 13)$ID_SAMPLE)
    TotalNewCase2Plus <- n_distinct(subset(filteredData, (ID_BL_DOTS == 1 & ID_BL_APOPO > 0) & ID_STATUS == 12)$ID_SAMPLE)
    TotalNewCase1Plus <- n_distinct(subset(filteredData, (ID_BL_DOTS == 1 & ID_BL_APOPO > 0) & ID_STATUS == 11)$ID_SAMPLE)
    TotalNewCaseScanty <- n_distinct(subset(filteredData, (ID_BL_DOTS == 1 & ID_BL_APOPO > 0) & ID_STATUS > 1)$ID_SAMPLE) - TotalNewCase1Plus - TotalNewCase2Plus - TotalNewCase3Plus
    TotalNewCase_filter <- n_distinct(subset(filteredData, ID_BL_DOTS == 1 & ID_BL_APOPO > 1)$ID_SAMPLE)
    
    
    
    # Create the overview table with the calculated values
    overview_PLSD_newcase <- data.frame(
      "Bact load" = c("3+", "2+", "1+", "Scanty", "Total"),
      "Total" = c(TotalNewCase3Plus, TotalNewCase2Plus, TotalNewCase1Plus, TotalNewCaseScanty, TotalNewCase_filter),
      "Percentage" = c(
        paste0(round((TotalNewCase3Plus / TotalNewCase_filter) * 100, 1), "%"),
        paste0(round((TotalNewCase2Plus / TotalNewCase_filter) * 100, 1), "%"),
        paste0(round((TotalNewCase1Plus / TotalNewCase_filter) * 100, 1), "%"),
        paste0(round((TotalNewCaseScanty / TotalNewCase_filter) * 100, 1), "%"),
        paste0(100.0, "%")
      )
    )
    
    datatable(overview_PLSD_newcase, options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
      "}"), rowCallback = JS(rowCallback)))
  })
  
  # Create the bar chart Dots
  output$barChart_Dots <- renderHighchart({
    # Skip the render if there iuss no input

    filteredData <- filteredData_PLSD()
    
    # Calculate the required values based on the filtered data
    TotalDOTSPos3Plus_F <- n_distinct(subset(filteredData, ID_BL_DOTS == 13)$ID_SAMPLE)
    TotalDOTSPos2Plus_F <- n_distinct(subset(filteredData, ID_BL_DOTS == 12)$ID_SAMPLE)
    TotalDOTSPos1Plus_F <- n_distinct(subset(filteredData, ID_BL_DOTS == 11)$ID_SAMPLE)
    TotalDOTSPos_F <- n_distinct(subset(filteredData, ID_BL_DOTS != 1)$ID_SAMPLE)
    TotalDOTSPosScanty_F <- TotalDOTSPos_F - TotalDOTSPos3Plus_F - TotalDOTSPos2Plus_F - TotalDOTSPos1Plus_F
    
    # Prepare the data
    dots_cases <- data.frame(
      Bact.load = c("3+", "2+", "1+", "Scanty"),
      Total = c(TotalDOTSPos3Plus_F, TotalDOTSPos2Plus_F,
                TotalDOTSPos1Plus_F, TotalDOTSPosScanty_F)
    )
    #ggplot(dots_cases, aes(x = Bact.load, y = Total, color = Bact.load)) +
      #geom_bar(stat = "identity", fill = "blue") +
      #labs(x = "Bacterial Load", y = "Total Count", title = "DOTS Cases") +
      #theme(plot.title = element_text(face = "bold", size = 20)) +
      #labs(title = "DOTS Cases")
    hchart(dots_cases, type = "column", hcaes(x = Bact.load, y = Total)) %>%  hc_title(
      text = "BarChart for New cases",
      align = "left"
    )
  })
  
  
  # Create the bar chart newcases
  output$barChart_newcases <- renderHighchart({
    # Skip the render if there iuss no input

    filteredData <- filteredData_PLSD()
    
    # Calculate the required values based on the filtered data
    TotalNewCase3Plus_F <- n_distinct(subset(filteredData, (ID_BL_DOTS == 1 & ID_BL_APOPO > 0) & ID_STATUS == 13)$ID_SAMPLE)
    TotalNewCase2Plus_F <- n_distinct(subset(filteredData, (ID_BL_DOTS == 1 & ID_BL_APOPO > 0) & ID_STATUS == 12)$ID_SAMPLE)
    TotalNewCase1Plus_F <- n_distinct(subset(filteredData, (ID_BL_DOTS == 1 & ID_BL_APOPO > 0) & ID_STATUS == 11)$ID_SAMPLE)
    TotalNewCaseScanty_F <- n_distinct(subset(filteredData, (ID_BL_DOTS == 1 & ID_BL_APOPO > 0) & ID_STATUS > 1)$ID_SAMPLE) - TotalNewCase1Plus_F - TotalNewCase2Plus_F - TotalNewCase3Plus_F
    
    # Prepare the data
    new_cases <- data.frame(
      Bact.load = c("3+", "2+", "1+", "Scanty"),
      Total = c(TotalNewCase3Plus_F, TotalNewCase2Plus_F,
                TotalNewCase1Plus_F, TotalNewCaseScanty_F)
    )
    #ggplot(new_cases, aes(x = Bact.load, y = Total, color = Bact.load)) +
      #geom_bar(stat = "identity", fill = "blue") +
      #labs(x = "Bacterial Load", y = "Total Count", title = "New Cases") +
      #theme(plot.title = element_text(face = "bold", size = 20)) +
      #labs(title = "New Cases")
    hchart(new_cases, type = "column", hcaes(x = Bact.load, y = Total)) %>%  hc_title(
      text = "BarChart for New cases",
      align = "left"
    )
  })
  
  overview_AIRR <- reactive({
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    ## Reactive some required data
    TB_rat <- TB_rat()
    Sensitivity_table_Dots <- Sensitivity_table_Dots()
    Sensitivity_blind_table <- Sensitivity_blind_table()
    Specificity_table_Dots <- Specificity_table_Dots()
    NewCase_table<- NewCase_table()
    ## Some Basic Data
    TotalRats <- n_distinct(TB_rat$RAT_NAME)
    
    Average_Dots_Hits <- mean(Sensitivity_table_Dots$HIT_True)
    SD_Dots_Hits <- sd(Sensitivity_table_Dots$HIT_True)
    
    Average_Sen <- mean(Sensitivity_table_Dots$Sensitivity)
    SD_Average_Sen <- sd(Sensitivity_table_Dots$Sensitivity)
    
    Average_Blind_Hit <- mean(Sensitivity_blind_table$HIT_True_Blind)
    SD_Blind_Hit <- sd(Sensitivity_blind_table$HIT_True_Blind)
    
    Average_Blind_Sensitivity <- mean(Sensitivity_blind_table$Sensitivity_Blind)
    SD_Blind_Sensitivity <- sd(Sensitivity_blind_table$Sensitivity_Blind)
    
    Average_Negative_Hits <- mean(Specificity_table_Dots$HIT_FALSE)
    SD_Negative_Hits <- sd(Specificity_table_Dots$HIT_FALSE)
    
    Average_Negative_Specificity <- mean(Specificity_table_Dots$Specificity)
    SD_Negative_Specificity <- sd(Specificity_table_Dots$Specificity)
    
    Average_Hits_NewCase <- mean(NewCase_table$HIT_ON_NewCase)
    SD_Hits_NewCase <- sd(NewCase_table$HIT_ON_NewCase)
    
    ## The overall datatable
    overview_AIRR <- data.frame(
      Total = c("Rats", "DOTS Positive", "Sensitivity-Dots", "Blinds", 
                "Sensitivity-Blinds", "DOTS Negative", "Specificity-Dots", 
                "New Cases"),
      Indicated = c(TotalRats, round(Average_Dots_Hits,2), paste0(round(Average_Sen*100,1),'%'), round(Average_Blind_Hit,2),
                    paste0(round(Average_Blind_Sensitivity*100,1),'%'), round(Average_Negative_Hits,2),
                    paste0(round((Average_Negative_Specificity) * 100, 1), "%"), 
                    round(Average_Hits_NewCase,2)
      ),
      SD = c("", round(SD_Dots_Hits,2), round(SD_Average_Sen,2), round(SD_Blind_Hit,2)
             , round(SD_Blind_Sensitivity,2), round(SD_Negative_Hits,2),
             round(SD_Negative_Specificity,2), round(SD_Hits_NewCase,2))
    ) 
    return(overview_AIRR)
  })
  
  
  
  # Visualization for overview_AIRR data frame
  output$sensitivitySpecificityPlot <- renderHighchart({
    overview_AIRR <- overview_AIRR()
    # Filter the data for sensitivity and specificity rows
    filtered_data <- subset(overview_AIRR, Total %in% c("Sensitivity-Dots", "Sensitivity-Blinds", "Specificity-Dots"))
    
    # Convert the percentage values to numeric
    filtered_data$Percentage <- as.numeric(gsub("%", "", filtered_data$Indicated))
    
    # Create a line plot
    hchart(filtered_data, type = "line", hcaes(x = Total, y = Percentage))
  })
  
  ## Average Rat Sample Details 
  
  output$dateInput_ARSD <- renderUI({
    TB_Adtl <- TB_Adtl()
    if (input$timePeriod_ARSD == "Day") {
      selectInput("date_ARSD", "Select Date:", choices = unique(TB_Adtl$DATE), selected = Sys.Date())
    } else {
      NULL
    }
  })
  
  output$weekInput_ARSD <- renderUI({
    TB_Adtl <- TB_Adtl()
    if (input$timePeriod_ARSD == "Week") {
      selectInput("week_ARSD", "Select Week:", choices = unique(TB_Adtl$week_numbers))
    } else {
      NULL
    }
  })
  
  output$monthInput_ARSD <- renderUI({
    TB_Adtl <- TB_Adtl()
    if (input$timePeriod_ARSD == "Month") {
      selectInput("month_ARSD", "Select Month:", choices = unique(TB_Adtl$month_numbers))
    } else {
      NULL
    }
  })
  
  #filerdata
  filteredData_ARSD <- reactive({
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    TB_Adtl <- TB_Adtl()
    if (input$timePeriod_ARSD == "Overall") {
      TB_Adtl
    } else if (input$timePeriod_ARSD == "Day") {
      subset(TB_Adtl, as.Date(DATE) == input$date_ARSD)
    } else if (input$timePeriod_ARSD == "Week") {
      subset(TB_Adtl, week_numbers == input$week_ARSD)
    } else if (input$timePeriod_ARSD == "Month") {
      subset(TB_Adtl, month_numbers == input$month_ARSD)
    }
  })
  
  
  output$ARSD_DOTs <- DT::renderDataTable({
    
    filteredData <- filteredData_ARSD()
    
    total3plusHit <- n_distinct(subset(filteredData, filteredData$ID_BL_DOTS == 13 &
                                         filteredData$RatHit > 0)$ID_SAMPLE)
    total2plusHit <- n_distinct(subset(filteredData, filteredData$ID_BL_DOTS == 12 &
                                         filteredData$RatHit > 0)$ID_SAMPLE)
    total1plusHit <- n_distinct(subset(filteredData, filteredData$ID_BL_DOTS == 11 &
                                         filteredData$RatHit > 0)$ID_SAMPLE)
    totalScantyHit <- n_distinct(subset(filteredData, filteredData$ID_BL_DOTS != 11 &
                                          filteredData$ID_BL_DOTS != 12 &
                                          filteredData$ID_BL_DOTS != 13 &
                                          filteredData$ID_BL_DOTS != 1 &
                                          filteredData$ID_BL_DOTS != 0 &
                                          filteredData$RatHit > 0)$ID_SAMPLE)
    
    TotalDOTSPos3Plus_Filter <- n_distinct(subset(filteredData, ID_BL_DOTS == 13)$ID_SAMPLE)
    TotalDOTSPos2Plus_Filter <- n_distinct(subset(filteredData, ID_BL_DOTS == 12)$ID_SAMPLE)
    TotalDOTSPos1Plus_Filter <- n_distinct(subset(filteredData, ID_BL_DOTS == 11)$ID_SAMPLE)
    TotalDOTSPos_Filter <- n_distinct(subset(filteredData, ID_BL_DOTS != 1)$ID_SAMPLE)
    TotalDOTSPosScanty_Filter <- TotalDOTSPos_Filter - TotalDOTSPos3Plus_Filter - TotalDOTSPos2Plus_Filter - TotalDOTSPos1Plus_Filter
    
    
    
    # %HIT
    percentage3plusHit <- total3plusHit/TotalDOTSPos3Plus_Filter
    percentage2plusHit <- total2plusHit/TotalDOTSPos3Plus_Filter
    percentage1plusHit <- total1plusHit/TotalDOTSPos3Plus_Filter
    percentageScantyHit <- totalScantyHit/TotalDOTSPosScanty_Filter
    
    # 3+,2+,1+,Scanty
    total123plusScantyHit <- total3plusHit + total2plusHit 
    + total2plusHit+ totalScantyHit
    
    # %Total
    percentage3plus <- TotalDOTSPos3Plus_Filter/TotalDOTSPos_Filter
    percentage2plus <- TotalDOTSPos2Plus_Filter/TotalDOTSPos_Filter
    percentage1plus <- TotalDOTSPos1Plus_Filter/TotalDOTSPos_Filter
    percentageScanty <- TotalDOTSPosScanty_Filter/TotalDOTSPos_Filter
    
    #  Create the dataframe for the Average Rat Sample Details table
    overview_ARSD_Dots <- data.frame(
      "Bact load" = c("3+", "2+", "1+", "Scanty", "Samples"),
      "Percentage HIT" = c(percentage3plusHit, percentage2plusHit, percentage1plusHit,
                           percentageScantyHit, total123plusScantyHit),
      "Percentage Total" = c(paste0(round(percentage3plus * 100, 1), "%"),
                             paste0(round(percentage2plus * 100, 1), "%"),
                             paste0(round(percentage1plus * 100, 1),"%"),
                             paste0(round(percentageScanty * 100, 1),"%"),
                             paste0(100.0,"%"))
    )
    
    datatable(overview_ARSD_Dots, options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
      "}"), rowCallback = JS(rowCallback))) %>% formatRound(c(2), 4)
  })
  
  
  # New cases
  output$ARSD_newcase <- DT::renderDataTable({
    
    filteredData <- filteredData_ARSD()
    
    TotalNewCase3PlushHit <- n_distinct(subset(filteredData, 
                                               (filteredData$ID_BL_DOTS == 1 
                                                & filteredData$ID_BL_APOPO > 0)
                                               & filteredData$ID_STATUS == 13
                                               & filteredData$RatHit > 0)$ID_SAMPLE)  
    TotalNewCase2PlushHit <- n_distinct(subset(filteredData, 
                                               (filteredData$ID_BL_DOTS == 1 
                                                & filteredData$ID_BL_APOPO > 0)
                                               & filteredData$ID_STATUS == 12
                                               & filteredData$RatHit > 0)$ID_SAMPLE) 
    TotalNewCase1PlushHit <- n_distinct(subset(filteredData, 
                                               (filteredData$ID_BL_DOTS == 1 
                                                & filteredData$ID_BL_APOPO > 0)
                                               & filteredData$ID_STATUS == 11
                                               & filteredData$RatHit > 0)$ID_SAMPLE)
    
    TotalScantyHit_Newcase <- n_distinct(subset(filteredData, filteredData$ID_BL_DOTS != 11 &
                                                  filteredData$ID_BL_DOTS != 12 &
                                                  filteredData$ID_BL_DOTS != 13 &
                                                  filteredData$ID_BL_DOTS == 1 &
                                                  filteredData$ID_BL_APOPO > 0 &
                                                  filteredData$RatHit > 0)$ID_SAMPLE)
    TotalNewCase3Plus_Filter <- n_distinct(subset(filteredData, (ID_BL_DOTS == 1 & ID_BL_APOPO > 0) & ID_STATUS == 13)$ID_SAMPLE)
    TotalNewCase2Plus_Filter <- n_distinct(subset(filteredData, (ID_BL_DOTS == 1 & ID_BL_APOPO > 0) & ID_STATUS == 12)$ID_SAMPLE)
    TotalNewCase1Plus_Filter <- n_distinct(subset(filteredData, (ID_BL_DOTS == 1 & ID_BL_APOPO > 0) & ID_STATUS == 11)$ID_SAMPLE)
    TotalNewCaseScanty_Filter <- n_distinct(subset(filteredData, (ID_BL_DOTS == 1 & ID_BL_APOPO > 0) & ID_STATUS > 1)$ID_SAMPLE) - TotalNewCase1Plus_Filter - TotalNewCase2Plus_Filter - TotalNewCase3Plus_Filter
    TotalNewCase_Filter <- n_distinct(subset(filteredData, ID_BL_DOTS == 1 & ID_BL_APOPO > 1)$ID_SAMPLE)
    
    
    # %Hit
    percentage3plusHit_NewCase <- TotalNewCase3PlushHit/TotalNewCase3Plus_Filter
    percentage2plusHit_NewCase <- TotalNewCase2PlushHit/TotalNewCase2Plus_Filter
    percentage1plusHit_NewCase <- TotalNewCase1PlushHit/TotalNewCase1Plus_Filter
    percentageScanty_NewCase <- TotalScantyHit_Newcase/ TotalNewCaseScanty_Filter
    
    # 3+,2+,1+,Scanty - New sample
    total123plusScantyHit_Newcase <- TotalNewCase3PlushHit + TotalNewCase2PlushHit
    + TotalNewCase1PlushHit + TotalScantyHit_Newcase
    # %Total
    percentage3plus_Newcase <- TotalNewCase3Plus_Filter/TotalNewCase_Filter
    percentage2plus_Newcase <- TotalNewCase2Plus_Filter/TotalNewCase_Filter
    percentage1plus_Newcase <- TotalNewCase1Plus_Filter/TotalNewCase_Filter
    percentageScanty_Newcase <- TotalNewCaseScanty_Filter/TotalNewCase_Filter
    
    overview_ARSD_Newcase <- data.frame(
      "Bact load" = c("3+", "2+", "1+", "Scanty", "Samples"),
      "Percentage HIT" = c(percentage3plusHit_NewCase, percentage2plusHit_NewCase,
                           percentage1plusHit_NewCase,percentageScanty_NewCase,
                           total123plusScantyHit_Newcase),
      "Percentage Total" = c(paste0(round(percentage3plus_Newcase * 100, 1), "%"), 
                             paste0(round(percentage2plus_Newcase * 100, 1), "%"),
                             paste0(round(percentage1plus_Newcase * 100, 1), "%"),
                             paste0(round(percentageScanty_Newcase * 100, 1), "%"),
                             paste0(100.0,"%"))
    ) 
    
    datatable(overview_ARSD_Newcase, options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
      "}"), rowCallback = JS(rowCallback))) %>% formatRound(c(2), 4)
  })
  
  output$AIRR <- DT::renderDataTable({
    overview_AIRR <- overview_AIRR()
    datatable(overview_AIRR, options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
      "}"), rowCallback = JS(rowCallback)))
  })
  
  # Render the tables in the UI
  # Reference for the table: https://stackoverflow.com/questions/43739218/r-datatable-formatting-with-javascript
  output$Sensitivity_trainer_table_daily <- DT::renderDataTable({

    Sensitivity_trainer_table_daily <- Sensitivity_trainer_table_daily()
    datatable(Sensitivity_trainer_table_daily, options = list(pageLength = 8, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
      "}"))) %>% formatRound(c(4), 4)
  })
  
  output$Specificity_trainer_table_daily <- DT::renderDataTable({

    Specificity_trainer_table_daily <- Specificity_trainer_table_daily()
    datatable(Specificity_trainer_table_daily, options = list(pageLength = 8, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
      "}"))) %>% formatRound(c(4), 4)
  })
  output$Sensitivity_trainer_table_weekly <- DT::renderDataTable({
    Sensitivity_trainer_table_weekly <- Sensitivity_trainer_table_weekly()
    datatable(Sensitivity_trainer_table_weekly, options = list(pageLength = 8, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
      "}"))) %>% formatRound(c(4), 4)
  })
  
  output$Specificity_trainer_table_weekly <- DT::renderDataTable({
    Specificity_trainer_table_weekly <- Specificity_trainer_table_weekly()
    datatable(Specificity_trainer_table_weekly, options = list(pageLength = 8, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
      "}"))) %>% formatRound(c(4), 4)
  })
  
  
  output$Sensitivity_trainer_table_monthly <- DT::renderDataTable({
    Sensitivity_trainer_table_monthly <- Sensitivity_trainer_table_monthly()
    datatable(Sensitivity_trainer_table_monthly, options = list(pageLength = 8, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
      "}"))) %>% formatRound(c(4), 4)
  })
  
  output$Specificity_trainer_table_monthly <- DT::renderDataTable({
    Specificity_trainer_table_monthly <- Specificity_trainer_table_monthly()
    datatable(Specificity_trainer_table_monthly, options = list(pageLength = 8, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
      "}"))) %>% formatRound(c(4), 4)
  })
  
  # Generate choices for selectInput dynamically
  output$valueSelect <- renderUI({
    TB_rat <- TB_rat()
    # Create the selectInput with the dynamically generated choices
    selectInput("SenSpe", "Select the value", choices = c("Sensitivity", "Specificity"))
  })
  
  output$trainerSelect <- renderUI({
    TB_rat <- TB_rat()
    # Create the selectInput with the dynamically generated choices
    selectInput("Trainer_Name", "Select a Trainer", choices = unique(TB_rat$TRAINER))
  })
  
  output$timeSelect <- renderUI({
    TB_rat <- TB_rat()
    # Create the selectInput with the dynamically generated choices
    selectInput("Time_Select", "Display Time", choices = c("Daily", "Weekly", "Monthly"))
  })
  
  
  
  ## Table
  
  output$trainertable <- DT::renderDataTable({
    selectValue <- input$SenSpe
    selectTime <- input$Time_Select
    selectTrainer <- input$Trainer_Name
    if (selectValue == "Sensitivity"){
      if (selectTime == "Daily"){
        Sensitivity_trainer_table_daily <- Sensitivity_trainer_table_daily()
        Display_Table <- subset(Sensitivity_trainer_table_daily, Trainer_Name == selectTrainer)
        datatable(Display_Table, ,options = list(pageLength = 8, lengthMenu = c(10, 15, 20), initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
          "}"))) %>% formatRound(c(4), 4)
      }else if (selectTime == "Weekly"){
        Sensitivity_trainer_table_weekly <- Sensitivity_trainer_table_weekly()
        Display_Table <- subset(Sensitivity_trainer_table_weekly, Trainer_Name == selectTrainer)
        datatable(Display_Table, ,options = list(pageLength = 8, lengthMenu = c(10, 15, 20), initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
          "}"))) %>% formatRound(c(4), 4)
      }else{
        Sensitivity_trainer_table_monthly <- Sensitivity_trainer_table_monthly()
        Display_Table <- subset(Sensitivity_trainer_table_monthly, Trainer_Name == selectTrainer)
        datatable(Display_Table, ,options = list(pageLength = 8, lengthMenu = c(10, 15, 20), initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
          "}"))) %>% formatRound(c(4), 4)
      }
    }else{
      if (selectTime == "Daily"){
        Specificity_trainer_table_daily <- Specificity_trainer_table_daily()
        Display_Table <- subset(Specificity_trainer_table_daily, Trainer_Name == selectTrainer)
        datatable(Display_Table, ,options = list(pageLength = 8, lengthMenu = c(10, 15, 20), initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
          "}"))) %>% formatRound(c(4), 4)
      }else if (selectTime == "Weekly"){
        Specificity_trainer_table_weekly <- Specificity_trainer_table_weekly()
        Display_Table <- subset(Specificity_trainer_table_weekly, Trainer_Name == selectTrainer)
        datatable(Display_Table, ,options = list(pageLength = 8, lengthMenu = c(10, 15, 20), initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
          "}"))) %>% formatRound(c(4), 4)
      }else{
        Specificity_trainer_table_monthly <- Specificity_trainer_table_monthly()
        Display_Table <- subset(Specificity_trainer_table_monthly, Trainer_Name == selectTrainer)
        datatable(Display_Table, ,options = list(pageLength = 8, lengthMenu = c(10, 15, 20), initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
          "}"))) %>% formatRound(c(4), 4)
      }
    }
  })
  
  ## Line Chart 
  
  
  output$LineChart <- renderHighchart({
    
    selectValue <- input$SenSpe
    selectTime <- input$Time_Select
    selectTrainer <- input$Trainer_Name
    
    if (selectValue == "Sensitivity") {
      if (selectTime == "Daily") {
        Sensitivity_trainer_table_daily <- Sensitivity_trainer_table_daily()
        Display_Table <- subset(Sensitivity_trainer_table_daily, Trainer_Name == selectTrainer) %>% mutate_at(vars(Sensitivity), ~ round(., 4))
        #plot(Display_Table$Date, Display_Table$Sensitivity, type = "o", pch = 16, xlab = "Time Period", ylab = "Sensitivity", main = "Line Chart")
        plot_name <- paste("Daily data for", selectTrainer)
        #ggplot(Display_Table, aes(Date, Sensitivity)) + geom_point(fill = "blue", shape = 23, size = 3) + geom_line(colour = "brown") + ggtitle(plot_name) +
        #theme(plot.background = element_rect(fill = "azure3"))
        hchart(Display_Table, type = 'line', hcaes(x = Date, y = Sensitivity), name = plot_name,color = '#38b') 
      } else if (selectTime == "Weekly") {
        Sensitivity_trainer_table_weekly <- Sensitivity_trainer_table_weekly()
        Display_Table <- subset(Sensitivity_trainer_table_weekly, Trainer_Name == selectTrainer) %>% mutate_at(vars(Sensitivity), ~ round(., 4))
        #plot(Display_Table$Week, Display_Table$Sensitivity, type = "o", pch = 16, xlab = "Time Period", ylab = "Sensitivity", main = "Line Chart")
        plot_name <- paste("Weekly data for", selectTrainer)
        Display_Table$Week <- as.numeric(Display_Table$Week)
        #ggplot(Display_Table, aes(Week, Sensitivity)) + geom_point(fill = "blue", shape = 23, size = 3) + geom_line(colour = "brown") + ggtitle(plot_name) +
        #theme(plot.background = element_rect(fill = "azure3"))
        hchart(Display_Table, type = 'line', hcaes(x = Week, y = Sensitivity), name = plot_name,color = '#38b') 
      } else {
        Sensitivity_trainer_table_monthly <- Sensitivity_trainer_table_monthly()
        Display_Table <- subset(Sensitivity_trainer_table_monthly, Trainer_Name == selectTrainer) %>% mutate_at(vars(Sensitivity), ~ round(., 4))
        #plot(Display_Table$Month, Display_Table$Sensitivity, type = "o", pch = 16, xlab = "Time Period", ylab = "Sensitivity", main = "Line Chart")
        plot_name <- paste("Monthly data for", selectTrainer)
        Display_Table$Month <- as.numeric(Display_Table$Month)
        #ggplot(Display_Table, aes(as.numeric(Month), Sensitivity)) + geom_point(fill = "blue", shape = 23, size = 3) + geom_line(colour = "brown") + ggtitle(plot_name) +
        #theme(plot.background = element_rect(fill = "azure3"))
        hchart(Display_Table, type = 'line', hcaes(x = Month, y = Sensitivity), name = plot_name,color = '#38b') 
      }
    } else {
      if (selectTime == "Daily") {
        Specificity_trainer_table_daily <- Specificity_trainer_table_daily()
        Display_Table <- subset(Specificity_trainer_table_daily, Trainer_Name == selectTrainer) %>% mutate_at(vars(Specificity), ~ round(., 4))
        #plot(Display_Table$Date, Display_Table$Specificity, type = "o", pch = 16, xlab = "Time Period", ylab = "Specificity", main = "Line Chart")
        plot_name <- paste("Daily data for", selectTrainer)
        # ggplot(Display_Table, aes(Date, Specificity)) + geom_point(fill = "blue", shape = 23, size = 3) + geom_line(colour = "brown") + ggtitle(plot_name) +
        #theme(plot.background = element_rect(fill = "azure3"))
        hchart(Display_Table, type = 'line', hcaes(x = Date, y = Specificity), name = plot_name,color = '#38b') 
      } else if (selectTime == "weekly") {
        Specificity_trainer_table_weekly <- Specificity_trainer_table_weekly()
        Display_Table <- subset(Specificity_trainer_table_weekly, Trainer_Name == selectTrainer) %>% mutate_at(vars(Specificity), ~ round(., 4))
        #plot(Display_Table$Week, Display_Table$Specificity, type = "o", pch = 16, xlab = "Time Period", ylab = "Specificity", main = "Line Chart")
        plot_name <- paste("Weekly data for", selectTrainer)
        Display_Table$Week <- as.numeric(Display_Table$Week)
        # ggplot(Display_Table, aes(as.numeric(Week), Specificity)) + geom_point(fill = "blue", shape = 23, size = 3) + geom_line(colour = "brown") + ggtitle(plot_name) +
        # theme(plot.background = element_rect(fill = "azure3"))
        hchart(Display_Table, type = 'line', hcaes(x = Week, y = Specificity), name = plot_name,color = '#38b')
      } else {
        Specificity_trainer_table_monthly <- Specificity_trainer_table_monthly()
        Display_Table <- subset(Specificity_trainer_table_monthly, Trainer_Name == selectTrainer) %>% mutate_at(vars(Specificity), ~ round(., 4))
        #plot(Display_Table$Month, Display_Table$Specificity, type = "o", pch = 16, xlab = "Time Period", ylab = "Specificity", main = "Line Chart")
        plot_name <- paste("Monthly data for", selectTrainer)
        Display_Table$Month <- as.numeric(Display_Table$Month)
        #ggplot(Display_Table, aes(as.numeric(Month), Specificity)) + geom_point(fill = "blue", shape = 23, size = 3) + geom_line(colour = "brown") + ggtitle(plot_name) +
        #theme(plot.background = element_rect(fill = "azure3"))
        hchart(Display_Table, type = 'line', hcaes(x = Month, y = Specificity), name = plot_name,color = '#38b')
      }
    }
  })
  
  output$ratSelect <- renderUI({
    TB_rat <- TB_rat()
    unique_rat_names <- sort(unique(TB_rat$RAT_NAME))
    # Create the selectInput with the dynamically generated choices
    pickerInput(
      inputId = "Indi_rat",
      label = NULL,
      choices = unique_rat_names,
      selected = NULL,
      multiple = FALSE,
    )
  })
  
  ## BacterialLeverlTable
  BacterialLevelTable <- reactive({
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    TB_rat <- TB_rat()
    unique_rat_names <- sort(unique(TB_rat$RAT_NAME))
    BacterialLevelTable<- data.frame()
    HitTable<- data.frame()
    PercentageTable<- data.frame()
    # we have level from 0 to 24
    bacterial_level <- seq(0, 24)
    # Loop over all rats' name
    for (name in unique_rat_names) {
      for (SampleReuse in c("ALL", "FRESH", "RE-USED")){
        # Create a row
        new_row_total <- data.frame(
          Rat_Name = name,
          SampleReuse = SampleReuse
        )
        new_row_hit <- data.frame(
          Rat_Name = name,
          SampleReuse = SampleReuse
        )
        new_row_percentage <- data.frame(
          Rat_Name = name,
          SampleReuse = SampleReuse
        )
        
        if (SampleReuse == "ALL"){
          Sample_Reuse_Subset <- TB_rat
        } else if (SampleReuse == "FRESH"){
          Sample_Reuse_Subset <- subset(TB_rat, REUSED == 1)
        } else {
          Sample_Reuse_Subset <- subset(TB_rat, REUSED != 1)
        }
        
        
        # Splitting the wanted subset with specific name
        # Loop over all levels
        for (level in bacterial_level){
          # Find the amount of total sample
          Rat_level_total <- subset(Sample_Reuse_Subset, RAT_NAME == name & ID_STATUS == level)
          # Find the amount of hit sample
          Rat_level_hit <- subset(Sample_Reuse_Subset, RAT_NAME == name & ID_STATUS == level & HIT == "TRUE")
          # Amount of this level
          Amount_total <- nrow(Rat_level_total)
          Amount_hit <- nrow(Rat_level_hit)
          Amount_percentage <- Amount_hit * 100/ (Amount_total + 0.00000001)
          # Column bind
          new_row_total <- cbind(new_row_total, Amount_total)
          new_row_hit <- cbind(new_row_hit, Amount_hit)
          new_row_percentage <- cbind(new_row_percentage, Amount_percentage)
        }
        
        # Add sensitivity column
        pos_sample <- subset(Sample_Reuse_Subset, RAT_NAME == name & ID_STATUS != 1)
        pos_amount <- nrow(pos_sample)
        hits_amount <- nrow(subset(pos_sample, HIT == "TRUE"))
        sensitivity <- hits_amount/pos_amount
        new_row_total <- cbind(new_row_total, sensitivity)
        new_row_hit <- cbind(new_row_hit, sensitivity)
        new_row_percentage <- cbind(new_row_percentage, sensitivity)
        
        # Add total new case
        newcase_sample <- subset(Sample_Reuse_Subset, RAT_NAME == name & ID_BL_DOTS == 1 & ID_BL_APOPO > 1) # Check the definition of new case!!!
        newcases_amount <- nrow(newcase_sample)
        new_row_total <- cbind(new_row_total, newcases_amount)
        new_row_hit <- cbind(new_row_hit, newcases_amount)
        new_row_percentage <- cbind(new_row_percentage, newcases_amount)
        
        # Add detected new case
        newcase_detected <- subset(newcase_sample, HIT == "TRUE")
        newcasesdetected_amount <- nrow(newcase_detected)
        new_row_total <- cbind(new_row_total, newcasesdetected_amount)
        new_row_hit <- cbind(new_row_hit, newcasesdetected_amount)
        new_row_percentage <- cbind(new_row_percentage, newcasesdetected_amount)
        
        # Adding to existing table
        BacterialLevelTable <- rbind(BacterialLevelTable, new_row_total)
        HitTable <- rbind(HitTable, new_row_hit)
        PercentageTable <- rbind(PercentageTable, new_row_percentage)
      }
    }
    colnames(BacterialLevelTable) <- c("RAT_NAME", "SampleReuse", "UBL(0)", "Negative(1)",
                                       "1 AFB(2)", "2 AFB(3)", "3 AFB(4)", "4 AFB(5)",
                                       "5 AFB(6)", "6 AFB(7)", "7 AFB(8)", "8 AFB(9)",
                                       "9 AFB(10)", "1+(11)", "2+(12)", "3+(13)",
                                       " (14)", "10 AFB(15)", "11 AFB(16)", "12 AFB(17)",
                                       "13 AFB(18)", "14 AFB(19)", "15 AFB(20)",
                                       "16 AFB(21)", "17 AFB(22)", "18 AFB(23)",
                                       "19 AFB(24)", "Sensitivity","Total New case", "New Case Found")
    return(BacterialLevelTable)
  })
  
  
  
  
  ## HITLEVELTABLE
  HitTable <- reactive({
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    TB_rat <- TB_rat()
    unique_rat_names <- sort(unique(TB_rat$RAT_NAME))
    BacterialLevelTable<- data.frame()
    HitTable<- data.frame()
    PercentageTable<- data.frame()
    # we have level from 0 to 24
    bacterial_level <- seq(0, 24)
    # Loop over all rats' name
    for (name in unique_rat_names) {
      for (SampleReuse in c("ALL", "FRESH", "RE-USED")){
        # Create a row
        new_row_total <- data.frame(
          Rat_Name = name,
          SampleReuse = SampleReuse
        )
        new_row_hit <- data.frame(
          Rat_Name = name,
          SampleReuse = SampleReuse
        )
        new_row_percentage <- data.frame(
          Rat_Name = name,
          SampleReuse = SampleReuse
        )
        
        if (SampleReuse == "ALL"){
          Sample_Reuse_Subset <- TB_rat
        } else if (SampleReuse == "FRESH"){
          Sample_Reuse_Subset <- subset(TB_rat, REUSED == 1)
        } else {
          Sample_Reuse_Subset <- subset(TB_rat, REUSED != 1)
        }
        
        
        # Splitting the wanted subset with specific name
        # Loop over all levels
        for (level in bacterial_level){
          # Find the amount of total sample
          Rat_level_total <- subset(Sample_Reuse_Subset, RAT_NAME == name & ID_STATUS == level)
          # Find the amount of hit sample
          Rat_level_hit <- subset(Sample_Reuse_Subset, RAT_NAME == name & ID_STATUS == level & HIT == "TRUE")
          # Amount of this level
          Amount_total <- nrow(Rat_level_total)
          Amount_hit <- nrow(Rat_level_hit)
          Amount_percentage <- Amount_hit * 100/ (Amount_total + 0.00000001)
          # Column bind
          new_row_total <- cbind(new_row_total, Amount_total)
          new_row_hit <- cbind(new_row_hit, Amount_hit)
          new_row_percentage <- cbind(new_row_percentage, Amount_percentage)
        }
        
        # Add sensitivity column
        pos_sample <- subset(Sample_Reuse_Subset, RAT_NAME == name & ID_STATUS != 1)
        pos_amount <- nrow(pos_sample)
        hits_amount <- nrow(subset(pos_sample, HIT == "TRUE"))
        sensitivity <- hits_amount/pos_amount
        new_row_total <- cbind(new_row_total, sensitivity)
        new_row_hit <- cbind(new_row_hit, sensitivity)
        new_row_percentage <- cbind(new_row_percentage, sensitivity)
        
        # Add total new case
        newcase_sample <- subset(Sample_Reuse_Subset, RAT_NAME == name & ID_BL_DOTS == 1 & ID_BL_APOPO > 1) # Check the definition of new case!!!
        newcases_amount <- nrow(newcase_sample)
        new_row_total <- cbind(new_row_total, newcases_amount)
        new_row_hit <- cbind(new_row_hit, newcases_amount)
        new_row_percentage <- cbind(new_row_percentage, newcases_amount)
        
        # Add detected new case
        newcase_detected <- subset(newcase_sample, HIT == "TRUE")
        newcasesdetected_amount <- nrow(newcase_detected)
        new_row_total <- cbind(new_row_total, newcasesdetected_amount)
        new_row_hit <- cbind(new_row_hit, newcasesdetected_amount)
        new_row_percentage <- cbind(new_row_percentage, newcasesdetected_amount)
        
        # Adding to existing table
        HitTable <- rbind(HitTable, new_row_hit)
      }
    }
    colnames(HitTable) <- c("RAT_NAME", "SampleReuse", "UBL(0)", "Negative(1)",
                            "1 AFB(2)", "2 AFB(3)", "3 AFB(4)", "4 AFB(5)",
                            "5 AFB(6)", "6 AFB(7)", "7 AFB(8)", "8 AFB(9)",
                            "9 AFB(10)", "1+(11)", "2+(12)", "3+(13)",
                            " (14)", "10 AFB(15)", "11 AFB(16)", "12 AFB(17)",
                            "13 AFB(18)", "14 AFB(19)", "15 AFB(20)",
                            "16 AFB(21)", "17 AFB(22)", "18 AFB(23)",
                            "19 AFB(24)", "Sensitivity","Total New case", "New Case Found")
    return(HitTable)
  })
  
  ## PercentageTable
  PercentageTable <- reactive({
    req(input$fileUpload)
    file <- input$fileUpload$datapath
    TB_rat <- TB_rat()
    unique_rat_names <- sort(unique(TB_rat$RAT_NAME))
    BacterialLevelTable<- data.frame()
    HitTable<- data.frame()
    PercentageTable<- data.frame()
    # we have level from 0 to 24
    bacterial_level <- seq(0, 24)
    # Loop over all rats' name
    for (name in unique_rat_names) {
      for (SampleReuse in c("ALL", "FRESH", "RE-USED")){
        # Create a row
        new_row_total <- data.frame(
          Rat_Name = name,
          SampleReuse = SampleReuse
        )
        new_row_hit <- data.frame(
          Rat_Name = name,
          SampleReuse = SampleReuse
        )
        new_row_percentage <- data.frame(
          Rat_Name = name,
          SampleReuse = SampleReuse
        )
        
        if (SampleReuse == "ALL"){
          Sample_Reuse_Subset <- TB_rat
        } else if (SampleReuse == "FRESH"){
          Sample_Reuse_Subset <- subset(TB_rat, TB_rat$REUSED == 1)
        } else {
          Sample_Reuse_Subset <- subset(TB_rat, TB_rat$REUSED != 1)
        }
        
        
        # Splitting the wanted subset with specific name
        # Loop over all levels
        for (level in bacterial_level){
          # Find the amount of total sample
          Rat_level_total <- subset(Sample_Reuse_Subset, RAT_NAME == name & ID_STATUS == level)
          # Find the amount of hit sample
          Rat_level_hit <- subset(Sample_Reuse_Subset, RAT_NAME == name & ID_STATUS == level & HIT == "TRUE")
          # Amount of this level
          Amount_total <- nrow(Rat_level_total)
          Amount_hit <- nrow(Rat_level_hit)
          Amount_percentage <- Amount_hit * 100/ (Amount_total + 0.00000001)
          # Column bind
          new_row_total <- cbind(new_row_total, Amount_total)
          new_row_hit <- cbind(new_row_hit, Amount_hit)
          new_row_percentage <- cbind(new_row_percentage, Amount_percentage)
        }
        
        # Add sensitivity column
        pos_sample <- subset(Sample_Reuse_Subset, RAT_NAME == name & ID_STATUS != 1)
        pos_amount <- nrow(pos_sample)
        hits_amount <- nrow(subset(pos_sample, HIT == "TRUE"))
        sensitivity <- hits_amount/pos_amount
        new_row_total <- cbind(new_row_total, sensitivity)
        new_row_hit <- cbind(new_row_hit, sensitivity)
        new_row_percentage <- cbind(new_row_percentage, sensitivity)
        
        # Add total new case
        newcase_sample <- subset(Sample_Reuse_Subset, RAT_NAME == name & ID_BL_DOTS == 1 & ID_BL_APOPO > 1) # Check the definition of new case!!!
        newcases_amount <- nrow(newcase_sample)
        new_row_total <- cbind(new_row_total, newcases_amount)
        new_row_hit <- cbind(new_row_hit, newcases_amount)
        new_row_percentage <- cbind(new_row_percentage, newcases_amount)
        
        # Add detected new case
        newcase_detected <- subset(newcase_sample, HIT == "TRUE")
        newcasesdetected_amount <- nrow(newcase_detected)
        new_row_total <- cbind(new_row_total, newcasesdetected_amount)
        new_row_hit <- cbind(new_row_hit, newcasesdetected_amount)
        new_row_percentage <- cbind(new_row_percentage, newcasesdetected_amount)
        
        # Adding to existing table
        PercentageTable <- rbind(PercentageTable, new_row_percentage)
      }
    }
    colnames(PercentageTable) <- c("RAT_NAME", "SampleReuse", "UBL(0)", "Negative(1)",
                                   "1 AFB(2)", "2 AFB(3)", "3 AFB(4)", "4 AFB(5)",
                                   "5 AFB(6)", "6 AFB(7)", "7 AFB(8)", "8 AFB(9)",
                                   "9 AFB(10)", "1+(11)", "2+(12)", "3+(13)",
                                   " (14)", "10 AFB(15)", "11 AFB(16)", "12 AFB(17)",
                                   "13 AFB(18)", "14 AFB(19)", "15 AFB(20)",
                                   "16 AFB(21)", "17 AFB(22)", "18 AFB(23)",
                                   "19 AFB(24)", "Sensitivity","Total New case", "New Case Found")
    return(PercentageTable)
  })
  
  ## Render for BacterialLevelTable
  output$BacterialLevelTable <- renderDataTable({
    BacterialLevelTable <- BacterialLevelTable()
    colnames(BacterialLevelTable) <- c("RAT_NAME", "SampleReuse", "UBL(0)", "Negative(1)",
                                       "1 AFB(2)", "2 AFB(3)", "3 AFB(4)", "4 AFB(5)",
                                       "5 AFB(6)", "6 AFB(7)", "7 AFB(8)", "8 AFB(9)",
                                       "9 AFB(10)", "1+(11)", "2+(12)", "3+(13)",
                                       " (14)", "10 AFB(15)", "11 AFB(16)", "12 AFB(17)",
                                       "13 AFB(18)", "14 AFB(19)", "15 AFB(20)",
                                       "16 AFB(21)", "17 AFB(22)", "18 AFB(23)",
                                       "19 AFB(24)", "Sensitivity","Total New case", "New Case Found")
    datatable(BacterialLevelTable, ,options = list(pageLength = 20, lengthMenu = c(10, 15, 20), scrollX = TRUE, rowCallback = JS(rowCallback),initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
      "}"))) %>% formatRound(c(28), 4)
  })
  
  
  ## Render for HitTable
  output$HitTable <- renderDataTable({
    HitTable <- HitTable()
    colnames(HitTable) <- c("RAT_NAME", "SampleReuse", "UBL(0)", "Negative(1)",
                            "1 AFB(2)", "2 AFB(3)", "3 AFB(4)", "4 AFB(5)",
                            "5 AFB(6)", "6 AFB(7)", "7 AFB(8)", "8 AFB(9)",
                            "9 AFB(10)", "1+(11)", "2+(12)", "3+(13)",
                            " (14)", "10 AFB(15)", "11 AFB(16)", "12 AFB(17)",
                            "13 AFB(18)", "14 AFB(19)", "15 AFB(20)",
                            "16 AFB(21)", "17 AFB(22)", "18 AFB(23)",
                            "19 AFB(24)", "Sensitivity","Total New case", "New Case Found")
    datatable(HitTable, ,options = list(pageLength = 20, lengthMenu = c(10, 15, 20), scrollX = TRUE, rowCallback = JS(rowCallback), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
      "}"))) %>% formatRound(c(28), 4)
  })
  
  ## Render for PercentageTable
  
  output$PercentageTable <- renderDataTable({
    PercentageTable <- PercentageTable()
    colnames(PercentageTable) <- c("RAT_NAME", "SampleReuse", "UBL(0)", "Negative(1)",
                                   "1 AFB(2)", "2 AFB(3)", "3 AFB(4)", "4 AFB(5)",
                                   "5 AFB(6)", "6 AFB(7)", "7 AFB(8)", "8 AFB(9)",
                                   "9 AFB(10)", "1+(11)", "2+(12)", "3+(13)",
                                   " (14)", "10 AFB(15)", "11 AFB(16)", "12 AFB(17)",
                                   "13 AFB(18)", "14 AFB(19)", "15 AFB(20)",
                                   "16 AFB(21)", "17 AFB(22)", "18 AFB(23)",
                                   "19 AFB(24)", "Sensitivity","Total New case", "New Case Found")
    PercentageTable <- PercentageTable %>%  
      mutate_if(is.numeric,
                round,
                digits = 3)
    datatable(PercentageTable,options = list(pageLength = 20, lengthMenu = c(10, 15, 20),scrollX = TRUE,  initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#38b', 'color': '#fff', 'text-align': 'center',});",
      "}")))
  })
  
  # Render the rat hit Analyst in the UI
  output$ratHitAnalystInputs <- renderUI({
    HitTable <- HitTable()
    colnames(HitTable) <- c("RAT_NAME", "SampleReuse", "UBL(0)", "Negative(1)",
                            "1 AFB(2)", "2 AFB(3)", "3 AFB(4)", "4 AFB(5)",
                            "5 AFB(6)", "6 AFB(7)", "7 AFB(8)", "8 AFB(9)",
                            "9 AFB(10)", "1+(11)", "2+(12)", "3+(13)",
                            " (14)", "10 AFB(15)", "11 AFB(16)", "12 AFB(17)",
                            "13 AFB(18)", "14 AFB(19)", "15 AFB(20)",
                            "16 AFB(21)", "17 AFB(22)", "18 AFB(23)",
                            "19 AFB(24)", "Sensitivity","Total New case", "New Case Found")
    column(11, selectInput("rat", "Select Rat Name", choices = unique(HitTable$RAT_NAME)))
    column(11, selectInput("sample_reuse", "Select Sample Reuse:", choices = c("ALL", "FRESH", "RE-USED")))
  })
  
  # Rat Hit Analysis
  output$barplot <- renderHighchart({
    HitTable <- HitTable()
    rat_df <- subset(HitTable, RAT_NAME == input$rat & SampleReuse == input$sample_reuse)
    #rat_df <- HitTable[HitTable$RAT_NAME == input$rat & HitTable$SampleReuse == input$sample_reuse, ] # Subset data for the selected rat
    rat_hits <- as.numeric(unlist(rat_df[, 5:24]))  # Exclude RatName column and convert to numeric
    final_data <- data.frame(rat_hits,colnames(rat_df[, 5:24]))
    names(final_data) <- c("data", "name")
    #barplot(rat_hits, names.arg = colnames(rat_df[, 5:24]),
    #xlab = "Bacterial Level", ylab = "Number of Hits",
    #main = paste("Rat:", input$rat, " - Sample Reuse:", input$sample_reuse))
    hchart(object = final_data, type = "column", hcaes(x = name, y = data), color ='#38b')
  })
  
  ############## Connection of two tables ##################
  
  # Render the tables in the UI
  output$Selected_Bac_Level <- renderTable({
    BacterialLevelTable <- BacterialLevelTable()
    Selected_Bac_Level <- subset(BacterialLevelTable, RAT_NAME == input$rat & SampleReuse == input$sample_reuse)
    Selected_Bac_Level
  })
  
  # Render the tables in the UI
  output$Selected_Hits <- renderTable({
    HitTable <- HitTable()
    Selected_Hits <- subset(HitTable, RAT_NAME == input$rat & SampleReuse == input$sample_reuse)
    Selected_Hits
  })
  
  # Render the tables in the UI
  output$Selected_Percentage <- renderTable({
    PercentageTable <- PercentageTable()
    Selected_Percentage <- subset(PercentageTable, RAT_NAME == input$rat & SampleReuse == input$sample_reuse)
    Selected_Percentage
  })
  
  output$sidebarpanel <- renderUI({
    ## We only generate the UI after the user log in
    if (USER$login == TRUE & FILE$fileUpload == TRUE){ 
      sidebarMenu(
        menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
        menuItem("Rat Hit Analyst", tabName = "Rat_Hit_Analyst", icon = icon("chart-pie")),
        menuItem("Rat Performance", tabName = "Rat_Performance", icon = icon("chart-pie")),
        menuItem("Trainer Analysis", tabName = "Trainer_Analysis", icon = icon("chart-bar")),
        menuItem("Overall Table", tabName = "Overall_Table", icon = icon("file-excel")),
        menuItem("About The App", tabName = "About_App", icon = icon("address-card"))
      )
    }
  })
  

  
}


runApp(list(ui = ui, server = server))