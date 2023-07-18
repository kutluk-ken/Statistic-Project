#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

############### Libraries #################


library(shiny)
library(readxl)
library(ggplot2)
library(DT)
library(dplyr)
library(scales)
library(shinyWidgets)
library(plotly)

# NA function for datatable
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

# # Read the data file
# data <- read_excel("/Users/kutlukanwar/Desktop/STAD94/TBdetectionRats_Tanzania.xlsx", sheet = 3, col_names = TRUE)
# reused_data <- data$REUSED
# Define server logic required to draw a histogram
function(input, output, session){
  # Read the data file
  #data <- read_excel("/Users/kutlukanwar/Desktop/STAD94/TBdetectionRats_Tanzania.xlsx", sheet = 3, col_names = TRUE)
  
  TB_rat <- read_excel("TBdetectionRats_Tanzania.xlsx", sheet = 2, col_names = TRUE)
  
  TB_Adtl <- read_excel("TBdetectionRats_Tanzania.xlsx", sheet = 3, col_names = TRUE)
  
  # Pie Chart
  # reused_sample_counts <- reactive({
  #   fresh_count <- nrow(TB_Adtl[TB_Adtl$REUSED == 1, ])
  #   reused_count <- nrow(TB_Adtl[TB_Adtl$REUSED > 1, ])
  #   counts <- data.frame(REUSED = c("Fresh", "Reused"), COUNT = c(fresh_count, reused_count))
  #   counts
  # })
  # 
  # output$pie_chart <- renderPlot({
  #   counts <- reused_sample_counts()
  #   ggplot(counts, aes(x = "", y = COUNT, fill = REUSED)) +
  #     geom_bar(stat = "identity", width = 1, color = "white") +
  #     coord_polar("y") +
  #     labs(fill = "Sample Type") +
  #     theme_void() +
  #     theme(legend.position = "right")
  # })
  # 
  
  # Check if ID_BL_APOPO has a value and override ID_BL_DOTS
  TB_Adtl$ID_STATUS <- ifelse(!is.na(TB_Adtl$ID_BL_APOPO), TB_Adtl$ID_BL_APOPO, TB_Adtl$ID_BL_DOTS)
  TB_rat$ID_STATUS <- ifelse(!is.na(TB_rat$ID_BL_APOPO), TB_rat$ID_BL_APOPO, TB_rat$ID_BL_DOTS)
  
  # abstract negative in dar and positive in moro
  dar_negative_samples <- subset(TB_Adtl, PROGRAM == "DAR" & TB_Adtl$ID_STATUS == 1)
  dar_positive_samples <- subset(TB_Adtl, PROGRAM == "DAR" & TB_Adtl$ID_STATUS != 1)
  morogoro_negative_samples <- subset(TB_Adtl, PROGRAM == "MORO" & TB_Adtl$ID_STATUS == 1)
  morogoro_positive_samples <- subset(TB_Adtl, PROGRAM == "MORO" & TB_Adtl$ID_STATUS != 1)
  
  # Create a Name List without duplication
  unique_names <- unique(TB_rat$RAT_NAME)
  
  # Creating a table showing Sensitivity  - all (Including Dots and Apopo)
  Sensitivity_table <- data.frame()
  
  # Loop over all rats' name
  for (name in unique_names) {
    # Splitting the wanted subset with specific name
    # Total Positive samples
    Pos_sample <- subset(TB_rat, RAT_NAME == name & TB_rat$ID_STATUS != 1)
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
  
  # Creating a table showing Sensitivity - Dots
  unique_names <- unique(TB_rat$RAT_NAME)
  
  # Creating a table showing Sensitivity  - all (Including Dots and Apopo)
  Sensitivity_table_Dots <- data.frame()
  
  # Loop over all rats' name
  for (name in unique_names) {
    # Splitting the wanted subset with specific name
    # Total Positive samples
    Pos_sample <- subset(TB_rat, RAT_NAME == name & TB_rat$ID_BL_DOTS != 1)
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
  
  # Creating a table showing Sensitivity - All
  Specificity_table <- data.frame()
  
  # Loop over all rats' name
  for (name in unique_names) {
    # Splitting the wanted subset with specific name
    # Total Negative samples
    Neg_sample <- subset(TB_rat, RAT_NAME == name & TB_rat$ID_STATUS == 1)
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
  
  # Creating a table showing Sensitivity - Dots
  Specificity_table_Dots <- data.frame()
  
  # Loop over all rats' name
  for (name in unique_names) {
    # Splitting the wanted subset with specific name
    # Total Negative samples
    Neg_sample <- subset(TB_rat, RAT_NAME == name & TB_rat$ID_BL_DOTS == 1)
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
  
  # Render the tables in the UI
  # Render the tables in the UI
  output$basicInformation_Sensitivity <-  DT::renderDataTable({
    datatable(Sensitivity_table, options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#298', 'color': '#fff', 'text-align': 'center',});",
      "}")))%>% formatRound(c(4), 4)
  })
  
  output$basicInformation_Specificity <- 
    DT::renderDataTable({
      datatable(Specificity_table, options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#298', 'color': '#fff', 'text-align': 'center',});",
        "}")))%>% formatRound(c(4), 4)
    })
  
  Individual_rat_performance <- merge(Sensitivity_table, Specificity_table, by = "Rat_Name", all.x = TRUE)
  
  output$Individual_rat_performance <- renderPlot({
    #plot(Individual_rat_performance$Sensitivity, Individual_rat_performance$Specificity, pch = 19, col = c("red", "blue", "green", "orange", "purple"), ylab = "Specificity",xlab = "Sensitivity")
    #text(Individual_rat_performance$Sensitivity, Individual_rat_performance$Specificity,
    #labels = Individual_rat_performance$Rat_Name, pos = 4, cex = 0.8)
    ggplot(Individual_rat_performance, aes(Sensitivity, Specificity,color = Rat_Name)) + geom_point() + labs(fill = "Rat Name") + 
      theme(plot.background = element_rect(fill = "grey95"))
  })
  
  ## Trainer  
  
  # Create the week number
  TB_Adtl$DATE <- as.Date(TB_Adtl$SESSION_DATE, format = "%Y/%m/%d")
  TB_Adtl$week_numbers <- format(TB_Adtl$DATE, format = "%U")
  TB_Adtl$month_numbers <- format(TB_Adtl$DATE, format = "%m")
  TB_Adtl$day_of_week <- weekdays(TB_Adtl$DATE)
  
  TB_rat$DATE <- as.Date(TB_rat$SESSION_DATE, format = "%Y/%m/%d")
  TB_rat$week_numbers <- format(TB_rat$DATE, format = "%U")
  TB_rat$month_numbers <- format(TB_rat$DATE, format = "%m")
  TB_rat$day_of_week <- weekdays(TB_rat$DATE)
  
  # Trainer Sensitivity
  # Create a Name List without duplication
  unique_names <- unique(TB_rat$TRAINER)
  # Creating a table showing Sensitivity
  Sensitivity_trainer_table <- data.frame()
  
  # Loop over all trainers' name
  for (name in unique_names) {
    # Splitting the wanted subset with specific name
    # Total Positive samples
    Pos_sample <- subset(TB_rat, TRAINER == name & TB_rat$ID_STATUS != 1)
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
  
  # Trainer Specificity
  # Create a Name List without duplication
  # unique_names <- unique(TB_rat$RAT_NAME)
  
  # Creating a table showing Sensitivity
  Specificity_trainer_table <- data.frame()
  
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
  
  # Render the tables in the UI
  output$Trainer_Sensitivity <- DT::renderDataTable({
    datatable(Sensitivity_trainer_table, options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#298', 'color': '#fff', 'text-align': 'center',});",
      "}"))) %>% formatRound(c(4), 4)
  })
  
  output$Trainer_Specificity <- DT::renderDataTable({
    datatable(Specificity_trainer_table, options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#298', 'color': '#fff', 'text-align': 'center',});",
      "}"))) %>% formatRound(c(4), 4)
  })
  
  # Trainer Sensitivity Daily
  # Create a Name List without duplication
  # unique_names <- unique(TB_rat$TRAINER)
  
  # Create a Day List without duplication
  unique_days <- unique(TB_rat$DATE)
  
  # Creating a table showing Sensitivity
  Sensitivity_trainer_table_daily <- data.frame()
  
  # Loop over all dates
  for (day in unique_days) {
    Daily <- subset(TB_rat, DATE == day)
    unique_names <- unique(Daily$TRAINER)
    # Loop over all rats' name
    for (name in unique_names) {
      # Splitting the wanted subset with specific name
      # Total Positive samples
      Pos_sample <- subset(Daily, TRAINER == name & Daily$ID_STATUS != 1)
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
  
  
  # Trainer Specificity Daily
  # Create a Name List without duplication
  # unique_names <- unique(TB_rat$RAT_NAME)
  
  # Create a Day List without duplication
  unique_days <- unique(TB_rat$DATE)
  
  # Creating a table showing Sensitivity
  Specificity_trainer_table_daily <- data.frame()
  
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
  
  # Trainer Sensitivity Weekly
  # Create a Name List without duplication
  # unique_names <- unique(TB_rat$TRAINER)
  
  
  
  #fix
  
  
  # Create a Day List without duplication
  unique_weeks <- unique(TB_rat$week_numbers)
  
  # Creating a table showing Sensitivity
  Sensitivity_trainer_table_weekly <- data.frame()
  
  # Loop over all dates
  for (week in unique_weeks) {
    Weekly <- subset(TB_rat, week_numbers == week)
    unique_names <- unique(Weekly$TRAINER)
    # Loop over all rats' name
    for (name in unique_names) {
      # Splitting the wanted subset with specific name
      # Total Positive samples
      Pos_sample <- subset(Weekly, TRAINER == name & Weekly$ID_STATUS != 1)
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
  
  # Trainer Specificity Weekly
  # Create a Name List without duplication
  # unique_names <- unique(TB_rat$RAT_NAME)
  
  # Create a Day List without duplication
  unique_weeks <- unique(TB_rat$week_numbers)
  
  # Creating a table showing Sensitivity
  Specificity_trainer_table_weekly <- data.frame()
  
  # Loop over all dates
  for (week in unique_weeks) {
    Weekly <- subset(TB_rat, week_numbers == week)
    unique_names <- unique(Weekly$TRAINER)
    # Loop over all rats' name
    for (name in unique_names) {
      # Splitting the wanted subset with specific name
      # Total Negative samples
      Neg_sample <- subset(Weekly, TRAINER == name & Weekly$ID_STATUS == 1)
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
  
  
  # Trainer Sensitivity Monthly
  # Create a Name List without duplication
  # unique_names <- unique(TB_rat$TRAINER)
  
  # Create a Day List without duplication
  unique_months <- unique(TB_rat$month_numbers)
  
  # Creating a table showing Sensitivity
  Sensitivity_trainer_table_monthly <- data.frame()
  
  # Loop over all dates
  for (month in unique_months) {
    Monthly <- subset(TB_rat, month_numbers == month)
    unique_names <- unique(Monthly$TRAINER)
    # Loop over all rats' name
    for (name in unique_names) {
      # Splitting the wanted subset with specific name
      # Total Positive samples
      Pos_sample <- subset(Monthly, TRAINER == name & Monthly$ID_STATUS != 1)
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
  
  # Trainer Specificity Monthly
  
  # Create a Name List without duplication
  # unique_names <- unique(TB_rat$RAT_NAME)
  
  # Create a Day List without duplication
  unique_months <- unique(TB_rat$month_numbers)
  
  # Creating a table showing Sensitivity
  Specificity_trainer_table_monthly <- data.frame()
  
  # Loop over all dates
  for (month in unique_months) {
    Monthly <- subset(TB_rat, month_numbers == month)
    unique_names <- unique(Monthly$TRAINER)
    # Loop over all rats' name
    for (name in unique_names) {
      # Splitting the wanted subset with specific name
      # Total Negative samples
      Neg_sample <- subset(Monthly, TRAINER == name & Monthly$ID_STATUS == 1)
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
  
  # Table for sensitivity for blinds sample
  Sensitivity_blind_table<- data.frame()
  unique_names <- unique(TB_rat$RAT_NAME)
  for (name in unique_names) {
    # Splitting the wanted subset with specific name
    # Total Positive samples
    blind_sample <- subset(TB_rat, RAT_NAME == name & TB_rat$STATUS_BLINDPOS == "TRUE")
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
  
  # Table for new cases
  NewCase_table<- data.frame()
  unique_names <- unique(TB_rat$RAT_NAME)
  for (name in unique_names) {
    
    new_cases_sample <- subset(TB_rat, RAT_NAME == name & TB_rat$ID_BL_DOTS == 1 & TB_rat$ID_BL_APOPO > 1)
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
  
  
  # Total Program Results
  
  # Calculate the required values based on the filtered data
  TotalDOTSPos <- n_distinct(subset(TB_Adtl, ID_BL_DOTS != 1)$ID_SAMPLE)
  TotalDotsPosPatient <- n_distinct(subset(TB_Adtl, ID_BL_DOTS != 1)$ID_PATIENT)
  TotalBlindSample <- n_distinct(subset(TB_Adtl, STATUS_BLINDPOS == "TRUE")$ID_SAMPLE)
  TotalBlindPatient <- n_distinct(subset(TB_Adtl, STATUS_BLINDPOS == "TRUE")$ID_PATIENT)
  TotalDotsNegative <- n_distinct(subset(TB_Adtl, ID_BL_DOTS == 1)$ID_SAMPLE)
  TotalDotsNegPatient <- n_distinct(subset(TB_Adtl, ID_BL_DOTS == 1)$ID_PATIENT)
  TotalNegativeIndicated <- n_distinct(subset(TB_Adtl, ID_BL_DOTS == 1 & RatHit > 0)$ID_SAMPLE)
  UnconfirmedNegHit <- n_distinct(subset(TB_Adtl, RatHit > 0 & ID_BL_DOTS == 1 & ID_BL_APOPO == 1)$ID_SAMPLE)
  TotalNewCase <- n_distinct(subset(TB_Adtl, ID_BL_DOTS == 1 & ID_BL_APOPO > 1)$ID_SAMPLE)
  TotalNewCasePatient <- n_distinct(subset(TB_Adtl, ID_BL_DOTS == 1 & ID_BL_APOPO > 1)$ID_PATIENT)
  TotalSampleCase <- TotalDOTSPos + TotalDotsNegative
  TotalPatientCase <- TotalDotsPosPatient + TotalDotsNegPatient
  
  
  # Filter data base on user's choice - Overview table
  filteredData <- reactive({
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
  
  # User input
  output$dateInput <- renderUI({
    if (input$timePeriod == "Day") {
      selectInput("date", "Select Date:", choices = unique(TB_Adtl$DATE), selected = Sys.Date())
    } else {
      NULL
    }
  })
  
  output$weekInput <- renderUI({
    if (input$timePeriod == "Week") {
      selectInput("week", "Select Week:", choices = unique(TB_Adtl$week_numbers))
    } else {
      NULL
    }
  })
  
  output$monthInput <- renderUI({
    if (input$timePeriod == "Month") {
      selectInput("month", "Select Month:", choices = unique(TB_Adtl$month_numbers))
    } else {
      NULL
    }
  })
  
  # Pie Chart
  output$pieChart <- renderPlot({
    filteredData <- filteredData()
    
    TotalDOTSPos_Filter <- n_distinct(subset(filteredData, ID_BL_DOTS != 1)$ID_SAMPLE)
    TotalDotsNegative_Filter <- n_distinct(subset(filteredData, ID_BL_DOTS == 1)$ID_SAMPLE)
    Total <- TotalDOTSPos_Filter + TotalDotsNegative_Filter
    TotalBlindSample_Filter <- n_distinct(subset(filteredData, STATUS_BLINDPOS == "TRUE")$ID_SAMPLE)
    TotalNewCase_Filter <- n_distinct(subset(filteredData, ID_BL_DOTS == 1 & ID_BL_APOPO > 1)$ID_SAMPLE)
    
    
    
    proportions <- c(
      "DOTS Positive Sample" = TotalDOTSPos_Filter/Total,
      "Blinds Sample" = TotalBlindSample_Filter/Total,
      "New cases" = TotalNewCase_Filter/Total,
      "DOTS Negative Sample" = TotalDotsNegative_Filter/Total
    )
    
    proportions <- data.frame(name = names(proportions), value = proportions * Total)
    #pie(proportions, labels = names(proportions), main = "Category Distribution")
    ggplot(proportions, aes(x = "", y = value, fill = name))  + geom_col(color = "black") +
      geom_text(aes(label = value),
                position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") +
      scale_fill_brewer() + ggtitle("Pie Chart For Total Program results") +
      theme_void()
  })
  
  # Overview Table
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
    
    datatable(overview_TPR)
  })
 
  
 # Program-Level Sample Details
  filteredData_PLSD <- reactive({
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
    if (input$timePeriod_PLSD == "Day") {
      selectInput("date_PLSD", "Select Date:", choices = unique(TB_Adtl$DATE), selected = Sys.Date())
    } else {
      NULL
    }
  })
  
  output$weekInput_PLSD <- renderUI({
    if (input$timePeriod_PLSD == "Week") {
      selectInput("week_PLSD", "Select Week:", choices = unique(TB_Adtl$week_numbers))
    } else {
      NULL
    }
  })
  
  output$monthInput_PLSD <- renderUI({
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
      "$(this.api().table().header()).css({'background-color': '#298', 'color': '#fff', 'text-align': 'center',});",
      "}"), rowCallback = JS(rowCallback)))
  })
  
  
  output$PLSD_newcase <- renderDataTable({
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
      "$(this.api().table().header()).css({'background-color': '#298', 'color': '#fff', 'text-align': 'center',});",
      "}"), rowCallback = JS(rowCallback)))
  })
  
 
  # Create the bar chart Dots
  output$barChart_Dots <- renderPlot({
    filteredData <- filteredData_PLSD()
    
    # Calculate the required values based on the filtered data
    TotalDOTSPos3Plus_F <- n_distinct(subset(filteredData, ID_BL_DOTS == 13)$ID_SAMPLE)
    TotalDOTSPos2Plus_F <- n_distinct(subset(filteredData, ID_BL_DOTS == 12)$ID_SAMPLE)
    TotalDOTSPos1Plus_F <- n_distinct(subset(filteredData, ID_BL_DOTS == 11)$ID_SAMPLE)
    TotalDOTSPosScanty_F <- TotalDOTSPos - TotalDOTSPos3Plus - TotalDOTSPos2Plus - TotalDOTSPos1Plus
    
    # Prepare the data
    dots_cases <- data.frame(
      Bact.load = c("3+", "2+", "1+", "Scanty"),
      Total = c(TotalDOTSPos3Plus_F, TotalDOTSPos2Plus_F,
                TotalDOTSPos1Plus_F, TotalDOTSPosScanty_F)
    )
    ggplot(dots_cases, aes(x = Bact.load, y = Total, color = Bact.load)) +
      geom_bar(stat = "identity", fill = "white") +
      labs(x = "Bacterial Load", y = "Total Count", title = "DOTS Cases") +
      theme(plot.title = element_text(face = "bold", size = 20)) +
      labs(title = "DOTS Cases")
  })
  

  # Create the bar chart newcases
  output$barChart_newcases <- renderPlot({
    filteredData <- filteredData_PLSD()
    
    # Calculate the required values based on the filtered data
    TotalNewCase3Plus_F <- n_distinct(subset(filteredData, (ID_BL_DOTS == 1 & ID_BL_APOPO > 0) & ID_STATUS == 13)$ID_SAMPLE)
    TotalNewCase2Plus_F <- n_distinct(subset(filteredData, (ID_BL_DOTS == 1 & ID_BL_APOPO > 0) & ID_STATUS == 12)$ID_SAMPLE)
    TotalNewCase1Plus_F <- n_distinct(subset(filteredData, (ID_BL_DOTS == 1 & ID_BL_APOPO > 0) & ID_STATUS == 11)$ID_SAMPLE)
    TotalNewCaseScanty_F <- n_distinct(subset(filteredData, (ID_BL_DOTS == 1 & ID_BL_APOPO > 0) & ID_STATUS > 1)$ID_SAMPLE) - TotalNewCase1Plus - TotalNewCase2Plus - TotalNewCase3Plus
    
    # Prepare the data
    new_cases <- data.frame(
      Bact.load = c("3+", "2+", "1+", "Scanty"),
      Total = c(TotalNewCase3Plus_F, TotalNewCase2Plus_F,
                TotalNewCase1Plus_F, TotalNewCaseScanty_F)
    )
    ggplot(new_cases, aes(x = Bact.load, y = Total, color = Bact.load)) +
      geom_bar(stat = "identity", fill = "white") +
      labs(x = "Bacterial Load", y = "Total Count", title = "New Cases") +
      theme(plot.title = element_text(face = "bold", size = 20)) +
      labs(title = "New Cases")
  })
  
  
  # Average Individual Rat Results
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
  
  # Construct overview_AIRR data frame
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
  
  
  # Visualization for overview_AIRR data frame
  output$sensitivitySpecificityPlot <- renderPlot({
    # Filter the data for sensitivity and specificity rows
    filtered_data <- subset(overview_AIRR, Total %in% c("Sensitivity-Dots", "Sensitivity-Blinds", "Specificity-Dots"))
    
    # Convert the percentage values to numeric
    filtered_data$Indicated <- as.numeric(gsub("%", "", filtered_data$Indicated))
    
    # Create a line plot
    ggplot(filtered_data, aes(x = Total, y = Indicated, group = 1)) +
      geom_line() +
      geom_point() +
      labs(x = "Metric", y = "Percentage", fill = "Metric") +
      theme_light()
  })
  
  
  # Average Rat Sample Details 
  
  #filerdata
  filteredData_ARSD <- reactive({
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
  
  output$dateInput_ARSD <- renderUI({
    if (input$timePeriod_ARSD == "Day") {
      selectInput("date_ARSD", "Select Date:", choices = unique(TB_Adtl$DATE), selected = Sys.Date())
    } else {
      NULL
    }
  })
  
  output$weekInput_ARSD <- renderUI({
    if (input$timePeriod_ARSD == "Week") {
      selectInput("week_ARSD", "Select Week:", choices = unique(TB_Adtl$week_numbers))
    } else {
      NULL
    }
  })
  
  output$monthInput_ARSD <- renderUI({
    if (input$timePeriod_ARSD == "Month") {
      selectInput("month_ARSD", "Select Month:", choices = unique(TB_Adtl$month_numbers))
    } else {
      NULL
    }
  })
  
  
  # DOTs cases
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
      "$(this.api().table().header()).css({'background-color': '#298', 'color': '#fff', 'text-align': 'center',});",
      "}"), rowCallback = JS(rowCallback)))
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
    TotalNewCaseScanty_Filter <- n_distinct(subset(filteredData, (ID_BL_DOTS == 1 & ID_BL_APOPO > 0) & ID_STATUS > 1)$ID_SAMPLE) - TotalNewCase1Plus - TotalNewCase2Plus - TotalNewCase3Plus
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
      "$(this.api().table().header()).css({'background-color': '#298', 'color': '#fff', 'text-align': 'center',});",
      "}"), rowCallback = JS(rowCallback)))
  })
  
  
  # total3plusHit <- n_distinct(subset(TB_Adtl, TB_Adtl$ID_BL_DOTS == 13 &
  #                                      TB_Adtl$RatHit > 0)$ID_SAMPLE)
  # total2plusHit <- n_distinct(subset(TB_Adtl, TB_Adtl$ID_BL_DOTS == 12 &
  #                                      TB_Adtl$RatHit > 0)$ID_SAMPLE)
  # total1plusHit <- n_distinct(subset(TB_Adtl, TB_Adtl$ID_BL_DOTS == 11 &
  #                                      TB_Adtl$RatHit > 0)$ID_SAMPLE)
  # totalScantyHit <- n_distinct(subset(TB_Adtl, TB_Adtl$ID_BL_DOTS != 11 &
  #                                       TB_Adtl$ID_BL_DOTS != 12 &
  #                                       TB_Adtl$ID_BL_DOTS != 13 &
  #                                       TB_Adtl$ID_BL_DOTS != 1 &
  #                                       TB_Adtl$ID_BL_DOTS != 0 &
  #                                       TB_Adtl$RatHit > 0)$ID_SAMPLE)
  # # %HIT
  # percentage3plusHit <- total3plusHit/TotalDOTSPos3Plus
  # percentage2plusHit <- total2plusHit/TotalDOTSPos2Plus
  # percentage1plusHit <- total1plusHit/TotalDOTSPos1Plus
  # percentageScantyHit <- totalScantyHit/TotalDOTSPosScanty
  # 
  # # 3+,2+,1+,Scanty
  # total123plusScantyHit <- total3plusHit + total2plusHit 
  # + total2plusHit+ totalScantyHit
  # 
  # # %Total
  # percentage3plus <- TotalDOTSPos3Plus/TotalDOTSPos
  # percentage2plus <- TotalDOTSPos2Plus/TotalDOTSPos
  # percentage1plus <- TotalDOTSPos1Plus/TotalDOTSPos
  # percentageScanty <- TotalDOTSPosScanty/TotalDOTSPos
  
  
  # # NEW cases
  # TotalNewCase3PlushHit <- n_distinct(subset(TB_Adtl, 
  #                                            (TB_Adtl$ID_BL_DOTS == 1 
  #                                             & TB_Adtl$ID_BL_APOPO > 0)
  #                                            & TB_Adtl$ID_STATUS == 13
  #                                            & TB_Adtl$RatHit > 0)$ID_SAMPLE)  
  # TotalNewCase2PlushHit <- n_distinct(subset(TB_Adtl, 
  #                                            (TB_Adtl$ID_BL_DOTS == 1 
  #                                             & TB_Adtl$ID_BL_APOPO > 0)
  #                                            & TB_Adtl$ID_STATUS == 12
  #                                            & TB_Adtl$RatHit > 0)$ID_SAMPLE) 
  # TotalNewCase1PlushHit <- n_distinct(subset(TB_Adtl, 
  #                                            (TB_Adtl$ID_BL_DOTS == 1 
  #                                             & TB_Adtl$ID_BL_APOPO > 0)
  #                                            & TB_Adtl$ID_STATUS == 11
  #                                            & TB_Adtl$RatHit > 0)$ID_SAMPLE)
  # 
  # TotalScantyHit_Newcase <- n_distinct(subset(TB_Adtl, TB_Adtl$ID_BL_DOTS != 11 &
  #                                               TB_Adtl$ID_BL_DOTS != 12 &
  #                                               TB_Adtl$ID_BL_DOTS != 13 &
  #                                               TB_Adtl$ID_BL_DOTS == 1 &
  #                                               TB_Adtl$ID_BL_APOPO > 0 &
  #                                               TB_Adtl$RatHit > 0)$ID_SAMPLE)
  # 
  # # %Hit
  # percentage3plusHit_NewCase <- TotalNewCase3PlushHit/TotalNewCase3Plus
  # percentage2plusHit_NewCase <- TotalNewCase2PlushHit/TotalNewCase2Plus
  # percentage1plusHit_NewCase <- TotalNewCase1PlushHit/TotalNewCase1Plus
  # percentageScanty_NewCase <- TotalScantyHit_Newcase/ TotalNewCaseScanty
  # 
  # # 3+,2+,1+,Scanty - New sample
  # total123plusScantyHit_Newcase <- TotalNewCase3PlushHit + TotalNewCase2PlushHit
  # + TotalNewCase1PlushHit + TotalScantyHit_Newcase
  # # %Total
  # percentage3plus_Newcase <- TotalNewCase3Plus/TotalNewCase
  # percentage2plus_Newcase <- TotalNewCase2Plus/TotalNewCase
  # percentage1plus_Newcase <- TotalNewCase1Plus/TotalNewCase
  # percentageScanty_Newcase <- TotalNewCaseScanty/TotalNewCase
  # 
  # #  Create the data for the Average Rat Sample Details table
  # overview_ARSD_Dots <- data.frame(
  #   "Bact load" = c("3+", "2+", "1+", "Scanty", "Samples"),
  #   "Percentage HIT" = c(percentage3plusHit, percentage2plusHit, percentage1plusHit,
  #                        percentageScantyHit, total123plusScantyHit),
  #   "Percentage Total" = c(paste0(round(percentage3plus * 100, 1), "%"),
  #                          paste0(round(percentage2plus * 100, 1), "%"),
  #                          paste0(round(percentage1plus * 100, 1),"%"),
  #                          paste0(round(percentageScanty * 100, 1),"%"),
  #                          paste0(100.0,"%"))
  # )
  # overview_ARSD_Newcase <- data.frame(
  #   "Bact load" = c("3+", "2+", "1+", "Scanty", "Samples"),
  #   "Percentage HIT" = c(percentage3plusHit_NewCase, percentage2plusHit_NewCase,
  #                        percentage1plusHit_NewCase,percentageScanty_NewCase,
  #                        total123plusScantyHit_Newcase),
  #   "Percentage Total" = c(paste0(round(percentage3plus_Newcase * 100, 1), "%"), 
  #                          paste0(round(percentage2plus_Newcase * 100, 1), "%"),
  #                          paste0(round(percentage1plus_Newcase * 100, 1), "%"),
  #                          paste0(round(percentageScanty * 100, 1), "%"),
  #                          paste0(100.0,"%"))
  # ) 
  # 
  
  output$AIRR <- DT::renderDataTable({
    datatable(overview_AIRR, options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#298', 'color': '#fff', 'text-align': 'center',});",
      "}"), rowCallback = JS(rowCallback)))
  })
  
  # output$ARSD_DOTs <- DT::renderDataTable({
  #   datatable(overview_ARSD_Dots, options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
  #     "function(settings, json) {",
  #     "$(this.api().table().header()).css({'background-color': '#298', 'color': '#fff', 'text-align': 'center',});",
  #     "}"), rowCallback = JS(rowCallback)))
  # })
  # 
  # 
  # 
  # 
  # output$ARSD_newcase <- DT::renderDataTable({
  #   datatable(overview_ARSD_Newcase, options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
  #     "function(settings, json) {",
  #     "$(this.api().table().header()).css({'background-color': '#298', 'color': '#fff', 'text-align': 'center',});",
  #     "}"), rowCallback = JS(rowCallback)))
  # })
  

  # Render the tables in the UI
  # Reference for the table: https://stackoverflow.com/questions/43739218/r-datatable-formatting-with-javascript
  output$Sensitivity_trainer_table_daily <- DT::renderDataTable({
    datatable(Sensitivity_trainer_table_daily, options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#298', 'color': '#fff', 'text-align': 'center',});",
      "}"))) %>% formatRound(c(4), 4)
  })
  
  output$Specificity_trainer_table_daily <- DT::renderDataTable({
    datatable(Specificity_trainer_table_daily, options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#298', 'color': '#fff', 'text-align': 'center',});",
      "}"))) %>% formatRound(c(4), 4)
  })
  output$Sensitivity_trainer_table_weekly <- DT::renderDataTable({
    datatable(Sensitivity_trainer_table_weekly, options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#298', 'color': '#fff', 'text-align': 'center',});",
      "}"))) %>% formatRound(c(4), 4)
  })
  
  output$Specificity_trainer_table_weekly <- DT::renderDataTable({
    datatable(Specificity_trainer_table_weekly, options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#298', 'color': '#fff', 'text-align': 'center',});",
      "}"))) %>% formatRound(c(4), 4)
  })
  output$Sensitivity_trainer_table_monthly <- DT::renderDataTable({
    datatable(Sensitivity_trainer_table_monthly, options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#298', 'color': '#fff', 'text-align': 'center',});",
      "}"))) %>% formatRound(c(4), 4)
  })
  
  output$Specificity_trainer_table_monthly <- DT::renderDataTable({
    datatable(Specificity_trainer_table_monthly, options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20), initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#298', 'color': '#fff', 'text-align': 'center',});",
      "}"))) %>% formatRound(c(4), 4)
  })
  
  # Generate choices for selectInput dynamically
  output$valueSelect <- renderUI({
    # Create the selectInput with the dynamically generated choices
    selectInput("SenSpe", "Select the value", choices = c("Sensitivity", "Specificity"))
  })
  
  output$trainerSelect <- renderUI({
    # Create the selectInput with the dynamically generated choices
    selectInput("Trainer_Name", "Select a Trainer", choices = unique(TB_rat$TRAINER))
  })
  
  output$timeSelect <- renderUI({
    # Create the selectInput with the dynamically generated choices
    selectInput("Time_Select", "Display Time", choices = c("Daily", "Weekly", "Monthly"))
  })
  
  output$trainertable <- DT::renderDataTable({
    selectValue <- input$SenSpe
    selectTime <- input$Time_Select
    selectTrainer <- input$Trainer_Name
    if (selectValue == "Sensitivity"){
      if (selectTime == "Daily"){
        Display_Table <- subset(Sensitivity_trainer_table_daily, Sensitivity_trainer_table_daily$Trainer_Name == selectTrainer)
        datatable(Display_Table, ,options = list(pageLength = 8, lengthMenu = c(10, 15, 20), initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#298', 'color': '#fff', 'text-align': 'center',});",
          "}"))) %>% formatRound(c(4), 4)
      }else if (selectTime == "Weekly"){
        Display_Table <- subset(Sensitivity_trainer_table_weekly, Sensitivity_trainer_table_weekly$Trainer_Name == selectTrainer)
        datatable(Display_Table, ,options = list(pageLength = 8, lengthMenu = c(10, 15, 20), initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#298', 'color': '#fff', 'text-align': 'center',});",
          "}"))) %>% formatRound(c(4), 4)
      }else{
        Display_Table <- subset(Sensitivity_trainer_table_monthly, Sensitivity_trainer_table_monthly$Trainer_Name == selectTrainer)
        datatable(Display_Table, ,options = list(pageLength = 8, lengthMenu = c(10, 15, 20), initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#298', 'color': '#fff', 'text-align': 'center',});",
          "}"))) %>% formatRound(c(4), 4)
      }
    }else{
      if (selectTime == "Daily"){
        Display_Table <- subset(Specificity_trainer_table_daily, Specificity_trainer_table_daily$Trainer_Name == selectTrainer)
        datatable(Display_Table, ,options = list(pageLength = 8, lengthMenu = c(10, 15, 20), initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#298', 'color': '#fff', 'text-align': 'center',});",
          "}"))) %>% formatRound(c(4), 4)
      }else if (selectTime == "Weekly"){
        Display_Table <- subset(Specificity_trainer_table_weekly, Specificity_trainer_table_weekly$Trainer_Name == selectTrainer)
        datatable(Display_Table, ,options = list(pageLength = 8, lengthMenu = c(10, 15, 20), initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#298', 'color': '#fff', 'text-align': 'center',});",
          "}"))) %>% formatRound(c(4), 4)
      }else{
        Display_Table <- subset(Specificity_trainer_table_monthly, Specificity_trainer_table_monthly$Trainer_Name == selectTrainer)
        datatable(Display_Table, ,options = list(pageLength = 8, lengthMenu = c(10, 15, 20), initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#298', 'color': '#fff', 'text-align': 'center',});",
          "}"))) %>% formatRound(c(4), 4)
      }
    }
  })
  
  output$LineChart <- renderPlot({
    selectValue <- input$SenSpe
    selectTime <- input$Time_Select
    selectTrainer <- input$Trainer_Name
    
    if (selectValue == "Sensitivity") {
      if (selectTime == "Daily") {
        Display_Table <- subset(Sensitivity_trainer_table_daily, Sensitivity_trainer_table_daily$Trainer_Name == selectTrainer)
        #plot(Display_Table$Date, Display_Table$Sensitivity, type = "o", pch = 16, xlab = "Time Period", ylab = "Sensitivity", main = "Line Chart")
        plot_name <- paste("Daily data for", selectTrainer)
        ggplot(Display_Table, aes(Date, Sensitivity)) + geom_point(fill = "blue", shape = 23, size = 3) + geom_line(colour = "brown") + ggtitle(plot_name) +
          theme(plot.background = element_rect(fill = "azure3"))
      } else if (selectTime == "Weekly") {
        Display_Table <- subset(Sensitivity_trainer_table_weekly, Sensitivity_trainer_table_weekly$Trainer_Name == selectTrainer)
        #plot(Display_Table$Week, Display_Table$Sensitivity, type = "o", pch = 16, xlab = "Time Period", ylab = "Sensitivity", main = "Line Chart")
        plot_name <- paste("Weekly data for", selectTrainer)
        Display_Table$Week <- as.numeric(Display_Table$Week)
        ggplot(Display_Table, aes(Week, Sensitivity)) + geom_point(fill = "blue", shape = 23, size = 3) + geom_line(colour = "brown") + ggtitle(plot_name) +
          theme(plot.background = element_rect(fill = "azure3"))
      } else {
        Display_Table <- subset(Sensitivity_trainer_table_monthly, Sensitivity_trainer_table_monthly$Trainer_Name == selectTrainer)
        #plot(Display_Table$Month, Display_Table$Sensitivity, type = "o", pch = 16, xlab = "Time Period", ylab = "Sensitivity", main = "Line Chart")
        plot_name <- paste("Monthly data for", selectTrainer)
        Display_Table$Month <- as.numeric(Display_Table$Month)
        ggplot(Display_Table, aes(as.numeric(Month), Sensitivity)) + geom_point(fill = "blue", shape = 23, size = 3) + geom_line(colour = "brown") + ggtitle(plot_name) +
          theme(plot.background = element_rect(fill = "azure3"))
      }
    } else {
      if (selectTime == "Daily") {
        Display_Table <- subset(Specificity_trainer_table_daily, Specificity_trainer_table_daily$Trainer_Name == selectTrainer)
        #plot(Display_Table$Date, Display_Table$Specificity, type = "o", pch = 16, xlab = "Time Period", ylab = "Specificity", main = "Line Chart")
        plot_name <- paste("Daily data for", selectTrainer)
        ggplot(Display_Table, aes(Date, Specificity)) + geom_point(fill = "blue", shape = 23, size = 3) + geom_line(colour = "brown") + ggtitle(plot_name) +
          theme(plot.background = element_rect(fill = "azure3"))
      } else if (selectTime == "weekly") {
        Display_Table <- subset(Specificity_trainer_table_weekly, Specificity_trainer_table_weekly$Trainer_Name == selectTrainer)
        #plot(Display_Table$Week, Display_Table$Specificity, type = "o", pch = 16, xlab = "Time Period", ylab = "Specificity", main = "Line Chart")
        plot_name <- paste("Weekly data for", selectTrainer)
        Display_Table$Week <- as.numeric(Display_Table$Week)
        ggplot(Display_Table, aes(as.numeric(Week), Specificity)) + geom_point(fill = "blue", shape = 23, size = 3) + geom_line(colour = "brown") + ggtitle(plot_name) +
          theme(plot.background = element_rect(fill = "azure3"))
      } else {
        Display_Table <- subset(Specificity_trainer_table_monthly, Specificity_trainer_table_monthly$Trainer_Name == selectTrainer)
        #plot(Display_Table$Month, Display_Table$Specificity, type = "o", pch = 16, xlab = "Time Period", ylab = "Specificity", main = "Line Chart")
        plot_name <- paste("Monthly data for", selectTrainer)
        Display_Table$Month <- as.numeric(Display_Table$Month)
        ggplot(Display_Table, aes(as.numeric(Month), Specificity)) + geom_point(fill = "blue", shape = 23, size = 3) + geom_line(colour = "brown") + ggtitle(plot_name) +
          theme(plot.background = element_rect(fill = "azure3"))
      }
    }
  })
  ###################### Rat Performance ###########################
  unique_rat_names <- sort(unique(TB_rat$RAT_NAME))
  
  # Input from UI
  output$ratSelect <- renderUI({
    # Create the selectInput with the dynamically generated choices
    pickerInput(
      inputId = "Indi_rat",
      label = NULL,
      choices = unique_rat_names,
      selected = NULL,
      multiple = FALSE,
    )
  })
  
  # Accuracy by bacterial load
  
  
  #
  
  
  ###################### Overall Table ###########################
  
  ############## table ##################
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
  
  # Render the tables in the UI
  output$BacterialLevelTable <- renderTable({
    BacterialLevelTable
  })
  
  colnames(HitTable) <- c("RAT_NAME", "SampleReuse", "UBL(0)", "Negative(1)",
                          "1 AFB(2)", "2 AFB(3)", "3 AFB(4)", "4 AFB(5)",
                          "5 AFB(6)", "6 AFB(7)", "7 AFB(8)", "8 AFB(9)",
                          "9 AFB(10)", "1+(11)", "2+(12)", "3+(13)",
                          " (14)", "10 AFB(15)", "11 AFB(16)", "12 AFB(17)",
                          "13 AFB(18)", "14 AFB(19)", "15 AFB(20)",
                          "16 AFB(21)", "17 AFB(22)", "18 AFB(23)",
                          "19 AFB(24)", "Sensitivity","Total New case", "New Case Found")
  
  # Render the tables in the UI
  output$HitTable <- renderTable({
    HitTable
  })
  
  colnames(PercentageTable) <- c("RAT_NAME", "SampleReuse", "UBL(0)", "Negative(1)",
                                 "1 AFB(2)", "2 AFB(3)", "3 AFB(4)", "4 AFB(5)",
                                 "5 AFB(6)", "6 AFB(7)", "7 AFB(8)", "8 AFB(9)",
                                 "9 AFB(10)", "1+(11)", "2+(12)", "3+(13)",
                                 " (14)", "10 AFB(15)", "11 AFB(16)", "12 AFB(17)",
                                 "13 AFB(18)", "14 AFB(19)", "15 AFB(20)",
                                 "16 AFB(21)", "17 AFB(22)", "18 AFB(23)",
                                 "19 AFB(24)", "Sensitivity","Total New case", "New Case Found")
  
  # Render the tables in the UI
  output$PercentageTable <- renderTable({
    PercentageTable
  })
  
  # Render the rat hit Analyst in the UI
  output$ratHitAnalystInputs <- renderUI({
    fluidRow(
      box(
        title = "Select Rat and Sample Reuse",
        selectInput("rat", "Select Rat Name", choices = unique(HitTable$RAT_NAME)),
        selectInput("sample_reuse", "Select Sample Reuse:",
                    choices = c("ALL", "FRESH", "RE-USED"))
      )
    )
  })
  
  # Rat Hit Analysis
  output$barplot <- renderPlot({
    rat_df <- HitTable[HitTable$RAT_NAME == input$rat & HitTable$SampleReuse == input$sample_reuse, ] # Subset data for the selected rat
    rat_hits <- as.numeric(unlist(rat_df[, 5:24]))  # Exclude RatName column and convert to numeric
    barplot(rat_hits, names.arg = colnames(rat_df[, 5:24]),
            xlab = "Bacterial Level", ylab = "Number of Hits",
            main = paste("Rat:", input$rat, " - Sample Reuse:", input$sample_reuse))
  })
  
  ############## Connection of two tables ##################
  
  # Render the tables in the UI
  output$Selected_Bac_Level <- renderTable({
    Selected_Bac_Level <- subset(BacterialLevelTable, RAT_NAME == input$rat & SampleReuse == input$sample_reuse)
    Selected_Bac_Level
  })
  
  # Render the tables in the UI
  output$Selected_Hits <- renderTable({
    Selected_Hits <- subset(HitTable, RAT_NAME == input$rat & SampleReuse == input$sample_reuse)
    Selected_Hits
  })
  
  # Render the tables in the UI
  output$Selected_Percentage <- renderTable({
    Selected_Percentage <- subset(PercentageTable, RAT_NAME == input$rat & SampleReuse == input$sample_reuse)
    Selected_Percentage
  })
  
  # Server end mark
}
