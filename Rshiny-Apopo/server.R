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
  
  TB_rat <- read_excel("TBdetectionRats_Tanzania.xlsx", sheet = 2, col_names = TRUE)
  
  TB_Adtl <- read_excel("TBdetectionRats_Tanzania.xlsx", sheet = 3, col_names = TRUE)
  
  # Pie Chart
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
  
  # Creating a table showing Sensitivity
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
  
  # Creating a table showing Sensitivity
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
  
  # Render the tables in the UI
  output$basicInformation_Sensitivity <- renderTable({
    Sensitivity_table
  })
  
  output$basicInformation_Specificity <- renderTable({
    Specificity_table
  })
  
  Individual_rat_performance <- merge(Sensitivity_table, Specificity_table, by = "Rat_Name", all.x = TRUE)
  
  output$Individual_rat_performance <- renderPlot({
    plot(Individual_rat_performance$Sensitivity, Individual_rat_performance$Specificity, pch = 19, col = c("red", "blue", "green", "orange", "purple"))
    text(Individual_rat_performance$Sensitivity, Individual_rat_performance$Specificity,
         labels = Individual_rat_performance$Rat_Name, pos = 4, cex = 0.8)
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
  
  # Loop over all rats' name
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
  output$Trainer_Sensitivity <- renderTable({
    Sensitivity_trainer_table
  })
  
  output$Trainer_Specificity <- renderTable({
    Specificity_trainer_table
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

  # Render the tables in the UI
  output$Sensitivity_trainer_table_daily <- renderTable({
    Sensitivity_trainer_table_daily
  })
  
  output$Specificity_trainer_table_daily <- renderTable({
    Specificity_trainer_table_daily
  })
  output$Sensitivity_trainer_table_weekly <- renderTable({
    Sensitivity_trainer_table_weekly
  })
  
  output$Specificity_trainer_table_weekly <- renderTable({
    Specificity_trainer_table_weekly
  })
  output$Sensitivity_trainer_table_monthly <- renderTable({
    Sensitivity_trainer_table_monthly
  })
  
  output$Specificity_trainer_table_monthly <- renderTable({
    Specificity_trainer_table_monthly
  })
  
  # Generate choices for selectInput dynamically
  output$valueSelect <- renderUI({
    # Create the selectInput with the dynamically generated choices
    selectInput("SenSpe", "Select the value", choices = c("Sensitivity", "Specificity"))
  })
  
  output$trainerSelect <- renderUI({
    # Create the selectInput with the dynamically generated choices
    selectInput("Trainer_Name", "Select a Trainer", choices = unique_names)
  })
  
  output$timeSelect <- renderUI({
    # Create the selectInput with the dynamically generated choices
    selectInput("Time_Select", "Display Time", choices = c("daily", "weekly", "monthly"))
  })

  output$trainertable <- renderTable({
    selectValue <- input$SenSpe
    selectTime <- input$Time_Select
    selectTrainer <- input$Trainer_Name
    if (selectValue == "Sensitivity"){
      if (selectTime == "daily"){
        Display_Table <- subset(Sensitivity_trainer_table_daily, Sensitivity_trainer_table_daily$Trainer_Name == selectTrainer)
      }else if (selectTime == "weekly"){
        Display_Table <- subset(Sensitivity_trainer_table_weekly, Sensitivity_trainer_table_weekly$Trainer_Name == selectTrainer)
      }else{
        Display_Table <- subset(Sensitivity_trainer_table_monthly, Sensitivity_trainer_table_monthly$Trainer_Name == selectTrainer)
      }
    }else{
      if (selectTime == "daily"){
        Display_Table <- subset(Specificity_trainer_table_daily, Specificity_trainer_table_daily$Trainer_Name == selectTrainer)
      }else if (selectTime == "weekly"){
        Display_Table <- subset(Specificity_trainer_table_weekly, Specificity_trainer_table_weekly$Trainer_Name == selectTrainer)
      }else{
        Display_Table <- subset(Specificity_trainer_table_monthly, Specificity_trainer_table_monthly$Trainer_Name == selectTrainer)
      }
    }
  })

  output$LineChart <- renderPlot({
    selectValue <- input$SenSpe
    selectTime <- input$Time_Select
    selectTrainer <- input$Trainer_Name
    
    if (selectValue == "Sensitivity") {
      if (selectTime == "daily") {
        Display_Table <- subset(Sensitivity_trainer_table_daily, Sensitivity_trainer_table_daily$Trainer_Name == selectTrainer)
        plot(Display_Table$Date, Display_Table$Sensitivity, type = "o", pch = 16, xlab = "Time Period", ylab = "Sensitivity", main = "Line Chart")
      } else if (selectTime == "weekly") {
        Display_Table <- subset(Sensitivity_trainer_table_weekly, Sensitivity_trainer_table_weekly$Trainer_Name == selectTrainer)
        plot(Display_Table$Week, Display_Table$Sensitivity, type = "o", pch = 16, xlab = "Time Period", ylab = "Sensitivity", main = "Line Chart")
      } else {
        Display_Table <- subset(Sensitivity_trainer_table_monthly, Sensitivity_trainer_table_monthly$Trainer_Name == selectTrainer)
        plot(Display_Table$Month, Display_Table$Sensitivity, type = "o", pch = 16, xlab = "Time Period", ylab = "Sensitivity", main = "Line Chart")
      }
    } else {
      if (selectTime == "daily") {
        Display_Table <- subset(Specificity_trainer_table_daily, Specificity_trainer_table_daily$Trainer_Name == selectTrainer)
        plot(Display_Table$Date, Display_Table$Specificity, type = "o", pch = 16, xlab = "Time Period", ylab = "Specificity", main = "Line Chart")
      } else if (selectTime == "weekly") {
        Display_Table <- subset(Specificity_trainer_table_weekly, Specificity_trainer_table_weekly$Trainer_Name == selectTrainer)
        plot(Display_Table$Week, Display_Table$Specificity, type = "o", pch = 16, xlab = "Time Period", ylab = "Specificity", main = "Line Chart")
      } else {
        Display_Table <- subset(Specificity_trainer_table_monthly, Specificity_trainer_table_monthly$Trainer_Name == selectTrainer)
        plot(Display_Table$Month, Display_Table$Specificity, type = "o", pch = 16, xlab = "Time Period", ylab = "Specificity", main = "Line Chart")
      }
    }
  })
}
