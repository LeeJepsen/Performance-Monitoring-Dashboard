##clear working environment
rm(list = ls())

##install packages
##Making sure all packages are up to date 
##In recent Rstudio update some packages expect others to be up do date as well
##such as "promises" needs to be >= 1.5.0
##Remove the # in front of lines 10-18 to install packages

#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("readxl")
#install.packages("plotly")
#install.packages("DT")
#install.packages("zoo")
#install.packages("promises")
#install.packages(c("httpuv", "later", "fastmap", "R6"))

##load libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(plotly)
library(DT)
library(zoo)

##import CMJ data
Data <- read_excel("C:/FilePath/ExampleTeam.xlsx")

##changing date format
Data$Date <- as.Date(Data$Date)

##ui
ui <- fluidPage(
  titlePanel("CMJ Performance Monitoring"),
  
  ##Team view Side Bar with jump type and team selection
  tabsetPanel(id = "mainTabs",
              
              ##Tab 1: Team Overview
              tabPanel("Team Overview",
                       sidebarLayout(
                         sidebarPanel(
                           width = 3,
                           selectInput("Team", "Select Team:", choices = c("Team 1")),
                           selectInput("JumpType", "Select Jump Type:", choices = c("CMJ")),
                           div(
                             style = "background-color:#f9f9f9; border-radius:8px; padding:15px; margin-top:10px;",
                             h4(tags$span(style = "font-weight: bold; text-decoration: underline;", 
                                          "Variable Descriptions")),
                             tags$ul(
                               tags$li(HTML("<strong>Jump Height:</strong> Distance in (cm) off the force plate")),
                               tags$li(HTML("<strong>RSI-mod:</strong> Jump Height / Time-to-Takeoff")),
                               tags$li(HTML("<strong>Flagged Athletes:</strong> Performance below normal SWC")),
                               tags$li(HTML("<strong>SWC:</strong> mean - 0.2 × SD over 4 tests (excluding most recent)"))
                             )
                           )
                         ),
                         mainPanel(
                           fluidRow(
                             column(width = 12, htmlOutput("teamOverview")),
                             column(width = 12, plotlyOutput("JHplot", height = "300px")),
                             column(width = 12, plotlyOutput("RSImodplot", height = "300px"))
                           )
                         )
                       )
              ),
              
              ##Tab 2: Athlete Tables
              tabPanel("Athlete Tables",
                       sidebarLayout(
                         sidebarPanel(
                           width = 2,  
                           selectInput("selectedAthlete", "Select Athlete:", 
                                       choices = unique(Data$Name)),
                           helpText("Select an athlete to view full CMJ timeline.")
                         ),
                         mainPanel(
                           fluidRow(
                             column(6.5, h4("Athlete Timeline Table"),
                                    DT::dataTableOutput("individualTable",
                                                        height = "400px")),
                             column(6.5, h4("Team Summary Table"), 
                                    DT::dataTableOutput("teamSummaryTable", 
                                                        height = "400px")),

                           )
                         )
                       )
              )
  )
)


##Server
server <- function(input, output, session) {
  
  ####React with data, Filter date
  filteredData <- reactive({
    req(input$Team, input$JumpType)
    
    Data %>%
      filter(
        Team == input$Team,
        `Test Type` == input$JumpType,
        Date >= as.Date("2024-08-26") & Date <= as.Date("2024-11-19"),
        !is.na(`Jump Height (Imp-Mom) [cm]`) | !is.na(`RSI-modified [m/s]`)
      ) 
  })
  
  
  ####Jump Height Main Plot 
  output$JHplot <- renderPlotly({
    DataSummary <- filteredData() %>%
      group_by(Date) %>%
      summarize(`Avg Jump Height` = round(mean(`Jump Height (Imp-Mom) [cm]`, na.rm = TRUE), 1),
                SD = 0.5 * sd(`Jump Height (Imp-Mom) [cm]`, na.rm = TRUE),
                N = n_distinct(Name),
                .groups = "drop") %>%
      mutate(
        ymin = `Avg Jump Height` - SD,
        ymax = `Avg Jump Height` + SD,
        Label = paste0("Date: ", format(Date, "%B %d, %Y"),
                       "<br>Avg Jump Height: ", `Avg Jump Height`, " cm",
                       "<br>SD: ", round(SD, 1), " cm",
                       "<br>Tested: ", N, " Athletes")
      )
    
    ##plot
    gg <- ggplot(DataSummary, aes(x = Date, y = `Avg Jump Height`)) +
      geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "lightgrey", alpha = 0.2) +
      geom_line(color = "navy", size = .75) +
      geom_point(aes(text = Label), color = "yellow", size = 1.5) +
      labs(title = "Team Average Jump Height",
           x = "Date", y = "Jump Height (cm)") +
      theme_minimal()
    
    ggplotly(gg, tooltip = "text")
  })
  
  ####RSImod Main
  output$RSImodplot <- renderPlotly({
    allData <- filteredData() 
    
    
    ##Mean RSI-mod and average SWC per test date
    DataSummary <- allData%>%
      group_by(Date) %>%
      summarize(
        `Avg RSI-mod` = round(mean(`RSI-modified [m/s]`, na.rm = TRUE), 2),
        N = n_distinct(Name),
        .groups = "drop"
      ) %>%
      mutate(
        Label = paste0("Date: ", format(Date, "%B %d, %Y"),
                       "<br>Avg RSI-mod: ", `Avg RSI-mod`, "m/s",
                       "<br>Tested: ", N, " Athletes") 
      )
    
    ##plot
    gg <- ggplot(DataSummary, aes(x = Date, y = `Avg RSI-mod`)) +
      geom_line(color = "navy", size = 0.75) +
      geom_point(aes(text = Label), color = "yellow", size = 1.5) +
      labs(title = "Team Average RSI-modified",
           x = "Date", y = "RSI-modified (m/s)") +
      theme_minimal()
    
    ggplotly(gg, tooltip = "text")
  })
  
  
  ####Team Overview Header
  output$teamOverview <- renderUI({
    data <- filteredData()
    req(nrow(data) > 0)
    
    ##date of last test
    lastDate <- max(data$Date, na.rm = TRUE)
    lastDateFormatted <- format(lastDate, "%B %d, %Y")
    
    ##Summary of Current Week
    currentWeek <- data %>% filter(Date == lastDate)
    lastWeek <- data %>% filter(Date == sort(unique(Date), decreasing = TRUE)[2])
    
    ##Jump height
    avgJHnow <- mean(currentWeek$`Jump Height (Imp-Mom) [cm]`, na.rm = TRUE)
    avgJHlast <- mean(lastWeek$`Jump Height (Imp-Mom) [cm]`, na.rm = TRUE)
    deltaJH <- 100 * (avgJHnow - avgJHlast) / avgJHlast
    
    ##RSImod
    avgRSInow <- mean(currentWeek$`RSI-modified [m/s]`, na.rm = TRUE)
    avgRSIlast <- mean(lastWeek$`RSI-modified [m/s]`, na.rm = TRUE)
    deltaRSI <- 100 * (avgRSInow - avgRSIlast) / avgRSIlast
    
    ##implementing flagged athletes ~ based on SWC for RSImod
    ##Jumped on last test date, also jumped 4 times in last 5 weeks, NOT most recent
    ##already defined the most recent test date : lastDate
    
    ##5 most recent test dates including lastDate
    RecentDates <- data %>%
      distinct(Date) %>%
      arrange(desc(Date)) %>%
      slice_head(n = 5) %>%
      pull(Date)
    
    ##Define the 4 dates to calculate SWC (excluding lastDate)
    SWCdates <- setdiff(RecentDates, lastDate)
    
    ##Athletes who jumped on last date & jumped 4 times prior
    EligbleAthletes <- data %>%
      filter(Date %in% c(SWCdates, lastDate)) %>%
      group_by(Name) %>%
      summarize(JumpedonLast = any(Date == lastDate),
                SWCcount = sum(Date %in% SWCdates),
                .groups = "drop"
      ) %>%
      filter(JumpedonLast, SWCcount >= 4) %>%
      pull(Name)
    
    ##Flagged athletes based on RSI-mod SWC from 4 previous jumps
    summaryData <- data %>%
      filter(Name %in% EligbleAthletes) %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      mutate(RSIrollMean = lag(rollapply(`RSI-modified [m/s]`, width = 4, FUN = mean,
                                         align = "right", fill = NA, na.rm = TRUE)),
             
             RSIrollSD = lag(rollapply(`RSI-modified [m/s]`, width = 4, FUN = sd,
                                       align = "right", fill = NA, na.rm = TRUE)),
             
             SWC = 0.2 * RSIrollSD,
             flagged = `RSI-modified [m/s]` < (RSIrollMean - SWC)
      ) %>%
      filter(Date == lastDate)
    
    ##count number of athletes for both flagged and athletes that jump
    flaggedCount <- sum(summaryData$flagged, na.rm = TRUE)
    athleteCount <- n_distinct(currentWeek$Name)
    flaggedNames <- summaryData %>%
      filter(flagged == TRUE) %>%
      pull(Name)
    
    ##Color trend arrows
    ArrowSpan <- function(value) {
      color <- if (value >= 0) "green" else "red"
      arrow <- if (value >= 0) "▲" else "▼"
      span(style = paste0("color:", color, paste0(arrow, " ", 
                                                  round(abs(value), 1), "%"))
      )
    }
    
    ##The header styling above plot
    div(
      style = "display: flex; flex-wrap: wrap; gap: 16px; margin-bottom: 10px;",
      
      ##Last Test Date Box
      div(style = "flex: 1; background-color: #f1f1f1; border-radius: 8px; padding: 10px; text-align: center;",
          div(style = "font-size: 14px; font-weight: bold;", "Last Test"),
          div(style = "font-size: 16px;", format(lastDate, "%b %d, %Y"))
      ),
      
      ##Avg Jump Height
      div(style = "flex: 1; background-color: #f9f9f9; border-radius: 8px; padding: 10px; text-align: center;",
          div(style = "font-size: 14px; font-weight: bold;", "📏 Avg Jump Height"),
          div(style = "font-size: 16px;", 
              paste0(round(avgJHnow, 1), " cm "),
              span(style = paste0("color:", ifelse(deltaJH >= 0, "green", "red"), "; font-weight:bold;"),
                   ifelse(deltaJH >= 0, "▲", "▼"), " ", round(abs(deltaJH), 1), "%")
          )
      ),
      
      ##RSI-mod
      div(style = "flex: 1; background-color: #f9f9f9; border-radius: 8px; padding: 10px; text-align: center;",
          div(style = "font-size: 14px; font-weight: bold;", "⚡ Avg RSI-mod"),
          div(style = "font-size: 16px;", 
              paste0(round(avgRSInow, 2), " m/s "),
              span(style = paste0("color:", ifelse(deltaRSI >= 0, "green", "red"), "; font-weight:bold;"),
                   ifelse(deltaRSI >= 0, "▲", "▼"), " ", round(abs(deltaRSI), 1), "%")
          )
      ),
      
      ##Tested Count
      div(style = "flex: 1; background-color: #f3f7ff; border-radius: 8px; padding: 10px; text-align: center;",
          div(style = "font-size: 14px; font-weight: bold;", "Athletes Tested"),
          div(style = "font-size: 16px;", athleteCount)
      ),
      
      ##Flagged Athletes
      div(
        style = "flex: 1; background-color: #fff3f3; border-radius: 8px; padding: 10px; text-align: center;",
        div(style = "font-size: 14px; font-weight: bold;", "🔴 Flagged Athletes"),
        tags$span(
          title = if (length(flaggedNames) > 0) {
            paste("Athletes:", paste(flaggedNames, collapse = ", "))
          } else {
            "Athletes: None flagged"
          },
          style = "font-size: 16px; cursor: help; display: inline-block;",
          flaggedCount
        )
      )
    )
  })
  
  
  ##TEAM SNAPSHOT TABLE
  output$teamSummaryTable <- DT::renderDataTable({
    data <- filteredData()
    
    ## Compute rolling averages & personal bests per athlete
    tableData <- data %>%
      arrange(Name, Date) %>%
      group_by(Name) %>%
      mutate(RollRSI = lag(rollapply(`RSI-modified [m/s]`, 4, mean, fill = NA, 
                                align = "right", na.rm = TRUE)),
             PB_JH = cummax(`Jump Height (Imp-Mom) [cm]`),
             PB_RSI = cummax(`RSI-modified [m/s]`)
      ) %>%
      ungroup() %>%
      group_by(Name) %>%
      filter(Date == max(Date, na.rm = TRUE)) %>%  # Latest test only
      summarise(
        `Last Test` = max(Date),
        `PB Jump Height` = max(PB_JH, na.rm = TRUE),
        `PB RSI-mod` = max(PB_RSI, na.rm = TRUE),
        `Rolling Avg RSI` = last(RollRSI),
        .groups = "drop"
      )
    ##customize table function
    DT::datatable(
      tableData,
      options = list(
        scrollX = TRUE,
        scrollY = "350px",    
        paging = FALSE,       
        dom = 't',            
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      rownames = FALSE,
      class = "compact stripe nowrap"
    )
    
    
  })
  
  
  ###INDIVIDUAL ATHLETE TIMELINE TABLE
  output$individualTable <- DT::renderDataTable({
    req(input$selectedAthlete)
    data <- filteredData() %>%
      filter(Name == input$selectedAthlete) %>%
      arrange(Date) %>%
      group_by(Name) %>%
      mutate( AvgRSI = lag(rollapply(`RSI-modified [m/s]`, 4, mean, fill = NA, 
                                   align = "right", na.rm = TRUE)),
              Flagged = `RSI-modified [m/s]` < (AvgRSI - (0.2 * sd(`RSI-modified [m/s]`,
                                                                 na.rm = TRUE)))
      ) %>%
      select(Date, 
             `Jump Height (Imp-Mom) [cm]`,
             `RSI-modified [m/s]`, 
             AvgRSI, 
             Flagged)
    
    ##customize table function
    DT::datatable(
      data,
      options = list(
        scrollX = TRUE,
        scrollY = "350px",    
        paging = FALSE,       
        dom = 't',            
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      rownames = FALSE,
      class = "compact stripe nowrap"
    )
    
    
  })
  
  observe({
    updateSelectInput(session, "selectedAthlete",
                      choices = unique(filteredData()$Name))
  })
  
  
}

##Run App
shinyApp(ui = ui, server = server)
