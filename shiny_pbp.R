## Deliverable using Shiny


### PLAN

#Initial - Want to build simple tables that can show basic stats of hitters
# Batting avg, batting avg vs lhp vs rhp, obp, w/ risp, Over season and last x amount of games
# be able to filter by hitter(s) or pitcher

library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(data.table)
library(lubridate)

pbp_data <- read.csv("C:\\Users\\bfass\\OneDrive\\Desktop\\CS\\Misc\\Baseball Data Bowl\\Pitchproj\\pbp2022.csv")

#extracting days since today
pbp_data$game_date <- as.Date(pbp_data$game_date)

pbp_data$monthh <- as.integer(format(pbp_data$game_date, "%m"))

pbp_data$days_since_today <- today() - pbp_data$game_date

# Getting hits and the team the batter is on
pbp_data$hit <- ifelse(pbp_data$details.call.description == "Ball" | pbp_data$details.call.description == "Ball In Dirt" | pbp_data$details.call.description == "Hit By Pitch", 2, 
                     ifelse(pbp_data$details.call.description=="In play, no out" | pbp_data$details.call.description == "In play, run(s)", 1,0))

pbp_data$hitting_team <- ifelse(pbp_data$about.isTopInning, pbp_data$away_team, pbp_data$home_team)

last_p <- filter(pbp_data, last.pitch.of.ab == "true")

stats_of_int <- c("BA", "BA vs RHP", "BA vs LHP", "BA w/ risp", "OBP")


ui <- fluidPage(
  titlePanel("Hitting Stats Table"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("Home",
                       "Hitting Team:",
                       c("All",
                         unique(as.character(last_p$hitting_team))))
    ),
    column(4,
           selectInput("Batter",
                       "Batter:",
                       c("All",
                         unique(as.character(last_p$matchup.batter.fullName))))
    ),
    column(4,
           selectInput("Pitch",
                       "Pitch (last of at bat):",
                       c("All",
                         unique(as.character(last_p$details.type.description))))
    )
  ),
  # Create a new row for the table.
  DT::dataTableOutput("table"),
  fluidRow(
    column(width = 6,
           plotOutput("plott")
           ),
    column(width = 6,
           plotOutput("lineplot")
           )
  )
  
)

server <- function(input, output, session) {
  
  observeEvent(input$Home, {
    home_team <- input$Home
    batters <- unique(as.character(last_p[last_p$hitting_team == home_team, "matchup.batter.fullName"]))
    if ("All" %in% batters) {
      batters <- setdiff(batters, "All")
    }
    updateSelectInput(session, "Batter", choices = c("All", batters))
  })
  
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    filtered_data <- last_p
    if (input$Home != "All") {
      filtered_data <- filtered_data[filtered_data$hitting_team == input$Home,]
    }
    if (input$Batter != "All") {
      filtered_data <- filtered_data[filtered_data$matchup.batter.fullName == input$Batter,]
    }
    if (input$Pitch != "All") {
      filtered_data <- filtered_data[filtered_data$details.type.description == input$Pitch,]
    }
    
    
    
    
    
    
    summary_table <- data.table(
      Stat = c("BA", "BA vs. RHP", "BA vs. LHP", "BA w RISP", "OBP"),
      MLB_avg = c(
        round(sum(last_p$hit == 1)/sum(last_p$hit <= 1),3), # BA
        round(sum(last_p[last_p$matchup.pitchHand.code == "R",]$hit == 1)/sum(last_p[last_p$matchup.pitchHand.code == "R",]$hit <= 1),3), # BA vs. RHP
        round(sum(last_p[last_p$matchup.pitchHand.code == "L",]$hit == 1)/sum(last_p[last_p$matchup.pitchHand.code == "L",]$hit <= 1),3), # BA vs. LHP
        round(sum(last_p[last_p$matchup.splits.menOnBase == "RISP" | last_p$matchup.splits.menOnBase == "Loaded",]$hit == 1)/sum(last_p[last_p$matchup.splits.menOnBase == "RISP" | last_p$matchup.splits.menOnBase == "Loaded",]$hit <= 1),3), # BA w RISP
        round(sum(last_p$hit != 0) / length(last_p$hit),3) # OBP
      ),
      Avg. = c(
        round(sum(filtered_data$hit == 1)/sum(filtered_data$hit <= 1),3), # BA
        round(sum(filtered_data[filtered_data$matchup.pitchHand.code == "R",]$hit == 1)/sum(filtered_data[filtered_data$matchup.pitchHand.code == "R",]$hit <= 1),3), # BA vs. RHP
        round(sum(filtered_data[filtered_data$matchup.pitchHand.code == "L",]$hit == 1)/sum(filtered_data[filtered_data$matchup.pitchHand.code == "L",]$hit <= 1),3), # BA vs. LHP
        round(sum(filtered_data[filtered_data$matchup.splits.menOnBase == "RISP" | filtered_data$matchup.splits.menOnBase == "Loaded",]$hit == 1)/sum(filtered_data[filtered_data$matchup.splits.menOnBase == "RISP" | filtered_data$matchup.splits.menOnBase == "Loaded",]$hit <= 1),3), # BA w RISP
        round(sum(filtered_data$hit != 0) / length(filtered_data$hit),3) # OBP
      ),
      At_Bats = c(
        sum(filtered_data$hit <= 1),
        sum(filtered_data[filtered_data$matchup.pitchHand.code == "R",]$hit <= 1),
        sum(filtered_data[filtered_data$matchup.pitchHand.code == "L",]$hit <= 1),
        sum(filtered_data[filtered_data$matchup.splits.menOnBase == "RISP" | filtered_data$matchup.splits.menOnBase == "Loaded",]$hit <= 1),
        length(filtered_data$hit)
      )
    )
    summary_table
    
  }))
  
  output$plott <- renderPlot({
    filtered_data2 <- pbp_data
    if (input$Home != "All") {
      filtered_data2 <- filtered_data2[filtered_data2$hitting_team == input$Home,]
    }
    if (input$Batter != "All") {
      filtered_data2 <- filtered_data2[filtered_data2$matchup.batter.fullName == input$Batter,]
    }
    
    ggplot(filtered_data2, aes(details.type.description)) + geom_bar(aes(fill = matchup.pitchHand.code)) +
      xlab("Pitch") + ylab("Number of Pitches") + labs(fill="Pitching Hand")

    })
  
  output$lineplot <- renderPlot({
    
    averages <- last_p[last_p$hit <= 1,] %>%
      group_by(monthh, matchup.batter.fullName, hitting_team) %>%
      summarize(total_hits=sum(hit), abs = n())
    
    leag_avgs <- averages %>%
      group_by(monthh) %>%
      summarize(total_hits=sum(total_hits), abs = sum(abs))
    
    leag_avgs$batting_avg <- leag_avgs$total_hits/leag_avgs$abs
    
    if (input$Batter != "All") {
      averages <- averages[averages$matchup.batter.fullName == input$Batter,]
      averages$batting_avg <- averages$total_hits/averages$abs
      
      ggplot(averages) + geom_line(aes(x = monthh, y=batting_avg, colour = "Selected Avg"), size=1) +
        geom_line(data = leag_avgs, aes(x = monthh, y=batting_avg, colour = "League Avg"), size=1) +
        labs(x = "Date", y = "Batting avg") + scale_color_manual(name = "Legend", values = c("Selected Avg" = "red", "League Avg" = "darkblue")) +
        theme_minimal() +
        scale_x_continuous(breaks = unique(averages$monthh))
      
    }else if (input$Home != "All") {
      averages <- averages[averages$hitting_team == input$Home,]
      
      averages <- averages %>%
        group_by(monthh, hitting_team) %>%
        summarize(total_hits=sum(total_hits), abs = sum(abs))
      
      averages$batting_avg <- averages$total_hits/averages$abs
      
      ggplot(averages) + geom_line(aes(x = monthh, y=batting_avg, colour = "Selected Avg"), size=1) +
        geom_line(data = leag_avgs, aes(x = monthh, y=batting_avg, colour = "League Avg"), size=1) +
        labs(x = "Date", y = "Batting avg") + scale_color_manual(values = c("Selected Avg" = "red", "League Avg" = "darkblue")) +
        theme_minimal() +
        scale_x_continuous(breaks = unique(averages$monthh))
      
    } else {
      
      ggplot(leag_avgs) + geom_line(aes(x = monthh, y=batting_avg), color="blue") + 
        labs(x = "Date", y = "Batting avg") + theme_minimal() +
        scale_x_continuous(breaks = unique(averages$monthh))
    }
    
  })
      }

shinyApp(ui = ui, server = server)




## intermediate - pbp

# first pitch data, after how many at bats are pitchers less effective
# create heatmaps based on situational data. What pitches do they normally see, what pitches do they swing the most
# what pitches they hit/hit hard the most, what pitches produce most swings in misses

## advanced

# combinations of pitches that are most effective (2/3)
# adjustment of speed (is fastest always best, or a difference of speed)
# maybe build an algorithm that shows optimal pitch to be thrown given inputs


last_p[matchup.pitchHand.code == "R",]

View(last_p)

runExample("01_hello")



averages <- last_p %>%
  group_by(monthh) %>%
  summarize(total_hits=sum(hit), abs = n())

averages$batting_avg <- averages$total_hits/averages$abs


