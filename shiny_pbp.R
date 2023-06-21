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

pbp_data <- read.csv("C:\\Users\\bfass\\OneDrive\\Desktop\\CS\\Misc\\Baseball Data Bowl\\Pitchproj\\pbp2022.csv")

last_p <- filter(pbp_data, last.pitch.of.ab == "true")

last_p$hit <- ifelse(last_p$details.call.description == "Ball" | last_p$details.call.description == "Ball In Dirt" | last_p$details.call.description == "Hit By Pitch", 2, 
                     ifelse(last_p$details.call.description=="In play, no out" | last_p$details.call.description == "In play, run(s)", 1,0))

stats_of_int <- c("BA", "BA vs RHP", "BA vs LHP", "BA w/ risp", "OBP")


ui <- fluidPage(
  titlePanel("Hitting Stats Table"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("Pitch",
                       "Pitch:",
                       c("All",
                         unique(as.character(last_p$details.type.description))))
    ),
    column(4,
           selectInput("Home",
                       "Home_team:",
                       c("All",
                         unique(as.character(last_p$home_team))))
    )
  ),
  # Create a new row for the table.
  DT::dataTableOutput("table")
)

server <- function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    filtered_data <- last_p
    if (input$Pitch != "All") {
      filtered_data <- filtered_data[filtered_data$details.type.description == input$Pitch,]
    }
    if (input$Home != "All") {
      filtered_data <- filtered_data[filtered_data$home_team == input$Home,]
    }
    
    
    summary_table <- data.table(
      Stat = c("BA", "BA vs. RHP", "BA vs. LHP", "BA w RISP", "OBP"),
      output = c(
        sum(filtered_data$hit == 1)/sum(filtered_data$hit <= 1), # BA
        sum(filtered_data[filtered_data$matchup.pitchHand.code == "R",]$hit == 1)/sum(filtered_data[filtered_data$matchup.pitchHand.code == "R",]$hit <= 1), # BA vs. RHP
        sum(filtered_data[filtered_data$matchup.pitchHand.code == "L",]$hit == 1)/sum(filtered_data[filtered_data$matchup.pitchHand.code == "L",]$hit <= 1), # BA vs. LHP
        sum(filtered_data[filtered_data$matchup.splits.menOnBase == "RISP" | filtered_data$matchup.splits.menOnBase == "Loaded",]$hit == 1)/sum(filtered_data[filtered_data$matchup.splits.menOnBase == "RISP" | filtered_data$matchup.splits.menOnBase == "Loaded",]$hit <= 1), # BA w RISP
        sum(filtered_data$hit != 0) / length(filtered_data$hit) # OBP
      ),
      Obvs = c(
        sum(filtered_data$hit <= 1),
        sum(filtered_data[filtered_data$matchup.pitchHand.code == "R",]$hit <= 1),
        sum(filtered_data[filtered_data$matchup.pitchHand.code == "L",]$hit <= 1),
        sum(filtered_data[filtered_data$matchup.splits.menOnBase == "RISP",]$hit <= 1),
        length(filtered_data$hit)
      )
    )
    summary_table
  }))
  
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
