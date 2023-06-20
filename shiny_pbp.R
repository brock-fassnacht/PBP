## Deliverable using Shiny


### PLAN

#Initial - Want to build simple tables that can show basic stats of hitters
# Batting avg, batting avg vs lhp vs rhp, obp, w/ risp, Over season and last x amount of games
# be able to filter by hitter(s) or pitcher

library(shiny)
library(data.table)


pbp_data <- read.csv("C:\\Users\\BrocFassnacht\\OneDrive - Cobbs Creek Healthcare\\Desktop\\Training\\R Training\\GitR\\pbp2022.csv")


last_p <- filter(pbp_data, last.pitch.of.ab == "true")

last_p$hit <- ifelse(last_p$details.call.description == "Ball" | last_p$details.call.description == "Ball In Dirt" | last_p$details.call.description == "Hit By Pitch", 2, 
                     ifelse(last_p$details.call.description=="In play, no out" | last_p$details.call.description == "In play, run(s)", 1,0))

stats_of_int <- c("BA", "BA vs RHP", "BA vs LHP", "BA w/ risp", "OBP")


ui <- fluidPage(
  titlePanel("Exploratory 2022 Batting Stats"),
  sidebarLayout(
    sidebarPanel(
      selectInput("filter_col", "Column to Filter:", choices = c("home_team", "matchup.batter.id", "matchup.pitcher.id")),
      textInput("filter_val", "Filter Value:"),
      actionButton("apply_filter", "Apply Filter"),
      
      #selectInput(inputId = "Pitch",
       #           label = "Filter by: ",
        #          choices = c("ALL", "Fastball", "offspeed", "Changeup", "Slider", "Cutter", "Sinker", "Sweeper", "Splitter"),
         #         selected = "ALL")
    ),
    mainPanel(
      DT::dataTableOutput("summary_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Store the raw data table as a reactive data frame
  pbp <- reactive({
    # Replace the filtering logic with your actual filter based on user inputs
    last_p_filt <- last_p %>% 
      filter(last_p[[input$filter_col]] == input$filter_val)
    
    return(last_p_filt)
  })
  
  # Generate the summary table from the filtered raw data
  dt <- reactive({
    filtered_data <- pbp()
    
    # Replace with your code to calculate the summary table
    summary_table <- data.table(
      Stat = c("BA", "BA vs. RHP", "BA vs. LHP", "BA w RISP", "OBP"),
      output = c(
        mean(filtered_data$hit != 2), # BA
        mean(filtered_data$hit != 2 & filtered_data$matchup.pitchHand.code == "R"), # BA vs. RHP
        mean(filtered_data$hit != 2 & filtered_data$matchup.pitchHand.code == "L"), # BA vs. LHP
        mean(filtered_data$hit != 2 & filtered_data$matchup.splits.menOnBase == "RISP"), # BA w RISP
        sum(filtered_data$hit != 0) / length(filtered_data$hit) # OBP
      ),
      Obvs = c(
        length(filtered_data$hit != 2),
        length(filtered_data$hit != 2 & filtered_data$matchup.pitchHand.code == "R"),
        length(filtered_data$hit != 2 & filtered_data$matchup.pitchHand.code == "L"),
        length(filtered_data$hit != 2 & filtered_data$matchup.splits.menOnBase == "RISP"),
        length(filtered_data$hit)
      )
    )
    
    return(summary_table)
  })
  
  # Render the summary table using DT::renderDataTable()
  output$summary_table <- DT::renderDataTable({
    dt()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
## intermediate - pbp

# first pitch data, after how many at bats are pitchers less effective
# create heatmaps based on situational data. What pitches do they normally see, what pitches do they swing the most
# what pitches they hit/hit hard the most, what pitches produce most swings in misses

## advanced

# combinations of pitches that are most effective (2/3)
# adjustment of speed (is fastest always best, or a difference of speed)
# maybe build an algorithm that shows optimal pitch to be thrown given inputs





library(shiny)

runExample("01_hello")
