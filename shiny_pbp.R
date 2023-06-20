## Deliverable using Shiny


### PLAN

#Initial - Want to build simple tables that can show basic stats of hitters
# Batting avg, batting avg vs lhp vs rhp, obp, w/ risp, Over season and last x amount of games
# be able to filter by hitter(s) or pitcher

pbp_data <- read.csv("C:\\Users\\bfass\\OneDrive\\Desktop\\CS\\Misc\\Baseball Data Bowl\\Pitchproj\\pbp2022.csv")

library(shiny)

ui <- fluidPage(
  
  # App Title
  titlePanel("Exploratory 2022 Batting Stats"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId = "Team",
                  label = "Filter by: ",
                  choices = c("Player", "Team", "League"),
                  selected = "League"),
      
      
      selectInput(inputId = "Stats",
                  label = "Filter by: ",
                  choices = c("BA", "BA vs RHP", "BA vs LHP", "BA w/ risp", "OBP"),
                  selected = "BA"),
      

      selectInput(inputId = "Pitch",
                label = "Filter by: ",
                choices = c("ALL", "Fastball", "offspeed", "Changeup", "Slider", "Cutter", "Sinker", "Sweeper", "Splitter"),
                selected = "ALL")
    
      ),
    
    mainPanel(
      
      plotOutput(outputId = "BA TABLE"),
      textOutput("Number of observations")
      
       )
    )
)


server <- function(input, output) {
  output$textOutput <- renderText({
    variable <- "World"
    text <- paste("Hello", variable)
    text
  })
}

View(filter(pbp_data, last.pitch.of.ab == "true"))


pbp_data[,c()]




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
