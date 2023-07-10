## Deliverable using Shiny


### PLAN

#Initial - Want to build simple tables that can show basic stats of hitters
# Batting avg, batting avg vs lhp vs rhp, obp, w/ risp, Over season and last x amount of games
# be able to filter by hitter(s) or pitcher

# What pitches a batter swings and misses at, swings out of the zone for, hits the best etc.
# Turn batting average graph into lagging average. instead of grouped by 15 game groups
# MAke pitcher filter to see what pitches/ averages are based on pitcher/team. Cant include linegraph

###
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(data.table)
library(lubridate)
library(forcats)
library(zoo)

pbp_data <- read.csv("C:\\Users\\bfass\\OneDrive\\Desktop\\CS\\Misc\\Baseball Data Bowl\\Pitchproj\\pbp2022upd.csv")


# Filtering out all star game
pbp_data$hitting_team <- ifelse(pbp_data$about.isTopInning, pbp_data$away_team, pbp_data$home_team)
pbp_data$pitching_team <- ifelse(pbp_data$about.isTopInning, pbp_data$home_team, pbp_data$away_team)

pbp_data <- pbp_data[pbp_data$hitting_team != "American League All-Stars" | pbp_data$hitting_team != "National League All-Stars",]

#extracting days since today
pbp_data$game_date <- as.Date(pbp_data$game_date) - 1

pbp_data$monthh <- as.integer(format(pbp_data$game_date, "%m"))

opening_day <- min(pbp_data$game_date)
closing_day <- max(pbp_data$game_date)

pbp_data$week2 <- floor((pbp_data$game_date - opening_day)/14)

# Changing 4 seam fastballs to just fastballs

pbp_data$details.type.description[pbp_data$details.type.description == "Four-Seam Fastball"] <- "Fastball"
  
  
# setting walks as 2, hits as 1 and outs as 0
pbp_data$hit <- ifelse(pbp_data$result.eventType %in% c("single", "double", "triple", "home_run"), 1,
                              ifelse(pbp_data$result.eventType %in% c("walk","wild_pitch"), 2, 0))


pbp_data$bases <- ifelse(pbp_data$result.eventType == "single", 1,
                         ifelse(pbp_data$result.eventType == "double", 2,
                                ifelse(pbp_data$result.eventType == "triple", 3,
                                       ifelse(pbp_data$result.eventType == "home_run",4,0))))

# dataframe where only last pitch of each at bat is captured (to calculate batting averages)

# Need to create a new column for last pitch of at bat because last.pitch.of.ab has a lot of null values
pbp_data <- pbp_data %>%
  group_by(game_pk, atBatIndex) %>%
  mutate(max_pitch = pitchNumber == max(pitchNumber))


last_p <- filter(pbp_data, max_pitch == TRUE)



stats_of_int <- c("BA", "BA vs RHP", "BA vs LHP", "BA w/ risp", "OBP")


ui <- fluidPage(
  titlePanel("Hitter Analysis Chart"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(3,
           selectInput("Home",
                       "Hitting Team:",
                       c("All",
                         sort(unique(as.character(last_p$hitting_team)))))
    ),
    column(3,
           selectInput("Batter",
                       "Batter:",
                       c("All",
                         sort(unique(as.character(last_p$matchup.batter.fullName)))))
    ),
    column(3,
           selectInput("Pitch",
                       "Pitch (last of at bat):",
                       c("All",
                         sort(unique(as.character(last_p$details.type.description)))))
    ),
    column(3,
           dateRangeInput("daterange",
                          label = "Date range of interest:",
                          start = min(last_p$game_date), max(last_p$game_date)
           )
  )),
  titlePanel("Hitting Vs."),
  
  fluidRow(
    column(3,
           selectInput("VsTeam",
                       "Pitching Team:",
                       c("All",
                         sort(unique(as.character(last_p$pitching_team)))))
    ),
    column(3,
           selectInput("VsPitch",
                       "Opposing Pitcher(need to recollect data):",
                       c("All",
                         sort(unique(as.character(last_p$matchup.batter.fullName)))))
    ),
  ),
  # Create a new row for the table
  DT::dataTableOutput("table"),
  # New row with both plots on same row
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
  
  #This changes the batters input based on the team selection to narrow down batter list
  observeEvent(input$Home, {
    home_team <- input$Home
    last_p <- last_p[last_p$game_date >= input$daterange[1] & last_p$game_date <= input$daterange[2],]
    
    batters <- unique(last_p[last_p$hitting_team == home_team, "matchup.batter.fullName"])
    if ("All" %in% batters) {
      batters <- setdiff(batters, "All")
    }
    updateSelectInput(session, "Batter", choices = c("All", batters))
  })
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    filtered_data <- last_p[last_p$game_date >= input$daterange[1] & last_p$game_date <= input$daterange[2],] #basic team hitting stats
    last_p2 <- last_p[last_p$game_date >= input$daterange[1] & last_p$game_date <= input$daterange[2],]
    if (input$Home != "All") {
      filtered_data <- filtered_data[filtered_data$hitting_team == input$Home,]
    }
    if (input$Batter != "All") {
      filtered_data <- filtered_data[filtered_data$matchup.batter.fullName == input$Batter,]
    }
    if (input$Pitch != "All") {
      filtered_data <- filtered_data[filtered_data$details.type.description == input$Pitch,]
    }
    
    filtered_data_vs <- filtered_data # Creating a copy for hitting vs opponents
    
    if (input$VsTeam != "All") {
      filtered_data_vs <- filtered_data[filtered_data$pitching_team == input$VsTeam,]
    }
    if (input$VsPitch != "All") {
      filtered_data_vs <- filtered_data_vs #[filtered_data$details.type.description == input$Pitch,] Data needs to be collected
    }
    
    
    # Table for pitches seen overall
    pseen <- last_p %>%
      group_by(game_pk, matchup.batter.fullName, atBatIndex) %>%
      summarize(Pitches_seen=max(pitchNumber))
    
    # Dynamic table for pitches seen
    pseen_filt <- filtered_data %>%
      group_by(game_pk, matchup.batter.fullName, atBatIndex) %>%
      summarize(Pitches_seen=max(pitchNumber), abs = n())
    
    #summary table based on filtered data from the inputs
    summary_table <- data.table(
      Stat = c("BA", "BA vs. RHP", "BA vs. LHP", "BA w RISP", "SLUG", "OBP", "Pitches Seen per ab"),
      MLB_avg = c(
        round(sum(last_p2$hit == 1)/sum(last_p2$hit <= 1),3), # BA
        round(sum(last_p2[last_p2$matchup.pitchHand.code == "R",]$hit == 1)/sum(last_p2[last_p$matchup.pitchHand.code == "R",]$hit <= 1),3), # BA vs. RHP
        round(sum(last_p2[last_p2$matchup.pitchHand.code == "L",]$hit == 1)/sum(last_p2[last_p$matchup.pitchHand.code == "L",]$hit <= 1),3), # BA vs. LHP
        round(sum(last_p2[last_p2$matchup.splits.menOnBase == "RISP" | last_p2$matchup.splits.menOnBase == "Loaded",]$hit == 1)/sum(last_p2[last_p$matchup.splits.menOnBase == "RISP" | last_p2$matchup.splits.menOnBase == "Loaded",]$hit <= 1),3), # BA w RISP
        round(sum(last_p$bases)/sum(last_p2$hit <= 1),3),#SLUG
        round(sum(last_p2$hit != 0) / length(last_p2$hit),3),# OBP
        round(mean(pseen$Pitches_seen),3) #pitches seen
      ),
      Avg. = c(
        round(sum(filtered_data$hit == 1)/sum(filtered_data$hit <= 1),3), # BA
        round(sum(filtered_data[filtered_data$matchup.pitchHand.code == "R",]$hit == 1)/sum(filtered_data[filtered_data$matchup.pitchHand.code == "R",]$hit <= 1),3), # BA vs. RHP
        round(sum(filtered_data[filtered_data$matchup.pitchHand.code == "L",]$hit == 1)/sum(filtered_data[filtered_data$matchup.pitchHand.code == "L",]$hit <= 1),3), # BA vs. LHP
        round(sum(filtered_data[filtered_data$matchup.splits.menOnBase == "RISP" | filtered_data$matchup.splits.menOnBase == "Loaded",]$hit == 1)/sum(filtered_data[filtered_data$matchup.splits.menOnBase == "RISP" | filtered_data$matchup.splits.menOnBase == "Loaded",]$hit <= 1),3), # BA w RISP
        round(sum(filtered_data$bases)/sum(filtered_data$hit <= 1),3),
        round(sum(filtered_data$hit != 0) / length(filtered_data$hit),3), # OBP   
        round(mean(pseen_filt$Pitches_seen),3)
      ),
      Avg_vs_Selected = c(
        round(sum(filtered_data_vs$hit == 1)/sum(filtered_data_vs$hit <= 1),3), # BA
        round(sum(filtered_data_vs[filtered_data_vs$matchup.pitchHand.code == "R",]$hit == 1)/sum(filtered_data_vs[filtered_data_vs$matchup.pitchHand.code == "R",]$hit <= 1),3), # BA vs. RHP
        round(sum(filtered_data_vs[filtered_data_vs$matchup.pitchHand.code == "L",]$hit == 1)/sum(filtered_data_vs[filtered_data_vs$matchup.pitchHand.code == "L",]$hit <= 1),3), # BA vs. LHP
        round(sum(filtered_data_vs[filtered_data_vs$matchup.splits.menOnBase == "RISP" | filtered_data_vs$matchup.splits.menOnBase == "Loaded",]$hit == 1)/sum(filtered_data_vs[filtered_data_vs$matchup.splits.menOnBase == "RISP" | filtered_data_vs$matchup.splits.menOnBase == "Loaded",]$hit <= 1),3), # BA w RISP
        round(sum(filtered_data_vs$bases)/sum(filtered_data_vs$hit <= 1),3),
        round(sum(filtered_data_vs$hit != 0) / length(filtered_data_vs$hit),3), # OBP   
        round(mean(pseen_filt$Pitches_seen),3)
      ),
      At_Bats = c(
        sum(filtered_data$hit <= 1),
        sum(filtered_data[filtered_data$matchup.pitchHand.code == "R",]$hit <= 1),
        sum(filtered_data[filtered_data$matchup.pitchHand.code == "L",]$hit <= 1),
        sum(filtered_data[filtered_data$matchup.splits.menOnBase == "RISP" | filtered_data$matchup.splits.menOnBase == "Loaded",]$hit <= 1),
        sum(filtered_data$hit <= 1),
        length(filtered_data$hit),
        length(filtered_data$hit)
      ),
      At_Bats_vs_Selected = c(
        sum(filtered_data_vs$hit <= 1),
        sum(filtered_data_vs[filtered_data_vs$matchup.pitchHand.code == "R",]$hit <= 1),
        sum(filtered_data_vs[filtered_data_vs$matchup.pitchHand.code == "L",]$hit <= 1),
        sum(filtered_data_vs[filtered_data_vs$matchup.splits.menOnBase == "RISP" | filtered_data_vs$matchup.splits.menOnBase == "Loaded",]$hit <= 1),
        sum(filtered_data_vs$hit <= 1),
        length(filtered_data_vs$hit),
        length(filtered_data_vs$hit)
      )
    )
    summary_table
    
  }))
  
  #Plot is a frequency plot of all the pitches a batter faces 
  # note that we are not using last_p data frame
  output$plott <- renderPlot({
    filtered_data2 <- pbp_data[pbp_data$game_date >= input$daterange[1] & pbp_data$game_date <= input$daterange[2],]
    if (input$Home != "All") {
      filtered_data2 <- filtered_data2[filtered_data2$hitting_team == input$Home,]
    }
    if (input$Batter != "All") {
      filtered_data2 <- filtered_data2[filtered_data2$matchup.batter.fullName == input$Batter,]
    }
    if (input$VsTeam != "All") {
      filtered_data2 <- filtered_data2[filtered_data2$pitching_team == input$VsTeam,]
    }
    

    ggplot(filtered_data2, aes(fct_infreq(as.factor(details.type.description)), fill = matchup.pitchHand.code, weight = ..count..)) +
      geom_bar() +
      xlab("Pitches Seen") + ylab("Number of Pitches") + labs(fill = "Pitching Hand")
    })
  
  # plots batting average over course of the season. is grouped by every 14 days
  output$lineplot <- renderPlot({
    averages <- last_p[last_p$game_date >= input$daterange[1] & last_p$game_date <= input$daterange[2] & last_p$hit <= 1,] %>%
      group_by(week2, matchup.batter.fullName, hitting_team) %>%
      summarize(total_hits=sum(hit), abs = n())
    
    leag_avgs <- averages %>%
      group_by(week2) %>%
      summarize(total_hits=sum(total_hits), abs = sum(abs))
    
    leag_avgs$batting_avg <- leag_avgs$total_hits/leag_avgs$abs
    
    if (input$Batter != "All") {
      averages <- averages[averages$matchup.batter.fullName == input$Batter,]
      averages$batting_avg <- averages$total_hits/averages$abs
      
      ggplot(averages) + geom_line(aes(x = week2, y=batting_avg, colour = "Selected Avg"), size=1) +
        geom_line(data = leag_avgs, aes(x = week2, y=batting_avg, colour = "League Avg"), size=1) +
        labs(x = "Date", y = "Batting avg") + scale_color_manual(name = "Legend", values = c("Selected Avg" = "red", "League Avg" = "darkblue")) +
        theme_minimal() +
        scale_x_continuous(breaks = unique(averages$week2))
      
    }else if (input$Home != "All") {
      averages <- averages[averages$hitting_team == input$Home,]
      
      averages <- averages %>%
        group_by(week2, hitting_team) %>%
        summarize(total_hits=sum(total_hits), abs = sum(abs))
      
      averages$batting_avg <- averages$total_hits/averages$abs
      
      ggplot(averages) + geom_line(aes(x = week2, y=batting_avg, colour = "Selected Avg"), size=1) +
        geom_line(data = leag_avgs, aes(x = week2, y=batting_avg, colour = "League Avg"), size=1) +
        labs(x = "Date", y = "Batting avg") + scale_color_manual(values = c("Selected Avg" = "red", "League Avg" = "darkblue")) +
        theme_minimal() +
        scale_x_continuous(breaks = unique(averages$week2))
      
    } else {
      
      ggplot(leag_avgs) + geom_line(aes(x = week2, y=batting_avg), color="blue") + 
        labs(x = "Date", y = "Batting avg") + theme_minimal() +
        scale_x_continuous(breaks = unique(averages$week2))
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


swing_type <- pbp_data[pbp_data$details.call.description == "Swinging Strike"]



hits_best <- pbp_data %>%
  group_by(matchup.batter.fullName, matchup.pitchHand.code, details.type.description) %>%
  summarize()


averages <- last_p %>%
  group_by(game_pk, matchup.batter.fullName, atBatIndex) %>%
  summarize(Pitches_seen=max(pitchNumber), abs = n())

averages$batting_avg <- averages$total_hits/averages$abs

team_averages <- last_p %>%
  group_by(game_pk, hitting_team) %>%
  summarize(average_bygm = mean(hit), na.rm = TRUE)

player_averages <- last_p %>%
  group_by(game_pk, matchup.batter.fullName) %>%
  summarize(average_bygm = mean(hit), na.rm = TRUE)

player_averages <- last_p[last_p$hit <= 1,] %>%
  group_by(matchup.batter.fullName) %>%
  rollmean(hit, 40, fill="roll")

team_averages <- last_p[last_p$hit <= 1,] %>%
  group_by(hitting_team) %>%
  mutate(ba_rollmean = rollmean(hit, 400, fill = NA, align = "right", na.rm = TRUE))
