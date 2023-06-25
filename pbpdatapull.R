## Importing Pitch By Pitch Data
library(baseballr)
library(ggplot2)
library(dplyr)

## Initial Data pull


## finding first and last gamepks of 2022 season

opening_day <- as.Date("2022-10-28")
closing_day <- as.Date("2022-11-05")

game_pk_list_2022 <- list()
date_range <- seq.Date(opening_day, closing_day, by="day")


for (d in 1:length(date_range)){
  result <- try({
    df <- mlb_game_pks(date_range[d])
    df <- filter(df, status.detailedState == "Final")
    game_pk_list_2022 <- append(game_pk_list_2022 , df$game_pk)
  }, silent=TRUE
  )
  if (inherits(result, "try-error")) {
    next  
  }
}

# Using Game picks to get pbp data

initial_df <- mlb_pbp(game_pk = game_pk_list_2022[1])

needed_cols <- c("game_pk", "game_date", "pitchNumber","result.rbi", "result.awayScore", "result.homeScore", "details.call.description", "count.strikes.x", "count.balls.x", "count.outs.x","last.pitch.of.ab", "matchup.splits.menOnBase", "pitchData.coordinates.x", "pitchData.coordinates.y", "hitData.hardness", "about.inning", "matchup.batSide.code", "matchup.pitchHand.code", "matchup.batter.id", "matchup.pitcher.id", "home_team", "away_team", "details.type.description", "pitchData.startSpeed", "pitchData.zone","pitchData.breaks.breakLength", "pitchData.breaks.spinRate", "details.isBall","matchup.batter.fullName", "about.isTopInning")
initial_df <- subset(initial_df, type=="pitch")[,needed_cols]

miss_count = 0


for (x in 2:length(game_pk_list_2022)){
  test_df <- mlb_pbp(game_pk = game_pk_list_2022[x])
  test_df_filt <- subset(test_df, type=="pitch")
  
  if (all(needed_cols %in% colnames(mlb_pbp(game_pk = game_pk_list_2022[x])))){
    finaldf <- test_df_filt[,needed_cols]
    initial_df <- bind_rows(initial_df, finaldf)
    # adding dataframes if all columns exist in game x
  }else{
    #tracking how many are missed
    miss_count = miss_count + 1
  }
  
}

copy1 <- initial_df

copy1$game_pk <- as.integer(unlist(copy1$game_pk))

#Creating local copy of data
write.csv(copy1, "C:\\Users\\bfass\\OneDrive\\Desktop\\CS\\Misc\\Baseball Data Bowl\\Pitchproj\\pbp2022.csv")






## Strikezone Graph

ggplot(inital_df, aes(pitchData.coordinates.x, pitchData.coordinates.y, color=factor(details.isBall))) + geom_point() +
    labs(title = "Strikezone", x="X", y="Y") + scale_color_manual(values = c("red", "green"))



