# PBP

This project pulls data from baseballR, more specifically the pitch by pitch data and is used to make an interactive app using R's shiny.

The pbpdatapull file initialize the original pull and saves it as a csv. The data (I think) can only be pulled one game id (game_pk) at a time, 
so I had to pull all of the game pks from the 2022 season, then use that to pull all of the pbp data from each game and add them together. (there were maybe 4 games which did not have all columns so they were excluded)
The result was a data set with over 700,000 rows and 100+ columns which I shrunk down to what I needed.
Note: The pulling of the data can take hours, especially when pulling a whole season worth


The shiny_pbp is the main app where it uses the pbp_data and filters it to draw simple analytics for team/batter data. 
There are six made custom input filters that based on the inputs adjust a table and two graphs.

In the strike_predictor app I built a simple knn model using x and y coordinates to predict what is actually a strike. 
We can then define a strike zone and do further hitter analysis from this.


The integrity of this data can still be questioned, but based on some of my QC it appears to be generally correct and mostly complete.
