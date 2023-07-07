## Machine learning algorithm to determine the strike zone
library(caret)
library(tidyverse)
library(dslabs)
library(dpylr)

pbp_data <- read.csv("C:\\Users\\bfass\\OneDrive\\Desktop\\CS\\Misc\\Baseball Data Bowl\\Pitchproj\\pbp2022.csv")


strzone <- pbp_data[pbp_data$details.call.description %in% c("Called Strike", "Ball", "Ball In Dirt"),]

strzone <- strzone[, c("details.call.description", "pitchData.coordinates.x", "pitchData.coordinates.y")]

strzone$details.call.description <- sub("Ball In Dirt", "Ball", strzone$details.call.description)

strzone <- strzone[complete.cases(strzone),]

strzone$details.call.description <- as.factor(strzone$details.call.description)

nrow(strzone)

# CREATING TRAINING AND TEST SETS

test_index <- createDataPartition(strzone$details.call.description, times = 1, p = 0.1, list = FALSE)

sz_train <- strzone[-test_index,]
sz_test <- strzone[test_index,]


SZ_Fit <- train(details.call.description ~ pitchData.coordinates.x + pitchData.coordinates.y,
                method = "knn",
                data = sz_train,
                tuneGrid = data.frame(k = 25))


#plot(SZ_Fit)

# Testing KNN model on the test data

prediction <- predict(SZ_Fit, sz_test)

mean(sz_test$details.call.description == prediction) ## 92.7% on test (which is in line with average an umpire actually gets right)


## checking accuracy on train data
train_prediction <- predict(SZ_Fit, sz_train)

mean(sz_train$details.call.description == train_prediction) # 93.1% accurate  


# Creating a column that tells whether the pitch is actually a strike
# Training on whole data set would take all night
pbp_data_filt <- na.omit(pbp_data[,c("X","details.call.description", "pitchData.coordinates.x", "pitchData.coordinates.y")])

pbp_data_filt$is_strike <- as.character(predict(SZ_Fit, pbp_data[, c("details.call.description", "pitchData.coordinates.x", "pitchData.coordinates.y")]))

typeof(pbp_data$hitData.hardness)


pbp_data <- merge(pbp_data, pbp_data_filt[, c("X","is_strike")],by="X", all.x = TRUE)

write.csv(pbp_data, "C:\\Users\\bfass\\OneDrive\\Desktop\\CS\\Misc\\Baseball Data Bowl\\Pitchproj\\pbp2022upd.csv")

