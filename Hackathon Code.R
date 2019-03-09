setwd("C:\\Users\\DanielLee\\Documents\\R\\SwingProbability")

library(pitchRx)
library(dplyr)
library(RSQLite)
library(h2o)
h2o.init()
##############################
data2014
data2014 <- read.csv("2014.csv")
Cabrera2014 <- subset(data2014, data2014$batter == "Miguel Cabrera")

data2015 <- read.csv("2015.csv")
Cabrera2015 <- subset(data2015, data2015$batter == "Miguel Cabrera")

data2016 <- read.csv("2016.csv")
Cabrera2016 <- subset(data2016, data2016$batter == "Miguel Cabrera")

Cabrera <- rbind.data.frame(Cabrera2014,Cabrera2015,Cabrera2016)

###############################################################
###############################################################
#######     Convert all pitches to either Swing or Take
dim(Cabrera)
Cabrera <- Cabrera[-which(Cabrera$pitchResult == "FB" | Cabrera$pitchResult == "MB" | Cabrera$pitchResult == "HBP"
                                      | Cabrera$pitchResult == "IB" | Cabrera$pitchResult == "PO" |
                                        Cabrera$pitchResult == "AS" |Cabrera$pitchResult == "AB" |Cabrera$pitchResult == "UK" ),]
 
Cabrera$Decision <- 0
Cabrera$Decision[which(Cabrera$pitchResult == "SS"  | Cabrera$pitchResult == "F" | Cabrera$pitchResult == "FT" |
                 Cabrera$pitchResult == "IP" | Cabrera$pitchResult == "CI")] <- "Swing"

Cabrera$Decision[which(Cabrera$pitchResult == "SL"  | Cabrera$pitchResult == "B" | 
                      Cabrera$pitchResult == "BID")] <- "Take"


### combine Balls and Strikes into Count for interaction
for (i in 1:nrow(Cabrera)){
Cabrera$count[i] <- paste(Cabrera$balls[i],"-", Cabrera$strikes[i])
}

### Previous outcome
Cabrera$Decision_lag1 <- 0
for (ii in 2:(nrow(Cabrera))) {
  if (Cabrera$count[ii] != "0 - 0" ) {
    Cabrera$Decision_lag1[ii] <- Cabrera$Decision[ii-1]
  } else
    if (Cabrera$Decision_lag1[ii] == "0 - 0") {
      Cabrera$Decision_lag1[ii] <- 0
    }
}


#Create Score Differential for batter
Cabrera$score_diff = ifelse(Cabrera$side == 'B', Cabrera$homeTeamCurrentRuns - Cabrera$visitingTeamCurrentRuns,
                                  Cabrera$visitingTeamCurrentRuns - Cabrera$homeTeamCurrentRuns)

# Pitches Thrown this At-Bat
Cabrera$Pitch_In_AtBat <- 1
for (ii in 2:nrow(Cabrera)){
  if (Cabrera$count[ii] != "0 - 0") {
     Cabrera$Pitch_In_AtBat[ii] <- Cabrera$Pitch_In_AtBat[ii-1] + 1
  }
  else {
    Cabrera$Pitch_In_AtBat[ii] <- 1
  }
}

### Convert Miles Per Hour to Feet Per Second
Cabrera$releaseVelocity <- Cabrera$releaseVelocity*5280/60/60 


#####################
#### Decision MODEL
#####################
names(Cabrera)
Cabrera_train <- Cabrera[which(Cabrera$seasonYear == "2014" | Cabrera$seasonYear == "2015"),
                         c("releaseVelocity","batterHand", "pitcherHand", "x0","y0",	"z0",	"vx0",	"vy0",	"vz0",	"ax",	"ay",	"az",	"spinRate",	"spinDir",	"manOnFirst" ,	"manOnSecond",	"manOnThird",	"inning", "count", "Decision",	"Decision_lag1",	"score_diff",	"Pitch_In_AtBat")]

Cabrera_test  <- Cabrera[which(Cabrera$seasonYear == "2016") ,
                         c("releaseVelocity","batterHand", "pitcherHand", "x0","y0",	"z0",	"vx0",	"vy0",	"vz0",	"ax",	"ay",	"az",	"spinRate",	"spinDir",	"manOnFirst",	"manOnSecond",	"manOnThird",	"inning", "count", "Decision",	"Decision_lag1",	"score_diff",	"Pitch_In_AtBat")]

Cabrera_train <- Cabrera_train[complete.cases(Cabrera_train),]
Cabrera_test <- Cabrera_test[complete.cases(Cabrera_test),]

x <- which(Cabrera_train$Decision == "Swing")
x

dim(Cabrera_train)
names(Cabrera_train)
dim(Cabrera_test)

train_data_h2o_Decision <- as.h2o(data.frame(x =Cabrera_train[,-which(names(Cabrera_train) %in% c("Decision"))], y = Cabrera_train$Decision))
test_data_h2o_Decision <- as.h2o(data.frame(x =Cabrera_test[,-which(names(Cabrera_test) %in% c("Decision"))], y = Cabrera_test$Decision))

dim(train_data_h2o_Decision)
dim(test_data_h2o_Decision)


train_data_h2o_Decision$x.batterHand <- as.factor(train_data_h2o_Decision$x.batterHand)
train_data_h2o_Decision$x.pitcherHand <- as.factor(train_data_h2o_Decision$x.pitcherHand)
train_data_h2o_Decision$x.count <- as.factor(train_data_h2o_Decision$x.count)
train_data_h2o_Decision$x.Decision_lag1 <- as.factor(train_data_h2o_Decision$x.Decision_lag1)
train_data_h2o_Decision$x.manOnFirst <- as.factor(train_data_h2o_Decision$x.manOnFirst)
train_data_h2o_Decision$x.manOnSecond <- as.factor(train_data_h2o_Decision$x.manOnSecond)
train_data_h2o_Decision$x.manOnThird <- as.factor(train_data_h2o_Decision$x.manOnThird)
train_data_h2o_Decision[,ncol(train_data_h2o_Decision)] <- as.factor(train_data_h2o_Decision[,ncol(train_data_h2o_Decision)])

test_data_h2o_Decision$x.batterHand <- as.factor(test_data_h2o_Decision$x.batterHand)
test_data_h2o_Decision$x.pitcherHand <- as.factor(test_data_h2o_Decision$x.pitcherHand)
test_data_h2o_Decision$x.count <- as.factor(test_data_h2o_Decision$x.count)
test_data_h2o_Decision$x.Decision_lag1 <- as.factor(test_data_h2o_Decision$x.Decision_lag1)
test_data_h2o_Decision$x.manOnFirst <- as.factor(test_data_h2o_Decision$x.manOnFirst)
test_data_h2o_Decision$x.manOnSecond <- as.factor(test_data_h2o_Decision$x.manOnSecond)
test_data_h2o_Decision$x.manOnThird <- as.factor(test_data_h2o_Decision$x.manOnThird)
test_data_h2o_Decision[,ncol(test_data_h2o_Decision)] <- as.factor(test_data_h2o_Decision[,ncol(test_data_h2o_Decision)])



predictor_indices_Decision <- 1:(ncol(train_data_h2o_Decision)-1)
response_index_Decision <- ncol(train_data_h2o_Decision)

nn_model_Decision <- h2o.deeplearning(
  x=predictor_indices_Decision, y=response_index_Decision,
  training_frame=train_data_h2o_Decision,
  balance_classes=TRUE,
  activation="RectifierWithDropout",
  rate = .025,
  #input_dropout_ratio=.05,
  #hidden_dropout_ratios = c(0.6, 0.6, 0.6, 0.6),
  #classification_stop=-1,  # Turn off early stopping
  #l1=1e-5,                 # regularization
  hidden=c(250,250,250,250),
  epochs=69,
  model_id = "NeuralNet_MNIST_001",
  reproducible=TRUE,
  seed=99,
  export_weights_and_biases=TRUE,
  ignore_const_cols=FALSE,
  distribution = "bernoulli",
  variable_importances = TRUE,
  keep_cross_validation_predictions = TRUE)


predict <- h2o.predict(nn_model_Decision, test_data_h2o_Decision)

predict1 <- as.data.frame(predict)
performance = h2o.performance(nn_model_Decision, test_data_h2o_Decision)


write.csv(predict1, "C:\\Users\\mjuettn\\Documents\\R\\data.csv")
write.csv(cbind(Cabrera_test,predict1), "C:\\Users\\mjuettn\\Documents\\R\\Cabrera_test.csv")

performance
h2o.confusionMatrix(performance, threshold = .50)
help(h2o.performance)


Cabrera_train(which(Cabrera_train$Decision == "Swing")

plot("X", "Z", xlim=c(-3,3),ylim=c(0,6), type = "p",pch = 16,col= "blue")

bot = mean(Cabrera$szb[complete.cases(Cabrera$szb)])
top = mean(Cabrera$szt[complete.cases(Cabrera$szt)])
updist = (top - bot)/3
sidedist = 0.83917*2/3


lines(c(0.83917, 0.83917), c(bot, top), col="black", lty="dashed", lwd=2) 
lines(c(-0.83917, -0.83917),c(bot, top), col="black", lty="dashed", lwd=2)
lines(c(-0.83917, 0.83917), c(bot,bot), col="black", lty="dashed", lwd=2) 
lines(c(-0.83917, 0.83917), c(top,top), col="black", lty="dashed", lwd=2)
lines(c((0.83917-sidedist), (0.83917-sidedist)), c(bot, top), col="black", lty="dashed", lwd=2) 
lines(c((-0.83917+sidedist), (-0.83917+sidedist)),c(bot, top), col="black", lty="dashed", lwd=2)
lines(c(-0.83917, 0.83917), c((bot+updist),(bot+updist)), col="black", lty="dashed", lwd=2) 
lines(c(-0.83917, 0.83917), c((top-updist),(top-updist)), col="black", lty="dashed", lwd=2)

