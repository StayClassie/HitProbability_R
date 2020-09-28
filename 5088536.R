# Read Pitch Data CSV

library(readr)
library(dplyr)
library(ggplot2)
library(mgcv)


# GAM for predicting hits from LA & EV
# logit(prob(H)) = s(Exit Speed, Angle)

# log p/1-p = s(Exit Speed, Angle)

# if the probability exceeds 0.5 then we predict a "hit"
# if the probability is below 0.5 then we predict an "out"

#y denotes the outcome (0 or 1) (hit or out)
#yhat denotes the prediction (0 or 1)
# hit = yhat = 1
# we can predict this ^ if the predicted probability of hit 
# exceeds some constant k
# we can predict out = yhat = 0
# if the probability of hit is smaller than k

# Error Rates

## P(yhat = 1 | y = 0) #predicting a hit when its actually an out
## P(yhat = 0 | y = 1) #predicting an out when its actually a hit

# P(out) P(correct | out) + P(hit) P(correct | hit) 
# = 0.669 x 0.764 + 0.331 x 0.791  = 0.773


# Why I chose to model hits based on exit velocity and launch angle
## Exit Velocity is how hard a hitter hits the ball
### The batter has the control so its a better measure of 
### performance for a hitter
#### Hard hit balls don't always result in a hit
#### but harder hit balls are more likely to be hits

## Launch Angle is the angle the ball "launches" off the hitters bat
## ranges from -50 to 50 degrees generally
## different launch angles predict different outcomes of the ball
## -50 degrees to 10 degrees = ground ball
## 10 to 25 degrees = line drive
## 25 - 50 degrees = fly ball
## 50+ = pop ups

## we can identify who the best hitters are based on what they control
## based on how they hit the ball

#With the convergence of these two stats we are able to define
#who the best hitters are without guessing 

##Just a test of first 2000 training samples correlating Launch Angle & Exit Velocity to hits
## 0 = Hit / 1 = Out in this instance

MyData <- read.csv(file="Pitch_Data_v3.csv", header=TRUE, sep=",")



##Data Cleaning
## I need to work on my data cleaning
#3 Not my strongest skill right now

## this shows us that our data's class is a data frame
class(MyData)


## this code gives us the number of rows and colums
dim(MyData)

## This just gives us the summary stats for
## all the columns of the data frame
summary(MyData)

## Checks for missing values in the entire dataframe
any(is.na(MyData))


##The data was actual in pretty clean condition


## only looking at balls in play here
## we are defining what a hit is
MyData %>%
  mutate(outcome = ifelse(inPlayOut %in%
                            c("h1b", "h2b", "h3b", "hr", "fieldingError"), 0, 1)) -> MyData

TH <- theme(plot.title = element_text(colour = "blue", 
                                      size = 18, 
                                      hjust = 0.5, vjust = 0.8, angle = 0))


## 1.00 reprsents the probailtiy it's an out
## 0.00 represents the predicted probabiltiy its a hit
ggplot(slice(MyData, 1:2000),
       aes(Angle, ExitSpeed, color=inPlayOut)) +
    geom_point() +
  xlim(-25, 50) +
  ylim(40, 120) +
TH + ggtitle("Sample Hit Probability for 2000 points")


## Creating the sample and test data
## The sample data consists of the first 60,000
## points we are going to model are data based off of
## the test data is the remaining 60,000 points
## we are going to test and train are data with 

InPlay_Sample <- slice(MyData, 1:60000)
InPlay_Test <- slice(MyData, 60001:115137)


## Here we are creating a generalized additve model
## to fit our model to 
## here the linear predictor depends linearly
## on the Launch Angle and Exit Speed predictor
## variables connected with a smooth functin
## and were are fitting the Sample data to the model

fit <- gam(inPlayOut ~ s(Angle, ExitSpeed), 
           family = binomial, 
           data = InPlay_Sample)

## This is the inverse logit function
## It maps from the linear predictors to
## the probabilites 
## the value is a vector of estimated probabilties
invlogit <- function(x) exp(x) / (1 + exp(x))


## What is the probability of a hit when the 
## Launch Angle is 20 degrees and the Exit Velocity is 100?

invlogit(predict(fit, data.frame(Angle = 20,
                                 ExitSpeed = 100)))

df_p <- expand.grid(Angle = seq(-20, 50, length = 50), 
                    ExitSpeed = seq(40, 120, length = 50))
df_p$lp <- predict(fit, df_p)
df_p$Probability <- exp(df_p$lp) / (1 + exp(df_p$lp))



## Here is a contour map of the predicted hit prob
## The Red represents more likely to be a hit
## The yellow represents a predicted out
## I chose the contour map because it's a lot cleaner
## to visualize
ggplot(df_p, aes(x = Angle, y = ExitSpeed, z = Probability)) +
  stat_contour(geom="polygon",
               breaks= seq(1, 0, length.out = 21), 
               size = 1.5, 
               aes(fill=..level..)) +
  scale_fill_gradientn(colors = heat.colors(10)) +
  geom_vline(xintercept = 0, color = "black") +
  xlim(-25, 50) +
  ylim(40, 120) +
  TH + ggtitle("Predicted Hit Probability Contour Map")


## Here we are testing the rate of of predicted probabiltiy
## for our model 
## summarizing will show about a 77.7% chance
## for our predicted model to be correct on predicting hits
## which is slightly higher than the 77.3% we came up with at the
## beginning showing that our model is very accurate and in line
      
InPlay_Test$lp <- predict(fit, InPlay_Test)
InPlay_Test$predict <- ifelse(InPlay_Test$lp > 0,1,0)
InPlay_Test %>%
  summarize(Rate = mean(inPlayOut == predict, na.rm = TRUE))


## Balls hit under 100 mph will be outs 30% of the time
## Balls hit over 100 mph will be hits 70% of the time
InPlay_Test %>% summarize(mean(inPlayOut, na.rm = TRUE))
InPlay_Test$rule1 <- ifelse(InPlay_Test$ExitSpeed < 100, 1, 0)
InPlay_Test %>%
  summarize(Rate1 = mean(inPlayOut == rule1, na.rm = TRUE))



### Comparing the Predicted model to the actual model for hits
### Obviously only looking at balls in play

### Fix this, there has to be an error bc there is no way anything near 
### 300 hits should be on there 

library(tidyverse)
TH <- theme(plot.title = element_text(colour = "blue", 
                                      size = 18, 
                                      hjust = 0.5, vjust = 0.8, angle = 0))

## Same fit as before 
fit <- gam(inPlayOut ~ s(ExitSpeed, Angle),
           data = MyData, family = binomial)

## creating a function to visualize the two compared models

## function that computes the number of batted balls, actual hits,
## and predicted hits from the model for players

one_player <- function(id){
  invlogit <- function(x){ exp(x) / (1 + exp(x))}
  
  ## cleaning the data to filter it by batterID, and with the relevant columns
  MyData %>% filter (BatterId == id) %>%
    select(BatterId, inPlayOut, ExitSpeed, Angle) -> d
  
  
  d$Predict <- invlogit(predict(fit, d))
  c(length(d$inPlayOut), sum(d$inPlayOut), sum(d$Predict))
}


## This is how were able to test if this works for all batters
## The ability to correlate it to a specific condition or batter

unique_ids <- unique(MyData$BatterId)
S <- sapply(unique_ids, one_player)
S1 <- data.frame(BatterIds = unique_ids,
                 N = S[1, ],
                 H = S[2, ],
                 Expected = S[3, ])

## More cleaning up of data
## Grouping the data by batterID since we dont have player names
## So we can compute total stats for each specific ID
MyData %>% group_by(BatterId) %>%
  summarize(Player = first(BatterId)) -> SC
inner_join(S1, SC, 
           by = c("BatterIds" = "BatterId")) -> S2

## Computes the Z score which will help us contrast 
## the observed results and the expected results
S2$Z = with(S2, (H - Expected) / sqrt(Expected))


### Creating the Graphs to compare 

library(ggrepel)

## This is just a simple comparison of observed and expected hits
## Fit linearly to show our models accuracy


## The model is pretty linear for the most part 
## you can see that if 50 hits are observed
## 50 hits are also predicted 


## you see some deviation as the number of hits get higher up
## for example the point with 250 hits observed
## and only 225 hits predicted
ggplot(S2, aes(Expected, H, label = Player)) +
  geom_point() + geom_smooth(color = "red") + 
  ylim(0, 250) +
  xlim(0, 250) +
  TH + ggtitle("Observed and Expected Hits for All Players")


## This is a graph of how many more or fewer hits an individual player
## has based on what is predicted in my model
## This is what we consider the Residual 
## and it is formulated by Residual = Hits - Expected_Hits
## It just shows that for each indivual player at most our
## predicted model will be off by 25 and that's an extreme outlier
## The bulk of points near 0 shows our model is pretty strong

## Obviously these models would help better if we had batter_names
## but I used batterId just to give the general idea of what's possible

ggplot(S2, aes(N, H - Expected, label = Player)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  ylim(-30, 30) +
  xlim(0, 600) +
  TH + ggtitle("Graph of Residuals")


## Here were going to individualized a batterID 
##(again it would be much more effective with an actual batter name)
## and we are going to compare their predicted hit probability
## based on launch angle and exit speed
## with their actual hits based on launch angle and exit speed
## once again this will not only further strengthen the validity
## of my model but it also provides important individualized
## data that can be used to make adjustments for specific players
## this not only tells us at what angle and speed the individual player
## is most likely to get hits but also what angle and speed
## the player regularly hits the ball at and how often they hit the ball
## within the optimal launch angle and exit speed range



## ***here just like every other model - 0 represents a hit and 1 represents an out***
one_player_graph <- function(playerid){
  invlogit <- function(x){ exp(x) / (1 + exp(x))}

  ## The data cleanup here totals the relevant data 
  ## for each unique BatterID based on exit speed and launch angle
  filter(MyData, BatterId == playerid) %>%
    select(Angle, ExitSpeed, inPlayOut) -> samplebatter
  
  samplebatter$P <- invlogit(predict(fit, samplebatter))
  samplebatter$Predict <- ifelse(samplebatter$P > .5, 1, 0)
  
  ## Comparing the actual and predicted
  df1 <- select(samplebatter, Angle, ExitSpeed)
  df1$Outs = samplebatter$inPlayOut; df1$Type = "Actual"
  df2 <- select(samplebatter, Angle, ExitSpeed)
  df2$Outs = samplebatter$Predict; df2$Type = "Predicted"
  df <- rbind(df1, df2)
  df$Outs <- as.factor(df$Outs)
  print(ggplot(df, aes(Angle, ExitSpeed, color=Outs)) +
          geom_jitter() +
          facet_wrap(~ Type, ncol = 1) +
          ggtitle("Actual vs Predicted Hits For: ", subtitle = playerid)+ TH)
  samplebatter
}

## Just testing with 2 examples
samplebatter <- one_player_graph("48371")
samplebatter2 <- one_player_graph("99078")


## Summarizing the accuracy of the models
## The closer the numbers together the more accurate the model
summarize(samplebatter, H = sum(inPlayOut), P = sum(P))
summarize(samplebatter2, H = sum(inPlayOut), P = sum(P))

#########

#### Probability of a hit at specific ballparks
### This would probably be more effective when viewing home runs
### but the prompt asked for hits not home runs


one_ballpark_graph <- function(ballpark){
  invlogit <- function(x){ exp(x) / (1 + exp(x))}
  
  filter(MyData, BallparkId == ballpark) %>%
    select(Angle, ExitSpeed, inPlayOut) -> sampleballpark
  
  sampleballpark$P <- invlogit(predict(fit, sampleballpark))
  sampleballpark$Predict <- ifelse(sampleballpark$P > .5, 1, 0)
  
  df1 <- select(sampleballpark, Angle, ExitSpeed)
  df1$Outs = sampleballpark$inPlayOut; df1$Type = "Actual"
  df2 <- select(sampleballpark, Angle, ExitSpeed)
  df2$Outs = sampleballpark$Predict; df2$Type = "Predicted"
  df <- rbind(df1, df2)
  df$Outs <- as.factor(df$Outs)
  print(ggplot(df, aes(Angle, ExitSpeed, color=Outs)) +
          geom_jitter() +
          facet_wrap(~ Type, ncol = 1) +
          ggtitle("Ballpark Number: ", subtitle = ballpark) + TH)
  sampleballpark
}

sampleballpark <- one_ballpark_graph("8")
sampleballpark2 <-one_ballpark_graph("57")

summarize(sampleballpark, H = sum(inPlayOut), P = sum(P))
summarize(sampleballpark2, H = sum(inPlayOut), P = sum(P))

#### Probability of a hit at a certain spot in the order
#### This one is best when individualized by a specific player
### I'll do that later for each of these

one_orderspot_graph <- function(order){
  invlogit <- function(x){ exp(x) / (1 + exp(x))}
  
  filter(MyData, BatterPositionId == order) %>%
    select(Angle, ExitSpeed, inPlayOut) -> sampleorder
  
  sampleorder$P <- invlogit(predict(fit, sampleorder))
  sampleorder$Predict <- ifelse(sampleorder$P > .5, 1, 0)
  
  df1 <- select(sampleorder, Angle, ExitSpeed)
  df1$Outs = sampleorder$inPlayOut; df1$Type = "Actual"
  df2 <- select(sampleorder, Angle, ExitSpeed)
  df2$Outs = sampleorder$Predict; df2$Type = "Predicted"
  df <- rbind(df1, df2)
  df$Outs <- as.factor(df$Outs)
  print(ggplot(df, aes(Angle, ExitSpeed, color=Outs)) +
          geom_jitter() +
          facet_wrap(~ Type, ncol = 1) +
          ggtitle(order) + TH)
  sampleorder
}

sampleorder <- one_orderspot_graph("1")
sampleorder2 <- one_orderspot_graph("9")

summarize(sampleorder, H = sum(inPlayOut), P = sum(P))
summarize(sampleorder2, H = sum(inPlayOut), P = sum(P))

#### Probability of a hit third time facing a pitcher


one_times_graph <- function(times){
  invlogit <- function(x){ exp(x) / (1 + exp(x))}
  
  filter(MyData, batterTimesFaced == times) %>%
    select(Angle, ExitSpeed, inPlayOut) -> sampletimes
  
  sampletimes$P <- invlogit(predict(fit, sampletimes))
  sampletimes$Predict <- ifelse(sampletimes$P > .5, 1, 0)
  
  df1 <- select(sampletimes, Angle, ExitSpeed)
  df1$Outs = sampletimes$inPlayOut; df1$Type = "Actual"
  df2 <- select(sampletimes, Angle, ExitSpeed)
  df2$Outs = sampletimes$Predict; df2$Type = "Predicted"
  df <- rbind(df1, df2)
  df$Outs <- as.factor(df$Outs)
  print(ggplot(df, aes(Angle, ExitSpeed, color=Outs)) +
          geom_jitter() +
          facet_wrap(~ Type, ncol = 1) +
          ggtitle("At-Bats against one pitcher: ", subtitle = times) + TH)
  sampletimes
}

sampletimes <- one_times_graph("1")
sampletimes2 <- one_times_graph("2")
sampletimes3 <- one_times_graph("3")
sampletimes4 <- one_times_graph("4")
sampletimes4 <- one_times_graph("5")

summarize(sampletimes, H = sum(inPlayOut), P = sum(P))
summarize(sampletimes2, H = sum(inPlayOut), P = sum(P))


#### Predicted Probability of a hit on 1st pitch vs on 2nd pitch

one_pitch_graph <- function(pitch){
  invlogit <- function(x){ exp(x) / (1 + exp(x))}
  
  
  InPlay_Sample %>%
  filter(str_detect(PitchOfPA, "1")) %>%
    select(Angle, ExitSpeed, inPlayOut, PitchOfPA) -> samplepitch
  
  InPlay_Sample %>% 
  filter(str_detect(PitchOfPA, "2")) %>%
    select(Angle, ExitSpeed, inPlayOut, PitchOfPA) -> samplepitch2
  
  samplepitch$P <- invlogit(predict(fit, samplepitch))
  samplepitch$Predict <- ifelse(samplepitch$P > .5, 1, 0)
  
  samplepitch2$P <- invlogit(predict(fit, samplepitch2))
  samplepitch2$Predict <- ifelse(samplepitch2$P > .5, 1, 0)
  
  df1 <- select(samplepitch, Angle, ExitSpeed)
  df1$Outs = samplepitch$Predict; df1$Type = "1st Pitch"
  df2 <- select(samplepitch2, Angle, ExitSpeed)
  df2$Outs = samplepitch2$Predict; df2$Type = "2nd Pitch"
  df <- rbind(df1, df2)
  df$Outs <- as.factor(df$Outs)
  print(ggplot(df, aes(Angle, ExitSpeed, color=Outs)) +
          geom_jitter() +
          facet_wrap(~ Type, ncol = 1) +
          ggtitle(pitch) + TH)
  samplepitch
  samplepitch2
}

samplepitch <- one_pitch_graph("First Pitch vs Second Pitch")


summarize(samplepitch, H = sum(inPlayOut), P = sum(P))
summarize(samplepitch, H = sum(inPlayOut), P = sum(P))


#### Checking which player is predicted to get more hits 

two_players_graph <- function(player){
  invlogit <- function(x){ exp(x) / (1 + exp(x))}
  
MyData %>%
  filter(str_detect(BatterId, "48371")) %>%
  select(Angle, ExitSpeed, inPlayOut, BatterId) -> PlayerOne

MyData %>%
  filter(str_detect(BatterId, "99078")) %>%
  select(Angle, ExitSpeed, inPlayOut, BatterId) -> Player2

PlayerOne$P <- invlogit(predict(fit, PlayerOne))
PlayerOne$Predict <- ifelse(PlayerOne$P > .5, 1, 0)

Player2$P <- invlogit(predict(fit, Player2))
Player2$Predict <- ifelse(Player2$P > .5, 1, 0)

df1 <- select(PlayerOne, Angle, ExitSpeed)
df1$Outs = PlayerOne$Predict; df1$Type = "Player One"
df2 <- select(Player2, Angle, ExitSpeed)
df2$Outs = Player2$Predict; df2$Type = "Player 2"
df <- rbind(df1, df2)
df$Outs <- as.factor(df$Outs)
print(ggplot(df, aes(Angle, ExitSpeed, color=Outs)) +
        geom_jitter() +
        facet_wrap(~ Type, ncol = 1) +
        ggtitle("Who's The Better Hitter") + TH)
PlayerOne
Player2
}

PlayerOne <- two_players_graph("48371")
Player2 <- two_players_graph("99078")

summarize(PlayerOne, PlayerOneHits = sum(inPlayOut), Player2Hits = sum(P))


## Player 2 is projected to get more hits than Player 1


### Can add specific player variables to these as well
### For exmaple - Probability player "99078" gets a hit at Ballpark 8
### Probability player "99078" gets a hit batting 9th opposed to 1st


### Comparing the players on the same team 
## to see who should bat in a certain spot
## in the lineup

lineup_spots <- function(compare){
  invlogit <- function(x){ exp(x) / (1 + exp(x))}
  
  MyData %>%
    filter(str_detect(BallparkId, "387")) %>%
    select(Angle, ExitSpeed, inPlayOut, BatterId, BallparkId, BatterPositionId) -> OneTeam
  
  OneTeam %>%
    filter(str_detect(BatterPositionId, "8")) %>%
    select(Angle, ExitSpeed, inPlayOut, BatterId, BallparkId, BatterPositionId) -> OneTeamBatting8
  
  OneTeamBatting8 %>%
    filter(str_detect(BatterId, "77692")) %>%
    select(Angle, ExitSpeed, inPlayOut, BatterId, BatterPositionId) -> OnePlayerOneTeam
  
  OneTeamBatting8 %>%
    filter(str_detect(BatterId, "114724")) %>%
    select(Angle, ExitSpeed, inPlayOut, BatterId, BatterPositionId) -> SecondPlayerOneTeam
  
  
  OnePlayerOneTeam$P <- invlogit(predict(fit,OnePlayerOneTeam ))
  OnePlayerOneTeam$Predict <- ifelse(OnePlayerOneTeam$P > .5, 1, 0)
  
  SecondPlayerOneTeam$P <- invlogit(predict(fit, SecondPlayerOneTeam))
  SecondPlayerOneTeam$Predict <- ifelse(SecondPlayerOneTeam$P > .5, 1, 0)
  
  df1 <- select(OnePlayerOneTeam, Angle, ExitSpeed)
  df1$Outs = OnePlayerOneTeam$Predict; df1$Type = "Player 1"
  df2 <- select(SecondPlayerOneTeam, Angle, ExitSpeed)
  df2$Outs = SecondPlayerOneTeam$Predict; df2$Type = "Player 2"
  df <- rbind(df1, df2)
  df$Outs <- as.factor(df$Outs)
  print(ggplot(df, aes(Angle, ExitSpeed, color=Outs)) +
          geom_jitter() +
          facet_wrap(~ Type, ncol = 1) +
          ggtitle("Who should hit 8th") + TH)
  OnePlayerOneTeam
  SecondPlayerOneTeam
}

 OnePlayerOneTeam <- lineup_spots("77692")
SecondPlayerOneTeam <- lineup_spots("11424")

## Obviously there is not enough data but 
## player 1 is 4/20 in the 8th spot 
## and player 2 is 5/14
## Therefore you should start player 2 in the 8th spot over player 1



## Probability of a hit against a specific pitch
## Predicted Probability that a player is expected to get a hit on a curveball

specific_pitch <- function(compareCB){
  invlogit <- function(x){ exp(x) / (1 + exp(x))}
  
  MyData %>%
    filter(str_detect(PitchType, "CB")) %>%
    select(Angle, ExitSpeed, inPlayOut, BatterId, BallparkId, BatterPositionId) -> CurveBall
  
  filter(MyData, BatterId == compareCB) %>%
    select(Angle, ExitSpeed, inPlayOut, BatterId) -> CurveBall
  
  CurveBall$P <- invlogit(predict(fit, CurveBall))
  CurveBall$Predict <- ifelse(CurveBall$P > .5, 1, 0)
  
  df1 <- select(CurveBall, Angle, ExitSpeed)
  df1$Outs = CurveBall$inPlayOut; df1$Type = "Actual"
  df2 <- select(CurveBall, Angle, ExitSpeed)
  df2$Outs = CurveBall$Predict; df2$Type = "Predicted"
  df <- rbind(df1, df2)
  df$Outs <- as.factor(df$Outs)
  print(ggplot(df, aes(Angle, ExitSpeed, color=Outs)) +
          geom_jitter() +
          facet_wrap(~ Type, ncol = 1) +
          ggtitle("Predicted Hit Probability on CB") + TH)
  CurveBall
}

CurveBall <- specific_pitch("14863")
CurveBall2 <-specific_pitch("17258")


  
 



