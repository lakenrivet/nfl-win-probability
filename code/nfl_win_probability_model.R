# Win Probability Model
# Laken Rivet

# load relevant libraries
library(nflscrapR)
library(nflfastR)
library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(caTools)
library(ranger)
library(reshape)

#### compile data ####

# loading of play-by-play data is commented out and replaced with reading in .csv to save time

# get play-by-play data for 2011 to 2021 seasons
# pbp <- load_pbp(2011:2021)

# write data frame to csv file for reuse
# write.csv(pbp, file = "processed_data/pbp.csv", row.names = FALSE)

# note that processed_data is NOT included in repository due to size restrictions
# to replicate this code, user must read in play-by-play data on their personal machine

# read in previously saved csv with play-by-play data
pbp <- read.csv(file = "processed_data/pbp.csv", header = TRUE)

# filter data for just regular season games
pbp <- pbp %>% 
  filter(season_type == "REG")

# similar to play-by-play data, loading of game data is commented out and replaced by reading in a .csv

# get game results
# games <- nflreadr::load_schedules()

# filter data for 2011-2021 regular season games
# games <- games %>%
#  filter(game_type == "REG" & season %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021))

# write data frame to csv file for reuse
# write.csv(games, file = "processed_data/games.csv", row.names = FALSE)

# read in saved game results
games <- read.csv(file = "processed_data/games.csv", header = TRUE)

# combine the data 
pbp_final <- full_join(games, pbp, by = "game_id")

# create an outcome variable that states if the team wins or loses
pbp_final <- pbp_final %>% 
  mutate(winner = ifelse(home_score.x > away_score.x, home_team.x, away_team.x))

# create an indicator variable that states if the team in possession ultimately wins
pbp_final <- pbp_final %>% 
  mutate(poswins = ifelse(winner == posteam, "Yes","No"))

# convert relevant binary variable to factors
pbp_final$qtr <- as.factor(pbp_final$qtr) 
pbp_final$down <- as.factor(pbp_final$down)
pbp_final$poswins <- as.factor(pbp_final$poswins)
pbp_final$tackled_for_loss <- as.factor(pbp_final$tackled_for_loss)
pbp_final$sack <- as.factor(pbp_final$sack)
pbp_final$interception <- as.factor(pbp_final$interception)
pbp_final$pass_attempt <- as.factor(pbp_final$pass_attempt)
pbp_final$posteam_timeouts_remaining <- as.factor(pbp_final$posteam_timeouts_remaining)
pbp_final$defteam_timeouts_remaining <- as.factor(pbp_final$defteam_timeouts_remaining)

# subset data further for only variables of interest and complete information
pbp_reduced <- pbp_final %>% 
  filter(play_type != "no_play" & 
           qtr != 5 & down != "NA" & 
           poswins != "NA" & 
           tackled_for_loss != "NA" & 
           sack != "NA" & 
           interception != "NA" &
           pass_attempt != "NA" &
           posteam_timeouts_remaining != "NA" &
           defteam_timeouts_remaining != "NA") %>% 
  select(game_id, 
         game_date, 
         posteam, 
         home_team.x, 
         away_team.x, 
         winner, 
         qtr, 
         down, 
         ydstogo, 
         game_seconds_remaining, 
         yardline_100, 
         score_differential, 
         tackled_for_loss, 
         sack, 
         interception, 
         pass_attempt, 
         posteam_timeouts_remaining, 
         defteam_timeouts_remaining, 
         poswins, 
         home_wp)

##### create models (logistic regression and random forest) #####

# set seed for repeated results
set.seed(123)

# break data into train and test data sets (80/20 train/test ratio)
split <- sample.split(pbp_reduced$poswins, SplitRatio = 0.8)
train <- pbp_reduced %>% 
  filter(split == TRUE)
test <- pbp_reduced %>% 
  filter(split == FALSE)

# create a logistic regression model with the glm function
model1 <- glm(poswins ~ qtr + down + ydstogo + game_seconds_remaining + yardline_100 + score_differential + tackled_for_loss + sack + interception + pass_attempt + posteam_timeouts_remaining + defteam_timeouts_remaining, train, family = "binomial")

# output the results of the model
summary(model1)

# use model to estimate win probabilities for values in training data set
pred1 <- predict(model1, train, type = "response")

# merge train data frame and predictions
train <- cbind(train,pred1)

# create new column that always calculates win probability for home team
train <- mutate(train, pred1h = ifelse(posteam == home_team.x, pred1, 1-pred1))

# extract data for a single game for visualization
example <- train %>%
  filter(game_id == "2011_01_NO_GB")

# create visual to show prediction results
ggplot(example, aes(x = game_seconds_remaining, y = pred1h)) + 
  geom_line(colour="red") + 
  scale_x_reverse() + ylim(c(0,1)) + 
  theme_minimal() + xlab("Time Remaining (seconds)") + 
  ylab("Home Win Probability")

# now create a random forest model for comparison with logistic regression
rf1 <- ranger(poswins ~ qtr + down + ydstogo + game_seconds_remaining + yardline_100 + score_differential + tackled_for_loss + sack + interception + pass_attempt + posteam_timeouts_remaining + defteam_timeouts_remaining, train, write.forest = TRUE, num.trees = 1000, probability = TRUE)

# now use the random forest to make predictions on training data
predRF <- predict(rf1, train)

# extract the column that predicts win probabilities (rather than loss probability)
predRF_Yes <- predRF$predictions[,2]

# merge train data and RF predictions
train <- cbind(train,predRF_Yes)

# create new column that always calculates win probability for home team
train <- mutate(train, predRFh = ifelse(posteam == home_team.x, predRF_Yes, 1-predRF_Yes))

# isolate subset of columns for comparison
prob_reduced <- train %>% 
  select(game_id, game_seconds_remaining, pred1h, predRFh)

# alter format of data frame for easier comparison
prob_reduced_melt <- melt(prob_reduced, id=c("game_seconds_remaining","game_id"))

# visually compare two models on training data
ggplot(filter(prob_reduced_melt, game_id == "2011_01_NO_GB"), aes(x=game_seconds_remaining, y= value, color = variable)) +
  geom_line() +
  scale_x_reverse() +
  ylim(c(0,1)) +
  theme_minimal() +
  xlab("Time Remaining (seconds)") +
  ylab("Home Win Probability") +
  scale_color_manual(values=c("#FC4C02", "#0C2340"), name="Model", labels=c("Logistic", "RF"))

#### run models on test data ####

# predict win probs with logistic regression model 
pred2 <- predict(model1, test, type = "response")

# bind probs to test data set
test <- cbind(test, pred2)

# create new column that always calculates win probability for home team
test <- mutate(test, pred2h = ifelse(posteam == home_team.x, pred2, 1-pred2))

# predict win probs with random forest model
predRF2 <- predict(rf1, test)

# extract the column that predicts win probabilities (rather than loss probability)
predRF_Yes2 = predRF2$predictions[,2]

# bind RF probs to test data set
test <- cbind(test, predRF_Yes2)

# create new column that always calculates win probability for home team
test <- mutate(test, predRFh2 = ifelse(posteam == home_team.x, predRF_Yes2, 1-predRF_Yes2))

# isolate subset of columns for comparison
test_reduced <- test %>% 
  select(game_id, game_seconds_remaining, pred2h, predRFh2, home_wp)

# alter format of data frame for easier comparison
test_reduced_melt <- melt(test_reduced, id=c("game_seconds_remaining","game_id"))

# visually compare two models and nflfastR on test data
ggplot(filter(test_reduced_melt, game_id == "2021_14_SF_CIN"), aes(x=game_seconds_remaining, y= value, color = variable)) +
  geom_line() +
  scale_x_reverse() +
  ylim(c(0,1)) +
  theme_minimal() +
  xlab("Game Time Remaining (seconds)") +
  ylab(paste("Home Win Probability", "(Bengals)")) +
  labs(title = "Win Probability of Cincinnati Bengals vs. San Francisco 49ers") +
  scale_color_manual(values=c("red", "blue", "black"), name="Model Type", labels=c("Logistic", "RF", "nflfastR")) +
  theme(legend.position = c(.95, .95), legend.justification = c("right", "top"), legend.box.just = "right", legend.margin = margin(6, 6, 6, 6)) +
  theme(axis.title = element_text(face = "bold"), plot.title = element_text(face = "bold")) +
  theme(legend.text = element_text(face = "bold"), legend.title = element_text(face = "bold"))

# save output graphic
ggsave(filename = "output/CIN_SF_plot.pdf")

##### evaluating impact of plays on model #####

# create data set for visualization
q2 <- pbp_final %>% 
  filter(play_type != "no_play" &
           qtr != 5 &
           down != "NA" &
           poswins != "NA" &
           tackled_for_loss != "NA" &
           sack != "NA" &
           interception != "NA" &
           pass_attempt != "NA" &
           posteam_timeouts_remaining != "NA" &
           defteam_timeouts_remaining != "NA" &
           game_id == "2021_06_KC_WAS") %>% 
  select(game_id, 
         game_date, 
         desc, 
         posteam, 
         home_team.x, 
         away_team.x, 
         winner, 
         qtr, 
         down, 
         ydstogo, 
         game_seconds_remaining, 
         yardline_100, 
         score_differential, 
         tackled_for_loss, 
         sack, 
         interception, 
         pass_attempt, 
         posteam_timeouts_remaining, 
         defteam_timeouts_remaining, 
         poswins, 
         home_wp)

# run model on new data set to make predictions
predq2 <- predict(rf1, q2)

# extract the column that predicts win probabilities (rather than loss probability)
predq2_Yes = predq2$predictions[,2]

# bind RF probs to test data set
q2 <- cbind(q2, predq2_Yes)

# create new column that always calculates win probability for home team
q2 <- mutate(q2, predq2h = ifelse(posteam == home_team.x, predq2_Yes, 1-predq2_Yes))

# create visualization
ggplot(q2, aes(x = game_seconds_remaining, y = predq2h)) + 
  geom_line(colour="black") + 
  labs(title = "Win Probability of Washington Commanders vs. Kansas City Chiefs") +
  scale_x_reverse() + ylim(c(0,1)) + 
  theme_minimal() + 
  xlab("Time Remaining (seconds)") + 
  ylab("Home Win Probability (Commanders)") +
  theme(axis.title = element_text(face = "bold"), plot.title = element_text(face = "bold"))

# save output graphic
ggsave(filename = "output/WSH_KC_plot.pdf")
