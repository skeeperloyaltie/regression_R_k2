# read directory
setwd("D:/Desktop/regression_R/modelling")
# initialize the data variables
# the match data
match_data <- read.csv("match.csv")
# the players data
players_data <- read.csv("players.csv")
# the players ratings data
players_ratings <- read.csv("player_ratings.csv")

# View the data properties

summary(match_data)
summary(players_data)
summary(players_ratings)

# the structure of the data
str(match_data)
str(players_data)
str(players_ratings)

# sub-setting the data set
match_data <- head(match_data, 600)
players_data <- head(players_data, 600)
players_ratings <- head(players_ratings, 600)

# Plots
hist(players_data$gold_spent)
hist(players_data$gold)
hist(players_data$gold_per_min)


# match data
set.seed(100)
match <- sample(1:nrow(match_data), .75 * nrow(match_data))
train <- match_data[match, ]
test <- match_data[-match, ]

# players data
players <- sample(1:nrow(players_data), .75 * nrow(players_data))
train_players <-players_data[players, ]
test_players <- players_data[-players, ]

# ratings data
ratings <- sample(1:nrow(players_ratings), .75 * nrow(players_ratings))
train_ratings <- players_ratings[ratings, ]
test_ratings <- players_ratings[-ratings, ]


# Modelling the data all together

# Models
# Match Model
model_match <- lm(game_mode ~ radiant_win, data = train)
summary(model_match)

# Players Model
model_players <- lm(gold_spent ~ player_slot + gold, data = train_players)
summary(model_players)

# Ratings Model
model_ratings <- lm(total_matches ~ total_wins + trueskill_mu, data = players_ratings)
summary(model_ratings)



# Model classification
model_fit <- anova(model_ratings, model_match, model_players)


# Plots

par(mfrow = c(2,2))
plot(model_ratings)
plot(model_players)
plot(model_match)


# Model prediction 
anova_model <- anova(model_match, model_players, players_ratings)


# Prediction
pred <- predict(model_total, test)
pred2 <- predict(model_players, test_players)
pred3 <- predict(model_ratings, test_ratings)

pred1 <- data.frame(cbind(actuals=test$game_mode, predicteds = pred))
pred_2 <- data.frame(cbind(actuals=test_players$gold_spent, predicteds = pred2))
pred_3 <- data.frame(cbind(actuals = test_ratings$total_wins, predicteds = pred3))

# Finding the accuracy of the data
accuracy1 <- cor(pred)
accuracy2 <- cor(pred_2)
accuracy3 <- cor(pred_3)




