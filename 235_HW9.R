HW9

library(moderndive)
library(tidyverse)
library(Metrics)
library(ggfortify)
library(ggplot2)
library(cowplot)

#Question 1
fullmodel = lm(Balance ~ ., data = credit)
step(fullmodel)

#Question 2
summary(fullmodel)
sqrt(mean(fullmodel$residuals^2))

backmodel = lm(Balance ~ Income + Limit + Cards + Age + Student, data = credit)
summary(backmodel)
sqrt(mean(backmodel$residuals^2))

#Question 3
N/A

#Question 4
set.seed(82, sample.kind = "Rejection")
frac <- 0.7
n <- nrow(new_credit)
train.cases <- sample(1:n, frac*n)
train.set <- new_credit[train.cases,]
test.set <- new_credit[-train.cases,]

fulltrain = lm(Balance ~ ., data = train.set)
fulltest = lm(Balance ~ ., data = test.set)
backtrain = lm(Balance ~ Income + Limit + Cards + Age + Student, data = train.set)
backtest = lm(Balance ~ Income + Limit + Cards + Age + Student, data = test.set)

train_RMSE = sqrt(mean(fulltrain$residuals^2))
Y_hat = predict(fulltrain,test.set)
test_RMSE = sqrt(mean((test.set$Balance-Y_hat)^2))

train_RMSE = sqrt(mean(backtrain$residuals^2))
Y_hat = predict(backtrain,test.set)
test_RMSE = sqrt(mean((test.set$Balance-Y_hat)^2))

#Question 5
Smarket <- Smarket %>% mutate(Up = ifelse(Direction == "Up", 1, 0))
train.returns <- Smarket %>% filter(Year < 2005)
test.returns <- Smarket %>% filter(Year >= 2005)

blsh = glm(Up ~ . - Year - Today - Direction, data = Smarket)
blsh_train = glm(Up ~ . - Year - Today - Direction, data = train.returns)
blsh_test = glm(Up ~ . - Year - Today - Direction, data = test.returns)

train_RMSE = sqrt(mean(blsh_train$residuals^2))
Y_hat = predict(blsh_train,test.returns)
test_RMSE = sqrt(mean((test.returns$Up-Y_hat)^2))

#Question 6
final = step(blsh)
final_train = glm(Up ~ Lag1, data = train.returns)

train_RMSE = sqrt(mean(final_train$residuals^2))
Y_hat = predict(final_train,test.returns)
test_RMSE = sqrt(mean((test.returns$Up-Y_hat)^2))

#Question 7
N/A

