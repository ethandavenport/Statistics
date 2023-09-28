HW3

library(moderndive)
library(tidyverse)
library(Metrics)

#Question 1
ggplot(houses_mod) +
  geom_boxplot(aes(x = factor(Central_Air), y = Sale_Price))

#Question 2
ac_difference = lm(Sale_Price ~ factor(Central_Air), data = houses_mod)
summary(ac_difference)
get_regression_table(ac_difference)

#Question 3
ac_area = lm(Sale_Price ~ factor(Central_Air) + Area, data = houses_mod)
summary(ac_area)
get_regression_table(ac_area)
ggplot(houses_mod) +
  geom_point(aes(x = Area, y = Sale_Price, color = Central_Air)) +
  geom_line(aes(x = Area, y = predict(ac_area), color = Central_Air))

#Question 4
houses = houses_mod %>%
  mutate(acdummy = ifelse(Central_Air == "N", yes = 1, no = 0))
lmq4 = lm(Sale_Price ~ Area + acdummy, data = houses)
summary(lmq4)
get_regression_table(lmq4)

#Question 5
lmq5 = lm(Sale_Price ~ acdummy + Beds + Fireplaces + Year_Built, data = houses)
summary(lmq5)
get_regression_table(lmq5)

#Question 6
lmq6 = lm(Sale_Price ~ acdummy + Area + Beds + Fireplaces + Year_Built, data = houses)
summary(lmq6)
get_regression_table(lmq6)
data = data.frame(acdummy = c(0), Area = c(2000), Beds = c(3), Fireplaces = c(1), Year_Built = c(2005))
predict(lmq6, data, interval = "prediction")

#Question 7
lmq7 = lm(Sale_Price ~ acdummy + Area + Beds + Fireplaces + factor(Garage_Type), data = houses)
summary(lmq7)
get_regression_table(lmq7)

