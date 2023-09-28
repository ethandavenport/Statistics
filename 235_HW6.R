HW6

library(moderndive)
library(tidyverse)
library(Metrics)
library(ggfortify)

#Question 1
ggplot(techsoft, aes(x = quarter, y = sales)) +
  geom_point() +
  geom_line()
lm = lm(sales ~ quarter + S1 + S2 + S3, data = techsoft)
plot(lm)
acf(lm$res)
qm = lm(sales ~ quarter + I(quarter^2) + S1 + S2 + S3, data = techsoft)
plot(qm)
acf(qm$res)

#Question 2
data = data.frame(quarter = c(101), S1 = c(1), S2 = c(0), S3 = c(0))
predict(qm, data, interval = "prediction")

#Question 3
summary(qm)
data = data.frame(quarter = c(100), S1 = c(0), S2 = c(0), S3 = c(0))
predict(qm, data, interval = "prediction")

#Question 4
summary(qm)

#Question 5
t = NFLX[2:length(NFLX)]
t_1 = NFLX[1:length(NFLX) - 1]
AR1 = lm(NFLX[2:length(NFLX)] ~ NFLX[1:length(NFLX)-1], data = netflix)
autoplot(AR1)
acf(AR1$res)

#Question 6
get_regression_table(AR1)

#Question 7
acf(AR1$res)

#Question 8
data = data.frame(Date = 2/11/2022)
predict(AR1, data, interval = "prediction")
summary(AR1)

#Question 9
netflix <- netflix %>% mutate(nflx_return = NFLX/lag(NFLX)-1,
                              sp_return = SP500/lag(SP500)-1)
return_model = lm(nflx_return ~ sp_return, data = netflix)
summary(return_model)

#Question 10
get_regression_table(return_model)

