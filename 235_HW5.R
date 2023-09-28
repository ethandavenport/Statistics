HW5

library(moderndive)
library(tidyverse)
library(Metrics)

#Question 1
model_auto = lm(mpg ~ horsepower, data = auto)
resid_auto = resid(model_auto)
plot(fitted(model_auto),resid_auto)
qqnorm(auto$mpg, pch = 1, frame = FALSE)
qqline(auto$mpg, lwd = 2)

#Question 2
standard_res = rstandard(model_auto)
standard_auto = cbind(auto, standard_res)
ggplot(standard_auto, aes(x  = mpg, y  = standard_res)) +
  geom_point()
ggplot(auto, aes(x  = horsepower, y  = mpg)) +
  geom_point()

#Remove data point
auto2 = auto[-393,]
model_auto2 = lm(mpg ~ .-name, data = auto2)

#Question 3
get_regression_table(model_auto2)
summary(model_auto2)

#Question 4
model_auto3 = lm(mpg ~ weight + year, data = auto2)
get_regression_table(model_auto3)
summary(model_auto3)

#Question 5
model_auto4 = lm(mpg ~ engine.size + fuel, data = auto_new)
resid_auto_new = resid(model_auto4)
plot(fitted(model_auto4),resid_auto_new)

#Question 6
model_auto4 = lm(mpg ~ engine.size + fuel, data = auto_new)
auto_order2 = lm(mpg ~ engine.size + I(engine.size^2) + fuel, data = auto_new)
auto_order3 = lm(mpg ~ poly(engine.size,3) + fuel, data = auto_new)
auto_order9 = lm(mpg ~ poly(engine.size,9) + fuel, data = auto_new)
summary(model_auto4)
summary(auto_order2)
summary(auto_order3)
summary(auto_order9)

#Question 7
data = data.frame(fuel = "diesel", engine.size = c(6))
predict(auto_order9, data, interval = "prediction")

ggplot(auto_new, aes(x  = engine.size, y  = mpg, colour = fuel)) +
  geom_point() +
  geom_function(aes(color = "diesel"), fun = ~ 59.696 - 18.245*(.x) + 2.146*(.x)^2) +
  geom_function(aes(color = "gas"), fun = ~  52.148 - 18.245*(.x) + 2.146*(.x)^2)

#Question 8
log_auto = lm(log(mpg) ~ log(engine.size) + fuel, data = auto_new)
summary(log_auto)

resid_log_auto = resid(log_auto)
plot(fitted(log_auto),resid_log_auto)
ggplot(log_auto, aes(x = resid_log_auto)) +
  geom_histogram()

ggplot(auto_new, aes(x  = log(engine.size), y  = log(mpg), colour = fuel)) +
  geom_point() +
  geom_function(aes(color = "diesel"), fun = ~ 3.91526 - 0.68178*(.x)) +
  geom_function(aes(color = "gas"), fun = ~  3.63712 - 0.68178*(.x))

#Question 9
-0.68178*2

#Question 10
data = data.frame(fuel = "gas", engine.size = c(5))
exp(predict(log_auto, data, interval = "prediction"))

ggplot(auto_new, aes(x  = engine.size, y  = mpg, colour = fuel)) +
  geom_point() +
  geom_function(aes(color = "diesel"),fun = ~ exp(3.91526 - 0.68178*log(.x))) +
  geom_function(aes(color = "gas"),fun = ~ exp(3.63712 - 0.68178*log(.x)))

#Question 11
model_freshener = lm(log(Sales) ~ log(Price), data = car_freshener)
summary(model_car)
data = data.frame(Price = c(1.8))
exp(predict(model_freshener, data, interval = "prediction"))
