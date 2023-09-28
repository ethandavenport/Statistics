library(tidyverse)
library(mosaic)
library(MatchIt)
library(effectsize)
library(moderndive)
library(modelr)
library(rsample)
library(lubridate)


# Problem 1
lm_redlining = lm(policies ~ fire + age + income + minority, data = redlining)
get_regression_table(lm_redlining)
rmse(lm_redlining, redlining)
rsquared(lm_redlining)


# Problem 2

# Part A
groceries_store = groceries %>%
  group_by(Store) %>%
  summarize(avgprice = mean(Price)) %>%
  arrange(desc(avgprice))
ggplot(groceries_store) +
  geom_col(aes(x = avgprice, y = Store))

# Part B
groceries_product = groceries %>%
  group_by(Product) %>%
  summarize(count = count(Product)) %>%
  arrange(desc(count))
ggplot(groceries_product) +
  geom_col(aes(x = count, y = Product))

# Part C
lm_type = lm(Price ~ Product + Type, data = groceries)
reg_type = get_regression_table(lm_type)

# Part D
lm_store = lm(Price ~ Product + Store, data = groceries)
reg_store = get_regression_table(lm_store)
reg_store = reg_store %>%
  filter(str_detect(term, pattern = "Store"))

# Part F
groceries_income = groceries %>%
  mutate(Income10K = Income/10000)
lm_income = lm(Price ~ Product + Income10K, data = groceries_income)
reg_income = get_regression_table(lm_income)
standardize_parameters(lm_income)


# Problem 3
lm_small = lm(children ~ market_segment + adults + customer_type +
                is_repeated_guest, data = hotels_train)
lm_big = lm(children ~ . - arrival_date, data = hotels_train)
lm_huge = lm(children ~ (. - arrival_date)^2, data = hotels_train)
length(coef(lm_big2))
  
engineered_train = hotels_train %>%
  mutate(arrival_date = ymd(arrival_date)) %>%
  mutate(month = month(arrival_date) %>% factor())
lm_big2 = lm(children ~ . - arrival_date, data = engineered_train)

set.seed(523719468)
rmse(lm_small, hotels_train) %>% round(4)
rmse(lm_big, hotels_train)  %>% round(4)
rmse(lm_huge, hotels_train)  %>% round(4)
rmse(lm_big2, engineered_train)  %>% round(4)

engineered_test = hotels_test %>%
  mutate(arrival_date = ymd(arrival_date)) %>%
  mutate(month = month(arrival_date) %>% factor())

rmse(lm_small, hotels_test)  %>% round(4)
rmse(lm_big, hotels_test)  %>% round(4)
rmse(lm_huge, hotels_test)  %>% round(4)
rmse(lm_big2, engineered_test)  %>% round(4)


# Problem 4

# Summaries
epl_2018.19_away %>%
  summarize(awayGF = sum(GF))
epl_2018.19_home %>%
  summarize(homeGF = sum(GF))
avg_goals = (476 + 596)/20

home_goals = 596/(19*20)
away_goals = 476/(19*20)

#Liverpool(home) vs Tottenham(away)
liverpool_as = (55+34)/avg_goals
liverpool_dw = (10+12)/avg_goals
tottenham_as = (33+34)/avg_goals
tottenham_dw = (23+16)/avg_goals

liverpool_goals = home_goals*liverpool_as*tottenham_dw
tottenham_goals = away_goals*tottenham_as*liverpool_dw

liverpool_home = rpois(100000, liverpool_goals)
tottenham_away = rpois(100000, tottenham_goals)

sum(liverpool_home > tottenham_away)/100000
sum(liverpool_home == tottenham_away)/100000
sum(liverpool_home < tottenham_away)/100000

dpois(2, liverpool_goals) * dpois(1, tottenham_goals)

#Man City(home) vs Arsenal(away)
mancity_as = (57+38)/avg_goals
mancity_dw = (12+11)/avg_goals
arsenal_as = (31+42)/avg_goals
arsenal_dw = (35+16)/avg_goals

mancity_goals = home_goals*mancity_as*arsenal_dw
arsenal_goals = away_goals*arsenal_as*mancity_dw

mancity_home = rpois(100000, mancity_goals)
arsenal_away = rpois(100000, arsenal_goals)

sum(mancity_home > arsenal_away)/100000
sum(mancity_home == arsenal_away)/100000
sum(mancity_home < arsenal_away)/100000

dpois(2, mancity_goals) * dpois(1, arsenal_goals)
