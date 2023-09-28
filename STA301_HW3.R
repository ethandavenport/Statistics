library(tidyverse)
library(mosaic)
library(MatchIt)
library(effectsize)


# Problem 1
2021*0.024
sim_iron = do(10000)*nflip(n = 2021, prob = 0.024)
ggplot(sim_iron) +
  geom_histogram(aes(x = nflip), binwidth = 1)
sum(sim_iron >= 70)/10000


# Problem 2
# Part A
filter_A = nbc_pilotsurvey %>%
  filter(Show == "Living with Ed" | Show == "My Name is Earl")
t.test(Q1_Happy ~ Show, data = filter_A)

# Part B
filter_B = nbc_pilotsurvey %>%
  filter(Show == "The Biggest Loser" | Show == "The Apprentice: Los Angeles")
t.test(Q1_Annoyed ~ Show, data = filter_B)

# Part C
filter_C = nbc_pilotsurvey %>%
  filter(Show == "Dancing with the Stars") %>%
  mutate(Agree = ifelse(Q2_Confusing >= 4, yes = TRUE, no = FALSE))
prop.test(~Agree, data = filter_C)


# Problem 3
ebay_ratio = ebay %>%
  mutate(rev_ratio = rev_after/rev_before)
t.test(rev_ratio ~ adwords_pause, data = ebay_ratio)


# Problem 4
# Part A
turnout_GOTV = turnout %>%
  filter(GOTV_call == 1)
prop(~voted1998, data = turnout_GOTV)

turnout_nonGOTV = turnout %>%
  filter(GOTV_call == 0)
prop(~voted1998, data = turnout_nonGOTV)

prop.test(voted1998 ~ GOTV_call, data = turnout)

# Part B
prop(GOTV_call ~ voted1996, data = turnout)
prop.test(GOTV_call ~ voted1996, data = turnout)
prop(voted1998 ~ voted1996, data = turnout)
prop.test(voted1998 ~ voted1996, data = turnout)

mean(AGE ~ GOTV_call, data = turnout)
t.test(AGE ~ GOTV_call, data = turnout)
ggplot(turnout) +
  geom_boxplot(aes(x = factor(GOTV_call), y = AGE))
mean(AGE ~ voted1998, data = turnout)
t.test(AGE ~ voted1998, data = turnout)
ggplot(turnout) +
  geom_boxplot(aes(x = factor(voted1998), y = AGE))

prop(GOTV_call ~ MAJORPTY, data = turnout)
prop.test(GOTV_call ~ MAJORPTY, data = turnout)
prop(voted1998 ~ MAJORPTY, data = turnout)
prop.test(voted1998 ~ MAJORPTY, data = turnout)

# Part C
turnout_match = matchit(GOTV_call ~ voted1996 + AGE + MAJORPTY,
                        data = turnout, ratio = 5)
summary(turnout_match)
turnout_matched = match.data(turnout_match)

matched_GOTV = turnout_matched %>%
  filter(GOTV_call == 1)
prop(~voted1998, data = matched_GOTV)

matched_nonGOTV = turnout_matched %>%
  filter(GOTV_call == 0)
prop(~voted1998, data = matched_nonGOTV)

prop.test(voted1998 ~ GOTV_call, data = turnout_matched)


# Problem 5

# Part A
solder_opening = solder %>%
  group_by(Opening) %>%
  summarize(mean_skips = mean(skips))
ggplot(solder_opening) +
  geom_col(aes(x = Opening, y = mean_skips))

solder_solder = solder %>%
  group_by(Solder) %>%
  summarize(mean_skips = mean(skips))
ggplot(solder_solder) +
  geom_col(aes(x = Solder, y = mean_skips))

# Part B
model = lm(skips ~ Opening + Solder + Opening:Solder, data = solder)
confint(model)
model

