library(tidyverse)
library(caret)
library(data.table)
library(lubridate)

train_set %>% summarize(n_distinct(movieId))
#10,638

mean(train_set$rating)
#3.512567


fit_mu_by_rate <- train_set %>% 
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  lm(rating ~ rate, data = .)

fit_mu_by_rate$coef

k <- anti_join(test_set, train_set, by = "movieId")

s <-setdiff(test_set$movieId, train_set$movieId)

test_set %>% filter(movieId %in% s) %>% group_by(movieId) %>%
  summarise(n = n())

#rating predict(fit, rate)


test_set_rate <- rbind(train_set, test_set) %>% 
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  mutate(rating_by_rate = fit_mu_by_rate$coef[1] + 
           fit_mu_by_rate$coef[2] * rate) %>%
  ungroup()

test_set_rate


predicted_ratings <- test_set %>% 
  group_by(movieId) %>%
  summarize(n = n()) %>% ungroup()

predicted_ratings

%>%
  left_join(test_set_rate, by='movieId') 

predicted_ratings

%>%
  pull(rating_by_rate)

mu_by_rate_rmse <- RMSE(predicted_ratings, test_set$rating)
mu_by_rate_rmse
