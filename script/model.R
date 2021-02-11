library(tidyverse)
library(caret)
library(data.table)
library(lubridate)

train_set %>% summarize(n_distinct(movieId))
#10,638

mu <- mean(train_set$rating)
#3.512567

max(train_set$year)
#2008

max(train_set$date)
#"2009-01-05 04:52:22 UTC"

### fit_rateperyear model, use 2009 as end since max year is 2008
fit_rateperyear <- train_set %>% 
  group_by(movieId) %>%
  summarize(n = n(), years = 2009 - first(year),
            rating = mean(rating)) %>%
  mutate(rateperyear = n/years) %>%
  lm(rating ~ rateperyear, data = .)

### end fit_rateperyear model 

rateperyear_avgs <- test_set %>% semi_join(train_set, by = "movieId") %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2009 - first(year),
            rating = mean(rating)) %>%
  mutate(rateperyear = n/years) %>%
  #mutate(pred = mu) %>%
  mutate(pred = ifelse(n < (mu - fit_rateperyear$coef[1])/fit_rateperyear$coef[2], 
                       fit_rateperyear$coef[1] + fit_rateperyear$coef[2] * rateperyear, 
                       mu)) %>%
  mutate(b_r = pred - mu) %>%
  select(movieId, b_r)

temp <- test_set %>% 
  filter(movieId %in% setdiff(test_set$movieId, train_set$movieId)) %>% 
  group_by(movieId) %>%
  summarize(n = n(), years = 2009 - first(year),
            rating = mean(rating)) %>%
  mutate(rateperyear = n/years) %>%
  mutate(pred = fit_rateperyear$coef[1] + fit_rateperyear$coef[2] * rateperyear) %>%
  mutate(b_r = pred - mu) %>%
  select(movieId, b_r)

rateperyear_avgs <- rbind(rateperyear_avgs, temp)
rm(temp)


predicted_ratings <- mu + test_set %>% 
  left_join(rateperyear_avgs, by='movieId') %>%
  pull(b_r)

model_0 <- RMSE(mu, test_set$rating)
rmse_results <- data_frame(method = "Just the average", 
                           RMSE = model_0)

model_1 <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "RateperYear", 
                                     RMSE = model_1))

options(pillar.sigfig = 7)
#rmse_results

#end rateperyear_avg


head(rateperyear_avgs)

movie_avgs <- test_set %>%
  left_join(rateperyear_avgs, by='movieId') %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu - b_r))

predicted_ratings <- test_set %>% 
  left_join(rateperyear_avgs, by='movieId') %>%
  left_join(movie_avgs, by='movieId') %>%
  mutate(pred = mu + b_r + b_i) %>%
  pull(pred)

model_2 <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="RateperYear + Movie",  
                                     RMSE = model_2 ))

#rmse_results

#end movie_avgs

user_avgs <- test_set %>%
  left_join(rateperyear_avgs, by='movieId') %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_r - b_i))

predicted_ratings <- test_set %>% 
  left_join(rateperyear_avgs, by='movieId') %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_r + b_i + b_u) %>%
  pull(pred)

model_3 <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="RateperYear + Movie + User",  
                                     RMSE = model_3 ))
rmse_result_test <- rmse_results


