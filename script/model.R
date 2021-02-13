library(tidyverse)
library(caret)
library(data.table)
library(lubridate)


#lambdas <- seq(3, 6, 0.25)

lambdas <- 4.75
l <- lambdas

# rmses <- sapply(lambdas, function(l) {

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

rateperyear_avgs <- train_set %>% 
  group_by(movieId) %>%
  summarize(n = n(), years = 2009 - first(year),
            rating = mean(rating)) %>%
  mutate(rateperyear = n/years) %>%
  #mutate(pred = mu) %>%
  mutate(pred = ifelse(n < (mu - fit_rateperyear$coef[1])/fit_rateperyear$coef[2], 
                       fit_rateperyear$coef[1] + fit_rateperyear$coef[2] * rateperyear, 
                       mu)) %>%
  mutate(b_r = pred - mu) %>%
  #mutate(b_r = sum(pred - mu) / (n() + l)) %>%
  select(movieId, b_r)

rateperyear_avgs

 # temp <- test_set %>%
 #   filter(movieId %in% setdiff(test_set$movieId, train_set$movieId)) %>%
 #   group_by(movieId) %>%
 #   summarize(n = n(), years = 2009 - first(year),
 #             rating = mean(rating)) %>%
 #   mutate(rateperyear = n/years) %>%
 #   mutate(pred = fit_rateperyear$coef[1] + fit_rateperyear$coef[2] * rateperyear) %>%
 #   mutate(b_r = pred - mu) %>%
 #   select(movieId, b_r)
 # 
 # rateperyear_avgs <- rbind(rateperyear_avgs, temp)
 # rm(temp)


predicted_ratings <- mu + test_set %>% 
  left_join(rateperyear_avgs, by='movieId') %>%
  pull(b_r)

predicted_ratings <- ifelse(predicted_ratings <0, 0, ifelse(predicted_ratings >5 , 5, predicted_ratings))

model_0 <- RMSE(mu, test_set$rating)
rmse_results <- tibble(method = "Just the average", 
                       RMSE = model_0)

model_1 <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          tibble(method = "RateperYear", 
                                 RMSE = model_1))

#options(pillar.sigfig = 7)
#rmse_results

#end rateperyear_avg


#head(rateperyear_avgs)


movie_avgs <- train_set %>%
  left_join(rateperyear_avgs, by='movieId') %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu - b_r) / (n() + l))

predicted_ratings <- test_set %>% 
  left_join(rateperyear_avgs, by='movieId') %>%
  left_join(movie_avgs, by='movieId') %>%
  mutate(pred = mu + b_r + b_i) %>%
  pull(pred)

predicted_ratings <- ifelse(predicted_ratings <0, 0, ifelse(predicted_ratings >5 , 5, predicted_ratings))

model_2 <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="RateperYear + Movie",  
                                 RMSE = model_2 ))

#end movie_avgs

user_avgs <- train_set %>%
  left_join(rateperyear_avgs, by='movieId') %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_r - b_i) / (n() + l))

predicted_ratings <- test_set %>% 
  left_join(rateperyear_avgs, by='movieId') %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_r + b_i + b_u) %>%
  pull(pred)

predicted_ratings <- ifelse(predicted_ratings <0, 0, ifelse(predicted_ratings >5 , 5, predicted_ratings))

model_3 <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="RateperYear + Movie + User",  
                                 RMSE = model_3 ))
#rmse_results

#end user_avgs

genre_avgs <- train_set %>%
  left_join(rateperyear_avgs, by='movieId') %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_r - b_i - b_u) / (n() + l))

predicted_ratings <- test_set %>% 
  left_join(rateperyear_avgs, by='movieId') %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_r + b_i + b_u + b_g) %>%
  pull(pred)

predicted_ratings <- ifelse(predicted_ratings <0, 0, ifelse(predicted_ratings >5 , 5, predicted_ratings))

model_4 <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="RateperYear + Movie + User + Genre",  
                                 RMSE = model_4 ))
rmse_results

# return(model_4)
# })

# qplot(lambdas, rmses)
# 
# min(rmses)
# lambdas[which.min(rmses)]

qplot(predicted_ratings)

min(predicted_ratings)
max(predicted_ratings)
