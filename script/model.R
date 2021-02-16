library(tidyverse)
library(caret)
library(data.table)
library(lubridate)



# lambdas <- seq(4, 5, 0.05)

l <- 4.95

# rmses <- sapply(lambdas, function(l) {

mu <- mean(train_set$rating)
#3.512567

max(train_set$year)
#2008

max(train_set$date)
#"2009-01-05 04:52:22 UTC"

model_0 <- RMSE(mu, test_set$rating)

rmse_results <- tibble(method = "Just the average", 
                       RMSE = model_0)

movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu) / (n() + l))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  mutate(pred = ifelse(pred <0, 0, ifelse(pred >5 , 5, pred))) %>%
  pull(pred)


model_1 <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie",  
                                 RMSE = model_1 ))

user_avgs <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i) / (n() + l))

predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  mutate(pred = ifelse(pred <0, 0, ifelse(pred >5 , 5, pred))) %>%
  pull(pred)

model_2 <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User",
                                 RMSE = model_2 ))

genre_avgs <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u) / (n() + l))

predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  mutate(pred = ifelse(pred <0, 0, ifelse(pred >5 , 5, pred))) %>%
  pull(pred)

model_3 <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User + Genre",
                                 RMSE = model_3 ))

### fit_rateperyear model, use 2009 as end since max year is 2008
fit_rateperyear <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(rating = rating - mu - b_i - b_u - b_g) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2009 - min(year),
            rating = mean(rating)) %>%
  mutate(rateperyear = n/years) %>%
  lm(rating ~ rateperyear, data = .)


rateperyear <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(rating = rating - mu - b_i - b_u - b_g) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2009 - min(year),
            rating = mean(rating)) %>%
  mutate(rateperyear = n/years,
         pred = ifelse(n < (mean(rating) - fit_rateperyear$coef[1])/fit_rateperyear$coef[2],
                       fit_rateperyear$coef[1] + fit_rateperyear$coef[2] * rateperyear,
                       rating )) %>%
  mutate(b_r = pred - mean(rating)) %>%
  select(movieId, b_r)


predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  left_join(rateperyear, by='movieId') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_r) %>%
  mutate(pred = ifelse(pred <0, 0, ifelse(pred >5 , 5, pred))) %>%
  pull(pred)


model_4 <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          tibble(method = "Movie + User + Genre + RateperYear",
                                 RMSE = model_4))

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


# ### validation set
# 
# temp <- validation %>%
#   semi_join(train_set, by = "movieId") %>%
#   semi_join(train_set, by = "userId")
# 
# predicted_ratings <- temp %>%
#   left_join(movie_avgs, by='movieId') %>%
#   left_join(user_avgs, by='userId') %>%
#   left_join(genre_avgs, by='genres') %>%
#   left_join(rateperyear, by='movieId') %>%
#   mutate(pred = mu + b_i + b_u + b_g + b_r) %>%
#   pull(pred)
# 
# predicted_ratings <- ifelse(predicted_ratings <0, 0, ifelse(predicted_ratings >5 , 5, predicted_ratings))
# 
# RMSE(predicted_ratings, temp$rating)
# 
# # qplot(predicted_ratings)
# # 
# # min(predicted_ratings)
# # max(predicted_ratings)