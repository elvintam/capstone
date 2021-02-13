library(tidyverse)
library(caret)
library(data.table)
library(lubridate)

# ### pre-processing
# temp <- edx
# 
# temp <- temp %>% separate(title, into = c("title", "year"), sep = "\\s\\((?=[0-9]{4}\\))", remove = TRUE) %>%
#   mutate(year = as.numeric(str_sub(year, 1, 4))) %>%
#   mutate(genrescount = str_count(genres, pattern = "\\|") + 1) %>%
#   mutate(date = as_datetime(timestamp)) %>% select(-timestamp)
# ### pre-processing
# 
# ### partition creation
# set.seed(78789, sample.kind="Rounding")
# test_index <- createDataPartition(y = temp$rating, times = 1,
#                                   p = 0.2, list = FALSE)
# test_set <- temp[test_index,]
# train_set <- temp[-test_index,]
# 
# test_set <- test_set %>%
#   semi_join(train_set, by = "movieId") %>%
#   semi_join(train_set, by = "userId")
# 
# rm(temp, test_index)
# ### end partition creation

mu <- mean(train_set$rating)

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
  mutate(pred = ifelse(n < (mu - fit_rateperyear$coef[1])/fit_rateperyear$coef[2], 
                       fit_rateperyear$coef[1] + fit_rateperyear$coef[2] * rateperyear, 
                       mu)) %>%
  mutate(b_r = pred - mu) %>%
  select(movieId, b_r)

movie_avgs <- train_set %>%
  left_join(rateperyear_avgs, by='movieId') %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu - b_r))

user_avgs <- train_set %>%
  left_join(rateperyear_avgs, by='movieId') %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_r - b_i))

genre_avgs <- train_set %>%
  left_join(rateperyear_avgs, by='movieId') %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_r - b_i - b_u))

predicted_ratings <- test_set %>% 
  left_join(rateperyear_avgs, by='movieId') %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_r + b_i + b_u + b_g) %>%
  pull(pred)

RMSE(predicted_ratings, test_set$rating)