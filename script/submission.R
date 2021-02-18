library(tidyverse)
library(caret)
library(data.table)
library(lubridate)

### pre-processing
temp <- edx

temp <- temp %>% separate(title, into = c("title", "year"), sep = "\\s\\((?=[0-9]{4}\\))", remove = TRUE) %>%
  mutate(year = as.numeric(str_sub(year, 1, 4))) %>%
  mutate(date = as_datetime(timestamp)) %>% select(-timestamp)
### end pre-processing

### partition creation
set.seed(12345, sample.kind="Rounding")
test_index <- createDataPartition(y = temp$rating, times = 1,
                                  p = 0.2, list = FALSE)
test_set <- temp[test_index,]
train_set <- temp[-test_index,]

test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

rm(temp, test_index)
### end partition creation

mu <- mean(train_set$rating)

max(train_set$year)

max(train_set$date)

l <- 4.95
#cross validated with test set

movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu) / (n() + l))

user_avgs <- train_set %>%
  left_join(movie_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i) / (n() + l))

genre_avgs <- train_set %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u) / (n() + l))

### Rate per year model, use 2009 as end year since max year is 2008
fit_rateperyear <- train_set %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  left_join(genre_avgs, by="genres") %>%
  mutate(rating = rating - mu - b_i - b_u - b_g) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2009 - min(year),
            rating = mean(rating)) %>%
  mutate(rateperyear = n/years) %>%
  lm(rating ~ rateperyear, data = .)

rateperyear <- train_set %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  left_join(genre_avgs, by="genres") %>%
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


### test set

predicted_ratings <- test_set %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  left_join(genre_avgs, by="genres") %>%
  left_join(rateperyear, by="movieId") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_r) %>%
  mutate(pred = ifelse(pred <0, 0, ifelse(pred >5 , 5, pred))) %>%
  pull(pred)

print("Test Set")
RMSE(predicted_ratings, test_set$rating)


### validation set

temp <- validation %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

predicted_ratings <- temp %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  left_join(genre_avgs, by="genres") %>%
  left_join(rateperyear, by="movieId") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_r) %>%
  mutate(pred = ifelse(pred <0, 0, ifelse(pred >5 , 5, pred))) %>%
  pull(pred)

removed <- validation %>%
  anti_join(temp, by = "movieId")

temp <- rbind(temp, removed)

predicted_ratings <- c(predicted_ratings, rep(mu, nrow(removed)))

print("Validation Set")
RMSE(predicted_ratings, temp$rating)

rm(temp, removed)
