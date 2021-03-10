library(tidyverse)
library(caret)
library(data.table)
library(lubridate)

head(edx)

### pre-processing
temp <- edx

temp <- temp %>% separate(title, into = c("title", "year"), sep = "\\s\\((?=[0-9]{4}\\))", remove = TRUE) %>%
  mutate(year = as.numeric(str_sub(year, 1, 4))) %>%
  mutate(date = as_datetime(timestamp)) %>% select(-timestamp)
### end pre-processing

head(edx)

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

head(train_set)

mu <- mean(train_set$rating)

maxyear <- max(train_set$year) + 1


### data exploration


# movie specific bias

train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu)) %>%
  summarize(avg = mean(b_i)) %>% pull(avg)

train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu)) %>%
  ggplot(aes(b_i)) + 
    geom_histogram(bins = 10, color = "black")

# user specific bias

train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu)) %>%
  summarize(avg = mean(b_u)) %>% pull(avg)

train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu)) %>% 
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 10, color = "black")

## genre specific bias
train_set %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating - mu), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 20000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Rate per Year specific bias
train_set %>% 
  group_by(movieId) %>%
  summarize(n = n(), years = maxyear - min(year),
            rating = mean(rating - mu)) %>%
  mutate(rateperyear = n/years) %>%
  ggplot(aes(rateperyear, rating)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 0, color = "red", size = 1, linetype = "dashed") +
  geom_text(label = "mu", x= 1700, y = 0.15, color = "red")


## regularization

lambdas <- seq(2.5, 3.5, 0.1)

rmses <- sapply(lambdas, function(l) {
  
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
  
  ### Rate per year model
  fit_rateperyear <- train_set %>%
    left_join(movie_avgs, by="movieId") %>%
    left_join(user_avgs, by="userId") %>%
    left_join(genre_avgs, by="genres") %>%
    mutate(rating = rating - mu - b_i - b_u - b_g) %>%
    group_by(movieId) %>%
    summarize(n = n(), years = maxyear - min(year),
              rating = mean(rating)) %>%
    mutate(rateperyear = n/years) %>%
    lm(rating ~ rateperyear, data = .)
  
  rateperyear <- train_set %>%
    left_join(movie_avgs, by="movieId") %>%
    left_join(user_avgs, by="userId") %>%
    left_join(genre_avgs, by="genres") %>%
    mutate(rating = rating - mu - b_i - b_u - b_g) %>%
    group_by(movieId) %>%
    summarize(n = n(), years = maxyear - min(year),
              rating = mean(rating)) %>%
    mutate(rateperyear = n/years,
           pred = ifelse(n < (mean(rating) - fit_rateperyear$coef[1])/fit_rateperyear$coef[2],
                         fit_rateperyear$coef[1] + fit_rateperyear$coef[2] * rateperyear,
                         rating )) %>%
    mutate(b_r = pred - mean(rating)) %>%
    select(movieId, b_r)

  predicted_ratings <- train_set %>%
    left_join(movie_avgs, by="movieId") %>%
    left_join(user_avgs, by="userId") %>%
    left_join(genre_avgs, by="genres") %>%
    left_join(rateperyear, by="movieId") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_r) %>%
    mutate(pred = ifelse(pred <0, 0, ifelse(pred >5 , 5, pred))) %>%
    pull(pred)
  
  
  return(RMSE(predicted_ratings, train_set$rating))
  
  })

min(rmses)
lambdas[which.min(rmses)]

ggplot(data = data.frame(lambdas, rmses), aes(lambdas, rmses)) + 
  geom_point() +
  geom_text(label = "2.9", x = lambdas[which.min(rmses)], y = min(rmses) + 0.000001)





