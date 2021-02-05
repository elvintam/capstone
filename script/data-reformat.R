library(tidyverse)
library(caret)
library(data.table)
library(lubridate)

head(edx)

temp <- edx

temp <- temp %>% separate(title, into = c("title", "year"), sep = "\\s\\((?=[0-9]{4}\\))", remove = TRUE) %>% 
  mutate(year = as.numeric(str_sub(year, 1, 4))) %>%
  mutate(genrescount = str_count(genres, pattern = "\\|") + 1) %>%
  mutate(date = as_datetime(timestamp)) %>% select(-timestamp)
  
head(temp)

summary(temp)

##genres selection
#temp %>% filter_at(vars(starts_with("genres")), any_vars(. == "Sci-Fi"))

##avg rating for genre
edx %>% filter(str_detect(genres, "Drama")) %>% summarise(mean(rating))


### partition creation
set.seed(23456)
test_index <- createDataPartition(y = temp$rating, times = 1,
                                  p = 0.2, list = FALSE)
test_set <- temp[test_index,]
train_set <- temp[-test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

rm(temp)
rm(test_index)
### end partition creation

### create genre avg rating table
genreavgrating <- train_set %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>% 
  arrange(desc(n))

head(genreavgrating)

### end create genre avg rating table

mu <- mean(train_set$rating)
mu

naive_rmse <- RMSE(mu, test_set$rating)
naive_rmse

###naive + movie

movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

head(movie_avgs)

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
model_1_rmse

###end naive + movie

###naive + movie + user

user_avgs <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

predicted_ratings

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
