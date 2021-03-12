library(tidyverse)
library(caret)
library(data.table)
library(lubridate)


#Data Loading

ratings <- fread(text = gsub("::", "\t", readLines("ml-10M100K/ratings.dat")),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines("ml-10M100K/movies.dat"), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#rm(dl, ratings, movies, test_index, temp, movielens, removed)
rm(ratings, movies, test_index, temp, movielens, removed)


#Data Cleaning
temp <- edx

temp <- temp %>% separate(title, into = c("title", "year"), sep = "\\s\\((?=[0-9]{4}\\))", remove = TRUE) %>%
  mutate(year = as.numeric(str_sub(year, 1, 4))) %>%
  mutate(date = as_datetime(timestamp)) %>% select(-timestamp)


#Create Data Partition
set.seed(12345, sample.kind="Rounding")
test_index <- createDataPartition(y = temp$rating, times = 1,
                                  p = 0.2, list = FALSE)
test_set <- temp[test_index,]
train_set <- temp[-test_index,]

test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

rm(temp, test_index)


### data exploration

#Average Rating for all movie and all user
mu <- mean(train_set$rating)

#use year + 1 as end year to calculate Rate per Year
maxyear <- max(train_set$year) + 1

#cross validated with train set
l <- 2.9

# movie specific effect
movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu) / (n() + l))

# user specific effect
user_avgs <- train_set %>%
  left_join(movie_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i) / (n() + l))

# genre specific effect
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

## Rate per Year specific effect
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


### test set
#Prediction Restraint 0 to 5

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
#Prediction Restraint 0 to 5

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


#NA Handling
removed1 <- validation %>%
  anti_join(temp, by = "movieId") 

removed2 <- validation %>%
  anti_join(temp, by = "userId")

temp <- rbind(temp, removed1, removed2)

predicted_ratings <- c(predicted_ratings, rep(mu, nrow(removed1) + nrow(removed2)))

print("Validation Set")
RMSE(predicted_ratings, temp$rating)

rm(temp, removed1, removed2)
