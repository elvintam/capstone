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

##avg rating for genre
#edx %>% filter(str_detect(genres, "Drama")) %>% summarise(mean(rating))


### partition creation, previous one 1234
set.seed(78789, sample.kind="Rounding") 
test_index <- createDataPartition(y = temp$rating, times = 1,
                                  p = 0.2, list = FALSE)
test_set <- temp[test_index,]
train_set <- temp[-test_index,]

## to remove NA
test_set <- test_set %>%
 semi_join(train_set, by = "movieId") %>%
 semi_join(train_set, by = "userId")
## end to remove NA

rm(temp)
rm(test_index)
### end partition creation


### data explory

summary(train_set)

train_set %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




### rating based on rate chart

train_set %>% 
  group_by(movieId) %>%
  summarize(n = n(), years = 2009 - min(year),
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth(method = "lm")

#end chart

train_set %>% 
  mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

