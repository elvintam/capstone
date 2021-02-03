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

##genres selection
temp %>% filter_at(vars(starts_with("genres")), any_vars(. == "Sci-Fi"))

##avg rating for genre
#edx %>% filter(str_detect(genres, "Drama")) %>% summarise(mean(rating))

genrelist <- unique(temp$genres)

genreavgrating <- temp %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n()))

genreavgrating <- as.data.frame(genreavgrating) 

genreavgrating %>% arrange(desc(n))

remove(temp)

### partition creation
set.seed(23456)
test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.2, list = FALSE)
test_set <- edx[test_index,]
train_set <- edx[-test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

### partition creation

edx %>% summarise(n_distinct(movieId))
edx %>% summarise(n_distinct(genres))

edx %>% distinct(genres)
