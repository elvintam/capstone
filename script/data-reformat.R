library(tidyverse)
#library(dslabs)
library(caret)
library(data.table)

head(edx)

temp <- edx

temp %>% separate(title, into = c("titletemp", "year"), sep = "\\s\\((?=[0-9]{4}\\))", remove = TRUE)

help(str_split)
help(separate)

edx[7853]

###breakdown genres
temp <- temp %>% mutate(genrescount = str_count(edx$genres, pattern = "\\|") + 1) %>%
  separate(genres, into = c("genres1", "genres2", "genres3", 
                            "genres4","genres5", "genres6",
                            "genres7", "genres8"), 
                  sep = "\\|", remove = TRUE)

##genres selection
temp %>% filter_at(vars(starts_with("genres")), any_vars(. == "Sci-Fi"))

##avg rating for genre
#edx %>% filter(str_detect(genres, "Drama")) %>% summarise(mean(rating))

genrelist <- c(unique(temp$genres1), unique(temp$genres2),
               unique(temp$genres3), unique(temp$genres4),
               unique(temp$genres5), unique(temp$genres6),
               unique(temp$genres7), unique(temp$genres8))

genrelist <- unique(genrelist)

genreavgrating <- sapply(genrelist, function(x){
  result <- edx %>% filter(str_detect(genres, x)) %>% 
    summarise(avg = mean(rating)) %>% .$avg
  result
})

genreavgrating <- as.data.frame(genreavgrating)

genreavgrating

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
