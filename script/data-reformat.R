library(tidyverse)
library(dslabs)
library(caret)
library(data.table)

head(edx)

temp <- edx

temp <- temp %>% mutate(genrescount = str_count(edx$genres, pattern = "\\|") + 1) %>%
  separate(genres, into = c("genres1", "genres2", "genres3", 
                            "genres4","genres5", "genres6",
                            "genres7", "genres8"), 
                  sep = "\\|", remove = TRUE)

##genres selection
temp %>% filter_at(vars(starts_with("genres")), any_vars(. == "Sci-Fi"))

temp %>% filter_at(vars(starts_with("genres")), any_vars(. == "Drama")) %>% mean(rating)

edx %>% mean(rating)
  
class(temp$rating)
