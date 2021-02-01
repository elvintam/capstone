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

edx %>% filter(str_detect(genres, "Drama")) %>% summarise(mean(rating))



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
