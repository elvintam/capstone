library(tidyverse)
library(dslabs)
library(caret)
library(data.table)

###Quiz: MovieLens Dataset

#Q1
nrow(edx)
ncol(edx)

#Q2
edx %>% filter(rating == 0) %>% nrow()
edx %>% filter(rating == 3) %>% nrow()

#Q3
edx %>% distinct(movieId) %>% nrow()

#Q4
edx %>% distinct(userId) %>% nrow()

#Q5

edx %>% filter(str_detect(genres, "Drama")) %>% nrow()
edx %>% filter(str_detect(genres, "Comedy")) %>% nrow()
edx %>% filter(str_detect(genres, "Thriller")) %>% nrow()
edx %>% filter(str_detect(genres, "Romance")) %>% nrow()

#Q6

movieIDlist <- sapply(c("Forrest Gump", "Pulp Fiction", "Jurassic Park", "Shawshank Redemption", "Speed 2: Cruise Control"), function(x)({
  result <- edx %>% filter(str_detect(title, x)) %>% distinct(movieId)
  result$movieId[1]
})
)

edx %>% filter(movieId %in% movieIDlist) %>% 
  group_by(title) %>% summarise(n = n()) %>% arrange(desc(n))

#Q7

edx %>% group_by(rating) %>% summarise(n = n()) %>% arrange(desc(n)) %>% top_n(5)

#Q8

edx %>% group_by(rating) %>% summarise(n = n()) %>% arrange(desc(rating))

#Q9

edx %>% distinct(genres)

#Q10

edx %>% distinct(userId) %>% nrow()

#Q11

edx %>% distinct(movieId) %>% nrow()

#Q12

edx %>% group_by(userId) %>% summarise(n())

#Q13

edx %>% group_by(userId) %>% summarise(n = n()) %>% filter(n>=50)

#Q14

edx %>% group_by(userId) %>% summarise(n = n()) %>% filter(n>=50)