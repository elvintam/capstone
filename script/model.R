library(tidyverse)
library(caret)
library(data.table)
library(lubridate)

train_set %>% summarize(n_distinct(movieId))
#10,638

train_set_mu <- mean(train_set$rating)
#3.512567

max(train_set$year)
#2008

max(train_set$date)
#"2009-01-05 04:52:22 UTC"

### fit_mu_by_rate model, use 2009 as end since max year is 2008
fit_mu_by_rate <- train_set %>% 
  group_by(movieId) %>%
  summarize(n = n(), years = 2009 - first(year),
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  lm(rating ~ rate, data = .)

### end fit_mu_by_rate model 

mu <- test_set %>% semi_join(train_set, by = "movieId") %>%
  distinct(movieId) %>% mutate(mu = train_set_mu) 

  
temp <- test_set %>% 
  filter(movieId %in% setdiff(test_set$movieId, train_set$movieId)) %>% 
  group_by(movieId) %>%
  summarize(n = n(), years = 2009 - first(year),
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>% 
  mutate(mu = fit_mu_by_rate$coef[1] + fit_mu_by_rate$coef[2]* rate) %>%
  select(movieId, mu)

mu <- rbind(mu, temp)

rm(temp)
mu

#rating predict(fit, rate)


model_0 <- RMSE(train_set_mu, test_set$rating)
rmse_results <- data_frame(method = "Just the average", 
                           RMSE = model_0)


model_1 <- RMSE(test_set %>% left_join(mu, by = "movieId") %>% pull(mu),
              test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Avg with NA replace", 
                                     RMSE = model_1))

options(pillar.sigfig = 7)
rmse_results
