

# A factor that allows Alice’s rating to (linearly) depend on 
# the (square root of the) number of days since her first 
# rating. (For example, have you ever noticed that you 
# become a harsher critic over time?)

user_1stratedate <- train_set %>%
  group_by(userId) %>%
  summarize(firstratedate = min(date)) %>%
  ungroup()

predndayrating <- function(x, y){
  
  fit <- train_set %>% filter(userId == x) %>%
    left_join(user_1stratedate, by = 'userId') %>%
    mutate(nday = sqrt(interval(firstratedate, date) / ddays(1))) %>%
    lm(rating ~ nday, data = .)
  
  fit$coef[1] + fit$coef[2] * sqrt(y)
}


predicted_ratings <- test_set %>%
  left_join(rateperyear_avgs, by='movieId') %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(user_1stratedate, by = 'userId') %>%
  mutate(nday = sqrt(ifelse(date < firstratedate, 0 ,interval(firstratedate, date) / ddays(1))),
         pred = mu - b_r - b_i - b_u - (predndayrating(userId, nday) - mu - b_r - b_i - b_u) ) %>%
  pull(pred)

predicted_ratings <- ifelse(predicted_ratings <0, 0, ifelse(predicted_ratings >5 , 5, predicted_ratings))

model_3a <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="RateperYear + Movie + User + User nday",
                                 RMSE = model_3a ))