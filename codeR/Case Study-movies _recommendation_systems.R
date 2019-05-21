library(dslabs)
library(caret)
data("movielens")
head(movielens)
class(movielens)
###############################################################################
## About Tidyverse package
#                The Tidyverse empowers:
#   - Data manipulation (dplyr, tidyr)
#   - Working with data types (stringr for strings, lubridate for date/datetime, 
#     forcats for categorical/factors)
#   - Data visualization (ggplot2)
#   - Data-oriented programming (purrr, tidyeval)
#   - Communication (Rmarkdown, shiny)
###############################################################################
library(caret)
library(tidyverse)
library(dslabs)
data("movielens")

movielens %>% as_tibble()

movielens %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2, list = FALSE )
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

test_set <- test_set %>%
  semi_join( train_set, by = "movieId") %>%
  semi_join( train_set, by = "userId")

RMSE <- function(true_ratings, predicted_ratings){
  
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

mu_hat <- mean(train_set$rating)
mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

predictions <- rep(2.5, nrow(test_set))
RMSE(test_set$rating, predictions)

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
rmse_results

fit <- lm(rating ~ as.factor(userId), data = movielens)

mu <- mean(train_set$rating)
mu
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu))
movie_avgs

movie_avgs %>% qplot(b_i, geom = "histogram", bins = 20, data = ., color = I("black"))

predicted_ratings <- mu + test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  pull(b_i)

predicted_ratings
model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, data_frame(method = "Movie Effect Model", 
                                                   RMSE = model_1_rmse))
# compute the average rating for user u for those that have rated over 100 movies
train_set %>% 
  group_by(userId) %>% 
  summarise(b_u = mean(rating)) %>%
  filter(n() >= 100) %>% ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black")

lm(rating ~ as.factor(movieId) + as.factor(userId))

user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
model_2_rmse

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse))
rmse_results 

####Exercises

#Q1
library(caret)
library(tidyverse)
library(dslabs)
library(dplyr)
data("movielens")

movielens %>% 
  group_by(movieId,year) %>%
  summarize(n = n()) %>%
  group_by(year) %>% 
  ggplot(aes(x=year, y=sqrt(n), fill=year)) + 
  geom_col() + 
  ggtitle("Movies") +
  scale_fill_gradientn(colours = rainbow(100)) +
  scale_x_continuous(breaks = seq(1900,2020, 10)) +
  theme(legend.position="none")
# OR

movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
#Q2

movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate)) %>%
  
  

# Q3

  movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2017 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

# Q4

# Because a lack of ratings is associated with lower ratings, 
# it would be most appropriate to fill in the missing value with a lower value than the average. 
# You should try out different values to fill in the missing value and evaluate prediction in a test set.

#Q5

library(lubridate)
movielens <- mutate(movielens, date = as_datetime(timestamp))
movielens

#Q6

movielens %>% mutate( week = week(date)) %>%
              mutate( year_rate = as.numeric(format(date,'%Y'))) %>%
              mutate(week_year = paste(week,year_rate,sep = "_")) %>%
              filter(year >= 2015) %>%
              group_by(week) %>%
              summarize(
             rating = mean(rating)) %>%
            arrange(week) %>%
            ggplot(aes(x = week, y = rating )) +
            geom_line() + 
            geom_smooth()


filter(title == "Deadpool") %>%
  filter(title == "Zoolander 2") %>%
  filter(title == "Batman v Superman: Dawn of Justice") %>%
  
# Testing

library(tidyverse)
library(dslabs)
data(movielens)
library(lubridate)
  
movielens <- mutate(movielens, dates = as_datetime(timestamp))
movielens <- mutate(movielens, date = round_date(dates, unit = "week"))  
  
tib <- movielens %>% group_by(date) %>% summarize(rating = mean(rating)) 
mean <-  mean(tib$rating)
mean 
  
## Regressing rating on date
lm1 <- lm(rating~date,data = tib)
fitted <-  lm1$fitted.values
summary(lm1)
tib <- mutate(tib, lm1 = fitted)  

## Using smoothing spline to approximate time trend
## Asking for cross-validation
spline <- smooth.spline(x = tib$date, y = tib$rating, cv = TRUE)
spline
fitted <- fitted(spline)
tib <- mutate(tib, spline = fitted)

## Regressing rating on time trend
lm2 <- lm(rating~spline, data = tib)
summary(lm2)

## Plotting results
tib <- mutate(tib, lm2 = lm2$fitted.values)
tib %>% ggplot(aes(x = date)) + 
  geom_point(aes(y = rating)) +
  geom_line(aes(y = lm1), size = 2, col = "red") +
  geom_line(aes(y = lm2), size = 2, col = "blue") +
  geom_abline(intercept = mean, slope = 0, linetype=2, size = 2, col = "green")

# Q8

movielens %>% 
  group_by(genres) %>%
  summarize(n = n(),rating = mean(rating)) %>%
  filter (n >=1000) %>% 
  mutate(standard_error = sd(rating)/sqrt(n)) %>%
  ggplot(aes(y = genres, x = rating )) +
  geom_point() + 
  geom_smooth()

movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



