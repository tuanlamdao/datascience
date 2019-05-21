#Regularization
library(caret)
library(tidyverse)
library(dslabs)
library(dplyr)
library(lubridate)

set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2, list = FALSE )
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

mu <- mean(train_set$rating)
mu

movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu))
movie_avgs


test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>% 
  select(title,  residual) %>% slice(1:10) 
test_set

movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()
movie_titles

# 10 best movies
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i) %>% 
  slice(1:10) 

# 10 worst movies
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i) %>% 
  slice(1:10) 
movie_avgs

train_set %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) 


train_set %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10)

train_set %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) 



lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

data_frame(original = movie_avgs$b_i, 
           regularlized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10)

train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) 


predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(data_frame(method="Regularized Movie Effect Model",  RMSE = model_3_rmse))
rmse_results 


lambdas <- seq(0, 10, 0.25)
mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())

rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]



lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)  



#Exercise

#Regularization
library(caret)
library(tidyverse)
library(dslabs)
library(dplyr)
library(lubridate)

# Q1
   #Simulate number of students in each school
set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))
    #assign TRUE quality for each school
set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:100),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

   # Top 10 schools
schools %>% top_n(10, quality) %>% arrange(desc(quality))

  # Now let's have the students in the school take a test,
  # simulate the test scores as normally distributed with the average determined by the school quality 
  # with a standard deviation of 30 percentage points. 

set.seed(1)
scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))


   #Top 10 schools with sorted by avg score

top_10_schools <- schools %>% arrange(desc(score)) %>% slice(1:10) 

   #OR schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score)


#Q2
  # median schools size overall  
school_size_overall <- schools %>% select(size)
class(school_size_overall$size)
median_school_size_overall <- median(school_size_overall$size)
median_school_size_overall 
 
  # median schools size of top 10 by avg score

top_10_schools_size <- top_10_schools %>% select(size) %>% arrange(size)
median_top_10_school_size <- median(top_10_schools_size$size)
median_top_10_school_size 

median(c(54 ,  61,   64,   86 , 121,  151,  235,  341,  550, 1036))

   #OR median(schools$size)  
   #   schools %>% top_n(10, score) %>% .$size %>% median()

#Q3

 # 10 worst schools by score 
ten_worst_schools <- schools %>% arrange(score) %>% slice(1:10) %>% arrange(size)

median_ten_worst_schools_size <- median(ten_worst_schools$size)
median_ten_worst_schools_size

  # OR median(schools$size)
  #    schools %>% top_n(-10, score) %>% .$size %>% median()

#Q4
top_10_schools_by_quality <- schools %>% arrange(desc(quality)) %>% slice(1:10) %>% arrange(quality)

schools %>% select(size, score) 
  ggplot(schools,aes(x = size, y = score)) + geom_point() +
  geom_point(data = top_10_schools_by_quality , aes(x = size, y = score), colour = "red", size = 5)
  
  # OR 
  schools %>% ggplot(aes(size, score)) +
    geom_point(alpha = 0.5) +
    geom_point(data = filter(schools, rank<=10), col = 2) 

#Q5
  # Define the overall average score for all schools
  overall <- mean(sapply(scores, mean))
  alp <- 25

    schools_Q5 <- schools %>% 
    mutate(reg_score = overall + ((score - overall) * size/(size + alp))) %>% 
    arrange(desc(reg_score)) %>% slice(1:10)
    
    RMSE = function(q, e){
      mean((q - e)^2)
    }
    
    RMSE(schools_Q5$quality, schools_Q5$reg_score)   
    
      
#Q6
  
  alpha <- seq(10, 250)
  overall <- mean(sapply(scores, mean))
  rmse_Q6 <- sapply(alpha, function(alp){
    schools_Q6<- schools %>% 
    mutate(reg_score = overall + ((score - overall) * size/(size + alp))) %>% 
    arrange(desc(reg_score)) # %>% slice(1:10) 
    RMSE(schools_Q6$quality,schools_Q6$reg_score)
  })
  qplot(alpha, rmse_Q6)  
  alpha[which.min(rmse_Q6)]
  
#Q7
  
  overall <- mean(sapply(scores, mean))
  alp <- 128
  
  schools_Q7 <- schools %>% 
    mutate(reg_score = overall + ((score - overall) * size/(size + alp))) %>% 
    arrange(desc(reg_score)) %>% slice(1:10)  
  
#Q8
  
  alpha <- seq(10, 250)
  overall <- mean(sapply(scores, mean))
  #overall <- 0
  rmse_Q6 <- sapply(alpha, function(alp){
    schools_Q6<- schools %>% 
      mutate(reg_score = overall + ((score - 0) * size/(size + alp))) %>% 
      arrange(desc(reg_score)) # %>% slice(1:10) 
    RMSE(schools_Q6$quality,schools_Q6$reg_score)
  })
  qplot(alpha, rmse_Q6)  
  alpha[which.min(rmse_Q6)]
 