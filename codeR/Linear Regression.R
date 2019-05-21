# Linear Regression
  # load utility library

library(magrittr)
library(caret)
library(purrr)
library(dplyr)
  # load data library
library(HistData)

set.seed(2)
galton_heights <- GaltonFamilies %>% filter (childNum == 1 & gender == "male") %>% select(father,childHeight) %>% rename(son = childHeight)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set  <- galton_heights %>% slice(-test_index)
test_set   <- galton_heights %>% slice(test_index)
avg <- mean(train_set$son)
mean((avg - test_set$son)^2)

fit <- lm(son~father, data = train_set)
fit$coefficients
y_hat <- fit$coefficients[1] + fit$coefficients[2]*test_set$father
mean((y_hat - test_set$son)^2)
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)
   # using predict function, can run command ?predict.lm for getting guidelines document
y_hat <- predict(fit, test_set)
   # back to data set heights for talking about linear regression
library(dslabs)
data("heights")
y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set  <- heights %>% slice(-test_index)
test_set   <- heights %>% slice(test_index)
   # by computing average of these values, we get the conditional probability 
train_set %>% filter(round(height) == 66) %>% summarise(mean(sex == "Female"))
   # convert factore male/female to 0/1 
lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% lm(y ~ height, data = .)
p_hat <- predict(lm_fit,test_set)
y_hat <- ifelse(p_hat > 0.5, "Female","Male") %>% factor()
confusionMatrix(y_hat,test_set$sex)




