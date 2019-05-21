# Classification and Regression Trees
library(caret)
data("olive")
head(olive)
table(olive$region)
olive <- select(olive,-area)
# Using KNN
fit <- train(region ~., method = "knn", tuneGrid = data.frame(k = seq(1,15,2)), data = olive )
plot(fit)

ggplot(olive,aes(x=region,palmitic )) + geom_boxplot()
ggplot(olive,aes(x=region,palmitoleic )) + geom_boxplot()

# Using rpart for decision tree
data("polls_2008")
library(rpart)
fit <- rpart(margin ~., data = polls_2008)
plot(fit, margin = 0.1)
text(fit,cex = 0.75)

prune_fit <- prune(fit, cp = 0.01)


library(caret)
train_rpart <- train(margin ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0,0.05,len = 25)), data = polls_2008)
ggplot(train_rpart)

train_rpart <- train(y ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0.0,0.1, len = 25)), data =  mnist_27$train )
ggplot(train_rpart)
confusionMatrix(data = predict(train_rpart, mnist_27$test),reference = mnist_27$test$y )$overall["Accuracy"]
# Using ramdom forest
library(randomForest)
fit <- randomForest(margin ~ ., data = polls_2008)
plot(fit)

train_rf <- randomForest(y ~ ., data = mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

#Using Rborist of caret package
fit <- train(y ~ ., method = "Rborist", tuneGrid = data.frame(predFixed = 2, minNode = seq(5,50)),data = mnist_27$train )

###################################EXERCISE
#Q1
library(rpart)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
fit <- rpart(y ~ ., data = dat)
#Q2
plot(fit)
text(fit)
#Q3
dat %>% mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)

#Q4

library(randomForest)
fit <- randomForest(y ~ x, data = dat)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)
  
#Q5
plot(fit)

#Q6
  library(randomForest)
  fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
  dat %>% 
    mutate(y_hat = predict(fit)) %>% 
    ggplot() +
    geom_point(aes(x, y)) +
    geom_step(aes(x, y_hat), col = 2)  
  
#CARET package
modelLookup("glm") # to know which prameter is optimized or to get info of a method
ggplot(train_knn, highlight = TRUE) # use highlight argument to highlight the parameter that optimizes the algorithm

train_knn <- train(y ~ ., method = "knn", data = mnist_27$train, tuneGrid = data.frame(k=seq(9,71,2)))
ggplot(train_knn, highlight = TRUE) 
train_knn$bestTune # give us the parameters that maximizes the accuracy
train_knn$finalModel # access the best performing model

# change the way to perform crosss validation by using trainControl function
control <- trainControl(method = "cv", number = 10, p = .9) # using 10 fold cross validation, 10% of the observation each
train_knn_cv <- train(y ~ ., method = "knn", data = mnist_27$train, tuneGrid = data.frame(k=seq(9,71,2)))
ggplot(train_knn_cv, highlight = TRUE) 
# using loess to improve wiggly of knn method
modelLookup("gamLoess")
installed.packages("gam")
library(dslabs)
grid <- expand.grid(span = seq(0.15,0.65, len = 10), degree = 1)
train_loess <- train(y ~ ., method = "gamLoess",tuneGrid = grid, data = mnist_27$train)
ggplot(train_loess, hightlight = TRUE)
confusionMatrix(data = predict(train_loess, mnist_27$test), reference = mnist_27$test$y)$overall["Accuracy"]

####Excercise for Caret package###############
#Q1
library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)

library(randomForest)  
library(caret)  
library(Rborist)

n <- 1000
sigma <- 0.25
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x=x, y=y )
set.seed(1)    
fit <- train(y ~ ., method = "Rborist", tuneGrid = data.frame(predFixed = 1, minNode = seq(25,100,25)), data = dat)
ggplot(fit, highlight = TRUE)

#Q2
library(caret)
library(magrittr)
library(dplyr)
dat %>% mutate(y_hat = predict(fit)) %>% ggplot() + geom_point(aes(x,y)) +
  geom_step(aes(x, y_hat), col = 2)

#Q3
library(rpart)
library(caret)
library(dslabs)
set.seed(1991)
data("tissue_gene_expression")

fit <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

ggplot(fit ,highlight = TRUE)    
fit$bestTune

#Q4

confusionMatrix(fit)  

#Q5
library(rpart)
library(caret)
library(dslabs)
set.seed(1991)
data("tissue_gene_expression")

fit_rpart <- with(tissue_gene_expression, 
                  train(x, y, method = "rpart",
                        tuneGrid = data.frame(cp = seq(0, 0.10, 0.01)),
                        control = rpart.control(minsplit = 0)))

ggplot(fit_rpart, highlight = TRUE)
confusionMatrix(fit_rpart)
#Q6

plot(fit_rpart$finalModel)
text(fit_rpart$finalModel)

#Q7

library(randomForest)
set.seed(1991)
library(randomForest)
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rf", 
                  nodesize = 1,
                  tuneGrid = data.frame(mtry = seq(50, 200, 25))))

ggplot(fit)

#Q8
imp <- varImp(fit)

#Q9

data_frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)



