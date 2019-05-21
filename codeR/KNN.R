#KNN
  # Load ultility libraries
  library(caret)
  library(magrittr)
  library(dplyr)
  #Load data library
  library(dslabs)
  data("mnist_27")
 
  set.seed(0)
  if(!exists("mnist")) mnist <- read_mnist()
  ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
  x <- mnist$train$images[ind,]
  y <- mnist$train$labels[ind]
  y[1:3]
  x_1 <- x[1,]
  x_2 <- x[2,]
  x_3 <- x[3,]
  #predict using logistic regression
  fit_glm <- glm(y ~ x_1 + x_2, data = mnist_27$train, family = "binomial")
  p_hat_logistic <- predict(fit_glm,mnist_27$test)
  y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
  confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)
  
  #predict using KNN   
    # using test set 
  knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 5) # also can use x<- as.matrix(mnist_27$train[,2:3]), y<- mnist_27$train$y, knn_fit <- knn3(x,y)
  y_hat_knn <- predict(knn_fit,mnist_27$test, type = "class")
  confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)
  # using train set
  knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 5) # also can use x<- as.matrix(mnist_27$train[,2:3]), y<- mnist_27$train$y, knn_fit <- knn3(x,y)
  y_hat_knn <- predict(knn_fit,mnist_27$train, type = "class")
  
  ks <- seq(3, 251, 2)
  library(purrr)
  accuracy <- map_df(ks, function(k){
    fit <- knn3(y ~ ., data = mnist_27$train, k = k)
    y_hat <- predict(fit, mnist_27$train, type = "class")
    train_error <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)$overall["Accuracy"]
    
    y_hat <- predict(fit, mnist_27$test, type = "class")
    test_error <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]
    list(train = train_error, test = test_error)
    
  })
  # draw 2 lines in the same chart 
  ggplot(accuracy,aes(x=ks)) + geom_line(aes(y = train), colour = "red") + geom_line(aes(y = test), colour = "green")
  
  
  

  
  