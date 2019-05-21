#Distance
  #Load utility libraries
  library(dplyr)
  library(magrittr)
  library(caret)
  #load data library
  library(dslabs)
  data(mnist_27)
  set.seed(0)
  if(!exists("mnist")) mnist <- read_mnist()
  ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
  x <- mnist$train$images[ind,]
  y <- mnist$train$labels[ind]
  y[1:3]
  x_1 <- x[1,]
  x_2 <- x[2,]
  x_3 <- x[3,]
  sqrt(sum((x_1 - x_2)^2))
  sqrt(sum((x_1 - x_3)^2))
  sqrt(sum((x_2 - x_3)^2))
  sqrt(crossprod(x_1 - x_2))
  sqrt(crossprod(x_1 - x_3))
  sqrt(crossprod(x_2 - x_3))
  d <- dist(x)
  class(d)
  as.matrix(d)[1:3,1:3]
  image(as.matrix(d))
  image(as.matrix(d)[order(y),order(y)])