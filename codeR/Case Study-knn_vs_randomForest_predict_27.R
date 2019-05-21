library(caret)
library(magrittr)
library(dplyr)
library(dslabs)
library(matrixStats)

mnist <- read_mnist()

names(mnist)
dim(mnist$train$images)
class(mnist$train$labels)
table(mnist$train$labels)
####################################

set.seed(123)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$train$images), 1000)
x_test <- mnist$train$images[index,]
y_test <- factor(mnist$train$labels[index])

####compute the SD and plot them to histogram for figure out predictor with Zero variability or almost 

sds <- colSds(x)
qplot(sds, bins = 256, color = I("black"))
nzv <- nearZeroVar(x)

# Remove nzv from data set
col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(mnist$train$images)

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[,col_index],y , method = "knn", tuneGrid = data.frame(k = c(1,3,5,7)),trControl = control )

plot(train_knn)

n <- 1000
b <- 2
index <- sample(nrow(x),n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index,col_index],y[index], method = "knn", tuneGrid = data.frame(k=c(3,5,7)), trControl = control )

fit_knn <- knn3(x[ , col_index], y, k = 3)
y_hat_knn <- predict(fit_knn, x_test[ , col_index], type = "class")
cm <- confusionMatrix(data = y_hat_knn, reference = factor(y_test)) 
cm$byClass[,1:2]

# Using random forest
library(Rborist)
control <- trainControl(method ="cv", number = 5, p= 0.8)
grid <- expand.grid(minNode = c(1,5), predFixed = c(10,15,25,35,50))
train_rf <- train( x[ ,col_index], y, method = "Rborist", nTree = 50, trControl = control, tuneGrid = grid, nSamp = 5000 )


# variable importance
library(randomForest)

x <- mnist$train$images[index ,]
y <- factor(mnist$train$labels[index])
rf <- randomForest(x ,y , ntree = 50)
imp <- importance(rf) # Compute importance of each feature using
image(matrix(imp,28,28))
  #Compare KNN and random forest
p_max <- predict(fit_knn, x_test[,col_index])
p_max <- apply(p_max,1,max)
ind <- which(y_hat_knn != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]

image(matrix(ind,28,28))
# Ensemble KNN and random forest
p_rf <- predict(fit_rf,x_test[,col_index])$census
p_rf <- p_rf/rowSums(p_rf)
p_knn <- predict(fit_knn,x_test[,col_index])
p <- (p_rf + p_knn)/2
y_pred <- factor(apply(p,1,which.max)-1)
confusionMatrix(y_pred,y_test)

########Exercise
library(caret)
library(magrittr)
library(dplyr)
library(dslabs)
library(matrixStats)

#Q1
models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
            "gamboost",  "gamLoess", "qda", 
            "knn", "kknn", "loclda", "gam",
            "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp",
            "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")
set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

#Q2
pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)

#Q3
acc <- colMeans(pred == mnist_27$test$y)
acc
mean(acc)

#Q4
votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

#Q5
ind <- acc > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]

#Q6
acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)

#Q7
ind <- acc_hat >= 0.8
votes <- rowMeans(pred[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)

