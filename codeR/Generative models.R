library(caret)
data("heights")
y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, time = 1, p=0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

class(heights )

# Estimate the conditional distribution by simly estimating average and standard deviations from the data
params <- train_set %>% group_by(sex) %>% summarise(avg = mean(height), sd=sd(height))
params
# prevalence 
pi <- train_set %>% summarise(pi = mean(sex=="Female")) %>% .$pi
pi
# use estimated avg and sd to get the acutal rule
x <- test_set$height
f0 <- dnorm(x,params$avg[2],params$sd[2])
f1 <- dnorm(x,params$avg[1],params$sd[1])
p_hat_bayes <- f1*pi /(f1*pi + f0*(1-pi))
plot(p_hat_bayes)

#control prevalence
y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female","Male")
sensitivity(data = factor(y_hat_bayes),reference = factor(test_set$sex))
specificity(data = factor(y_hat_bayes),reference = factor(test_set$sex))
#change pi to balance specificity and sensitivity ( instead of changing cutoff in decision rule)
p_hat_bayes_unbiased <- f1*0.5 / (f1*0.5 + f0*(1 - 0.5))
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased > 0.5, "Female","Male")
sensitivity(data = factor(y_hat_bayes_unbiased),reference = factor(test_set$sex))

#QDA and IDA
data("mnist_27")
params <- mnist_27$train %>% group_by(y) %>% summarize(avg_1 = mean(x_1), avg_2 = mean(x_2), sd_1= sd(x_1), sd_2 = sd(x_2), r = cor(x_1,x_2))
params

library(caret)
train_qda <- train(y ~., method = "qda", data = mnist_27$train)
y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]

params <- mnist_27$train %>% group_by(y) %>% summarize(avg_1 = mean(x_1), avg_2 = mean(x_2), sd_1= sd(x_1), sd_2 = sd(x_2), r = cor(x_1,x_2))
params <- params %>% mutate(sd_1 = mean(sd_1),sd_2 = mean(sd_2), r = mean(r) )
params

train_lda <- train(y ~., method = "lda", data = mnist_27$train)
y_hat <- predict(train_lda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]

########CASE STUDY MORE THAN 3 predictors

# Read in data
mnist <- read_mnist()
set.seed(3456)
index_127 <- sample(which(mnist$train$labels %in% c(1,2,7)), 2000)
y <- mnist$train$labels[index_127]
x <- mnist$train$images[index_127,]
index_train <- createDataPartition(y, p=0.8, list = FALSE)
# get the Quandrants
row_column <- expand.grid(row=1:28,col=1:28)
# temporary object to figure out the quandrants
uper_left_ind <- which(row_column$row <= 14 & row_column$col <= 14)
lower_right_ind <- which(row_column$row > 14 & row_column$col > 14)
x <- x > 200
# binarize the values, above 200 is ink, below is no ink
x<- cbind(rowSums(x[ ,uper_left_ind])/rowSums(x),
          # proportion of pixels in upper right quandrant
        rowSums(x[ ,lower_right_ind])/rowSums(x))
train_set <- data.frame(y = factor(y[index_train]), x_1 = x[index_train,1],
                                                    x_2 = x[index_train,2])
test_set <- data.frame(y = factor(y[-index_train]), x_1 = x[-index_train,1],
                        x_2 = x[-index_train,2])
library(ggplot2)
ggplot(train_set,aes(x=x_1,y=x_2), colour = y)  + geom_point() 

train_qda <- train(y ~ ., method = "qda", data = train_set)
predict(train_qda, test_set, type = "prob") %>% head()

predict(train_qda, test_set)
confusionMatrix(data = predict(train_qda, test_set), reference = test_set$y )

# using lda
train_lda <- train(y ~ ., method = "lda", data = train_set)
confusionMatrix(data = predict(train_lda, test_set), reference = test_set$y )

# using KNN
train_knn <- train(y ~ ., method = "knn", tuneGrid = data.frame(k=seq(15,51,2)) ,data = train_set)
confusionMatrix(data = predict(train_knn, test_set), reference = test_set$y )

#Q1
library(caret)

set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

train_lda <- train(x, y, method = "lda")
train_lda$results
train_lda$finalModel$means

#Q2
means <- data.frame(t(train_lda$finalModel$means)) 
means <- means %>% mutate(gene = as.factor(rownames(means)))

means %>% ggplot(aes(x = cerebellum, y = hippocampus, colour = gene, label = gene)) +
  ggtitle("LDA Means - Cerebellum vs Hippocampus") +
  geom_point()  + geom_text (aes(label=gene)) +
  theme(legend.position="none")  

#Q3

set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
train_qda <- train(x, y, method = "qda")
train_qda$results
train_qda$finalModel$means

#Q4
means <- data.frame(t(train_qda$finalModel$means)) 
means <- means %>% mutate(gene = as.factor(rownames(means)))

means %>% ggplot(aes(x = cerebellum, y = hippocampus, colour = gene, label = gene)) +
  ggtitle("QDA Means - Cerebellum vs Hippocampus") +
  geom_point()  + geom_text (aes(label=gene)) +
  theme(legend.position="none")  
#Q5

set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

train_lda <- train(x, y, method = "lda", preProcess =  "center")
train_lda$results
train_lda$finalModel$means

means <- data.frame(t(train_lda$finalModel$means)) 
means <- means %>% mutate(gene = as.factor(rownames(means)))

means %>% ggplot(aes(x = cerebellum, y = hippocampus, colour = gene, label = gene)) +
  ggtitle("LDA Means - Cerebellum vs Hippocampus") +
  geom_point()  + geom_text (aes(label=gene)) +
  theme(legend.position="right")  

#Q6

set.seed(1993)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

train_lda <- train(x, y, method = "lda")
train_lda$results
train_lda$finalModel$means
