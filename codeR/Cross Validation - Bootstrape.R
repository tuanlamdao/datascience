library(caret) 
library(magrittr)
library(dplyr)
library(dslabs)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()
x_subset <- x[ ,sample(p, 100)]

fit <- train(x_subset, y, method = "glm")
fit$results
install.packages("devtools")
install.packages("genefilter")
library(devtools)
source("https://bioconductor.org/biocLite.R")  

biocLite("genefilter")
library(genefilter)
tt <- colttests(x, y)
pvals <- tt$p.value
ind <- which(pvals <= 0.01)
x_subset <- x[,ind]

fit <- train(x_subset, y, method = "glm")
fit$results

/# using Knn

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

data(tissue_gene_expression)
x <- tissue_gene_expression$x
p <- 500
x_subset <- x[ ,sample(p, 100)]
y <- tissue_gene_expression$y
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(1, 71, 2)))
ggplot(fit)

data("tissue_gene_expression")
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y
train_knn <- train(y ~ ., method = "knn", data = tissue_gene_expression, tuneGrid = data.frame(k = seq(1, 71, 2)))
##########################################################################

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

ind_3 <- which(indexes$Resample01 == 3)
ind_4 <- which(indexes$Resample01 == 4)
ind_7 <- which(indexes$Resample01 == 7)

sum(indexes$Resample01==3) 
sum(indexes$Resample02==3) 
sum(indexes$Resample03==3) 
sum(indexes$Resample04==3) 
sum(indexes$Resample05==3) 
sum(indexes$Resample06==3) 
sum(indexes$Resample07==3) 
sum(indexes$Resample08==3) 
sum(indexes$Resample09==3)
sum(indexes$Resample10==3) 

set.seed(1)
y <- rnorm(100, 0, 1)

qnorm(0.75)
quantile(y, 0.75)

set.seed(1)
n <- 100
B <- 10000

est <- replicate(B, {
  y_star <- rnorm(n, 0, 1)
  q_star <- quantile(y_star, 0.75)
  
})  
mean(est)
sd(est)

#Set up your random parameters
n_val <- 100
B <- 10000
#Generate your random y
set.seed(1)
y <- rnorm(n = n_val, mean = 0, sd = 1)
#Create a random sample of your generated y values
set.seed(1)
indexes <- createResample(y, B)
#Create your sample outcomes (y_hat) from the sample index and use these to find the 75th percentile for each one; 
#using map allows you to effectively loop over each value of indexes to get a vector of q75's.
Q75s <- map_dbl(indexes, function(ind)
{
  y_hat <- y[ind]
  q75 <- quantile(y_hat, 0.75)
})
mean(Q75s)
sd(Q75s)

par(mfrow=c(1,2))
hist(Q75s)
qqnorm(Q75s)
qqline(Q75s)




