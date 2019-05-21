#Cross validation
   # Load ultilities libraries
   library(caret)
   library(magrittr)
   library(dplyr)

   set.seed(1996)
   n <- 1000
   p <- 10000
   x <- matrix(rnorm(n*p),n,p)
   colnames(x) <- paste("x", 1:ncol(x), sep = "_")
   y <- rbinom(n, 1, 0.5) %>% factor()
   x_subset <- x[ , sample(p , 100)]
   
   fit <- train(x_subset, y, method = "glm")
   fit$results
   install.packages("devtools")
   
   library(devtools)
   devtools::install_bioc("genefilter")
   library(genefilter)
   tt <- colttests(x, y)
   
   path.package()
   installed.packages()
   library(devtools)
   devtools::install_bioc("genefilter")
   
   install.packages("pkgload", repo = NULL)
   
   
   source("https://bioconductor.org/biocLite.R")   
   biocLite("genefilter")
   library(genefilter)