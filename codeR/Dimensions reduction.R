library(caret)
library(magrittr)
library(dplyr)
library(dslabs)
library(matrixStats)

#Q1
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
X <- tissue_gene_expression$x
class(X)
Y <- tissue_gene_expression$y
tissue_gene_expression$y
class(tissue_gene_expression$y)
pca_tgev <- prcomp(X) 
summary(pca_tgev)
cor(X) 
data.frame(pca_tgev$x[,1:2], tissue_type = Y) %>%
  ggplot(aes(PC1,PC2, fill = tissue_type)) +
  geom_point(cex=3, pch=21)

# or 
pc <- prcomp(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()


#Q2

rowMeans(X)
cor(X)

#Q3

library(caret)
library(magrittr)
library(dplyr)
library(dslabs)
library(matrixStats)
library(tidyr)
library(tidyverse)
library(ggplot2)
install.packages("tidyverse")

x <- with(tissue_gene_expression, sweep(x, 1, mean(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()


x <- tissue_gene_expression$x - mean(tissue_gene_expression$x)
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()



x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

#Q4

data.frame(pc_7 = pc$x[,7],
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(x = tissue, y = pc_7)) +
  geom_boxplot()  

#Q5

importance_df <- data.frame(Sum_Exp = summary(pc)$importance[3, ]) %>%
            rownames_to_column( "PCA")

importance_df[1:20,] %>%
  ggplot( aes(reorder(PCA, Sum_Exp), Sum_Exp)) + 
  geom_point() +
  geom_hline( aes(yintercept = 0.5), color = "red") +
  ggtitle("First observation above the line") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))