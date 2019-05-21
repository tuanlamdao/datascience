library(caret)
library(dslabs)
library(magrittr)
library(purrr)
data(heights)
y <- heights$sex
x <- heights$height
#*******************************
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set  <- heights[-test_index, ]
test_set   <- heights[test_index,  ]
#*******************************
# Guessing the outcomes
y_hat <- sample(c("Male","Female"), length(test_index), replace = TRUE) %>% factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex) # 0.54 ~ 54% correct 
#explore data to get some hints
heights %>% group_by(sex) %>% summarise(mean(height),sd(height))
#predict male if height is within two SD from the mean
y_hat <- ifelse( x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat) # 0.7933 ~ 80% correct
mean(y_hat == test_set$sex)
#looping for cutoff value to find the best prediction
cutoff <- seq(61,70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
}
)
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff # 64
#Predction accuracty based on the best_cutoff
y_hat <- ifelse( test_set$height > best_cutoff, "Male", "Female") %>% factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex) # 0.817 ~ 81.7% correct
# evaluate the prediction with confusion matrix
  # sensitivity means Y = 1 => Y hat = 1 or Y hat = 0 => Y = 0
  # specificity means Y = 0 => Y hat = 0 or Y hat = 1 => Y = 1
  # TP = actual positive is predicted positive, FP = actual negative is predicted positive
  # TN = actual negative is predicted negative, FN = actual positive is predicted negavite
  # sensitivity = TP/(TP+FN), also called true positive rate (TPR) or recall
  # specificity = TN/(TN+FP), also called true negative rate (TNR) or or TP/(TP+FP), also called precision or positive predict value (PPV) 
table(predicted = y_hat, actual = test_set$sex)
test_set %>% mutate(y_hat = y_hat) %>% group_by(sex) %>% summarise(accuracy = mean(y_hat == sex))
confusionMatrix(data = y_hat, reference = test_set$sex)

# Using F1 measure to have a balanced accuracy
  # F_meas ( carat function for F1) = 1/[(1/2)*(1/recall) + (1/2)*(1/precision)]
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
  
}) 
best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

y_hat <- ifelse( test_set$height > best_cutoff, "Male", "Female") %>% factor(levels = levels(test_set$sex))
confusionMatrix(data = y_hat, reference = test_set$sex)

# ROC and precision recall curve
    # Guessing male with equal probability
p <- 0.9
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, prob = c(p, 1-p) ) %>% factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)
confusionMatrix(data = y_hat, reference = test_set$sex)

guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, prob = c(p,1-p)) %>% factor(levels = c("Female","male"))
  list(method = "Guess", recall = sensitivity(y_hat, test_set$sex), precision = precision(y_hat, test_set$sex))
})
   # construct an ROC curve for the height-based approach
cutoff <- c(50, seq(60,75), 80)
height_cutoff <- map_df(cutoff, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% factor(levels = c("Female","Male"))
  list(method = "Height cutoff", FPR = 1-specificity(y_hat, test_set$sex), TPR = sensitivity(y_hat, test_set$sex))
  
} )

height_cutoff <- map_df(cutoff, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% factor(levels = c("Female","Male"))
  list(method = "Height cutoff", recall = sensitivity(y_hat, test_set$sex), precision = precision(y_hat, test_set$sex))
  
} )


