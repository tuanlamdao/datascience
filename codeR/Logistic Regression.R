#Logistic regression
  #load ultilities library
  library(caret)
  library(magrittr)
  library(dbplyr)
  
  #load data library
  library(dslabs)
  data("heights")
  
  y <- heights$height
  set.seed(2)
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train_set  <- heights %>% slice(-test_index)
  test_set   <- heights %>% slice(test_index)
  glm_fit <- train_set %>% mutate(y = as.numeric(sex == "Female")) %>% glm(y ~ height, data =., family = "binomial")
  p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
  y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor(levels = c("Female","Male"))
  confusionMatrix(y_hat_logit, test_set$sex)