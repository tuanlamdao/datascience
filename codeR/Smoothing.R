# Smoothing using bin smoother
   #Load ultility libraries
   library(caret)
   library(magrittr)
   library(dplyr)
   #Load data library
   library(dslabs)
   data("polls_2008")
   qplot(day, margin, data = polls_2008)
   span <- 7
   fit <- with(polls_2008, ksmooth(day, margin, x.points = day, kernel = "box", bandwidth = span))
   polls_2008 %>% mutate(smooth = fit$y) %>% ggplot(aes(day, margin)) + geom_point(size = 3, alpha = .5, color = "grey") +
     geom_line(aes(day, smooth),color = "red")
# smoothing using LOESS
   total_days <- diff(range(polls_2008$day))
   span <- 21/total_days
   fit <- loess(margin ~ day, degree = 1, span = span, data = polls_2008)
   polls_2008 %>% mutate(smooth = fit$fitted) %>% ggplot(aes(day, margin)) + geom_point(size = 3, alpha = 0.5, color = "grey") +
     geom_line(aes(day, smooth) , color = "red")
   
   polls_2008 %>% mutate(smooth = fit$fitted) %>% ggplot(aes(day, margin)) + geom_point() +
     geom_smooth()
   
   polls_2008 %>% mutate(smooth = fit$fitted) %>% ggplot(aes(day, margin)) + geom_point() +
     geom_smooth(color = "red", span = 0.15, method.args = list(degree = 1))