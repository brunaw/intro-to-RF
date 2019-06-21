#-----------------------------------------------------------------------
# Random forests in R
#-----------------------------------------------------------------------
library(ranger)
library(tidyverse)

set.seed(2019)
# Selecting the diamonds data
data <- diamonds %>% 
  mutate(set = ifelse(runif(nrow(.)) > 0.75, "test", "train"))

train <- data %>% filter(set == "train") %>% select(-set)
test <- data %>% filter(set == "test") %>% select(-set)

# only a tree
first_rf <- ranger(price ~ cut + color + depth + carat, 
                   num.trees = 1, mtry = 4, data = train)

first_rf
rmse(predict(first_rf, data = test)$predictions)


# A forest, adding all the variables as predictors
second_rf <-  ranger(price ~ ., num.trees = 50, data = train,
                     importance = "impurity")

second_rf

# Comparing
pred_rf_first <- predict(first_rf, test)$predictions
pred_rf_second <- predict(second_rf, test)$predictions
rmse(pred_rf_first)
rmse(pred_rf_second)

# How are the predictions compared to the observed data?
test %>% 
  mutate(predicted = predict(second_rf, test)$predictions) %>% 
  ggplot(aes(predicted, price)) +
  geom_point(colour = "#ff6767", alpha = 0.3) +
  labs(title = "Predicted and observed") + 
  theme_bw(18)


imps <- data.frame(var = names(train)[-7],
                   imps = second_rf$variable.importance/max(second_rf$variable.importance))

# Evaluating importances
imps %>% 
  ggplot(aes(imps, x = reorder(var, imps))) +
  geom_point(size = 3, colour = "#ff6767") +
  coord_flip() +
  labs(x = "Predictors", y = "Importance scores") +
  theme_bw(18) 

# Checking correlation between predictors
corrplot::corrplot(cor(diamonds %>% select_if(is.numeric), 
                       method = "spearman"))

# New model removing correlated variables

third_rf <- ranger(price ~ table + depth + carat +
                     color + clarity + cut, num.trees = 50, 
                   importance = "impurity", data = train)
third_rf

# New results
# Predictions
pred_rf_third <- predict(third_rf, test)$predictions
rmse(pred_rf_third)

imps <- data.frame(var = third_rf$forest$independent.variable.names,
                   imps = third_rf$variable.importance/max(third_rf$variable.importance))


# Importances
imps %>% 
  ggplot(aes(imps, x = reorder(var, imps))) +
  geom_point(size = 3, colour = "#ff6767") +
  coord_flip() +
  labs(x = "Predictors", y = "Importance scores") +
  theme_bw(18) 

# Predictions
test %>% 
  mutate(predicted = predict(third_rf, test)$predictions) %>% 
  ggplot(aes(predicted, price)) +
  geom_point(colour = "#ff6767", alpha = 0.3) +
  labs(title = "Predicted and observed") + 
  theme_bw(18)
#-----------------------------------------------------------------------
