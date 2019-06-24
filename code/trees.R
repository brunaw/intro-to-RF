#-----------------------------------------------------------------------
# Trees in R
#-----------------------------------------------------------------------

library(tidyverse)
library(rpart)
library(rpart.plot)

set.seed(2019)
# Selecting the diamonds data
data <- diamonds %>% 
  mutate(set = ifelse(runif(nrow(.)) > 0.75, "test", "train"))

train <- data %>% filter(set == "train") %>% select(-set)
test <- data %>% filter(set == "test") %>% select(-set)

# Quite a few outliers in the price
train %>% 
  ggplot(aes(price)) +
  geom_density() +
  theme_bw()

# Using some of the variables 
first_model <- rpart(price ~  color, 
                     data = train)


first_model
rpart.plot(first_model)
# Not such a good model!

# Adding now the carat as a predictor
second_model <- rpart(price ~ cut + color + depth + carat, 
                     data = train)

second_model
rpart.plot(second_model)
# The carat is very important when predicting the price of a diamond!
# A normalized importance measure
second_model$variable.importance/max(second_model$variable.importance)

# Predictions
test <- test %>% 
  mutate(pred_first = predict(first_model, newdata = test),
         pred_second = predict(second_model, newdata = test))


sqrt(sum((test$pred_first - test$price)^2))

sqrt(sum((test$pred_second - test$price)^2))

test %>% 
  ggplot(aes(price, pred_first)) +
  geom_point() +
  theme_bw()


test %>% 
  ggplot(aes(price, pred_second)) +
  geom_point() +
  theme_bw()

# Much better! but should we discard the "not so good" tree?
#-----------------------------------------------------------------------
