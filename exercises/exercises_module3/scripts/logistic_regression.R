
# title: Logistic Regression
# author: Audrey Baguette (Workshop Lead)
# based on material by Alex Diaz-Papkovich and Adrien Osakwe
# date:  2025-09-09
library(tidyverse)
library(nnet)

############################
# Predicting species with a linear model
############################
# We remove the versicolor species to only have two species
iris_subset <- iris %>% filter(Species != 'versicolor')

# Predict the species based on the petal dimensions
iris_subset$train_test <- sample(c('train', 'test'), nrow(iris_subset), replace = TRUE, prob = c(0.8, 0.2))
iris_model <- lm(Species ~ Petal.Length + Petal.Width, data = iris_subset[iris_subset$train_test == 'train',])
predictions <- predict.lm(iris_model, iris_subset[iris_subset$train_test == 'test',], type = 'response')

# Convert the predictions to species and compare to the truth
table(round(predictions), iris_subset[iris_subset$train_test == 'test', 'Species'])


############################
# Predicting 2 species with a log model
############################
iris_log_model <- glm(Species ~ Sepal.Length +  Sepal.Width,
                  family = 'binomial',
                  data = iris_subset[iris_subset$train_test == 'train',])

predictions_log <- predict.glm(iris_log_model,
                               iris_subset[iris_subset$train_test == 'test',],
                              type = 'response')

predictions_log <- ifelse(predictions_log >= 0.5,"virginica", "setosa")

table(predictions_log, iris_subset[iris_subset$train_test == 'test', 'Species'])


############################
# Predicting 3 species with a log model
############################
iris$train_test <- sample(c('train', 'test'), nrow(iris), replace = TRUE, prob = c(0.8, 0.2))
iris_train <- iris %>% filter(train_test == 'train')
iris_test <- iris %>% filter(train_test == 'test')

iris_multinomial <- multinom(Species ~ Sepal.Length +  Sepal.Width,
                      data = iris_train)

predictions_multi <- predict(iris_multinomial,
                               iris_test,
                               'prob')

# Convert the probabilities to species with a softmax
species_predictions <- predictions_multi %>%
  apply(1, function(x) return(c('setosa', 'versicolor', 'virginica')[which.max(x)]))

table(species_predictions, iris_test$Species)


############################
# Hands-on
############################
# From the diamonds model, predict the cut
# First divide the data in training and testing datasets
diamonds$train_test <- sample(c('train', 'test'), nrow(diamonds), replace = TRUE, prob = c(0.8, 0.2))
diamonds_train <- diamonds %>% filter(train_test == 'train')
diamonds_test <- diamonds %>% filter(train_test == 'test')

## Solution




#