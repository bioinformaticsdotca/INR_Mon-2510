
# title: Linear Regression
# author: Audrey Baguette (Workshop Lead)
# based on material by Alex Diaz-Papkovich and Adrien Osakwe
# date:  2025-09-09
library(tidyverse)

############################
# Verifying the dataset
############################
# Let's take the iris data set, and check if the petal length and width
# follow a linear correlation
ggplot(iris) + geom_point(aes(Petal.Length, Petal.Width, col = Species))
cor(iris$Petal.Length, iris$Petal.Width)
# The data seems to be linearly correlated. The species have different ranges
# of sizes, but they seem to all follow the same trend.


############################
# Fitting with MLE
############################
# 1- Estimating beta1
# $$\hat{\beta_1} = \frac{\sum^{N}_{i=1}(x_{i} - \bar{x})(y_{i} - \bar{y})}
# {\sum_{i=1}^{N}(x_{i} - \bar{x})^{2}} = \frac{SS_{xy}}{SS_{xx}}$$
mean_x = mean(iris$Petal.Length)
mean_y = mean(iris$Petal.Width)
numerator = sum((iris$Petal.Length - mean_x) * (iris$Petal.Width - mean_y))
denominator = sum((iris$Petal.Length - mean_x)**2)
beta1 = numerator / denominator

# 2- Estimating beta0
# $$\hat{\beta_0} = \bar{y} - \hat{\beta_{1}}\bar{x}$$
beta0 = mean_y - beta1*mean_x

# 3- Visualize the regression
regression_df = data.frame(x = 1:7, y = beta0+c(1:7)*beta1)
ggplot(iris) + geom_point(aes(Petal.Length, Petal.Width, col = Species)) +
            geom_line(aes(x, y), data = regression_df)


############################
# Build-in linear regression
############################
# 1- lm is the build-in regression function
linear_model <- lm(Petal.Width ~ Petal.Length, data = iris)
summary(linear_model)

# 2- Plot the results to compare with the MLE
regression_df2 = data.frame(x = 1:7, y = linear_model$coefficients[1]+c(1:7)*linear_model$coefficients[2])
ggplot(iris) + geom_point(aes(Petal.Length, Petal.Width, col = Species)) +
  geom_line(aes(x, y), data = regression_df2)


############################
# Predict results
############################
# Say we remove the versicolor species during model construction, 
# can we predict the Petal Width in function of its Length?

# 1- We remove the versicolor data and compute the model
iris_subset <- iris %>% filter(Species != 'versicolor')
iris_model <- lm(Petal.Width ~ Petal.Length, data = iris_subset)

# 2- We extract the versicolor Petal Length
versicolor_p <- iris %>% filter(Species == 'versicolor') %>%
                          mutate(Predicted_pw = iris_model$coefficients[1] + Petal.Length*iris_model$coefficients[2])
# Alternatively
predict.lm(iris_model, versicolor_p, type = 'response')

# 3- Plot the results
ggplot(iris) + geom_point(aes(Petal.Length, Petal.Width, col = Species)) +
  geom_point(aes(Petal.Length, Predicted_pw), data = versicolor_p, color = 'darkgreen', shape = '+', size = 5)

# 4- Compute the MSE
MSE = mean((versicolor_p$Petal.Width - versicolor_p$Predicted_pw)**2)


############################
# Use multiple variables
############################
# What if we also use the Sepal values to predict the Petal Width?

# 1- We compute the model after removal of versicolor
iris_model2 <- lm(Petal.Width ~ Petal.Length + Sepal.Length + Sepal.Width, data = iris_subset)

# 2- We extract the versicolor Petal Length
versicolor_p2 <- iris %>% filter(Species == 'versicolor') %>%
  mutate(Predicted_pw = iris_model2$coefficients[1] +
                        Petal.Length*iris_model2$coefficients[2] +
                        Sepal.Length*iris_model2$coefficients[3] +
                        Sepal.Width*iris_model2$coefficients[4])

# 3- Plot the results
ggplot(iris) + geom_point(aes(Petal.Length, Petal.Width, col = Species)) +
  geom_point(aes(Petal.Length, Predicted_pw), data = versicolor_p2, color = 'darkgreen', shape = '+', size = 5)

# 4- Compute the MSE
MSE2 = mean((versicolor_p2$Petal.Width - versicolor_p2$Predicted_pw)**2)
# In this case, the MSE is a lot larger than the previous model, so adding more variables did not help
ggplot(iris) + geom_point(aes(Sepal.Width, Petal.Width, col = Species))
cor(iris$Sepal.Width, iris$Petal.Width)
ggplot(iris) + geom_point(aes(Sepal.Length, Petal.Width, col = Species))
cor(iris$Sepal.Length, iris$Petal.Width)

# Quick test with only the Petal Length and Sepal Length
iris_model3 <- lm(Petal.Width ~ Petal.Length + Sepal.Length, data = iris_subset)
versicolor_p3 <- iris %>% filter(Species == 'versicolor') %>%
  mutate(Predicted_pw = iris_model3$coefficients[1] +
                        Petal.Length*iris_model3$coefficients[2] +
                        Sepal.Length*iris_model3$coefficients[3])
MSE3 = mean((versicolor_p3$Petal.Width - versicolor_p3$Predicted_pw)**2)
ggplot(iris) + geom_point(aes(Petal.Length, Petal.Width, col = Species)) +
  geom_point(aes(Petal.Length, Predicted_pw), data = versicolor_p3, color = 'darkgreen', shape = '+', size = 5)
# This is the worst MSE of the 3, possibly because of overfitting


############################
# Hands-on
############################
# Create your own linear model
# Using mtcars, find the best linear model to predict the miles per gallon (mpg)
# Use the first 2/3 of the data as training data for the model and the last third as test data
mtcars_train <- mtcars[1:22,]
mtcars_test <- mtcars[23:32,]


## Solution




#