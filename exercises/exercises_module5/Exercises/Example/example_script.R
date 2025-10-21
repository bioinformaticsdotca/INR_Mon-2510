library(ggplot2)
library(dplyr)

#Script for modelling iris species based on sepal characteristics
## Iris Dataset Modelling
#The iris dataset is composed of 50 flowers from three different species. 
#Measurements include width and length (centimeters) for petals and sepals of each flower.

#Filtering out versicolor to run a binary example
#focus is on viridis and seratosa species
mini_iris <- iris %>%
  filter(Species != 'versicolor')



### Scatter Plot of Iris dataset with Sepal Length and Width as axes
### Points are colored by their species
ggplot(mini_iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 3, alpha = 0.7) +  # Add points with some transparency
  theme_minimal() +  # Use a clean theme
  labs(
    title = "Iris Dataset: Sepal Dimensions by Species",
    x = "Sepal Length",
    y = "Sepal Width"
  )


#Objective
#Goal of this analysis is to identify features capable of distinguishing flower species with high accuracy. We achieve this task using the **logistic regression model**, focusing on 1 vs. all comparisons.


#Training logistic regression model
iris.fit <- glm(Species ~ Sepal.Length +  Sepal.Width,
                family = 'binomial',
                data = mini_iris)


#Calculating class predictions
mini_iris$Prob <- predict.glm(iris.fit,
                              mini_iris[,c('Sepal.Length','Sepal.Width')],
                              type = 'response')

#Assigning class labels
mini_iris$Pred <- ifelse(mini_iris$Prob >= 0.5,"virginica", "setosa")

#Comparing to ground truth labels
mini_iris$Result <- mini_iris$Species == mini_iris$Pred


## Visualizing Sepal dimensions by species
ggplot(mini_iris, aes(x = Sepal.Length, y = Sepal.Width, color = Pred)) +
  geom_point(size = 3, alpha = 0.7) +  # Add points with some transparency
  theme_minimal() +  # Use a clean theme
  labs(
    title = "Iris Dataset: Training Classification Performance",
    x = "Sepal Length",
    y = "Sepal Width"
  )


## Repeating analysis between Viridis and versicolor
mini_iris <- iris %>%
  filter(Species != 'setosa')
ggplot(mini_iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point(size = 3, alpha = 0.7) +  # Add points with some transparency
  theme_minimal() +  # Use a clean theme
  labs(
    title = "Iris Dataset: Dimensions by Species",
    x = "Petal Length",
    y = "Petal Width"
  )

iris.fit <- glm(Species ~ Sepal.Length + Sepal.Width,
                family = 'binomial',
                data = mini_iris)

iris_pred <- predict.glm(iris.fit,
                         mini_iris[,c('Sepal.Length','Sepal.Width')],
                         type = 'response')
table(mini_iris$Species == ifelse(iris_pred >= 0.5, 'virginica','versicolor'))