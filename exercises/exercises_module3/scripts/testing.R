
# title: Statistical tests
# author: Audrey Baguette (Workshop Lead)
# based on material by Alex Diaz-Papkovich and Adrien Osakwe
# date:  2025-09-09
library(tidyverse)

############################
# Two-samples t-test
############################
# 1- First, we create two random samples from Normal distributions
set.seed(20221011) # Seed for reproducibility
n <- 100 # Sample size

mu1 <- 0 # Mean of sample 1
mu2 <- 0.1 # Mean of sample 2

sd1 <- 1 # Standard deviation 1
sd2 <- 1 # Standard deviation 2

# Generate two samples of of size n of normally-distributed data.
# Sample x1 has a mean of mu1 and standard deviation sd1
# Sample x2 has a mean of mu2 and standard deviation sd2
x1 <- rnorm(n, mu1, sd1)
x2 <- rnorm(n, mu2, sd2)

# 2- We can take a look at the data with boxplots
x_df <- data.frame(Values = c(x1,x2),
                   Group = c(rep('G1',n),rep('G2',n)))
ggplot(x_df) + geom_boxplot(aes(y = Values, color = Group))

# 3- t-test
t.test(x1, x2)

# 4- The result of the test is influenced by the sample size
x3 <- rnorm(10000, mu1, sd1)
x4 <- rnorm(10000, mu2, sd2)
t.test(x3, x4)

# 5- We can also do a one-sided test
# Check if group2 is greater than group1 (aka the difference 1-2 is smaller)
t.test(x1, x2, alternative = 'less')


############################
# ANOVA
############################
# 1- Create mutliple datasets
set.seed(20221011)

# Our artificial data will have three different means
m1 <- rnorm(30, mean = 25, sd = 5)
m2 <- rnorm(30, mean = 30, sd = 6)
m3 <- rnorm(30, mean = 22, sd = 7)

ms <- c(m1, m2, m3) # Combine our three measures into one vector

# Our vector of group labels A, B, and C
ls <- as.factor(c(rep("A",30),rep("B",30),rep("C",30)))

# 2- Visualize the data
df_anova <- data.frame(Measure = ms, Group = ls)
ggplot(df_anova) + geom_boxplot(aes(y = Measure, color = Group))

# 3- ANOVA
results_aov <- aov(Measure ~ Group, data = df_anova)
summary(results_aov)
# We can look at the coefficients to find the difference across groups
results_aov$coefficients


############################
# Hands-on
############################
# 1- ANOVA vs multiple t-tests
# Data PlantGrowth has multiple groups: one control and two treatments
head(PlantGrowth)
# Make an ANOVA and consecutive t-tests between the control and each treatment
# Do they lead to the same conclusion?


## Solution



# 2- Multiple testing correction
# Take the co2 dataset (note: not CO2), that records the CO2 level for each month across several years
# We want to test if the CO2 levels from 1959 to 1978 were lower than those between 1979 and 1997
# for each month separately.
# Then, we correct for multiple testing and compare the results (hint: see p.adjust)
# Do the conclusions change?
co2_df <- data.frame(matrix(co2, 39, 12), row.names = as.character(c(1959:1997)))
colnames(co2_df) <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')


## Solution


#