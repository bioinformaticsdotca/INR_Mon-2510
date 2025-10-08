
# title: Data Wrangling
# author: Audrey Baguette (Workshop Lead)
# based on material by Larisa M. Soto and Adrien Osakwe
# date:  2025-09-01
library(dplyr)
library(tidyr)


############################
# Selected dplyr functions
############################

# 1- Select: extract (and rename) columns
help(select)
## Examples
## Select the column "Species" from data frame iris
select(iris, Species)
## Select the columns "Sepal.Length" and "Species" from data frame iris
select(iris, Sepal.Length, Species)
## Select the columns "Sepal.Length" and "Species" from data frame iris AND rename "Sepal.Length" as "Sepal"
select(iris, Sepal = Sepal.Length, Species)

# 2- Filter: extract rows based on some conditions
help(filter)
## Examples
## Select rows of Species "versicolor"
filter(iris, Species == 'versicolor')
## Select rows of Species "versicolor" AND Sepal.Length is smaller than 6
filter(iris, Species == 'versicolor' & Sepal.Length < 6)

# 3- Group by: split data frame based on values in column
help(group_by)
## Example
## Group by Species
group_by(iris, Species)

# 4- Summarize: group by column then produce a summary statistic
help(summarize)
## Examples
## Get the mean Petal.Length
summarize(iris, mean(Petal.Length))
## Get the mean Petal.Length, Petal.Length standard deviation and mean Sepal.Length AND rename them
summarize(iris,
          Mean_petal_length = mean(Petal.Length),
          SD_petal_length = sd(Petal.Length),
          Mean_sepal_length = mean(Sepal.Length))

# 5- Count: count the number of individuals
help(count)
## Examples
## Count the number of individual in each species
count(iris, Species)
## Count the number of individuals with Sepal.Length above 6
count(iris, Sepal.Length > 6)
## Count the number of individuals with Sepal.Length above 6, group by Species AND rename the column
count(iris, Sepal.Length > 6, Species, name = 'sepal_length_bigger_6')

# 6- Mutate: transform columns
help(mutate)
## Examples
## Compute the Petal area (assuming diamond shapes)
mutate(iris, Petal_surface = Petal.Length * Petal.Width / 2)
## Compute the sums of lengths and widths
mutate(iris, Length_sum = Petal.Length + Sepal.Length, Width_sum = Petal.Width + Sepal.Width)

# Cheatsheet link: https://github.com/rstudio/cheatsheets/blob/main/data-transformation.pdf


############################
# Piping functions
############################
# Group by species, then summarize statistics
## Witout piping
grouped_by_species <- group_by(iris, Species)
mean_stats <- summarize(grouped_by_species,
                        mean(Petal.Length), 
                        mean(Petal.Width), 
                        mean(Sepal.Length), 
                        mean(Sepal.Width))
mean_stats
## With piping
iris %>%
  group_by(Species) %>%
  summarize(mean(Petal.Length), mean(Petal.Width), mean(Sepal.Length), mean(Sepal.Width))


# Hands on 1: Conversion of script
## Convert the following script to its piped version:
grouped_cars <- group_by(mtcars, vs, hp >= 100) # group by vs (engine shape) and hp (horsepower)
ratio_mpg_wt <- mutate(grouped_cars,ratio = mpg/wt) # add a column with the ratio of mpg (miles per gallon) and wt (weight)
summarize(ratio_mpg_wt,
          Mean_weight = mean(wt),
          Mean_mpg = mean(mpg),
          Mean_ratio = mean(ratio)) # summarize statistics

## Solution


# Hands on 2:
## Count the number of cars with (1) a sum of gears (gear) and carburators (carb) >= 6, 
## (2) a number of cylinders (cyl) < 8 and (3) stratified by engine shape (vs)

## Solution



# Hands on 3: Find the errors
## We want to get the number of flowers with a sepal area (assuming area = width * length / 2) > 10,
## stratified by species
iris %>%
  mutate(area = Sepal.Length * Sepal.Width / 2) %>%
  group_by(Species) %>%
  summarize(sum(area))

## Solution


## We want summary statistics by brand
mtcars %>%
  mutate(brand = sub(' .*', '', row.names(mtcars))) %>%
  group_by(brand) %>%
  summarize(sum(brand), mean(gear), mean(carb))

## Solution



############################
# tidyr pivot functions
############################
# Wide format
billboard_subset <- billboard[1:15, 1:11]

# To long
billboard_long <- billboard_subset %>%
  pivot_longer(cols = starts_with("wk"),
               names_to = "week",
               names_prefix = "wk",
               values_to = "rank")

# Back to wide
billboard_wide <- billboard_long %>%
  pivot_wider(names_from = "week",
              values_from = "rank",
              names_prefix = "wk")

############################
# Managing missing values
############################
View(who2)

# 1- Ignoring missing values
## We want to have the average number and standard deviation of cases per country
## for positive smear in males between 0 and 14 years old (sp_m_014)
who2 %>%
  group_by(country) %>%
  summarize(Mean = mean(sp_m_014),
            SD = sd(sp_m_014))
## With na.rm
who2 %>%
  group_by(country) %>%
  summarize(Mean = mean(sp_m_014, na.rm = TRUE),
            SD = sd(sp_m_014, na.rm = TRUE))

# 2- Dropping missing values
## We want to clean the table once and for all
## We can remove rows that have missing values in specified columns only
who2 %>% drop_na(sp_m_014)
## By default, drop_na will drop all rows with a missing value in any column
who2 %>% drop_na()
## Alternatively, we can drop rows that only have missing data
to_keep <- who2[, c(-1, -2)] %>% # remove the first 2 columns
  is.na() %>% # test if each entry is an NA
  rowSums() != (ncol(who2)-2) # count the NAs in each row and check if different from the max
who2[to_keep,]

# 3- Replacing missing values
## We assume NA replace the absence of cases and are equivalent to 0
## First, we create the named list used as parameter in replace_na()
replacing_list <- rep(list(0), ncol(who2))
names(replacing_list) <- colnames(who2)
replacing_list['country'] <- 'unknown'
replacing_list['year'] <- 'unknown'
## Then, we replace the NAs in the data frame
who2 %>% replace_na(replacing_list)

# 4- Imputing missing values
## Case-by-case


# Hands-on 4: 
## 1- Drop the relapse columns
who2_ho4 <- who2[,grep('rel', colnames(who2), invert = TRUE)]
## 2- Remove empty rows 
## Optional: remove rows with at least 80% of the data missing
## 3- Pivot to long, then create new columns such that the data frame contains columns
## country, year, method, sex, age. Hint: search the doc!
## 4- Produce statistics of your choice
## 5- Re-pivot to wide


## Solution


#