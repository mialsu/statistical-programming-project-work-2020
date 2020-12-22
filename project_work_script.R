library(tidyverse)

# Read required files to environment and convert them to tibbles
tree_measurements <- read.csv("~/Documents/Coding/statistical-programming-project-work-2020/tree_measurements.csv")
tree_variables <- read.csv("~/Documents/Coding/statistical-programming-project-work-2020/tree_variables.csv")
tree_measurements <- as_tibble(tree_measurements)
tree_variables <- as_tibble(tree_variables)

# Join tree_measurements and tree_variables to tree_tidy based on "variable_id"
tree_tidy <- left_join(tree_measurements, tree_variables, by = ("variable_id"))

# Remove variable id-column as not needed anymore
tree_tidy <- select(tree_tidy, -variable_id)

# Change rows containing separate variables from instances of tree as columns in tibble
# for obtaining cleaner data
tree_tidy <- tree_tidy %>%
  pivot_wider(names_from = variable_name, values_from = value)

# Make three linear models as instructed, first linear model between DBH and SLA
model1 <- lm(SLA ~ DBH, data = tree_tidy)
# Next linear model with categorial variable species added as a predictor
model2 <- lm(SLA ~ DBH + species, data = tree_tidy)
# Last linear model with DBH*species added as an interaction term
model3 <- lm(SLA ~ DBH + species + DBH * species, data = tree_tidy)

# Add predictions of all models to tree_tidy
tree_tidy$pred1 <- model1$fitted.values
tree_tidy$pred2 <- model2$fitted.values
tree_tidy$pred3 <- model3$fitted.values

# Add residuals of all models to tree_tidy
tree_tidy$resid1 <- model1$residuals
tree_tidy$resid2 <- model2$residuals
tree_tidy$resid3 <- model3$residuals

# Plot DBH/SLA, DBH/pred and DBH/resid
qplot(DBH, SLA, data = tree_tidy, colour = species)
qplot(DBH, pred1, data = tree_tidy, colour = species)
qplot(DBH, pred2, data = tree_tidy, colour = species)
qplot(DBH, pred3, data = tree_tidy, colour = species)
qplot(DBH, resid1, data = tree_tidy, colour = species)
qplot(DBH, resid1, data = tree_tidy, colour = species)
qplot(DBH, resid1, data = tree_tidy, colour = species)

# Check model summaries
summary(model1)
summary(model2)
summary(model3)


