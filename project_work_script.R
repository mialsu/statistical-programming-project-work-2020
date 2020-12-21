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

# Make three linear models as instructed
model1 <- lm(SLA ~ DBH, data = tree_tidy)
model2 <- lm(SLA ~ DBH + species, data = tree_tidy)
model3 <- lm(SLA ~ DBH + species + DBH * species, data = tree_tidy)

# Add predictions of all models to tree_tidy
tree_tidy <- tree_tidy %>%
  add_predictions(model1, "pred1")
tree_tidy <- tree_tidy %>%
  add_predictions(model2, "pred2")
tree_tidy <- tree_tidy %>%
  add_predictions(model3, "pred3")

# Add residuals of all models to tree_tidy
tree_tidy <- tree_tidy %>%
  add_residuals(model1, "resid1")
tree_tidy <- tree_tidy %>%
  add_residuals(model2, "resid2")
tree_tidy <- tree_tidy %>%
  add_residuals(model3, "resid3")

# Plot DBH/SLA
ggplot(tree_tidy, aes(DBH, SLA)) +
  geom_hex(bins=75)

ggplot(tree_tidy, aes(DBH, pred1)) +
  geom_hex(bins=75) 


