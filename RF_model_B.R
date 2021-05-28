# RF tuning ----

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(tictoc)

set.seed(123) # setting seed

# load required objects ----
load("data/setup_B.rda") # loading in data


# Define model ----
rf_model <- rand_forest(mode = "classification", # creating random forest model for classification
                        mtry = tune(), # tuning mtry
                        min_n = tune()) %>%  # tuning min_n
  set_engine("ranger") # using ranger engine for random forest

# set-up tuning grid ----
rf_params <- parameters(rf_model) %>% # adding model to parameters
  update(mtry = mtry(range = c(2, 10))) # use 10 as upper bound


# define tuning gridgrid
rf_grid <- grid_regular(rf_params, levels = 5) # trying out every single combination of mtry and min_n

# workflow ----
rf_workflow <- 
  workflow() %>% # creating workflow
  add_model(rf_model) %>% # adding model to workflow
  add_recipe(recipe) # adding recipe to workflow

# Tuning/fitting ----
rf_tune <- rf_workflow %>% # creating tuning object
  tune_grid(resamples = fold, grid = rf_grid) # tuning grid

# Write out results & workflow
save(rf_tune, rf_workflow, file = "data/rf_tune_B.rda") # saving results
