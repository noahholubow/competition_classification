# loading packages
library(tidymodels)
library(tidyverse)
library(patchwork)

# setting seed
set.seed(3012)


# Loading data --------------------------------------------------------------
# loading data
train <- read_csv("data/train.csv") %>% # creating training data
  janitor::clean_names() %>% # using janitor to clean names
  mutate(
    hi_int_prncp_pd = as_factor(hi_int_prncp_pd) # turning hi_int_prncp_pd into factor
  )
test <- read_csv("data/test.csv") %>% # creating testing data
  janitor::clean_names() # using janitor to clean names


# EDA --------------------------------------------------------------
# viewing statistics
summary(train) # looking at summary of training data
train %>% 
  skimr::skim() # skimming data

# finding proportion of yes/no
train %>% 
  count(hi_int_prncp_pd) %>%
  mutate(prop = n / sum(n), # finding proportion of yes/no
         pct = prop * 100) # turning proportion into percentage 

# bar plot - looking for class imbalance
train %>% 
  ggplot(aes(x = hi_int_prncp_pd)) + # looking at distribution of hi_int_prncp_pd
  geom_bar() # looks pretty good # bar graph

# Folding -----------------------------------------------------------------
fold <- vfold_cv(train, v= 5, repeats = 3, strata = hi_int_prncp_pd) # setting up folding with 5 folds and 3 repeats
fold

# Recipe --------------------------------------------------------------
# setting up recipe
recipe <- recipe( # setting up recipe
  hi_int_prncp_pd ~ ., # regressing hi_int_prncp_pd on all variables
  data = train # using training data
) %>% 
  step_rm(id, earliest_cr_line, last_credit_pull_d, addr_state) %>% # removing unusable/undesirable variables
  step_novel(purpose) %>% # used to handle new levels of purpose in testing data
  step_other(all_nominal(), -all_outcomes(), threshold = 0.2) %>% # binning sparse nominal variables
  step_dummy(all_nominal(), -all_outcomes()) %>%  # creating dummy variables for nominal variables
  step_normalize(all_predictors(), -all_outcomes()) %>% # normalizing all predictors
  step_zv(all_predictors(), -all_outcomes()) # removing variables that have no variance

# prepping and baking data
recipe %>%
  prep(train) %>% # prepping data
  bake(new_data = NULL) %>% # baking data
  View()


# Exercise 5 --------------------------------------------------------------

# objects required for tuning
save(fold, recipe, split, file = "data/setup_A.rda") # saving recipe, folds, splits in new file for training