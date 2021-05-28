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

###############################################################
################# LOADING DATA BACK IN ########################
###############################################################

# Prediction --------------------------------------------------------------
load(file = "data/rf_tune_B.rda") # loading file back in

# selecting best metric
rf_tune %>% 
  select_best(metric = "roc_auc") # autoplot of tuned data

# store results
tune_results <- tibble( # setting up tibble of results
  model_type = "rf",  # adding RF model
  tune_info = list(rf_tune), # gathering tuning info in list
  assessment_info = map(tune_info, collect_metrics), # looping through assessment info
  best_model = map(tune_info, ~ select_best(.x, metric = "roc_auc")) # pulling out best model
)
tune_results

# metrics for top performers
tune_results_best <- tune_results %>% 
  select(model_type, best_model) %>% # selecting best model exclusively
  unnest(best_model) # unnesting best model to view information
tune_results_best

# tuned results in descending order by roc_auc
tune_results_best_assessment <- tune_results %>% 
  select(model_type, assessment_info) %>%  # select assessment info
  unnest(assessment_info) %>% # unnest assessment info
  filter(.metric == "roc_auc") %>% # looking at ROC_AUC
  arrange(desc(mean)) # arranging by mean ROC_AUC
tune_results_best_assessment

# Predictions -------------------------------------------------------------



# Best Model --------------------------------------------------------------
# slnn tuned workflow
rf_workflow_tuned <- rf_workflow %>% # looking at tuned workflow
  finalize_workflow(select_best(rf_tune, metric = "roc_auc")) # finalizing best model

# viewing results
rf_results <- fit(rf_workflow_tuned, train) # fitting best model
rf_results

# predict
predict <-  predict(rf_results, new_data = test) %>% # predicting using new testing data
  bind_cols(Id = test$id) %>% # combining ID
  rename(Category = .pred_class) %>% # renaming to Category
  select(Id, Category)  # selecting only ID and category
predict

write_csv(predict, file = "predictions_B.csv") # outputting results


