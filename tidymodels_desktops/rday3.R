library(tidymodels)

computers <- read.table("tidymodels_desktops/tidy_computers.csv", header =  TRUE) 

# splits
splits <- initial_split(computers, strata = brand)

computers_trainning <- training(splits)
computers_testing <- testing(splits)

# recipe
class_recipe <- recipe(
  brand ~ reviews + rating + price + status,
  data = computers_trainning
) %>% 
  step_impute_knn(all_predictors()) 

# engine
dt_model <- decision_tree(
  cost_complexity = tune(), min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")

rf_model <- rand_forest(
  mtry = tune(), min_n = tune(), trees = tune()
) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")


# workflow
workflow <- workflow_set(
  preproc = list(preprocess = class_recipe), 
  models = list(decision_tree = dt_model,
                rand_forest = rf_model
  )) %>% 
  mutate(wflow_id = gsub("preprocess_", "", wflow_id))

# Cross validation
val_set <- vfold_cv(computers_trainning, v = 4, strata = brand)

# trainning
grid_ctrl <- control_grid(
  save_pred = TRUE,
  parallel_over = "everything",
  save_workflow = TRUE
)

grid_results <- workflow %>%
  workflow_map(
    resamples= val_set,
    grid = 10,
    control = grid_ctrl
  )

grid_results$result[[1]] %>% show_best()

# autoplot
ggplot2::autoplot(grid_results)

# see the magic
class_best_tune <- select_best(grid_results$result[[2]], "accuracy")
final_class_model <- rf_model %>% 
  finalize_model(class_best_tune)

workflow() %>% 
  add_recipe(class_recipe) %>% 
  add_model(final_class_model) %>% 
  last_fit(splits) %>% 
  collect_predictions() 

workflow() %>% 
  add_recipe(class_recipe) %>% 
  add_model(final_class_model) %>% 
  fit(computers_testing) %>% 
  predict(computers) %>% 
  cbind(computers) %>% 
  select(brand, .pred_class) %>% 
  table()

# save the results
class_fitted <- workflow() %>% 
  add_recipe(class_recipe) %>% 
  add_model(final_class_model) %>% 
  fit(computers_testing) 

saveRDS(class_fitted, "tidymodels_desktops/class_fitted")

# test the saved files
computers <- read.table("tidymodels_desktops/tidy_computers.csv", header =  TRUE)

class_fitted <- readRDS("tidymodels_desktops/class_fitted")


predict(class_fitted, computers[1,])

