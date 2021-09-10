library(tidymodels)

computers <- read.table("tidymodels_desktops/tidy_computers.csv", header =  TRUE) 

# splits
splits <- initial_split(computers, strata = brand)

computers_trainning <- training(splits)
computers_testing <- testing(splits)

# recipe
dt_recipe <- recipe(
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

# workflow
dt_workflow <- workflow() %>% 
  add_recipe(dt_recipe) %>% 
  add_model(dt_model)

# creating grid_param
grid_param <- dt_workflow %>% 
  parameters() %>% 
  update(
    `min_n` = min_n(range = c(5L,10L))
  ) %>% 
  grid_max_entropy(size = 10)

# Cross validation
val_set <- vfold_cv(computers_trainning, v = 4, strata = brand)

# trainning
class_trained <- dt_workflow %>% 
  tune_grid(
    val_set,
    grid = grid_param,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(accuracy)
  )

class_trained %>% show_best()

# autoplot
ggplot2::autoplot(class_trained)

# see the magic
class_best_tune <- select_best(class_trained, "accuracy")
final_class_model <- dt_model %>% 
  finalize_model(class_best_tune)

workflow() %>% 
  add_recipe(dt_recipe) %>% 
  add_model(final_class_model) %>% 
  last_fit(splits) %>% 
  collect_predictions() %>% 
  select(brand, .pred_class) %>% 
  table()

workflow() %>% 
  add_recipe(dt_recipe) %>% 
  add_model(final_class_model) %>% 
  fit(computers) %>% 
  predict(computers) %>% 
  cbind(computers) %>% 
  select(brand, .pred_class) %>% 
  table()


