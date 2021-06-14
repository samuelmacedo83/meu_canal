iris_b <- iris[-c(1:20), ]
iris_b[c(1, 51, 101), 1] <- NA
iris_b <- dplyr::tibble(iris_b)

library(tidymodels)
splits <- initial_split(iris_b, strata = Species)

iris_trainning <- training(splits)
iris_testing <- testing(splits)

# engine
rf_model <- rand_forest(
  mtry = tune(), min_n = tune(), trees = 10
) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

#recipe
rf_recipe <- recipe(
  Species ~ ., data = iris_trainning
) %>% 
  step_impute_knn(all_predictors()) 

# workflow
rf_workflow <- workflow() %>% 
  add_recipe(rf_recipe) %>% 
  add_model(rf_model)

# Cross validation
val_set <- vfold_cv(
  iris_trainning, v = 4, strata = Species
)

# trainning
rf_trained <- rf_workflow %>% 
  tune_grid(
    resamples = val_set,
    grid = 10,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(accuracy)
  )

rf_trained %>%
  show_best(n = 10) 

# autoplot
ggplot2::autoplot(rf_trained)

# see the magic
best_tune <- select_best(rf_trained, "accuracy")
final_model <- rf_model %>% 
  finalize_model(best_tune)

workflow() %>% 
  add_recipe(rf_recipe) %>% 
  add_model(final_model) %>% 
  last_fit(splits) %>% 
  collect_predictions() %>% 
  roc_curve(Species, .pred_setosa:.pred_virginica) %>%
  autoplot()



