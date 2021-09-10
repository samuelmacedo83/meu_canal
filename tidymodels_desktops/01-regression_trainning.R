library(tidymodels)

computers <- read.table("tidymodels_desktops/tidy_computers.csv", header =  TRUE) 

# splits
splits <- initial_split(computers, strata = brand)

computers_trainning <- training(splits)
computers_testing <- testing(splits)

# recipe
reg_recipe <- recipe(
  price ~ reviews + rating + brand + status,
  data = computers_trainning
) %>% 
  step_impute_knn(all_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors())


# engine
reg_mod <- linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")

# workflow
reg_workflow <- workflow() %>% 
  add_model(reg_mod) %>% 
  add_recipe(reg_recipe)

# Cross validation
val_set <- vfold_cv(computers_trainning, v = 4, strata = brand)

# trainning
reg_trained <- reg_workflow %>% 
  tune_grid(
    val_set,
    grid = 10,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(rmse)
  )

reg_trained %>% show_best()

# autoplot
ggplot2::autoplot(reg_trained)

# see the magic
reg_best_tune <- select_best(reg_trained, "rmse")
final_reg_model <- reg_mod %>% 
  finalize_model(reg_best_tune)

workflow() %>% 
  add_recipe(reg_recipe) %>% 
  add_model(final_reg_model) %>% 
  last_fit(splits) %>% 
  collect_predictions() %>% 
  select(.row, price, .pred) %>% 
  ggplot() +
  aes(x= price, y = .pred) +
  geom_point()

# save the results
reg_fitted <- workflow() %>% 
  add_recipe(reg_recipe) %>% 
  add_model(final_reg_model) %>% 
  fit(computers)   

saveRDS(reg_recipe, "tidymodels_desktops/reg_recipe")
saveRDS(final_reg_model, "tidymodels_desktops/reg_model")
saveRDS(reg_fitted, "tidymodels_desktops/reg_fitted")

# test the saved files
computers <- read.table("tidymodels_desktops/tidy_computers.csv", header =  TRUE)

reg_recipe <- readRDS("tidymodels_desktops/reg_recipe")
reg_model <- readRDS("tidymodels_desktops/reg_model")
reg_fitted <- readRDS("tidymodels_desktops/reg_fitted")

# two options
workflow() %>% 
  add_recipe(reg_recipe) %>% 
  add_model(reg_model) %>% 
  fit(computers) %>% 
  predict(computers[1,])

predict(reg_fitted, computers[10,])
