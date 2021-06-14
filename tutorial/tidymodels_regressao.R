iris_b <- iris[-c(1:20), ]
iris_b[c(1, 51, 101), 1] <- NA
iris_b[c(10, 60, 110), 5] <- NA
iris_b <- dplyr::tibble(iris_b)

library(tidymodels)

splits <- initial_split(iris_b, strata = Species)

iris_trainning <- training(splits)
iris_testing <- testing(splits)

# engine
reg_mod <- linear_reg(
  penalty = tune(), mixture = tune()
) %>% 
  set_engine("glmnet")

#recipe
reg_recipe <- recipe(
  Petal.Length ~ ., data = iris_trainning
) %>% 
  step_impute_knn(all_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors())

# workflow
reg_workflow <- workflow() %>% 
  add_model(reg_mod) %>% 
  add_recipe(reg_recipe)

# cv
val_set <- vfold_cv(
  iris_trainning, v = 4, strata = Species
)

# trainning
reg_trained <- reg_workflow %>% 
  tune_grid(
    val_set,
    grid = 10,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(rmse)
  )

reg_trained %>% show_best(n = 10)

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
  select(Petal.Length, .pred) %>% 
  ggplot() +
  aes(x= Petal.Length, y = .pred) +
  geom_point()