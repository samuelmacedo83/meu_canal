library(tidymodels)

data(ames)

# split dos dados
splits <- ames %>%
  initial_split(strata = Sale_Price) 

ames_train <- training(splits)
ames_test  <- testing(splits)

# recipes
recipe_normalized <- recipe(
  Sale_Price ~ Gr_Liv_Area + Longitude + Latitude,
  data = ames_train
) %>% 
  step_normalize(all_predictors())

recipe_simple <- recipe(
  Sale_Price ~ Gr_Liv_Area + Longitude + Latitude,
  data = ames_train
) 

# modelos
reg_model <- linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")

nn_model <- mlp(
  hidden_units = tune(), penalty = tune(), epochs = tune()
  ) %>% 
  set_engine("nnet") %>% 
  set_mode("regression")

svm_model <- svm_linear(cost = tune(), margin = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("regression")

dt_model <- decision_tree(
  cost_complexity = tune(), min_n = tune()
  ) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

rf_model <- rand_forest(
  mtry = tune(), min_n = tune(), trees = 1000
  ) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

xgb_model <- boost_tree(
  tree_depth = tune(), learn_rate = tune(), 
  loss_reduction = tune(),  min_n = tune(), 
  sample_size = tune(), trees = tune()
  ) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

# workflow
normalized <- workflow_set(
    preproc = list(normalized = recipe_normalized), 
    models = list(reg_linear = reg_model,
                  neural_network = nn_model, 
                  svm = svm_model
))

simple <- workflow_set(
  preproc = list(simple = recipe_simple), 
  models = list(decision_tree = dt_model,
                rand_forest = rf_model, 
                xgboost = xgb_model
))

complete_workflows <- bind_rows(normalized, simple) %>% 
  mutate(
    wflow_id = gsub("(simple_)|(normalized_)", "", wflow_id)
  )

# treinando
cv_splits <- vfold_cv(
  ames_train, v = 10, strata = Sale_Price
)

grid_ctrl <- control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
)

grid_results <- complete_workflows %>%
  workflow_map(
    resamples = cv_splits,
    grid = 15,
    control = grid_ctrl
  )

autoplot(grid_results)
autoplot(grid_results, id = "dap_reg_model")
autoplot(grid_results, id = "neural_network")

autoplot(
  grid_results,
  rank_metric = "rmse",  
  metric = "rmse",       
  select_best = TRUE     
)

autoplot(
  grid_results,
  rank_metric = "rsq",  
  metric = "rsq",       
  select_best = TRUE  
)

