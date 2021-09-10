library(gridExtra)
library(tidymodels)

data(ames)

# análise descritiva
p1 <- qplot(Gr_Liv_Area, Sale_Price, data = ames)
p2 <- qplot(Latitude, Sale_Price, data = ames)
p3 <- qplot(Longitude, Sale_Price, data = ames)
grid.arrange(p1, p2, p3, nrow = 1)


# split dos dados
splits <- ames %>%
  mutate(Sale_Price = log10(Sale_Price)) %>%
  initial_split(strata = Sale_Price) # breaks

ames_train <- training(splits)
ames_test  <- testing(splits)

# recipe
ames_recipe <- recipe(
  Sale_Price ~ Gr_Liv_Area + Longitude + Latitude,
  data = ames_train
  ) %>% 
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_ns(Longitude, deg_free = tune("df_long")) %>%
  step_ns(Latitude, deg_free = tune("df_lat"))
  step_ns(Longitude, Latitude) 

# step_ns(Longitude, deg_free = tune("df_long")) %>%
# step_ns(Latitude, deg_free = tune("df_lat"))


ames_param <- ames_recipe %>% 
  parameters()

# modelo
reg_model <- linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")
  # ver parametro intercept

# workflow
ames_workflow <- workflow() %>% 
  add_recipe(ames_recipe) %>% 
  add_model(reg_model)

ames_param <- ames_workflow %>% 
  parameters()

# mostrando o grid
grid_param <- grid_max_entropy(ames_param, size = 10)

# update parametros
ames_param <- ames_workflow %>% 
  parameters() %>% 
  update(
    `df_long` = deg_free(), # spline_degree()
    `df_lat` = deg_free()
  )

# cross validation
cv_splits <- vfold_cv(
  ames_train, v = 10, strata = Sale_Price
) # breaks

# treinando
# doParallel::registerDoParallel()
ames_trained <- ames_workflow %>% 
  tune_grid(
    resamples = cv_splits,
    grid = grid_param,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(rmse)
)


# vendo os resultados
ames_trained %>%
  show_best(n = 10)

ggplot2::autoplot(ames_trained)


# atualizar recipe e model
best_tune <- select_best(ames_trained, "rmse")
final_reg_model <- reg_model %>% 
  finalize_model(best_tune)
final_recipe <- ames_recipe %>% 
  finalize_recipe(best_tune)


# see the magic
workflow() %>% 
  add_recipe(final_recipe) %>% 
  add_model(final_reg_model) %>% 
  last_fit(splits) %>% 
  collect_predictions() %>% 
  select(Sale_Price, .pred) %>% 
  ggplot() +
  aes(x= Sale_Price, y = .pred) +
  geom_point()

# na base toda
ames2 <- ames %>%
  mutate(Sale_Price = log10(Sale_Price))

resp <- workflow() %>% 
  add_recipe(final_recipe) %>% 
  add_model(final_reg_model) %>% 
  fit(ames2) %>% 
  predict(new_data = ames2)

plot(ames2$Sale_Price, resp$.pred )



