library(tidymodels)

df <- read.csv(
  "C:/Pesquisa/meu_canal_dados/diego_ambiental/dados.csv",
  sep = ";", dec = ","
)

# Universidades Federais dos Vales do Jequitinhonha e Mucuri (UFVJM) 
# e do Oeste do Pará  (UFOPA). 
# Sou engenheiro florestal, mestre e doutor em Ciência Florestal.
# 
ggplot(df) +
  aes(x = altura, volume_real) +
  geom_point() 

ggplot(df) +
  aes(x = dap, log(volume_real)) +
  geom_point() +
  facet_wrap(~nome)

ggplot(df) +
  aes(x = log(altura*dap^2), log(volume_real)) +
  geom_point() +
  facet_wrap(vars(nome, fuste))

# splits
splits <- initial_split(df, strata = volume_real)

train <- training(splits)
test <- testing(splits)

# recipes
# da um glimpse na tela que fica mais facil de acompanhar
recipe_volume <- recipe(volume_real ~ ., data = train) %>% 
  step_mutate(
    fuste = as.factor(fuste),
    volume_est = altura*dap^2
  ) %>% 
  step_log(volume_est, volume_real) %>% 
  step_dummy(nome, fuste) 

# modelo
reg_model <- linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")

# workflow
workflow <- workflow() %>% 
  add_recipe(recipe_volume) %>% 
  add_model(reg_model)

# treinando
cv_splits <- vfold_cv(train, v = 5, strata = volume_real)

results <- workflow %>% 
  tune_grid(
    cv_splits,
    grid = 10,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(rmse)
  )

#
autoplot(results)

reg_best_tune <- select_best(results, "rmse")
final_reg_model <- reg_model %>% 
  finalize_model(reg_best_tune)

workflow() %>% 
  add_recipe(recipe_volume) %>% 
  add_model(final_reg_model) %>% 
  last_fit(splits) %>% 
  collect_predictions() %>% 
  select(volume_real, .pred) %>% 
  ggplot() +
  aes(x = volume_real, y = .pred) +
  geom_point()

workflow() %>% 
  add_recipe(recipe_volume) %>% 
  add_model(final_reg_model) %>% 
  fit(data = df) %>% 
  pluck("fit") %>% 
  coef() 

workflow() %>% 
  add_recipe(recipe_volume) %>% 
  add_model(final_reg_model) %>% 
  fit(data = df) %>% 
  pluck("fit") -> a


