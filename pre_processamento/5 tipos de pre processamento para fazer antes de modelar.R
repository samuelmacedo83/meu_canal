# dummy e one-hot-encoder
# zero variance e near zero variance
# correlação e multicolinearidade
# center e scale
# imputação

library(caret)

########################################
################ dummy e one-hot-encoder

parede <- data.frame(
  cor = c("verde", "vermelho", "verde", "vermelho", "verde", "vermelho"),
  tamanho = c("baixa", "media", "alta", "baixa", "media", "alta"),
  custo = c(10, 12, 14, 11, 12, 15),
  pintar = c("sim", "sim", "não", "não", "sim", "sim")
)

# dicotomica
parede_dummy <- dummyVars( ~  pintar, data = parede, fullRank = TRUE)
predict(parede_dummy, newdata = parede)

# ordinal
parede_dummy <- dummyVars( ~  tamanho, data = parede, fullRank = TRUE)
predict(parede_dummy, newdata = parede)

# todas as variaveis
parede_dummy <- dummyVars( ~  ., data = parede, fullRank = TRUE)
predict(parede_dummy, newdata = parede)

###################################################
################ zero variance e near zero variance
sim_nao_1 <- c(
  "sim", "não", "sim", "não", "sim", "não", "sim", "não", "sim", "sim"
)

sim_nao_2 <- c(
  "sim", "sim", "sim", "sim", "sim", "sim", "sim", "sim", "sim", "não"
)

nearZeroVar(sim_nao_1, saveMetrics = TRUE)

# exemplo com uma dataset
data(GermanCredit)
nearZeroVar(GermanCredit, saveMetrics = TRUE)

# analisando as variaves a serem retiradas
table(GermanCredit$ForeignWorker)
table(GermanCredit$Purpose.Vacation)

#########################################
########## correlação, multicolinearidade

cor(iris[, -5])
findCorrelation(cor(iris[, -5]))

iris_tbl <- iris
iris_tbl$Sepal.Length_cor <- iris_tbl$Sepal.Length * 2

findCorrelation(cor(iris_tbl[, -5]))
findLinearCombos(iris_tbl[, -5])

##########################
########## feature scaling
standarization <- preProcess(iris, method = c("center", "scale"))
predict(standarization, newdata = iris)

min_max <- caret::preProcess(iris, method = c("range"))
predict(min_max, newdata = iris)

####################
########## imputação
iris_tbl <- iris
iris_tbl[1, 1] <- NA
iris_impute <- preProcess(iris_tbl, method = "knnImpute")
predict(iris_impute, newdata = iris_tbl)





# referencias
https://topepo.github.io/caret/pre-processing.html
https://towardsdatascience.com/one-hot-encoding-multicollinearity-and-the-dummy-variable-trap-b5840be3c41a
https://machinelearningmastery.com/one-hot-encoding-for-categorical-data/
https://www.algosome.com/articles/dummy-variable-trap-regression.html#:~:text=The%20Dummy%20Variable%20trap%20is,%2Ffemale)%20as%20an%20example.
https://en.wikipedia.org/wiki/Feature_scaling


