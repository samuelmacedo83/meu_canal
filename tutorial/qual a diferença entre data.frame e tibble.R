library(tibble) # já vem exportada no dplyr
library(dplyr)

# mostrar que é a mesma classe
iris_tbl <- as_tibble(iris)
class(iris)
class(iris_tbl)

# mostrar documentação
tibble::as_tibble()
dplyr::as_tibble()



# visualização na tela, estatisticas básicas, tipos
iris
iris_tbl

# tibble retorna warning para variaveis inexistentes
iris$ola
iris_tbl$ola

# tibble retorna tibble
class(iris[, 1]) 
class(iris_tbl[, 1]) 

iris_tbl$Sepal.Length 
iris_tbl[[1]] 
iris_tbl["Sepal.Length"] 

class(iris[1])
iris_tbl[1]

# tibbles não mudam o tipo da variável

tibble(
  var1 = c("a", "b", "c"),
  var2 = c(1, 2, 3)
)

# tibbles são criadas sequencialmente

data.frame(
  a = c(1, 2, 3),
  b = c(1, 2, 3),
  c = a + b
)

tibble(
  a = c(1, 2, 3),
  b = c(1, 2, 3),
  c = a + b
)

