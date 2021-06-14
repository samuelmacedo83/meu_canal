# SINTAXE

# |> precisa dos parentesis
mtcars %>% head()
mtcars %>% head

mtcars |> head()
mtcars |> head

# só passa pro primeiro parametro

c("cachorro", "gato", "papagaio") %>% 
  grepl("g", .)

achar_g <- function(x) grepl("g", x)
c("cachorro", "gato", "papagaio") |>
  achar_g()


c("dogs", "cats", "rats") |>
  {function(x) grepl("g", x)}()


# performance
raiz <- function(val) sqrt(val)
x <- 1:100
bm <- microbenchmark::microbenchmark(
  normal = raiz(x),
  pipe_magrittr = x %>% raiz(),
  pipe_base = x |> raiz(),
  times = 1000L
)

ggplot2::autoplot(bm)

rlang::exprs(
  normal = raiz(x),
  pipe_magrittr = x %>% raiz(),
  pipe_base = x |> raiz(),
)


erro <- function(x){
  stop("OMG!")
}

1:10 %>%
  erro()

traceback()

1:10 |>
  throw_error()

traceback()

rlang::exprs(2 + 3 + 4)
letters %>% head()

find_at = function(x) grepl("at", x)
c("dogs", "cats", "rats") |> find_at()



c("dogs", "cats", "rats") |>
  {function(x) grepl("at", x)}()
