devtools::install_github("slowkow/ggrepel")

library(ggplot2)
library(ggrepel)
library(tibble)

# gráfico de pontos com texto e com label. Tudo no aes()
ggplot(mtcars) +
  aes(wt, mpg, label = rownames(mtcars)) +
  geom_point() + 
  geom_text()

# gráfico de pontos com texto e com label
ggplot(mtcars) +
  aes(wt, mpg) +
  geom_point() +
  geom_text(aes(label = rownames(mtcars)))

# remove rownames
mtcars2 <- mtcars
mtcars2$cars <- rownames(mtcars)
rownames(mtcars2) <- NULL

mtcars2 <- tibble::rownames_to_column(mtcars, var = "cars")

ggplot(mtcars2) +
  aes(wt, mpg) +
  geom_point() +
  geom_text(aes(label = cars))

ggplot(mtcars2) +
  aes(wt, mpg) +
  geom_point() +
  geom_label(aes(label = cars))

# angle e size
ggplot(mtcars2) +
  aes(wt, mpg) +
  geom_point() +
  geom_text(aes(label = cars))

ggplot(mtcars2) +
  aes(wt, mpg) +
  geom_point() +
  geom_label(aes(label = cars, angle = 45))

# color e fill
ggplot(mtcars2) +
  aes(wt, mpg) +
  geom_point() +
  geom_text(aes(label = cars, color = factor(cyl))) +
  guides(color = guide_legend(title = "cyl"))

ggplot(mtcars2) +
  aes(wt, mpg) +
  geom_point() +
  geom_label(aes(label = cars, fill = factor(cyl))) +
  guides(fill = guide_legend(title = "cyl"))



### ggrepel

ggplot(mtcars2) +
  aes(wt, mpg) +
  geom_point() +
  geom_text_repel(aes(label = cars))

ggplot(mtcars2) +
  aes(wt, mpg) +
  geom_point() +
  geom_label_repel(aes(label = cars))

ggplot(mtcars2) +
  aes(wt, mpg) +
  geom_point() +
  geom_text_repel(aes(label = cars, color = factor(cyl))) +
  scale_color_manual(values= c("green", "red", "blue"))+
  guides(color = guide_legend(title = "cyl")) 

ggplot(mtcars2) +
  aes(wt, mpg) +
  geom_point() +
  geom_label_repel(aes(label = cars, fill = factor(cyl)))+
  guides(fill = guide_legend(title = "cyl")) 




