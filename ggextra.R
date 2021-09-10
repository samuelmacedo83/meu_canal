library(ggplot2)
library(ggExtra)

df <- data.frame(x = rnorm(500, 50, 10), y = runif(500, 0, 50))
plot <- ggplot(df, aes(x, y)) + geom_point()

# density
ggMarginal(plot)
ggMarginal(plot, type = "density")
ggMarginal(plot, type = "density", colour = "red")
ggMarginal(plot, type = "density",
           colour = "red", margins = "y"
)

ggMarginal(plot, type = "density",
  xparams = list(colour = "red"),
  yparams = list(colour = "blue")
)

ggMarginal(plot, type = "density",
  xparams = list(colour = "red"),
  yparams = list(colour = "blue", size = 3)
)

# violin
ggMarginal(plot, type = "violin")


# histograma
ggMarginal(plot, type = "histogram")
ggMarginal(plot, type = "histogram", bins = 10)

ggMarginal(plot, type = "histogram" , colour = "black", fill = "red")
ggMarginal(plot, type = "histogram" , colour = "white", fill = "red")

ggMarginal(plot, type = "histogram", 
           xparams =  list(colour = "white", fill = "red", size = 4)
)

# densigram
ggMarginal(plot, type = "densigram")

ggMarginal(plot, type = "densigram", bins = 10, fill = "red",
           xparams = list(size = 1.5))


# boxplot
ggMarginal(plot, type = "boxplot")
ggMarginal(plot, type = "boxplot", margins = "y")
ggMarginal(plot, type = "boxplot", colour = "red", outlier.colour = "blue")

# remove grid
plot + removeGrid()

# rotate x
df <- data.frame(x = paste("Letra", LETTERS, sep = "_"),
                 y = seq_along(LETTERS))

ggplot(df, aes(x, y)) + 
  geom_point() + 
  rotateTextX()


plot <- ggplot(iris) +
  aes(x = Sepal.Length, y = Sepal.Width) +
  geom_point() + 
  removeGrid()

ggMarginal(plot, type = "histogram",
           fill = "red")



