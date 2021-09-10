library(tidymodels)
library(ggrepel)

computers <- read.table("tidymodels_desktops/tidy_computers.csv", header =  TRUE) 

clusters <- computers %>% 
  select(price, rating, reviews) %>% 
  kmeans(centers = 2)

augment(clusters, computers) %>% 
  sample_n(20) %>% 
  ggplot(aes(x = price, y = reviews, color = .cluster)) +
  geom_point() +
  geom_text_repel(aes(label = brand))


tibble(k = 1:9) %>% 
  mutate(
    kmeans= map(k, ~ kmeans(select(computers, price, rating, reviews), .x)),
    glanced = map(kmeans, glance)
  ) %>% 
  unnest(glanced) %>% 
  ggplot(aes(x = k, y = tot.withinss)) +
  geom_line() +
  geom_point(size = 2)

# testing
clusters <- computers %>% 
  select(price, rating, reviews) %>% 
  kmeans(centers = 3)

augment(clusters, computers) %>% 
  sample_n(30) %>% 
  ggplot(aes(x = price, y = reviews, color = .cluster)) +
  geom_point() +
  geom_text_repel(aes(label = brand))
