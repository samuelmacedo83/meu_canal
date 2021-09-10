library(dplyr)
library(ggplot2)

computers <- read.table("tidymodels_desktops/tidy_computers.csv", header =  TRUE) 

# analise descritva
qplot(sqrt(price), reviews, data = computers)

computers %>% 
  group_by(status) %>% 
  summarise(media = mean(price)) %>% 
  arrange(-media)

computers %>% 
  group_by(status) %>% 
  summarise(media = mean(reviews)) %>% 
  arrange(-media)

computers %>% 
  group_by(status) %>% 
  summarise(media = mean(rating)) %>% 
  arrange(-media)

computers %>% 
  group_by(brand) %>% 
  summarise(media = mean(price)) %>% 
  arrange(-media)

computers %>% 
  group_by(brand) %>% 
  summarise(media = mean(reviews)) %>% 
  arrange(-media)

computers %>% 
  group_by(brand) %>% 
  summarise(media = mean(rating)) %>% 
  arrange(-media)

computers %>% 
  group_by(brand, status) %>% 
  summarise(media = mean(price))