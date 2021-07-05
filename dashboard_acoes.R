library(tidyquant)
library(dplyr)
library(ggplot2)


papeis  <- c("VALE3", "BRAP4", "BTOW3", "RENT3", "SUZB3", "NTCO3", "LREN3", "SANB11",
             "TOTS3", "SULA11")

papeis <- paste0(papeis, ".SA")

prices <- tq_get(papeis, 
  get = "stock.prices", from = " 2010-01-01"
) %>%
  mutate(symbol = sub(".SA", "", symbol))

prices <- c(
  "VALE3", "BRAP4", "BTOW3", "RENT3", "GGBR4",
  "NTCO3", "CIEL3", "SANB11", "TOTS3", "SULA11"
) %>% 
  paste0(".SA") %>% 
  tq_get(get = "stock.prices", from = " 2010-01-01") %>% 
  mutate(symbol = sub(".SA", "", symbol))

write.table(prices, "prices.csv", sep = ",", row.names = FALSE)
prices <- read.csv("prices.csv") %>% 
  mutate(date = as.Date(date))


plot <- ggplot(prices) +
  aes(x= date, y = adjusted, color = symbol) +
  geom_line(size = 1.1) +
  labs(x = "", y = "Cotação", color = "Papel") +
  theme(panel.background = element_blank())
  ggplotly()


prices %>% 
  filter(symbol == "VALE3")






prices <- mutate(prices, symbol = sub(".SA", "", symbol))


coca %>% 
  #filter(date >= Sys.Date() - 365) %>% 
  ggplot() + 
  aes(x = date, y = adjusted) +
  geom_line() 

S1 <- 7
M1 <- 30
M6 <- 180
A1 <- 365
A10 <- 3650
ALL <- TRUE

tempo <- 7

coca %>% 
  filter(
    if(tempo == "todo") {
      TRUE
      } else {
        date >= Sys.Date() - tempo
      }
  )
    
    
    date >= Sys.Date() - A1) %>% 
  ggplot() + 
  aes(x = date, y = adjusted) +
  geom_line() 