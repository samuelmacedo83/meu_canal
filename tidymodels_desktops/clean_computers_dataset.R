library(stringr)

computers <- read.table("tidymodels_desktops/computer-laptops.csv", sep = ",", header = TRUE)

computers <- computers %>% 
  mutate(
    product_description = product.title,
    product_description_lower = str_to_lower(product.title),
    brand = case_when(
      str_detect(product_description_lower, "imac") ~ "APPLE",
      str_detect(product_description_lower, "lenovo") ~ "LENOVO",
      str_detect(product_description_lower, "acer") ~ "ACER",
      str_detect(product_description_lower, "asus") ~ "ASUS",
      str_detect(product_description_lower, "dell") ~ "DELL",
      str_detect(product_description_lower, "fujitsu") ~ "FUJITSU",
      str_detect(product_description_lower, "compac") ~ "COMPAC",
      str_detect(product_description_lower, "toshiba") ~ "TOSHIBA",
      str_detect(product_description_lower, "hp mini") ~ "HP",
      str_detect(product_description_lower, "hp slim") ~ "HP",
      str_detect(product_description_lower, "hp elite") ~ "TOSHIBA"
    ),
    status = case_when(
      str_detect(product_description_lower, "used") ~ "USED",
      str_detect(product_description_lower, "renewed") ~ "RENEWED",
      TRUE ~ "NEW"
    ), 
    price = as.numeric(str_remove_all(price, "Rs. |,")), 
    reviews = as.numeric(str_remove_all(reviews, "\\(|\\)"))
  ) %>% 
  filter(status != "USED") %>% 
  select(
    product_description, price, reviews, rating, brand, status
  ) 

write.table(computers, "tidymodels_desktops/tidy_computers.csv",
            row.names = FALSE
)

