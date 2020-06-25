# SALES BY STATE ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)


# 2.0 Importing Files ----

sellers_tbl <- read_csv(file = "Rstudio/DS_business_case_01/00_data/01_e-commerce/01_raw_data/olist_sellers_dataset.csv") 
order_items_tbl <- read_csv(file = "Rstudio/DS_business_case_01/00_data/01_e-commerce/01_raw_data/olist_order_items_dataset.csv")
orders_tbl      <- read_csv(file = "Rstudio/DS_business_case_01/00_data/01_e-commerce/01_raw_data/olist_orders_dataset.csv")


# 3.0 Examining Data ----

# 4.0 Joining Data ----

sellers_joined_tbl  <- order_items_tbl %>%
  left_join(orders_tbl) %>%
  left_join(sellers_tbl)

# 5.0 Wrangling Data ----

sellers_wrangled_tbl <- sellers_joined_tbl %>%
  
  separate(col    = seller.location,
           into   = c("seller.city", "seller.state"),
           sep    = ", ",
           remove = FALSE) %>%
  
  mutate(total.price = price + freight.value) %>%
  
  select(-starts_with("product.")) %>%
  
  select(-ends_with(".date")) %>%
  
  
  rename(order_date = order.purchase.timestamp) %>% 
  
  set_names(names(.) %>% 
              
              str_replace_all("\\.", "_"))

# 6.0 Business Insights ----

library(lubridate)

# 6.1 Sales by Year and Category 2 ----

# Step 1 - Manipulate

revenue_by_year_and_state_tbl <- sellers_wrangled_tbl %>%
  
  # Select columns and add a year
  select(order_date, total_price, seller_city, seller_state) %>% 
  mutate(year = year(order_date)) %>%
  
  # Filter  > 1.000.000
  group_by(seller_state) %>% 
  filter(sum(total_price) > 1000000) %>% # If you run the code up here, R will tell you that we have 6 groups
  ungroup() %>%
  
  # Group by and summarize year and main catgegory
  group_by(year, seller_state) %>% 
  summarise(revenue = sum(total_price)) %>% 
  ungroup() %>%
  
  # Format $ Text
  mutate(revenue_text = scales::dollar(revenue))

revenue_by_year_and_state_tbl 


# Step 2 - Visualize
revenue_by_year_and_state_tbl  %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = revenue, fill = seller_state)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ seller_state) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Revenue by year and state",
    subtitle = "SP had the largest portion of revenues",
    fill = "State" # Changes the legend name
  )

# 7.0 Writing Files ----

library(fs)
fs::dir_create("Rstudio/DS_business_case_01/00_data/01_e-commerce/04_wrangled_data_student")

# 7.1 Excel ----

install.packages("writexl")
library("writexl")
write_xlsx( sellers_wrangled_tbl, "Rstudio/DS_business_case_01/00_data/01_e-commerce/04_wrangled_data_student/sellers_table.xlsx")
write_xlsx(revenue_by_year_and_state_tbl, "Rstudio/DS_business_case_01/00_data/01_e-commerce/04_wrangled_data_student/revenue_by_year_and_state.xlsx")

# 7.2 CSV ----

write_csv( sellers_wrangled_tbl, "Rstudio/DS_business_case_01/00_data/01_e-commerce/04_wrangled_data_student/sellers_table.csv")
write_csv( revenue_by_year_and_state_tbl , "Rstudio/DS_business_case_01/00_data/01_e-commerce/04_wrangled_data_student/revenue_by_year_and_state.csv")

# 7.3 RDS ----
write_rds( sellers_wrangled_tbl, "Rstudio/DS_business_case_01/00_data/01_e-commerce/04_wrangled_data_student/sellers_table.rds")
write_rds( revenue_by_year_and_state_tbl , "Rstudio/DS_business_case_01/00_data/01_e-commerce/04_wrangled_data_student/revenue_by_year_and_state.rds")



