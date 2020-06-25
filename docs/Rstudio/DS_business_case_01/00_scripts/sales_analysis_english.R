
# SALES ANALYSIS WITH ENGLISH TRANSLATION ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)


# 2.0 Importing Files ----
# A good convention is to use the csv file name and suffix it with tbl for the data structure tibble
order_items_tbl <- read_csv(file = "Rstudio/DS_business_case_01/00_data/01_e-commerce/01_raw_data/olist_order_items_dataset.csv") 
products_tbl    <- read_csv(file = "Rstudio/DS_business_case_01/00_data/01_e-commerce/01_raw_data/olist_products_dataset.csv")
orders_tbl      <- read_csv(file = "Rstudio/DS_business_case_01/00_data/01_e-commerce/01_raw_data/olist_orders_dataset.csv")
product_cat_name_english  <- read_excel("Rstudio/DS_business_case_01/00_data/01_e-commerce/01_raw_data/product_category_name_translation.xlsx")

# 3.0 Fixing name on new table ----
prod_cat_name_eng_corrected <- product_cat_name_english %>%
  
  set_names(names(.) %>% 
              
              str_replace_all("_", "\\."))

prod_cat_name_eng_corrected

# 4.0 Joining Data ----

order_items_joined_tbl  <- order_items_tbl %>%
  left_join(orders_tbl) %>%
  left_join(products_tbl)%>%
  left_join(prod_cat_name_eng_corrected)

# 5.0 Wrangling Data ----

order_items_wrangled_tbl <- order_items_joined_tbl %>%
  
  separate(col    = product.category.name.english,
           into   = c("main.category.name", "sub.category.name"),
           sep    = " - ",
           remove = FALSE) %>%
  
  mutate(total.price = price + freight.value) %>%
  
  select(-starts_with("product.")) %>%
  
  select(-ends_with(".date")) %>%
  
  bind_cols(order_items_joined_tbl %>% select(product.id)) %>% 
  
  select(contains("timestamp"), contains(".id"),
         main.category.name, sub.category.name, price, freight.value, total.price,
         everything()) %>% 
  
  rename(order_date = order.purchase.timestamp) %>% 
  
  set_names(names(.) %>% 
              
              str_replace_all("\\.", "_"))

# 6.0 Business Insights ----
# 6.1 Sales by Year ----
library(lubridate)
# Step 1 - Manipulate

# Create a table revenue_by_year_tbl

revenue_by_year_tbl <- order_items_wrangled_tbl %>%
  
  # Select Columns
  select(order_date, total_price) %>%
  
  # add column with year by using mutate and extracting the year from order date
  mutate(year =year(order_date)) %>%
  
  # Grouping by year and summarizing sales
  group_by(year) %>% 
  summarize(revenue = sum(total_price)) %>%
  
  # Optional: Add a column that turns the numbers into a currency format (makes it in the plot optically more appealing)
  mutate(revenue_text = scales::dollar(revenue, prefix = "$"))

revenue_by_year_tbl

# Step 2 - Visualize


revenue_by_year_tbl %>%
  
  # Setup canvas with the columns year (x-axis) and revenue (y-axis)
  ggplot(aes(x = year, y = revenue)) +
  
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = revenue_text)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  # Formatting
  scale_y_continuous(labels = scales::dollar) + # Change the y-axis
  labs(
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )
# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate

revenue_by_year_cat_main_tbl <- order_items_wrangled_tbl %>%
  
  # Select columns and add a year
  select(order_date, total_price, main_category_name) %>% 
  mutate(year = year(order_date)) %>%
  
  # Filter  > 1.000.000
  group_by(main_category_name) %>% 
  filter(sum(total_price) > 1000000) %>% # If you run the code up here, R will tell you that we have 6 groups
  ungroup() %>%
  
  # Group by and summarize year and main catgegory
  group_by(year, main_category_name) %>% 
  summarise(revenue = sum(total_price)) %>% 
  ungroup() %>%
  
  # Format $ Text
  mutate(revenue_text = scales::dollar(revenue))

revenue_by_year_cat_main_tbl  


# Step 2 - Visualize
revenue_by_year_cat_main_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = revenue, fill = main_category_name)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ main_category_name) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )


# 7.0 Writing Files ----
# If you want to interact with the filesystem use the fs package

library(fs)
fs::dir_create("Rstudio/DS_business_case_01/00_data/01_e-commerce/04_wrangled_data_student")

# 7.1 Excel ----

install.packages("writexl")
library("writexl")
write_xlsx( order_items_wrangled_tbl, "Rstudio/DS_business_case_01/00_data/01_e-commerce/04_wrangled_data_student/order_items_english.xlsx")

# 7.2 CSV ----
write_csv( order_items_wrangled_tbl, "Rstudio/DS_business_case_01/00_data/01_e-commerce/04_wrangled_data_student/order_items_english.csv")

# 7.3 RDS ----
write_rds( order_items_wrangled_tbl, "Rstudio/DS_business_case_01/00_data/01_e-commerce/04_wrangled_data_student/order_items_english.rds")

