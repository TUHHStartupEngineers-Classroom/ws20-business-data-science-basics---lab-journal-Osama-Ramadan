library(tidyverse)
# Excel Files
library(readxl)
# replace . with _ 
bikes_tbl <- read_excel("DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx") %>%
  
  # Separate product category name in main and sub
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%
  
  # Renaming columns
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# Using Select ----------------------------------------------------------
bikes_tbl %>%
  select(bike_id, model, model_year)

bikes_tbl %>%
  select(1:3)

bikes_tbl %>%
  select(1, contains("model"))

bikes_tbl %>%
  select(model, price)

bikes_tbl %>%
  select(category_1:category_3, everything())

# Alternative using relocate()
bikes_tbl %>%
  relocate(category_1:category_3)

bikes_tbl %>%
  select(starts_with("model"))

bikes_tbl %>%
  # select(price) %>% Does not work
  pull(price) %>%
  mean()

bikes_tbl %>%
  select(where(is.character))

bikes_tbl %>%
  select(where(is.numeric))

bikes_tbl %>%
  select(!where(is.numeric))

bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>% 
  rename(
    Model           = model,
    `Bike Family`   = category_1,
    `Ride Style`    = category_2,
    `Bike Category` = category_3,
    `Price in Euro` = price
  )

bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>% 
  set_names(c("Model", "Bike Family", "Ride Style", "Bike Category", "Price in Euro"))

# An example using str_replace
bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>% 
  set_names(names(.) %>% str_replace("_", " ") %>% str_to_title())

# Working with Rows --------------------------------------------------------------------

bikes_tbl %>%
  select(model, price) %>%
  arrange(desc(price)) %>%
  View()

bikes_tbl %>%
  select(model, price) %>%
  filter(price > mean(price))

bikes_tbl %>%
  select(model, price) %>%
  filter((price > 5000) | (price < 1000)) %>%
  arrange(desc(price)) %>%
  View()

bikes_tbl %>%
  select(model, price) %>%
  filter(price > 5000,
         model %>% str_detect("Endurace")
  )

bikes_tbl %>%
  filter(category_1 %in% c("Hybrid / City", "E-Bikes"))

bikes_tbl %>%
  filter(category_2 == "E-Mountain")

bikes_tbl %>%
  filter(category_2 != "E-Mountain")

bikes_tbl %>%
  filter(!(category_2 %in% c("Hybrid / City", "E-Bikes")))

bikes_tbl %>%
  arrange(desc(price)) %>%
  slice(1:5)

bikes_tbl %>%
  arrange(price) %>%
  slice(1:5)

bikes_tbl %>%
  arrange(desc(price)) %>%
  slice((nrow(.)-4):nrow(.))

bikes_tbl %>%
  distinct(category_1)

bikes_tbl %>%
  distinct(category_1, category_2)

bikes_tbl %>%
  distinct(category_1, category_2, category_3)

# Column Transformation ---------------------------------------------------

bike_orderlines_tbl <- read_excel("DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")

bike_orderlines_tbl %>%
  mutate(freight_costs = 2 * weight)

bike_orderlines_tbl %>%
  mutate(total_price = log(total_price))

bike_orderlines_tbl %>%
  mutate(price_log = log(total_price)) %>%
  mutate(price_sqrt = total_price^0.5)

bike_orderlines_tbl %>%
  mutate(is_strive = model %>% str_to_lower() %>% str_detect("strive")) %>%
  filter(is_strive)

bike_orderlines_tbl %>%
  mutate(price_binned = ntile(total_price, 3)) %>% 
  select(total_price, price_binned, everything())

bike_orderlines_tbl %>%
  mutate(price_binned = ntile(total_price, 3)) %>%
  mutate(price_binned2 = case_when(
    total_price > quantile(total_price, 0.75) ~ "High",
    total_price > quantile(total_price, 0.25) ~ "Medium",
    TRUE ~ "Low" # Everything else
  )) %>% 
  select(total_price, price_binned, price_binned2, everything())

bike_orderlines_tbl %>%
  mutate(bike_type = case_when(
    model %>% str_to_lower() %>% str_detect("aeroad") ~ "Aeroad",
    model %>% str_to_lower() %>% str_detect("ultimate") ~ "Ultimate",
    TRUE ~ "Not Aeroad or Ultimate" # Everything else
  )) %>% 
  select(bike_type, everything())

# Summary Calculation --------------------------------------------------------

bike_orderlines_tbl %>%
  summarise(
    revenue = sum(total_price)
  )

bike_orderlines_tbl %>%
  group_by(category_1) %>%
  summarise(revenue = sum(total_price))

bike_orderlines_tbl %>%
  group_by(category_1, category_2) %>%
  summarise(revenue = sum(total_price)) %>%
  # Always ungroup() after you summarise(). Left-over groups will cause difficult-to-detect errors.
  ungroup() %>%
  arrange(desc(revenue))

bike_orderlines_tbl %>%
  group_by(category_1, category_2) %>%
  summarise(
    count = n(),
    avg   = mean(total_price),
    med   = median(total_price),
    sd    = sd(total_price),
    min   = min(total_price),
    max   = max(total_price)
  ) %>%
  ungroup() %>%
  arrange(desc(count))

bike_orderlines_missing <- bike_orderlines_tbl %>%
  mutate(total_price = c(rep(NA, 4), total_price[5:nrow(.)]))

# detect missing (absolute)
bike_orderlines_missing %>%
  summarise(across(everything(), ~sum(is.na(.))))

# detect missing (relative)
bike_orderlines_missing %>%
  summarise(across(everything(), ~sum(is.na(.)) / length(.)))

# Handling missing data
bike_orderlines_missing %>%
  filter(!is.na(total_price))

# Reshaping --------------------------------------------------------------------------

bike_data_sizes_tbl %>% 
  select(name, year, price_euro, color, size, stock_availability) %>% 
  pivot_wider(names_from  = size, 
              values_from = stock_availability)

bikeshop_revenue_tbl <- bike_orderlines_tbl %>%
  select(bikeshop, category_1, total_price) %>%
  
  group_by(bikeshop, category_1) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  arrange(desc(sales))

bikeshop_revenue_formatted_tbl <- bikeshop_revenue_tbl %>%
  pivot_wider(names_from  = category_1,
              values_from = sales) %>%
  mutate(
    Mountain = scales::dollar(Mountain, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    Gravel = scales::dollar(Gravel, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    Road     = scales::dollar(Road, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    `Hybrid / City` = scales::dollar(`Hybrid / City`, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    `E-Bikes` = scales::dollar(`E-Bikes`, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €")
  )

bikeshop_revenue_formatted_tbl %>%
  pivot_longer(cols           = c(names(.)[2:6]),
               names_to       = "category_1",
               values_to      = "sales",
               values_drop_na = T) %>%
  mutate(sales =  sales %>% str_remove_all("€|\\.") %>% as.double())

# Joining and Bending -----------------------------------------------------------

order_dates_tbl <- bike_orderlines_tbl %>% select(1:3)
order_items_tbl  <- bike_orderlines_tbl %>% select(1:2,4:8)

order_dates_tbl %>%
  
  # By argument not necessary, because both tibbles share the same column names
  left_join(y = order_items_tbl, by = c("order_id" = "order_id", "order_line" = "order_line"))

bike_orderlines_tbl %>%
  select(-contains("category")) %>%
  
  bind_cols(
    bike_orderlines_tbl %>% select(category_1)
  )

train_tbl <- bike_orderlines_tbl %>%
  slice(1:(nrow(.)/2))

test_tbl <- bike_orderlines_tbl %>%
  slice((nrow(.)/2 + 1):nrow(.))

train_tbl %>%
  bind_rows(test_tbl)

# Splitting and combining -------------------------------------------------------------

bike_orderlines_tbl %>% 
  select(order_date) %>% 
  mutate(order_date = as.character(order_date)) %>%
  
  # separate
  separate(col  = order_date,
           into = c("year", "month", "day"),
           sep  = "-", remove = FALSE) %>%
  
  mutate(
    year  = as.numeric(year),
    month = as.numeric(month),
    day   = as.numeric(day)
  ) %>%
  
  # unite
  unite(order_date_united, year, month, day, sep = "-", remove = FALSE) %>%
  mutate(order_date_united = as.Date(order_date_united))

# Data.table -----------------------------------------------------------------------------

library(data.table)
url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_dt <- fread(url)

class(covid_data_dt)

# Create a large .csv file
test_df <- data.frame(matrix(runif(10000000), nrow=1000000))
write.csv(test_df, 'test_df.csv', row.names = F)

# Time taken by read.csv to import
system.time({test_df_base <- read.csv("test_df.csv")})
# Time taken by read_csv to import
system.time({test_df_readr <- read_csv("test_df.csv")})
# Time taken by fread to import
system.time({test_dt <- fread("test_df.csv")})

test_dt <- data.table(ID = c("b","b","b","a","a","c"),
                      a  = 1:6,
                      b  = 7:12,
                      c  = 13:18)

## FROM[WHERE, SELECT/ORDER BY/UPDATE, GROUP BY]
covid_data_dt[i, j, by]

# Example (filter by year, sum cases, group by continent)
covid_data_dt[year == 2019, sum(cases), by = continentExp]

covid_data_dt[countriesAndTerritories == "Germany" & 
                lubridate::month(dateRep, label = T, abbr = F) == "June"]

covid_data_dt[order(year, month, day, -countriesAndTerritories)]

# Return as a vector
covid_data_dt[,geoId]
# Select multiple columns
covid_data_dt[,c("geoId", "countriesAndTerritories")]

# Return as a data.table
covid_data_dt[,list(geoId)]
# Short form using .
covid_data_dt[,.(geoId)]
# Select multiple columns
covid_data_dt[,.(geoId, countriesAndTerritories)]

# Rename them directly
covid_data_dt[,.(CountryCode = geoId, country = countriesAndTerritories)]

# select columns named in a variable using the ..prefix
select_cols = c("cases", "deaths")
covid_data_dt[, ..select_cols]

# List names 
colnames(covid_data_dt)
setnames(covid_data_dt, "dateRep", "date")
setnames(covid_data_dt, "countriesAndTerritories", "country")
setnames(covid_data_dt, "continentExp", "continent")

# List all internal data sets
data()

# Load specified data sets
data("airquality")


aq_dt <- data.table(airquality)
aq_dt[!is.na(Ozone), .(Solar.R, Wind, Temp)]


# Solution 2
setDT(airquality)
airquality[!is.na(Ozone), .(Solar.R, Wind, Temp)]

covid_data_dt[,sum(deaths > 1000)]

# to list the observations put it in i
covid_data_dt[deaths > 1000]

covid_data_dt[,  `:=`(deaths_per_capita = deaths / popData2019,
                      cases_per_capita = cases / popData2019,
                      deaths_per_cases = deaths / cases)]

# To delete a column, assign it to NULL
covid_data_dt[, deaths_per_cases := NULL]

covid_data_dt[,date := lubridate::dmy(date)]

data("mtcars") # step not absolutely necessary
mtcars$carname <- rownames(mtcars)
mtcars_dt <- as.data.table(mtcars)
mtcars_dt[, mileage_type := ifelse(mpg > 20, 'high', 'low')]

covid_data_dt[country == "Germany" & month == 4, 
              .(m_cases = mean(cases), 
                m_death = mean(deaths)
              )
]

covid_data_dt[country == "United_States_of_America" & 
                month == 5 & deaths < 1000, 
              length(day)
]

covid_data_dt[country == "United_States_of_America" & 
                month == 5 & deaths < 1000, 
              .N
]

covid_data_dt[deaths > 1000, .N, by = country]
covid_data_dt[,.I[deaths > 1000]]

covid_data_dt[continent == "Europe",
              .(mean(cases), mean(deaths)),
              by = .(country, month, year)
]

library(magrittr) # to use the pipe
mtcars_dt[, .(.N, mileage = mean(mpg) %>% round(2)), by=gear]

covid_data_dt[, .(
  m_cases  = round(mean(cases),  digits = 1), 
  m_deaths = round(mean(deaths), digits = 1)
), 
by = .(country)][order(-m_cases)]

covid_data_dt[, .N, 
              .(
                death_gt_1k = deaths > 1000, 
                cases_lt_1k = cases < 1000
              )
]

covid_data_dt[, lapply(.SD, mean), by = year]
covid_data_dt[, lapply(.SD, mean), 
              by = .(year, month), 
              .SDcols = c("cases", "deaths")
]

setkey(covid_data_dt, date, country)

covid_data_EUR_dt <- covid_data_dt[ continent == "Europe", 
                                    lapply(.SD, function(x) {
                                      x %>% 
                                        mean() %>% 
                                        round(1)
                                    }
                                    ), 
                                    by = .(country), 
                                    .SDcols = c("cases", "deaths")
]

# Set key
setkey(covid_data_EUR_dt, country)
key(covid_data_EUR_dt)

# Create two data.tables from that
cd_dt1 <- covid_data_EUR_dt[, .(country, cases)]
cd_dt2 <- covid_data_EUR_dt[1:20, .(country, deaths)]

# Join them
cd_dt1[cd_dt2]

# Remove keys
setkey(cd_dt1, NULL)
setkey(cd_dt2, NULL)
# Join
cd_dt1[cd_dt2, on = "country"]
# If they had different colnames
cd_dt1[cd_dt2, on = c(colA = "colB")]

# Alternatively you can use the function merge()
# Inner Join
merge(cd_dt1, cd_dt2, by='country')
# Left Join
merge(cd_dt1, cd_dt2, by='country', all.x = T)
# Outer Join
merge(cd_dt1, cd_dt2, by='country', all = T)
# If they had different colnames use by.x="colA", by.y="colB"

cd_dt1[cd_dt2, on = "country", deaths := i.deaths]

dt_list    <- list(cd_dt1, cd_dt2, cd_dt3)
merge_func <- function(...) merge(..., all = TRUE, by='country')
dt_merged  <- Reduce(merge_func, dt_list)

m  = matrix(1,nrow=100000,ncol=100)
DF = as.data.frame(m)
DT = as.data.table(m)

system.time(for (i in 1:10000) DF[i,1] <- i)
## 591 seconds

system.time(for (i in 1:10000) DT[i,V1:=i])
## 2.4 seconds  ( 246 times faster, 2.4 is overhead in [.data.table )

system.time(for (i in 1:10000) set(DT,i,1L,i))
## 0.03 seconds  ( 19700 times faster, overhead of [.data.table is avoided )

