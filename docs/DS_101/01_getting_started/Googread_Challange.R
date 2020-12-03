
#MYAPI

# Libraries 
library(httr)
library(jsonlite)
library(dplyr)
library(keyring)
library("data.table")
library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing
library(lubridate)
library(anytime)
# Securing My API 
keyring::key_set("MYAPI")

# get data from API
my_url <- paste0("http://dataservice.accuweather.com/forecasts/",
                 "v1/daily/5day/178556?apikey=", 
                 key_get("MYAPI"))
my_url
my_raw_result <- httr::GET(my_url)
my_content <- httr::content(my_raw_result, as = 'text') %>%
  fromJSON()
forcast <- my_content$DailyForecasts

Weather <- forcast %>% select(EpochDate,Temperature)
Temperature<- Weather %>% select(Temperature)%>% as.data.table()  
date <- Weather %>% select(EpochDate) %>% as_tibble()

date <- map(date, anytime)
date<- date %>% as_tibble()
Max_Temp <- Temperature %>% select(Temperature.Maximum.Value)
Min_Temp <- Temperature %>% select(Temperature.Minimum.Value)

names(Max_Temp)[1]<-paste("Maximum Temperature")
names(Min_Temp)[1]<-paste("Minimum Temperature")

Final_Forcast <- cbind(date,Max_Temp,Min_Temp)
Final_Forcast %>% as_tibble()
barplot(Final_Forcast,beside=T)
Final_Forcast %>%
  
  ggplot(aes(x = EpochDate, y = `Maximum Temperature`)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot

  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )

# Challange 2 -------------------------------------------------------------
url_home          <- "https://www.rosebikes.de/fahrr%C3%A4der"
xopen(url_home) # Open links directly from RStudio to inspect them

# Read in the HTML for the entire webpage
html_home         <- read_html(url_home)

bike_family_tbl <- html_home %>%
  
  html_nodes(css = ".catalog-categories-item__link") %>%
  html_text()
  # ...and extract the information of the id attribute
 # html_attr('title')
bike_family_tbl

price <- html_home %>%
  
  html_nodes(css = ".catalog-categories-item__link") %>%
  html_text() %>%
  stringr::str_replace_all("[[/.]]", replacement = "") %>%
  stringr::str_extract(pattern = "[0-9 | /,]+") %>% 
  as_tibble()
# ...and extract the information of the id attribute
# html_attr('title')
price

model <- html_home %>%
  
  html_nodes(css = ".catalog-categories-item__link") %>%
  html_text() %>%
  stringr::str_extract("[a-z | A-Z | /-]+") %>%
  as_tibble()
#as.numeric()
# ...and extract the information of the id attribute
# html_attr('title')
model

names(model)[1]<-paste("Model")
names(price)[1]<-paste("Price")

Data_Final <- cbind(model,price)

