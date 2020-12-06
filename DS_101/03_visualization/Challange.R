
# Libraries -----------------------------
library(tidyverse)
library(lubridate)

# Data Processing -----------------------------------------
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
covid_data_dated_tbl <- covid_data_tbl %>% mutate(date = make_date(year, month, day))

Total_casses <- covid_data_dated_tbl %>% group_by(date)%>% summarise(Total = sum(cases))
Total_casses <- Total_casses %>% arrange(!desc(date))
cumsum <- Total_casses %>% mutate(cumsum = cumsum(Total))

# Visualization ------------------------------------------
cumsum %>%
  
  ggplot(aes(date, cumsum)) +
  
  theme_light() +
  theme(
    
    title = element_text(face = "bold", color = "#08306B")
  ) +
  scale_x_date(limits = as.Date(c("2019-12-31","2020-12-06"))) +
  ggtitle("limits = as.Date(c(\"2019-12-31\",\"2020-12-06\"))")+
  
  scale_y_continuous(labels = scales::scientific_format(scale = 1e-6, 
                                                    preix = "*",
                                                    suffix = "")) +
  
  labs(
    title = "Covid-19 Cases",
    subtitle = "Number of cases trending up",
    caption = "",
    x = "Date",
    y = "Cases",
    color = "cumsum" # Legend text
  )+
  
  geom_line(size = 0.8, linetype = 1 , color = "blue")

#-----------------------------------------------------------------------------------
# Import Data & Processing

world_map <- map_data("world")


mor_rate_by_country <- covid_data_dated_tbl %>% group_by(countriesAndTerritories)%>% summarise(Mor_rate = sum(deaths)/sum(popData2019))


mor_rate_by_country <- mor_rate_by_country %>% mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
    
  ))
Corona_Data <- world_map %>% left_join(mor_rate_by_country, by = c("region" = "countriesAndTerritories"))

Corona_Data <- Corona_Data %>% mutate(More_rate_factored = Mor_rate*10000000000) %>% filter(Mor_rate > 0) 

# Visualization ---------------------------------------------------

ggplot(Corona_Data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=More_rate_factored), colour = "white") +
  expand_limits(x = Corona_Data$long, y = Corona_Data$lat)+
  scale_fill_viridis_c(option = "C")+
  labs(
  title = "World Wide Mortality Rate",
  subtitle = "",
  fill = "Mortality Rate" # Changes the legend name
)
