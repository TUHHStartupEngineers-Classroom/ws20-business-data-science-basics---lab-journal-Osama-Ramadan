# Chapter 4 Challenges

library(vroom)
library(tidyverse)
library(data.table)
library(dplyr)
library(plyr)


# Import Data -----------------------------------

col_types <- list(
  id = col_character(),
  type = col_character(),
  name_first = col_character(),
  name_last = col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "DS_101/02_data_wrangling/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)


col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_character()
)

pat_assignee_tbl <- vroom(
  file       = "DS_101/02_data_wrangling/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

assignee_tbl <- assignee_tbl %>% select(-(3:4))

assignee_tbl %>% setDT()
pat_assignee_tbl %>% setDT()

assignee_uni <- pat_assignee_tbl$assignee_id %>% unique() %>% as_tibble()

pat_per_assignee <- ddply(pat_assignee_tbl,.(assignee_id),nrow)

test <- assignee_tbl %>% left_join(y = pat_per_assignee , by = c ("id" = "assignee_id"))

final <- test %>% arrange(desc(V1))
