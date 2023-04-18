# 1.0 Load libraries ----
library(vroom)
library(tidyverse)
library(data.table)
library(readr)

# 2.0 DATA IMPORT & Data Table Conversion ----

# 2.1 Assignee Data ----

col_types_assignee <- list(
  id = col_character(),
  type = col_integer(),
  name_first = col_skip(),
  name_last = col_skip(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "DS_101/02_data_wrangling/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_assignee,
  na         = c("", "NA", "NULL")
)

setDT(assignee_tbl)

# 2.2 Patent Assignee Data ----

col_types_patent_assignee <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_skip()
)

patent_assignee_tbl <- vroom(
  file       = "DS_101/02_data_wrangling/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent_assignee,
  na         = c("", "NA", "NULL")
)

setDT(patent_assignee_tbl)

# 2.3 Patent Data ----

col_types_patent <- list(
  id = col_character(),
  type = col_character(),
  number = col_skip(),
  country = col_skip(),
  date = col_date("%Y-%m-%d"),
  abstract = col_skip(),
  title = col_skip(),
  kind = col_skip(),
  num_claims = col_double(),
  filename = col_skip(),
  withdrawn = col_skip()
)

patent_tbl <- vroom(
  file       = "DS_101/02_data_wrangling/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent,
  na         = c("", "NA", "NULL")
)

setDT(patent_tbl)

# 2.4 USPC Data ----

col_types_uspc <- list(
  uuid = col_skip(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_skip(),
  sequence = col_skip()
)

uspc_tbl <- vroom(
  file       = "DS_101/02_data_wrangling/uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types_uspc,
  na         = c("", "NA", "NULL")
)

setDT(uspc_tbl)

# 3.0 DATA WRANGLING ----

# 3.1 Renaming columns ----

setnames(assignee_tbl, "id", "assignee_id")
setnames(assignee_tbl, "type", "assignee_type")
setnames(patent_tbl, "id", "patent_id")
setnames(patent_tbl, "type", "patent_type")


# 3.2 Joining / Merging Data ----

ch1_combined_data <- merge(x = assignee_tbl, y = patent_assignee_tbl, 
                           by    = "assignee_id", 
                           all.x = TRUE, 
                           all.y = FALSE)

ch2_combined_data <- merge(x = ch1_combined_data, y = patent_tbl, 
                           by    = "patent_id", 
                           all.x = TRUE, 
                           all.y = FALSE)

ch3_combined_data <- merge(x = ch1_combined_data, y = uspc_tbl, 
                           by    = "patent_id", 
                           all.x = TRUE, 
                           all.y = FALSE)

# Challenge 4 task 1 ----

ch1_keep_cols <- c("organization","assignee_type","patent_id")

ch1_combined_data <- ch1_combined_data[, ..ch1_keep_cols]

us_top10 <- ch1_combined_data[ assignee_type==2 & !is.na(patent_id) & !is.na(organization) , .N , by = organization][order(N, decreasing = TRUE)] %>% head(10)

write_rds(us_top10, "DS_101/02_data_wrangling/us_top10.rds")

us_top10 <- readRDS("DS_101/02_data_wrangling/us_top10.rds")
us_top10

# Challenge 4 task 2 ----

ch2_keep_cols <- c("date","organization","assignee_type","patent_id")

ch2_combined_data <- ch2_combined_data[, ..ch2_keep_cols]

us_top10_2019 <- ch2_combined_data[ assignee_type==2 & lubridate::year(date) == "2019" & !is.na(patent_id) & !is.na(organization) , .N , by = organization][order(N, decreasing = TRUE)] %>% head(10)

write_rds(us_top10_2019, "DS_101/02_data_wrangling/us_top10_2019.rds")

us_top10_2019 <- readRDS("DS_101/02_data_wrangling/us_top10_2019.rds")
us_top10_2019

# Challenge 4 task 3 ----

ch3_keep_cols <- c("organization","assignee_type","patent_id","mainclass_id")

ch3_combined_data <- ch3_combined_data[, ..ch3_keep_cols]

ww_top10 <- ch1_combined_data[ assignee_type==2 || assignee_type==3 & !is.na(patent_id) & !is.na(organization) , .N , by = organization][order(N, decreasing = TRUE)] %>% head(10)

main_class_top5 <- ch3_combined_data[ organization %in% as.vector(unlist(ww_top10$organization)) & !is.na(patent_id) & !is.na(mainclass_id), .N , by = mainclass_id][order(N, decreasing = TRUE)] %>% head(5)

write_rds(main_class_top5, "DS_101/02_data_wrangling/main_class_top5.rds")

main_class_top5 <- readRDS("DS_101/02_data_wrangling/main_class_top5.rds")
main_class_top5
