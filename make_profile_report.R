
################################################################
###### Load packages
################################################################
# File to run and combine Alcohol-related and Alcohol-related mental health hospital admissions
# Note - make sure to load odbc before loading tidyverse
library(odbc)
library(janitor)
library(lubridate)
library(zoo)
library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(stringr)
library(readxl)
library(writexl)
library(here)
library(purrr)
# phsmethods package
# to install uncomment following lines
# install.packages("remotes")
# library(remotes)
# remotes::install_github("Public-Health-Scotland/phsmethods", upgrade = "never")
library(phsmethods)

source(here("extract", "profile_annual", "coordinate_profile_extract.R"))




start_year <- 2015
end_year <- 2020




start_year_str <- start_year %>% as.character()
end_year_str <- end_year %>% as.character()
start_year <- start_year  %>% ymd(truncated = 2L)
end_year <- end_year %>% ymd(truncated = 2L)

# hospital admissions are fin years
smr01_start_date <- start_year %m+% months(3)
smr01_end_date <- end_year + period("1 year 2 months 30 days") 


# mh admissions are 3 fin year aggregates
smr04_start_date <- (start_year - period("2 years")) %m+% months(3)
smr04_end_date <- end_year + period("1 year 2 months 30 days")


# deaths are y cal year aggregates
deaths_start_date <- start_year - period("2 year")
deaths_end_date <- end_year + period("1 year") - period("1 day")



coordinate_profile_extract(start_year_str,
                           end_year_str,
                           smr01_start_date,
                           smr01_end_date,
                           smr04_start_date,
                           smr04_end_date,
                           deaths_start_date,
                           deaths_end_date)


#creates the output
outfile <- here("output", "reports", paste0("GGC_profile_report_", start_year_str, "_to_", end_year_str, ".html"))
rmarkdown::render(here("dashboard", "profile_report", "GGC_profile_report.rmd"),
                  output_file = outfile,
                  params = list(start_year = start_year_str, end_year = end_year_str))
