#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
#
# MAIN (To create a new alcohol harms annual IZ report, source this script only!)
#
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}


################################################################
###### Load packages
################################################################
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

# set proj wd
setwd("/conf/LIST_analytics/Glasgow City/Drugs & Alcohol/Alcohol/Regular Report RAPs/")

# decalres the position of theis script within project root
here::i_am("make_profile_report.R")

source(here("extract", "profile_annual", "coordinate_profile_extract.R"))



#~~~ User Parameters ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# There are many different relevant dates for this extract
# please refer to the SOP or look at code below to 
# see what the start/end years imply for these.


# earliest cal year that wholly appears in each type of extract
start_year <- 2015

# latest cal year that wholly appears in each type of extract
end_year <- 2021


# Population lookup file
# Edit this when new population data becomes available
pop_lookup_path <- paste0("/conf/linkage/output/lookups/Unicode/Populations/Estimates/",
                          "DataZone2011_pop_est_2011_2020.rds")


# Run both false if not debugging. 
# This will save the raw extracts to a cache.
# for 2015 to 2021, this file was ~118Mb!! Delete after use.
save_cache <-  FALSE

# To load from cache instead of running extract.
# For debugging
load_from_cache <-  FALSE


# NOTE seting:
#   save_cache <- TRUE and
#   load_cache <- TRUE
# makes no sense. This will not do what you expect it to.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# manipulating start/end year in to more exact start/end dates for extracts

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




# performs extract, returns output dataframe and most recent year
# present in population data
profile_extract_outputs <- coordinate_profile_extract(start_year_str,
                                                      end_year_str,
                                                      smr01_start_date,
                                                      smr01_end_date,
                                                      smr04_start_date,
                                                      smr04_end_date,
                                                      deaths_start_date,
                                                      deaths_end_date,
                                                      pop_lookup_path,
                                                      save_cache = save_cache,
                                                      load_from_cache = load_from_cache)

profile_extract_outputs[["output"]] %>% 
  write_csv(here("output", "profile_annual_data", paste0("profile_data_", start_year_str, "_to_", end_year_str, ".csv")))

population_year <- profile_extract_outputs[["population_year"]]

# creates (renders) the html output
message(".... rendering output.")
outfile <- here("output", "reports", "profile", paste0("GGC_profile_report_", start_year_str, "_to_", end_year_str, ".html"))
rmarkdown::render(here("dashboard", "profile_report", "GGC_profile_report.rmd"),
                  output_file = outfile,
                  params = list(start_year = start_year_str,
                                end_year = end_year_str,
                                population_year = population_year),
                  quiet = TRUE)
