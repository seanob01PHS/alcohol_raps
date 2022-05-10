#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
#
# MAIN (To create a new alcohol harms quarterly report, source this script only!)
#
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
library(here)
library(odbc)
library(tidyverse)
library(knitr)
library(lubridate)
library(reactable)
library(sparklines)
library(phsstyles)
library(phsmethods)
library(plotly)
library(downloadthis)
library(purrr)
library(janitor)
library(lubridate)
library(zoo)

# set proj wd
setwd("/conf/LIST_analytics/Glasgow City/Drugs & Alcohol/Alcohol/Regular Report RAPs/")

# decalres the position of theis script within project root
here::i_am("make_qtr_report.R")

#source the coordinate function
source(here("extract", "admissions_qtr", "coordinate_qtr_extract.R"))

#~~~ User Parameters ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The end of the first quarter you want to appear in the report
# Must be the last date in a quarter
qtr_start <- ymd("2019-06-30")

# The end of the latest quarter you want to appear in the report
# Must be the last date in a quarter
qtr_end <- ymd("2021-12-31")

# Population lookup files
# Edit these when new population data becomes available
hscp_pop_lookup_path <- paste0("/conf/linkage/output/lookups/Unicode/Populations/Estimates/",
                               "HSCP2019_pop_est_5year_agegroups_1981_2020.rds")

hb_pop_lookup_path <- paste0("/conf/linkage/output/lookups/Unicode/Populations/Estimates/",
                             "HB2019_pop_est_5year_agegroups_1981_2020.rds")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#coordinates and runs the data extract
coordinate_qtr_extract(qtr_start,
                       qtr_end,
                       hscp_pop_lookup_path,
                       hb_pop_lookup_path)

#creates the output
message(".... rendering output.")
rmarkdown::render(here("dashboard", "qtr_report", "GGC_qtr_report.rmd"),
     output_file = here("output", "reports", "qtr", paste0("GGC_qtr_report_", qtr_start, "_to_", qtr_end, ".html")),
     params = list(qtr_start = qtr_start,
                   qtr_end = qtr_end),
     quiet = TRUE
     )

