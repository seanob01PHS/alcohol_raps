#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
#
# MAIN (To create a new alcohol harms quarterly report, source this script only!)
#
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

# This script should be in the following location for it to work
# "/conf/LIST_analytics/Glasgow City/Drugs & Alcohol/Alcohol/Regular Report RAPs/"

library(here)
library(tidyverse)
library(knitr)
library(lubridate)
library(reactable)
library(sparklines)
library(phsstyles)
library(plotly)
library(downloadthis)
library(purrr)

# set proj wd
setwd("/conf/LIST_analytics/Glasgow City/Drugs & Alcohol/Alcohol/Regular Report RAPs/")

# decalres the position of theis script within project root
here::i_am("make_qtr_report.R")

#source the coordinate function
source(here("extract", "admissions_qtr", "coordinate_qtr_extract.R"))



# The end of the first quarter you want to appear in the report
# Must be the last date in a quarter
qtr_start <- ymd("2019-06-30")

# The end of the latest quarter you want to appear in the report
# Must be the last date in a quarter
qtr_end <- ymd("2021-09-30")




#coordinates and runs the data extract
coordinate_qtr_extract(qtr_start, qtr_end)

#creates the output
rmarkdown::render(here("dashboard", "qtr_report", "GGC_qtr_report.rmd"),
     output_file = here("output", "reports", "qtr", paste0("GGC_qtr_report_", qtr_start, "_to_", qtr_end, ".html")),
     params = list(qtr_start = qtr_start,
                   qtr_end = qtr_end)
     )

