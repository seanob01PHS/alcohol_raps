### Previously "Alcohol Harms Script.R"
###
### Run each year. Produces output for 3 alcohol harms indicatrs at IZ
### level for releavnt time periods.

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


coordinate_profile_extract <- function(){
  
  # Source all extract files
  list.files(path = here("extract", "profile_annual", "db_extract"),
             pattern = "*.R",
             full.names = TRUE) %>% 
    walk(source)
  
  # Source all wrangle files
  list.files(path = here("extract", "profile_annual", "wrangle"),
             pattern = "*.R",
             full.names = TRUE) %>% 
    walk(source)
  
  
  # Source pops lookup
  source(here("extract", "profile_annual", "pop_lookups.R"))
  
  data.out <- here("output", "profile_annual_data")
  
  # start and end dates for filtering stay data 
  # changed to date format
  smr01_start_date <- ymd("2015-04-01")
  smr01_end_date <- ymd("2021-03-31")
  
  smr04_start_date <- ymd("2013-04-01")
  smr04_end_date <- ymd("2021-03-31")
  
  deaths_start_date <- ymd("2013-01-01")
  deaths_end_date <- ymd("2020-12-31")
  
  # calc 3 months before start date for extract - accounts for admissions before start date (discharge based analysis)
  # changed to date format
  buffer_smr01_start_date <-  smr01_start_date - months(3)
  buffer_smr04_start_date <-  smr04_start_date - months(3)
  
  channel <- suppressWarnings(
    dbConnect(odbc(),
              dsn = "SMRA",
              uid = Sys.info()[['user']],
              pwd = .rs.askForPassword("What is your LDAP password?"))
  )
  
  
  alc_diag <- "E244|E512|F10|G312|G621|G721|I426|K292|K70|K852|K860|O354|P043|Q860|R780|T510|T511|T519|X45|X65|Y15|Y573|Y90|Y91|Z502|Z714|Z721"
  
  data_alcohol_episodes_ha <- ha_smr01_extract(channel,
                                               alc_diag,
                                               buffer_smr01_start_date,
                                               smr01_start_date,
                                               smr01_end_date)
  data_alcohol_episodes_mh <- mh_smr04_extract(channel,
                                               alc_diag,
                                               buffer_smr04_start_date,
                                               smr04_start_date,
                                               smr04_end_date)
  data_alcohol_deaths <- deaths_nrs_extract(channel,
                                            deaths_start_date,
                                            deaths_end_date)
  
  
  # pop lookup
  pop_lookup_total <- pop_lookups()
  fin_year_pops <- pop_lookup_total[["fin_year_pops"]]
  cal_year_pops <- pop_lookup_total[["cal_year_pops"]]
  
  
  # postcode lookup
  postcode_lookup <- read.csv('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2021_2.csv',
                              stringsAsFactors=F) %>%
    clean_names() %>%
    select(pc7, intzone2011) %>%
    rename(postcode = "pc7") %>% 
    mutate(postcode = gsub(" ", "", postcode)) %>% 
    # old pc lookup had differently named columns
    # this is to avoid renaming
    rename(int_zone2011 = intzone2011)
  
  
  alcohol_admission_output_ha_smr01 <- ha_smr01_wrangle(data_alcohol_episodes_ha,
                                                        smr01_start_date,
                                                        smr01_end_date,
                                                        postcode_lookup,
                                                        fin_year_pops)
  
  alcohol_admission_output_mh_smr04 <- mh_smr04_wrangle(data_alcohol_episodes_mh,
                                                        smr04_start_date,
                                                        smr04_end_date,
                                                        postcode_lookup,
                                                        fin_year_pops)
  
  alcohol_deaths_output_nrs <- deaths_nrs_wrangle(data_alcohol_deaths,
                                                  deaths_start_date,
                                                  deaths_end_date,
                                                  cal_year_pops)
  
  
  alcohol_data_output <- bind_rows(alcohol_admission_output_ha_smr01,
                                   alcohol_admission_output_mh_smr04,
                                   alcohol_deaths_output_nrs) %>% 
    arrange(iz, hscp, hscp_locality, year, indicator) %>% 
    mutate(iz = case_when(is.na(iz) ~ hscp_locality,
                          TRUE ~ iz),
           iz_name = case_when(is.na(iz_name) ~ "",
                               TRUE ~ iz_name),
           hscp = case_when(is.na(hscp) ~ "",
                            TRUE ~ hscp),
           lookup = paste0(indicator, iz, year)) %>% 
    select(lookup, colnames(alcohol_admission_output_ha_smr01))
  
  
  alcohol_data_output %>% 
    write_csv(here("output", "profile_annual_data", "profile.csv"))
}


