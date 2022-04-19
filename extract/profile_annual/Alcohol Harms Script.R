# File to run and combine Alcohol-related and Alcohol-related mental health hospital admissions
# Note - make sure to load odbc before loading tidyverse
library(odbc)
library(janitor)
library(lubridate)
library(zoo)

# tidyverse packages
library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(stringr) 

# xl packages
library(readxl)
library(writexl)

# phsmethods package
# to install uncomment following lines
# install.packages("remotes")
# library(remotes)
# remotes::install_github("Public-Health-Scotland/phsmethods", upgrade = "never")
library(phsmethods)

# set working directory to project folder
setwd("/conf/LIST_analytics/Glasgow City/Drugs & Alcohol/Alcohol/")


## set folder locations 
dir <- getwd()
data.out = paste0(dir,"/Output/")

# start and end dates for filtering stay data 
# changed to date format
date_start_smr01 <- ymd("2015-04-01")
date_end_smr01 <- ymd("2021-03-31")

date_start_smr04 <- ymd("2013-04-01")
date_end_smr04 <- ymd("2021-03-31")

date_start_deaths <- ymd("2013-01-01")
date_end_deaths <- ymd("2020-12-31")

# calc 3 months before start date for extract - accounts for admissions before start date (discharge based analysis)
# changed to date format
extract_smr01_startdate = date_start_smr01 - months(3)

extract_smr04_startdate = date_start_smr04 - months(3)

channel <- suppressWarnings(
  dbConnect(odbc(),
            dsn = "SMRA",
            uid = Sys.info()[['user']],
            pwd = .rs.askForPassword("What is your LDAP password?"))
)


alc_diag <- "E244|E512|F10|G312|G621|G721|I426|K292|K70|K852|K860|O354|P043|Q860|R780|T510|T511|T519|X45|X65|Y15|Y573|Y90|Y91|Z502|Z714|Z721"

##### Extract SMR01 data

data_alcohol_episodes <- as_tibble(dbGetQuery(channel, statement=paste0(
  "SELECT LINK_NO, CIS_MARKER, ADMISSION_DATE, DISCHARGE_DATE, ADMISSION_TYPE, INPATIENT_DAYCASE_IDENTIFIER, DISCHARGE_TRANSFER_TO,
  UPI_NUMBER, SEX, AGE_IN_YEARS, LENGTH_OF_STAY,
  MAIN_CONDITION, OTHER_CONDITION_1, OTHER_CONDITION_2, OTHER_CONDITION_3, OTHER_CONDITION_4, OTHER_CONDITION_5,
  POSTCODE, DATAZONE_2011, INTZONE_2011, COUNCIL_AREA_2019, HBRES_CURRENTDATE
  FROM ANALYSIS.SMR01_PI z
  WHERE discharge_date between '", format(extract_smr01_startdate, '%e %B %Y'), "' and '", format(date_end_smr01, '%e %B %Y'), "'
  and sex <> 9
  and exists (
  select * 
  from ANALYSIS.SMR01_PI 
  where link_no=z.link_no and cis_marker=z.cis_marker
  and discharge_date between '", format(date_start_smr01 - years(1), '%e %B %Y'), "' and '", format(date_end_smr01, '%e %B %Y'), "'
  and (regexp_like(main_condition, '", alc_diag ,"')
  or regexp_like(other_condition_1,'", alc_diag ,"')
  or regexp_like(other_condition_2,'", alc_diag ,"')
  or regexp_like(other_condition_3,'", alc_diag ,"')
  or regexp_like(other_condition_4,'", alc_diag ,"')
  or regexp_like(other_condition_5,'", alc_diag ,"')))
  ORDER BY LINK_NO, ADMISSION_DATE, DISCHARGE_DATE, ADMISSION, DISCHARGE, URI" ))) %>%  #smra guidance suggests always sorting the data in this order to arrange episodes into chronolgical order for each stay))) %>% 
  setNames(tolower(names(.)))  #variables to lower case

##### Extract SMR04 data

# odbcPreviewObject(channel, table="ANALYSIS.SMR04_PI", rowLimit=0)

# define SQL Query
# new server: last line of sql query >> {d '2008-04-01'} replaced with TO_DATE('2008-04-01','YYYY-MM-DD')

data_alcohol_episodes_smr04 <- as_tibble(dbGetQuery(channel, statement=paste0(
  "SELECT LINK_NO, CIS_MARKER, ADMISSION_DATE, DISCHARGE_DATE, ADMISSION_TYPE, DISCHARGE_TRANSFER_TO,
  UPI_NUMBER, SEX, AGE_IN_YEARS, LENGTH_OF_STAY,
  MAIN_CONDITION, OTHER_CONDITION_1, OTHER_CONDITION_2, OTHER_CONDITION_3, OTHER_CONDITION_4, OTHER_CONDITION_5,
  POSTCODE, DATAZONE_2011, INTZONE_2011, COUNCIL_AREA_2019, HBRES_CURRENTDATE
  FROM ANALYSIS.SMR04_PI z
  WHERE discharge_date between '", format(extract_smr04_startdate, '%e %B %Y'), "' and '", format(date_end_smr04, '%e %B %Y'), "'
  and sex <> 9
  and exists (
  select * 
  from ANALYSIS.SMR04_PI 
  where link_no=z.link_no and cis_marker=z.cis_marker
  and discharge_date between '", format(date_start_smr04 - years(1), '%e %B %Y'), "' and '", format(date_end_smr04, '%e %B %Y'), "'
  and (regexp_like(main_condition, '", alc_diag ,"')
  or regexp_like(other_condition_1,'", alc_diag ,"')
  or regexp_like(other_condition_2,'", alc_diag ,"')
  or regexp_like(other_condition_3,'", alc_diag ,"')
  or regexp_like(other_condition_4,'", alc_diag ,"')
  or regexp_like(other_condition_5,'", alc_diag ,"')))
  ORDER BY LINK_NO, ADMISSION_DATE, DISCHARGE_DATE, ADMISSION, DISCHARGE, URI" ))) %>%  #smra guidance suggests always sorting the data in this order to arrange episodes into chronolgical order for each stay))) %>% 
  setNames(tolower(names(.)))  #variables to lower case

#### Extract Deaths Data

# Select alcohol specific deaths from SMRA
# Select only deaths for scottish residents (COR=XS)
# Exclude any with null age group
# Exclude deaths where sex is unknown (9)
# Selections based on primary cause of death
# ICD10 codes to match NRS definitions of alcohol-specific deaths (ie wholly attributable to alcohol)

data_alcohol_deaths <- as_tibble(dbGetQuery(channel, statement=paste0(
  "SELECT year_of_registration year, age, SEX sex_grp, postcode,
                                    datazone_2011, intzone_2011 int_zone2011, council_area_2019, hbres_currentdate, underlying_cause_of_death
                                    FROM ANALYSIS.GRO_DEATHS_C 
                                    WHERE date_of_registration between '", format(date_start_deaths, '%e %B %Y'), "' AND '", format(date_end_deaths, '%e %B %Y'), "'
                                          AND country_of_residence ='XS'
                                          AND regexp_like(underlying_cause_of_death,'E244|F10|G312|G621|G721|I426|K292|K70|K852|K860|Q860|R78|X45|X65|Y15|E860') 
                                          AND age is not NULL
                                          AND sex <> 9"))) %>%
  setNames(tolower(names(.)))  #variables to lower case

# Close odbc connection
dbDisconnect(channel)

# postcode lookup
postcode_lookup <- read.csv('/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2021_1.csv',
                            stringsAsFactors=F) %>%
  clean_names() %>%
  select(pc7, int_zone2011) %>%
  rename(postcode = "pc7") %>% 
  mutate(postcode = gsub(" ", "", postcode))

# Load in populations from population_lookup.R file
source(paste0(dir, '/Code/population_lookup.R'))

# Alcohol-related hospital admissions
source(paste0(dir, '/Code/Alcohol Hospital Admissions SMR01.R'))

# Alcohol-related mental health hospital admissions
source(paste0(dir, '/Code/Alcohol Hospital Admissions SMR04.R'))

# Alcohol-specidic deaths
source(paste0(dir, '/Code/Alcohol Specific Deaths.R'))

alcohol_data_output <- bind_rows(alcohol_admission_output,
                                 alcohol_admission_output_smr04_final,
                                 alcohol_deaths_output_final) %>% 
  arrange(iz, hscp, hscp_locality, year, indicator) %>% 
  mutate(iz = case_when(is.na(iz) ~ hscp_locality,
                        TRUE ~ iz),
         iz_name = case_when(is.na(iz_name) ~ "",
                             TRUE ~ iz_name),
         hscp = case_when(is.na(hscp) ~ "",
                          TRUE ~ hscp),
         lookup = paste0(indicator, iz, year)) %>% 
  select(lookup, colnames(alcohol_admission_output))

write_xlsx(alcohol_data_output, paste0(data.out, "GGC Alcohol Data.xlsx"))
