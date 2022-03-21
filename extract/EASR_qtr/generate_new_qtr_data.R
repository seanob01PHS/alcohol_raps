### Previously "Alcohol admits qtr.R"
### GK 08 Aug 2021
### Alcohol-related hospital admissions by financial year  
### Number, EASR and crude rates by HSCP, HB and Scotland 
###
### Run this each quarter. Produces 2 output files - one each for HSCP and HB 

################################################################
###### Load packages
################################################################

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
library(haven)
library(phsmethods)


generate_new_qtr_data <- function(date_end){
  
  ################################################################
  ##### Set file names and folders
  ################################################################
  
  # set working directory to project folder
  #setwd("/conf/LIST_analytics/Glasgow City/Drugs & Alcohol/Alcohol/EASR/")
  
  
  ## Set folder locations 
  dir <- getwd()
  
  # output
  data.out = "output/EASR_qtr_data/"
  
  
  ## Lookups
  
  # HSCP pop
  filename.pop.hscp = "HSCP2019_pop_est_5year_agegroups_1981_2020.rds"
  filepath.pop.hscp = "/conf/linkage/output/lookups/Unicode/Populations/Estimates/"
  filepathname.pop.hscp = paste0(filepath.pop.hscp, filename.pop.hscp)
  
  # HB pop
  filename.pop.hb = "HB2019_pop_est_5year_agegroups_1981_2020.rds"
  filepath.pop.hb = "/conf/linkage/output/lookups/Unicode/Populations/Estimates/"
  filepathname.pop.hb = paste0(filepath.pop.hb, filename.pop.hb)
  
  # Euro std pop
  filename.pop.std = "ESP2013 by sex.csv"
  filepath.pop.std = "/conf/LIST_analytics/Glasgow City/#lookups/Populations/standard/"
  filepathname.pop.std = paste0(filepath.pop.std, filename.pop.std)
  
  # issues with sav files - converted to csv instead
  #filename.pop.std = "ESP2013_by_sex.sav"
  #filepath.pop.std = "/conf/linkage/output/lookups/Unicode/Populations/Standard/"
  #filepathname.pop.std = paste0(filepath.pop.std, filename.pop.std)
  
  
  ################################################################
  ##### Set script variables
  ################################################################
  
  ### Start and end dates (for query and filtering stay data after) 
  
  # convert end date for query (e.g. 31 March 2021) - test showed that needed extra 6 months so cases not cut off mid-stay and assigned to time period incorrectly
  #date_end_sql <- format(as_date(date_end), format = "%-d %B %Y")
  date_end_sql <- format(as_date(date_end)%m+% months(6), format = "%-d %B %Y")
  
  # calculate start date as 12 months back (+ 1 day) from end date to get 1 year
  date_start <- as.character(as_date(date_end)%m-% months(12) + 1)
  
  # query start date 3 months prior to account for admissions and convert for query (e.g. 1 April 2020)
  date_start_sql <- format(as_date(date_start)%m-% months(3), format = "%-d %B %Y")
  
  # create time period
  time_period = paste0(format(as_date(date_start), format = "%-d %b %Y"), " to ", format(as_date(date_end), format = "%-d %b %Y"))
  
  # print to console
  paste0("The data set time period is ", time_period)
  
  
  # rate mutiplier
  ratemulti = 10000
  
  ################################################################
  ##### SMRA extraction
  ################################################################
  
  # Connect to SMRA tables using odbc connection
  # The suppressWarnings function prevents your password from
  # appearing in the console if the connection is unsuccessful
  channel <- suppressWarnings(
    dbConnect(odbc(),
              dsn = "SMRA",
              uid = Sys.info()[['user']],
              pwd = .rs.askForPassword("What is your LDAP password?"))
  )
  
  
  ##### Extract SMR01 data
  
  # set alcohol diagnoses
  alc_diag <- "E244|E512|F10|G312|G621|G721|I426|K292|K70|K852|K860|O354|P043|Q860|R780|T510|T511|T519|X45|X65|Y15|Y573|Y90|Y91|Z502|Z714|Z721"
  
  # define SQL Query
  data_episodes <- tbl_df(dbGetQuery(channel, statement= paste0(
    "SELECT LINK_NO, CIS_MARKER, ADMISSION_DATE, DISCHARGE_DATE, ADMISSION_TYPE, INPATIENT_DAYCASE_IDENTIFIER, DISCHARGE_TRANSFER_TO,
    UPI_NUMBER, SEX, AGE_IN_YEARS, LENGTH_OF_STAY,
    MAIN_CONDITION, OTHER_CONDITION_1, OTHER_CONDITION_2, OTHER_CONDITION_3, OTHER_CONDITION_4, OTHER_CONDITION_5,
    POSTCODE, DATAZONE_2011, COUNCIL_AREA_2019, HSCP_2019, HBRES_CURRENTDATE
    FROM ANALYSIS.SMR01_PI z
    WHERE discharge_date between '", date_start_sql ,"' and '", date_end_sql ,"'
    and sex <> 9
    and exists (
    select * 
    from ANALYSIS.SMR01_PI 
    where link_no=z.link_no and cis_marker=z.cis_marker
    and discharge_date between '", date_start_sql ,"' and '", date_end_sql ,"'
    and (regexp_like(main_condition, '", alc_diag ,"')
    or regexp_like(other_condition_1,'", alc_diag ,"')
    or regexp_like(other_condition_2,'", alc_diag ,"')
    or regexp_like(other_condition_3,'", alc_diag ,"')
    or regexp_like(other_condition_4,'", alc_diag ,"')
    or regexp_like(other_condition_5,'", alc_diag ,"')))
    ORDER BY LINK_NO, ADMISSION_DATE, DISCHARGE_DATE, ADMISSION, DISCHARGE, URI" ))) %>%  #smra guidance suggests always sorting the data in this order to arrange episodes into chronolgical order for each stay))) %>% 
    setNames(tolower(names(.)))  #variables to lower case
  
  # Close odbc connection
  dbDisconnect(channel)
  
  
  #############################################
  # Data Processing
  #############################################
  
  # episodes to stays
  data_stays <- data_episodes %>%
    data.table::as.data.table() %>%
    .[,
      list(
        hbres = first(hbres_currentdate),
        hscp = first(hscp_2019),
        dz = first(datazone_2011),
        pc7 = first(postcode),
        admission_date = first(admission_date),
        discharge_date = last(discharge_date),
        admission_type = first(admission_type),
        daycase = first(inpatient_daycase_identifier),
        los = sum(length_of_stay),
        discharge_transfer = last(discharge_transfer_to),
        sex = last(sex),
        age = first(age_in_years),
        main_condition = first(main_condition),
        other_condition_1 = first(other_condition_1),
        other_condition_2 = first(other_condition_2),
        other_condition_3 = first(other_condition_3),
        other_condition_4 = first(other_condition_4),
        other_condition_5 = first(other_condition_5)
      ),
      by = 'link_no,cis_marker'] %>% 
    as_tibble() 
  
  
  ### Filter dates
  
  # filter stay time period required (data_start and data_end assigned at top of script)
  data_stays2 <- data_stays %>% 
    filter(discharge_date %within% interval(ymd(date_start), ymd(date_end)))
  
  ### Filter non-scottish
  
  # remove non-Scottish residents and only M/F as per scotPHO scripts
  data_stays2 <- data_stays2 %>%
    filter(!is.na(dz), sex %in% c(1,2))
  
  
  ### Create age group (1-19)
  
  data_stays2 <- data_stays2 %>%
    mutate(age_group = case_when(
      between(age, 0, 4) ~ 1,
      between(age, 5, 9) ~ 2,
      between(age, 10, 14) ~ 3,
      between(age, 15, 19) ~ 4,
      between(age, 20, 24) ~ 5,
      between(age, 25, 29) ~ 6,
      between(age, 30, 34) ~ 7,
      between(age, 35, 39) ~ 8,
      between(age, 40, 44) ~ 9,
      between(age, 45, 49) ~ 10,
      between(age, 50, 54) ~ 11,
      between(age, 55, 59) ~ 12,
      between(age, 60, 64) ~ 13,
      between(age, 65, 69) ~ 14,
      between(age, 70, 74) ~ 15,
      between(age, 75, 79) ~ 16,
      between(age, 80, 84) ~ 17,
      between(age, 85, 89) ~ 18,
      age >=90 ~ 19))
  
  
  #############################################
  # Populations
  #############################################
  
  ### Will only need to use 1 year's population
  ### Choose year from when the mid-year populatin date (30 June) falls within the date time period
  
  # set popyear date with only day / month format
  # dont worry, the use of 2020 here is completely arbitrary
  popmidyear_daymonth = format(as_date("2020-06-30"), format="%d-%m")
  
  # convert start date to day / month format 
  date_start_daymonth <- format(as_date(date_start), format="%d-%m")
  
  # get pop year - if start date is before mid year then use start date year if not use end date year 
  data_popyear <- ifelse(date_start_daymonth <= popmidyear_daymonth, year(as_date(date_start)), year(as_date(date_end)))
  
  
  
  ### HSCP pop
  
  # read in data
  pop.hscp <- readRDS(filepathname.pop.hscp) %>%
    select(-hscp2018, -hscp2016) %>%
    rename(popyear = year, hscp = hscp2019, hscp_name = hscp2019name)
  
  ## Add age group 0 to 1
  
  # select age groups 0 and 1
  pop.hscp.fix <- pop.hscp %>%
    filter(age_group %in% c(0,1))
  
  # aggregate age groups together
  pop.hscp.fix <- pop.hscp.fix %>%
    group_by(popyear, hscp, hscp_name, sex, sex_name) %>%
    summarise(pop=sum(pop)) %>%
    ungroup()
  
  # rename dropped columns
  pop.hscp.fix$age_group = 1
  pop.hscp.fix$age_group_name = "0-4"
  
  # select remove old age groups
  pop.hscp <- pop.hscp %>%
    filter(age_group > 1)
  
  # add on new joined age group
  pop.hscp <- rbind(pop.hscp, pop.hscp.fix) %>%
    arrange(popyear, hscp, age_group)
  
  
  ### Scotland pop
  
  # aggregate for Scotland
  pop.scot <- pop.hscp %>%
    group_by(popyear, sex, sex_name, age_group, age_group_name) %>%
    summarise(pop=sum(pop)) %>%
    ungroup()
  
  # rename dropped columns
  pop.scot$hscp = "Scotland"
  pop.scot$hscp_name = "Scotland"
  
  
  ### Join HSCP and Scotland pops
  pop.hscp <- rbind(pop.hscp, pop.scot)
  
  
  ### HB pop
  
  # read in data
  pop.hb <- readRDS(filepathname.pop.hb) %>%
    select(-hb2018, -hb2014) %>%
    rename(popyear = year, hbres = hb2019, hbres_name = hb2019name)
  
  ## Add age group 0 to 1
  
  # select age groups 0 and 1
  pop.hb.fix <- pop.hb %>%
    filter(age_group %in% c(0,1))
  
  # aggregate age groups together
  pop.hb.fix <- pop.hb.fix %>%
    group_by(popyear, hbres, hbres_name, sex, sex_name) %>%
    summarise(pop=sum(pop)) %>%
    ungroup()
  
  # rename dropped columns
  pop.hb.fix$age_group = 1
  pop.hb.fix$age_group_name = "0-4"
  
  # select remove old age groups
  pop.hb <- pop.hb %>%
    filter(age_group > 1)
  
  # add on new joined age group
  pop.hb <- rbind(pop.hb, pop.hb.fix) %>%
    arrange(popyear, hbres, age_group)
  
  
  ### Scotland pop
  
  # aggregate for Scotland
  pop.scot <- pop.hb %>%
    group_by(popyear, sex, sex_name, age_group, age_group_name) %>%
    summarise(pop=sum(pop)) %>%
    ungroup()
  
  # rename dropped columns
  pop.scot$hbres = "Scotland"
  pop.scot$hbres_name = "Scotland"
  
  
  ### Join HSCP and Scotland pops
  pop.hb <- rbind(pop.hb, pop.scot)
  
  
  ### ESP2013 std pop
  
  # read in data
  pop.std  <- read_csv(filepathname.pop.std)
  names(pop.std) <- c('age_group','sex','ESP2013pop')
  
  
  #############################################
  # Output table build
  #############################################
  # - 2 tables; hb and hscp 
  # - build from pop data first so all sex / age groups are included
  
  ### HSCP table 
  
  # aggregate data table - add popyear from FY and convert sex to number
  agg_hscp <- data_stays2 %>%
    group_by(hscp, sex, age_group) %>%
    summarise(number=n()) %>%
    ungroup() %>%
    mutate(sex = as.numeric(sex)) 
  
  ### Scotland data 
  
  # aggregate data table - add popyear from FY and convert sex to number
  agg_scot <- data_stays2 %>%
    group_by(sex, age_group) %>%
    summarise(number=n()) %>%
    ungroup() %>%
    mutate(sex = as.numeric(sex)) 
  
  # rename dropped columns
  agg_scot$hscp = "Scotland"
  
  
  ### Join HSCP and Scotland data
  agg_hscp <- rbind(agg_hscp, agg_scot)
  
  
  ### Join pop and data
  
  # get popyear and hscps values from data to then filter pop data
  hscp_list <- unique(agg_hscp$hscp)
  
  # select popyear from pop data
  pop.hscp <- pop.hscp %>%
    filter(popyear == data_popyear) 
  
  # join data onto pop table
  tbl_hscp <- left_join(pop.hscp, agg_hscp, by = c("hscp", "sex", "age_group")) %>%
    filter(popyear == popyear, hscp %in% hscp_list)
  
  
  ### Fill missing data
  
  # change numbers NA to 0
  tbl_hscp$number[is.na(tbl_hscp$number)] <- 0
  
  # select only columns needed
  tbl_hscp <- tbl_hscp %>%
    select(hscp_name, sex, sex_name, age_group, age_group_name, pop, number)
  
  
  ### HB table 
  
  # aggregate data table - add popyear from FY and convert sex to number
  agg_hb <- data_stays2 %>%
    group_by(hbres, sex, age_group) %>%
    summarise(number=n()) %>%
    ungroup() %>%
    mutate(sex = as.numeric(sex)) 
  
  ### Scotland data 
  
  # aggregate data table - add popyear from FY and convert sex to number
  agg_scot <- data_stays2 %>%
    group_by(sex, age_group) %>%
    summarise(number=n()) %>%
    ungroup() %>%
    mutate(sex = as.numeric(sex)) 
  
  # rename dropped columns
  agg_scot$hbres = "Scotland"
  
  
  ### Join HB and Scotland data
  agg_hb <- rbind(agg_hb, agg_scot)
  
  
  ### Join pop and data
  
  # get popyear and hscps values from data to then filter pop data
  hb_list <- unique(agg_hb$hbres)
  
  # select popyear from pop data
  pop.hb <- pop.hb %>%
    filter(popyear == data_popyear) 
  
  
  # join data onto pop table
  tbl_hb <- left_join(pop.hb, agg_hb, by = c("hbres", "sex", "age_group")) %>%
    filter(hbres %in% hb_list)
  
  
  ### Fill missing data
  
  # change numbers NA to 0
  tbl_hb$number[is.na(tbl_hb$number)] <- 0
  
  # select only columns needed
  tbl_hb <- tbl_hb %>%
    select(hbres_name, sex, sex_name, age_group, age_group_name, pop, number)
  
  
  
  ### EASR calcs
  
  # add ESP2013 pops
  tbl_hscp <- left_join(tbl_hscp, pop.std, by = c("age_group", "sex"))
  tbl_hb <- left_join(tbl_hb, pop.std, by = c("age_group", "sex"))
  
  # calculate ASR and ASR*ESP2013pop
  tbl_hscp$ASR = tbl_hscp$number / tbl_hscp$pop
  tbl_hscp$ASRESP = tbl_hscp$ASR * tbl_hscp$ESP2013pop
  tbl_hb$ASR = tbl_hb$number / tbl_hb$pop
  tbl_hb$ASRESP = tbl_hb$ASR * tbl_hb$ESP2013pop
  
  # aggregate by age / sex
  tbl_hscp2 <- tbl_hscp %>%
    group_by(hscp_name) %>%
    summarise(pop = sum(pop, na.rm=TRUE),
              number = sum(number, na.rm=TRUE),
              ESP2013pop = sum(ESP2013pop, na.rm=TRUE),
              TASRESP = sum(ASRESP, na.rm=TRUE)) %>%
    ungroup()
  
  tbl_hb2 <- tbl_hb %>%
    group_by(hbres_name) %>%
    summarise(pop = sum(pop, na.rm=TRUE),
              number = sum(number, na.rm=TRUE),
              ESP2013pop = sum(ESP2013pop, na.rm=TRUE),
              TASRESP = sum(ASRESP, na.rm=TRUE)) %>%
    ungroup()
  
  # calc EASR
  tbl_hscp2$EASR = tbl_hscp2$TASRESP / tbl_hscp2$ESP2013pop * ratemulti
  tbl_hb2$EASR = tbl_hb2$TASRESP / tbl_hb2$ESP2013pop * ratemulti
  
  # calc crude rate
  tbl_hscp2$crude_rate = tbl_hscp2$number / tbl_hscp2$pop * ratemulti
  tbl_hb2$crude_rate = tbl_hb2$number / tbl_hb2$pop * ratemulti
  
  # remove unnecessary columns and add time period column
  tbl_hscp2 <- tbl_hscp2 %>%
    select(-ESP2013pop, -TASRESP) %>%
    mutate(data_period = time_period, date_end = date_end)
  
  tbl_hb2 <- tbl_hb2 %>%
    select(-ESP2013pop, -TASRESP) %>%
    mutate(data_period = time_period, date_end = date_end)
  
  
  # save output
  write.csv(tbl_hscp2, file = paste0(data.out, 'Qtr alcohol admissions - HSCP', ' (', date_end, ')', '.csv'), row.names=FALSE)
  write.csv(tbl_hb2, file = paste0(data.out, 'Qtr alcohol admissions - HB', ' (', date_end, ')', '.csv'), row.names=FALSE)
}





