# All extract functions


##### Extract SMR01 data
#########################################################################################

ha_smr01_extract <- function(channel,
                             alc_diag,
                             buffer_start_date,
                             start_date,
                             end_date){
  
  message(".... performing SMR01 extract.")
  
  data_alcohol_episodes_smr01 <- as_tibble(dbGetQuery(channel, statement=paste0(
    "SELECT LINK_NO, CIS_MARKER, ADMISSION_DATE, DISCHARGE_DATE, ADMISSION_TYPE, INPATIENT_DAYCASE_IDENTIFIER, DISCHARGE_TRANSFER_TO,
    UPI_NUMBER, SEX, AGE_IN_YEARS, LENGTH_OF_STAY,
    MAIN_CONDITION, OTHER_CONDITION_1, OTHER_CONDITION_2, OTHER_CONDITION_3, OTHER_CONDITION_4, OTHER_CONDITION_5,
    POSTCODE, DATAZONE_2011, INTZONE_2011, COUNCIL_AREA_2019, HBRES_CURRENTDATE
    FROM ANALYSIS.SMR01_PI z
    WHERE discharge_date between '", format(buffer_start_date, '%e %B %Y'), "' and '", format(end_date, '%e %B %Y'), "'
    and sex <> 9
    and exists (
    select * 
    from ANALYSIS.SMR01_PI 
    where link_no=z.link_no and cis_marker=z.cis_marker
    and discharge_date between '", format(start_date - years(1), '%e %B %Y'), "' and '", format(end_date, '%e %B %Y'), "'
    and (regexp_like(main_condition, '", alc_diag ,"')
    or regexp_like(other_condition_1,'", alc_diag ,"')
    or regexp_like(other_condition_2,'", alc_diag ,"')
    or regexp_like(other_condition_3,'", alc_diag ,"')
    or regexp_like(other_condition_4,'", alc_diag ,"')
    or regexp_like(other_condition_5,'", alc_diag ,"')))
    ORDER BY LINK_NO, ADMISSION_DATE, DISCHARGE_DATE, ADMISSION, DISCHARGE, URI" ))) %>%  #smra guidance suggests always sorting the data in this order to arrange episodes into chronolgical order for each stay))) %>% 
    setNames(tolower(names(.)))  #variables to lower case
  
  data_alcohol_episodes_smr01
}

##### Extract SMR04 data
#########################################################################################

mh_smr04_extract <- function(channel,
                             alc_diag,
                             buffer_start_date,
                             start_date,
                             end_date){
  
  message(".... performing SMR00 extract.")
  
  # odbcPreviewObject(channel, table="ANALYSIS.SMR04_PI", rowLimit=0)
  
  # define SQL Query
  # new server: last line of sql query >> {d '2008-04-01'} replaced with TO_DATE('2008-04-01','YYYY-MM-DD')
  
  data_alcohol_episodes_smr04 <- as_tibble(dbGetQuery(channel, statement=paste0(
    "SELECT LINK_NO, CIS_MARKER, ADMISSION_DATE, DISCHARGE_DATE, ADMISSION_TYPE, DISCHARGE_TRANSFER_TO,
    UPI_NUMBER, SEX, AGE_IN_YEARS, LENGTH_OF_STAY,
    MAIN_CONDITION, OTHER_CONDITION_1, OTHER_CONDITION_2, OTHER_CONDITION_3, OTHER_CONDITION_4, OTHER_CONDITION_5,
    POSTCODE, DATAZONE_2011, INTZONE_2011, COUNCIL_AREA_2019, HBRES_CURRENTDATE
    FROM ANALYSIS.SMR04_PI z
    WHERE discharge_date between '", format(buffer_start_date, '%e %B %Y'), "' and '", format(end_date, '%e %B %Y'), "'
    and sex <> 9
    and exists (
    select * 
    from ANALYSIS.SMR04_PI 
    where link_no=z.link_no and cis_marker=z.cis_marker
    and discharge_date between '", format(start_date - years(1), '%e %B %Y'), "' and '", format(end_date, '%e %B %Y'), "'
    and (regexp_like(main_condition, '", alc_diag ,"')
    or regexp_like(other_condition_1,'", alc_diag ,"')
    or regexp_like(other_condition_2,'", alc_diag ,"')
    or regexp_like(other_condition_3,'", alc_diag ,"')
    or regexp_like(other_condition_4,'", alc_diag ,"')
    or regexp_like(other_condition_5,'", alc_diag ,"')))
    ORDER BY LINK_NO, ADMISSION_DATE, DISCHARGE_DATE, ADMISSION, DISCHARGE, URI" ))) %>%  #smra guidance suggests always sorting the data in this order to arrange episodes into chronolgical order for each stay))) %>% 
    setNames(tolower(names(.)))  #variables to lower case
  
  data_alcohol_episodes_smr04
}


#### Extract Deaths Data
#########################################################################################
# Select alcohol specific deaths from SMRA
# Select only deaths for scottish residents (COR=XS)
# Exclude any with null age group
# Exclude deaths where sex is unknown (9)
# Selections based on primary cause of death
# ICD10 codes to match NRS definitions of alcohol-specific deaths (ie wholly attributable to alcohol)


deaths_nrs_extract <- function(channel,
                               deaths_start_date,
                               deaths_end_date){
  
  message(".... performing NRS_DEATHS extract")
  
  data_alcohol_deaths_nrs <- as_tibble(dbGetQuery(channel, statement=paste0(
    "SELECT year_of_registration year, age, SEX sex_grp, postcode,
                                    datazone_2011, intzone_2011 int_zone2011, council_area_2019, hbres_currentdate, underlying_cause_of_death
                                    FROM ANALYSIS.GRO_DEATHS_C 
                                    WHERE date_of_registration between '", format(deaths_start_date, '%e %B %Y'), "' AND '", format(deaths_end_date, '%e %B %Y'), "'
                                          AND country_of_residence ='XS'
                                          AND regexp_like(underlying_cause_of_death,'E244|F10|G312|G621|G721|I426|K292|K70|K852|K860|Q860|R78|X45|X65|Y15|E860') 
                                          AND age is not NULL
                                          AND sex <> 9"))) %>%
    setNames(tolower(names(.)))  #variables to lower case
  
  data_alcohol_deaths_nrs
}

