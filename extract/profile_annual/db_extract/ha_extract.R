##### Extract SMR01 data

ha_smr01_extract <- function(channel,
                             alc_diag,
                             buffer_start_date,
                             start_date,
                             end_date){
  
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