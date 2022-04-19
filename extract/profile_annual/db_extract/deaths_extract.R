#### Extract Deaths Data

# Select alcohol specific deaths from SMRA
# Select only deaths for scottish residents (COR=XS)
# Exclude any with null age group
# Exclude deaths where sex is unknown (9)
# Selections based on primary cause of death
# ICD10 codes to match NRS definitions of alcohol-specific deaths (ie wholly attributable to alcohol)


deaths_nrs_extract <- function(channel,
                               deaths_start_date,
                               deaths_end_date){
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

