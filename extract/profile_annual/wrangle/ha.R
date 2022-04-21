#Original Author: Jancine szukalski
#Original Date: 18/08/2021

#############################################
# Emergency admissions
#############################################

ha_smr01_wrangle <-  function(data_alcohol_episodes,
                              date_start_smr01,
                              date_end_smr01,
                              postcode_lookup,
                              fin_year_pops
                              ){
  
  ca_populations = fin_year_pops[["ca"]]
  ggc_interzone_pop = fin_year_pops[["ggc_iz"]]
  glas_locality_pop = fin_year_pops[["glas_loc"]]
  interzone_populations = fin_year_pops[["iz"]]
  
  # aggregate using data table function (faster than dplyr)
  data_alcohol_stay <- data_alcohol_episodes %>%
    data.table::as.data.table() %>%
    .[,
      list(
        postcode = first(postcode),
        hbres = first(hbres_currentdate),
        datazone_2011 = first(datazone_2011),
        interzone_2011 = first(intzone_2011),
        council_area = first(council_area_2019),
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
  
  ##### Select stay admission dates
  
  # filter stay time period required (data_start and data_end assigned at top of script)
  data_alcohol_stay2 <- data_alcohol_stay %>% 
    filter(discharge_date %within% interval(date_start_smr01, date_end_smr01),
           !is.na(datazone_2011)) %>% 
    
    # create calendar and financial year
    # using phsmethods package for finyear
    mutate(year = year(discharge_date),
           finyear = fin_year(discharge_date))
  
  
  #Filter for NHSGGC residence
  ggc_alcohol_stay <- data_alcohol_stay2 %>%
    filter (hbres == 'S08000031')
  
  
  ##### Check GGC and Scotland Totals ####
  
  # Year_count_ggc <- ggc_alcohol_stay %>% group_by(finyear) %>% count()
  # 
  # Year_count_scotland <- data_alcohol_stay2 %>% group_by(finyear) %>% count()
  
  #############################################
  
  # Aggregate and split by interzones ####
  ggc_output <- ggc_alcohol_stay %>% 
    mutate(postcode = gsub(" ", "", postcode)) %>% 
    left_join(postcode_lookup, by = 'postcode') %>%
    mutate(iz_name = match_area(int_zone2011),
           ca_name = match_area(council_area),
           lookup = paste0(str_remove(ca_name, " "), int_zone2011),
           indicator = 'Alcohol Admissions'
    )
  
  ggc_output_final <- data.frame()
  
  # Combine financial years and NHS GGC totals
  ggc_output_final <- ggc_interzone_pop %>%
    left_join(ggc_output %>% 
                group_by(finyear, int_zone2011, iz_name, ca_name, lookup) %>% 
                summarise(number = n(), .groups = "drop"),
              by = c('finyear', 'int_zone2011', 'iz_name', 'ca_name')) %>%
    mutate(number = case_when(is.na(number) ~ 0,
                              TRUE ~ as.double(number)),
           lookup = case_when(is.na(ca_name) ~ str_remove(hscp_locality, " "),
                              TRUE ~ paste0(ca_name, int_zone2011))) %>% 
    group_by(finyear) %>% 
    mutate(number = case_when(hscp_locality == 'NHS GGC' ~ sum(number),
                              TRUE ~ number),
           rate = (number/total_pop)*10000) %>% 
    ungroup()
  
  
  # Add in HSCP and locality totals
  ggc_output_final <- bind_rows(ggc_output_final,
                                # hscp totals
                                ggc_output_final %>% 
                                  filter(!is.na(ca_name)) %>% 
                                  group_by(finyear, ca_name) %>% 
                                  summarise(number = sum(number), .groups = "drop") %>% 
                                  left_join(ca_populations, by = c('finyear', 'ca_name')) %>% 
                                  mutate(lookup = ca_name, hscp_locality = ca_name, int_zone2011 = NA, iz_name = NA,
                                         rate = (number/total_pop)*10000) %>%
                                  select(colnames(ggc_output_final)), 
                                
                                # Glasgow City locality totals
                                ggc_output_final %>%
                                  filter(grepl('Glasgow', hscp_locality)) %>% 
                                  group_by(finyear, hscp_locality) %>% 
                                  summarise(number = sum(number), .groups = "drop") %>%
                                  left_join(glas_locality_pop, by = c('finyear', 'hscp_locality')) %>% 
                                  mutate(lookup = hscp_locality, ca_name = 'Glasgow City', iz_name = NA,
                                         rate = (number/total_pop)*10000) %>% 
                                  select(colnames(ggc_output_final)))
  
  
  # Scotland total
  scotland_output <- data_alcohol_stay2 %>%
    group_by(finyear) %>% 
    count(name = 'number') %>% 
    ungroup() %>% 
    left_join(interzone_populations %>% filter(int_zone2011 == 'Scotland'), by = c('finyear')) %>% 
    mutate(lookup = 'Scotland', hscp_locality = "Scotland", ca_name = NA, int_zone2011 = NA, iz_name = NA,
           rate = (number/total_pop)*10000) %>% 
    ungroup() %>% 
    select(colnames(ggc_output_final))
  
  # Combine GGC and Scotland data 
  alcohol_admission_output <- bind_rows(ggc_output_final, scotland_output) %>% 
    rename('year' = finyear, iz = int_zone2011, hscp = ca_name, pop = total_pop) %>%
    mutate(indicator = 'Alcohol-related hospital admissions') %>% 
    select(indicator, iz, iz_name, hscp, hscp_locality, year, pop, number, rate) %>% 
    arrange(iz, hscp, hscp_locality, indicator, year) %>% 
    filter(!year %in% c('2013/14', '2014/15'),
           !is.na(hscp_locality))
  
  alcohol_admission_output
}
