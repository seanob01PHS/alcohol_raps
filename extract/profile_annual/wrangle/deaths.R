deaths_nrs_wrangle <- function(data_alcohol_deaths,
                               date_start_deaths,
                               date_end_deaths,
                               cal_year_pops
                               ){
  
  ca_populations_cal = cal_year_pops[["ca"]]
  ggc_interzone_pop_cal = cal_year_pops[["ggc_iz"]]
  glas_locality_pop_cal = cal_year_pops[["glas_loc"]]
  interzone_populations_cal = cal_year_pops[["iz"]]
  
  
  # Filter to Glasgow interzone level 
  ggc_alcohol_deaths <- data_alcohol_deaths %>% 
    filter(!is.na(datazone_2011),
           hbres_currentdate == 'S08000031') %>% 
    group_by(year, int_zone2011) %>% 
    summarise(number = n()) %>% 
    ungroup()
  
  ggc_output_deaths <- ggc_interzone_pop_cal %>% 
    left_join(ggc_alcohol_deaths, by = c('year', 'int_zone2011')) %>% 
    mutate(number = case_when(is.na(number) ~ 0,
                              TRUE ~ as.double(number))) %>%
    group_by(year) %>% 
    mutate(number = case_when(hscp_locality == 'NHS GGC' ~ sum(number),
                              TRUE ~ number)) %>% 
    ungroup()
  
  ggc_output_deaths <- bind_rows(ggc_output_deaths,
                                 
                                 # hscp totals
                                 ggc_output_deaths %>% 
                                   filter(!is.na(ca_name)) %>% 
                                   group_by(year, ca_name) %>% 
                                   summarise(number = sum(number)) %>% 
                                   ungroup() %>% 
                                   left_join(ca_populations_cal, by = c('year', 'ca_name')) %>% 
                                   mutate(int_zone2011 = NA, iz_name = "", hscp_locality = ca_name) %>% 
                                   select(colnames(ggc_output_deaths)),
                                 
                                 # Glasgow City locality totals
                                 ggc_output_deaths %>% 
                                   filter(grepl('Glasgow', hscp_locality)) %>% 
                                   group_by(year, hscp_locality) %>% 
                                   summarise(number = sum(number)) %>% 
                                   ungroup() %>% 
                                   left_join(glas_locality_pop_cal, by = c('year', 'hscp_locality')) %>% 
                                   mutate(int_zone2011 = NA, iz_name = '', ca_name = 'Glasgow City') %>% 
                                   select(colnames(ggc_output_deaths))
  )
  
  scotland_output <- data_alcohol_deaths %>%
    group_by(year) %>% 
    count(name = 'number') %>% 
    ungroup() %>% 
    left_join(interzone_populations_cal %>% filter(int_zone2011 == 'Scotland'), by = c('year')) %>% 
    mutate(iz = NA, iz_name = NA, ca_name = '', hscp_locality = "Scotland") %>%
    select(colnames(ggc_output_deaths))
  
  
  alcohol_deaths_output <- bind_rows(ggc_output_deaths, scotland_output) %>%
    rename(iz = int_zone2011, hscp = ca_name, pop = total_pop) %>%
    mutate(indicator = 'Alcohol-specific deaths') %>% 
    arrange(iz, hscp, hscp_locality, indicator, year) %>% 
    filter(!is.na(hscp_locality))
  
  # convert to 3-year aggregate
  start_year <- year(date_start_deaths)
  final_year <- max(ggc_output_deaths$year)
  end_year <- start_year + 2
  
  alcohol_deaths_output_final <- data.frame()
  
  while (end_year <= final_year) {
    alcohol_deaths_output_final <- bind_rows(alcohol_deaths_output_final,
                                             
                                             alcohol_deaths_output %>%
                                               mutate(year2 = paste0(start_year, "-", end_year)) %>%
                                               filter(between(year, start_year, end_year) 
                                                      # !is.na(iz)
                                               ) %>%
                                               group_by(year2, indicator, iz, iz_name, hscp, hscp_locality) %>%
                                               summarise(pop = sum(pop)/3,
                                                         number = sum(number),
                                                         rate = (number/pop)*10000) %>% 
                                               ungroup())
    start_year <- start_year + 1
    end_year <- end_year + 1
    
  }
  
  alcohol_deaths_output_final <- alcohol_deaths_output_final %>% 
    rename(year = year2) %>% 
    select(colnames(alcohol_deaths_output), rate)
  
}