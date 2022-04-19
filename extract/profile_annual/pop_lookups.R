# 2013 - 2019 Scotland and GGC hb, ca and iz populations
pop_lookups <- function(){
  # File with Interzones and associated hscp localities 
  int_locality_names <- read_xlsx('/conf/LIST_analytics/Glasgow City/Drugs & Alcohol/Alcohol/admin/work spec/docs sent/Alcohol Related Events NHSGGC updated 10dec19.xlsx', sheet = 'NHSGGC Ranked Rates', 'B10:E268') %>% 
    clean_names() %>% 
    select(-c(intermediate_zone_name, population_2018_sape)) %>% 
    rename(int_zone2011 = intermediate_zone_code)
  
  # Interzone populations
  estimates_folder <- '/conf/linkage/output/lookups/Unicode/Populations/Estimates/'
  dz_file <- "DataZone2011_pop_est_2011_2020.rds"
  
  pop_file <- readRDS(paste0(estimates_folder, dz_file)) %>% 
    clean_names() %>% 
    rename(int_zone2011 = intzone2011)
  
  interzone_populations <- pop_file %>% 
    filter(between(year, 2013, max(year))) %>% 
    select(year, int_zone2011, intzone2011name, ca2019name, hb2019name, total_pop) %>% 
    group_by(year, int_zone2011) %>% 
    summarise(intzone2011name = first(intzone2011name),
              ca2019name = first(ca2019name),
              hb2019name = first(hb2019name),
              total_pop = sum(total_pop)) %>% 
    ungroup() 
  
  interzone_populations <- bind_rows(interzone_populations,
                                     # Add Scotland totals
                                     interzone_populations %>%
                                       group_by(year) %>% 
                                       summarise(int_zone2011 = 'Scotland', intzone2011name = NA, ca2019name = NA, hb2019name = NA,
                                                 total_pop = sum(total_pop)) %>% 
                                       ungroup()) %>% 
    filter(int_zone2011 == 'Scotland' | hb2019name == "NHS Greater Glasgow and Clyde") %>% 
    mutate(finyear = paste0(year, "/", year-1999))
  
  # calyear
  interzone_populations_cal <- interzone_populations %>% select(-finyear)
  
  # Council area population totals
  ca_populations <- interzone_populations %>%
    group_by(finyear, ca2019name) %>% 
    summarise(total_pop = sum(total_pop)) %>% 
    ungroup() %>% 
    filter(!is.na(ca2019name)) %>% 
    rename(ca_name = ca2019name)
  
  # calyear
  ca_populations_cal <- ca_populations %>% 
    mutate(year = as.numeric(substr(finyear, 1, 4))) %>% 
    select(year, ca_name, total_pop)
  
  ggc_interzone_pop <- interzone_populations %>%
    left_join(int_locality_names, by = 'int_zone2011') %>% 
    select(finyear, int_zone2011, intzone2011name, ca2019name, hscp_locality, total_pop) %>% 
    rename(iz_name = intzone2011name, ca_name = ca2019name)
  
  
  
  # GGC population total
  ggc_interzone_pop <- bind_rows(ggc_interzone_pop,
                                 
                                 # GGC population total
                                 ggc_interzone_pop %>%
                                   filter(int_zone2011 != 'Scotland') %>% 
                                   group_by(finyear) %>% 
                                   summarise(int_zone2011 = NA, iz_name = NA, ca_name = NA, hscp_locality = 'NHS GGC', total_pop = sum(total_pop)))
  
  # calyear
  ggc_interzone_pop_cal <- ggc_interzone_pop %>% 
    mutate(year = as.numeric(substr(finyear, 1, 4))) %>% 
    select(year, int_zone2011:total_pop)
  
  # Glasgow City locality population totals
  glas_locality_pop <- ggc_interzone_pop %>% 
    filter(grepl('Glasgow', hscp_locality)) %>% 
    group_by(finyear, hscp_locality) %>% 
    summarise(int_zone2011 = NA, iz_name = NA, ca_name = 'Glasgow City', total_pop = sum(total_pop)) %>% 
    ungroup()
  
  # calyear
  glas_locality_pop_cal <- glas_locality_pop %>% 
    mutate(year = as.numeric(substr(finyear, 1, 4))) %>% 
    select(year, hscp_locality:total_pop)
  
  list("fin_year_pops" = list("ca" = ca_populations,
                              "ggc_iz" = ggc_interzone_pop,
                              "glas_loc" = glas_locality_pop,
                              "iz" = interzone_populations),
       
       "cal_year_pops" = list("ca" = ca_populations_cal,
                              "ggc_iz" = ggc_interzone_pop_cal,
                              "glas_loc" = glas_locality_pop_cal,
                              "iz" = interzone_populations_cal)
  )
}