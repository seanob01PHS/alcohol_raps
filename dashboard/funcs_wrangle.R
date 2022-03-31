# Utility functions for dashboard creation


#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Returns the first non-NA element in a list
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
first_non_na <- function(grp){
  first(na.omit(grp))
}



#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Vectorised rate of change where
#     a is (a/b-1) change from b
#     0 is (0) change from 0
#     a is (NA) change from 0
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
zero_safe_rate_of_change <- function(a,b){
  case_when(a>0 & b==0 ~ NA_real_,
            a==0 & b==0 ~ 0.,
            TRUE ~ a/b - 1)
}

#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Reordering of indicators
# Seems like a stupid function but to be safe and robust
# will fail if indicator names are not known before running
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
reorder_indicators <- function(indicators, order){
  if (!identical(sort(indicators), sort(order))){
    stop("Order must contain exactly all the indicators present in the data.")
  }
  order
}


#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Gets and returns a dt with iz and population
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
get_pops <- function(table_data, data, population_year){
  pops <- data %>%
    filter(year_start==population_year) %>% 
    select(iz, pop) %>% 
    group_by(iz) %>% 
    summarise(pop=first(pop)) %>% 
    ungroup()
  
  table_data %>%
    select(iz) %>% 
    left_join(pops, by="iz")
  
}

#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Just a little wrapper really
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
process_year <- function(data){
  data %>% 
    mutate(year_start=as.numeric(substr(year, 1, 4))) %>% 
    rename(year_str=year)
  
}


#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Gives the most up to date `year_start` for a given
# indicator 
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
most_up_to_date_date <- function(data, indicator_f){
  data %>% filter(indicator==indicator_f) %>% pull(year_start) %>% max()
}




#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Returns the first non-NA element in a list
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
val_col_infos <- function(value_col_names){
  col_info <- str_match_all(value_col_names, "[^\\|]+")
  indicators <- col_info %>% map_chr(2)
  year_strs <- col_info %>% map_chr(3)
  year_starts <- col_info %>% map_chr(4) %>% as.numeric()
  
  list(indicators, year_strs, year_starts)
}


#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Filters IZs only
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
iz_data <- function(profile_table){
  profile_table %>% filter(!is.na(iz_name))
}

#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Filters big areas only
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
big_area_data <- function(profile_table){
  profile_table %>% filter(is.na(iz_name))
}