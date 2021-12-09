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
# returns a list of [indicator, indicator_cur_year, indicator_prev_year]
# to help in calculating "difference from previous year" columns
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
rate_pairings <- function(data){
  #list of [[recent_col_1, old_col_1], [recent_col_2, old_col_2]...]
  column_pairings <- list()
  
  #names of columns that represent a value (not a difference etc)
  names_of_vals <- data %>% names()
  #starts with "val||"
  names_of_vals <- Filter(function(x) grepl("^(val\\|\\|)", x), names_of_vals)
  
  col_infos <- val_col_infos(names_of_vals)
  indicators <- col_infos[[1]]
  year_strs <- col_infos[[2]]
  year_starts <- col_infos[[3]]
  
  for (indicator in indicators %>% unique()){
    indicator_indexs <- which(indicator == indicators)
    
    max_year_start <- year_starts[indicator_indexs] %>% max()
    min_year_start <- year_starts[indicator_indexs] %>% min()
    
    max_year_str <- year_strs[[which(indicator==indicators & max_year_start==year_starts)]]
    min_year_str <- year_strs[[which(indicator==indicators & min_year_start==year_starts)]]

    
    column_pairings[[length(column_pairings)+1]] <- list(indicator,
                                                         paste0("val||", indicator, "||", max_year_str, "||", max_year_start),
                                                         paste0("val||", indicator, "||", min_year_str, "||", min_year_start))
  }

  column_pairings
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


#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Returns a df with all the columns ready to be displayed as they are
# in the profile tab of the alcohol harms workbook
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
make_profile_frame <- function(data){
  
  # A char vector of all the different types of  indicators in the data
  indicators <- data %>% pull(indicator) %>% unique()
  
  # A list of l[[indicator]] <- the most up to date `year_start` for that 
  #                               given indicator
  most_recent_dates <- list()
  for (indicator in indicators){
    most_recent_dates[[indicator]] <- most_up_to_date_date(data, indicator)
  }
  
  data_f <- data %>%
    #choosing the most recent and second most recent dates
    filter(year_start == most_recent_dates[indicator] |
           year_start == unlist(most_recent_dates[indicator]) - 1)
  
  # Column names are coded as
  # "val"||indicator||Year string||year start corr. to that year string
  data_piv <- data_f %>% 
    mutate(name_code_str = paste0("val||", indicator, "||", year_str, "||", year_start)) %>% 
    select(iz, iz_name, hscp, name_code_str, rate) %>% 
    pivot_wider(names_from = name_code_str, values_from = rate)
  
  
  rate_pairings <- rate_pairings(data_piv)
  
  for (pair in rate_pairings){
    indicator_name <- pair[[1]]
    curr_col <- pair[[2]]
    old_col <- pair[[3]]
    
    new_var_name <- paste0("roc||", indicator_name)
    
    data_piv[[new_var_name]] <- zero_safe_rate_of_change(data_piv[[curr_col]], data_piv[[old_col]])
  }
  
  
  
  curr_cols <- rate_pairings %>% map(2)
  old_cols <- rate_pairings %>% map(3)
  
  
  scot_figs <- list()
  ggc_figs <- list()
  for (indicator_f in indicators){
    scot_figs[[indicator_f]] <- data_f %>% 
      filter(year_start == most_recent_dates[[indicator_f]],
             indicator == indicator_f,
             iz=="Scotland") %>% 
      pull(rate) %>% 
      .[[1]]
    
    ggc_figs[[indicator_f]] <- data_f %>% 
      filter(year_start == most_recent_dates[[indicator_f]],
             indicator == indicator_f,
             iz=="NHS GGC") %>% 
      pull(rate) %>% 
      .[[1]]
  }
  
  for (pair in rate_pairings) {
    indicator <- pair[[1]]
    curr_col <- pair[[2]]
    
    new_var_name_scot <- paste("diff vs||Scotland||", indicator)
    new_var_name_ggc <- paste("diff vs||NHS GGC||", indicator)
    
    data_piv[[new_var_name_scot]] <- zero_safe_rate_of_change(data_piv[[curr_col]], scot_figs[[indicator]])
    data_piv[[new_var_name_ggc]] <- zero_safe_rate_of_change(data_piv[[curr_col]], ggc_figs[[indicator]])
    
  }
  
  
  #gets largest year_start in data
  population_year <- data %>%
    arrange(desc(year_start)) %>% 
    pull(year_start) %>% 
    .[[1]]
  
  populations <- get_pops(data_piv, data, population_year)
  
  data_piv %>% 
    select(-all_of(unlist(old_cols))) %>% 
    left_join(populations, by="iz") %>% 
    rename(!!paste0("pop||", as.character(population_year)) := pop)
}


#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# does some formatting and column renaming to make a nice to download table
# containg all info but very little formatting
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
table_for_download <- function(data){
  rescramble_diff_col <- function(diff_col_name){
    bits <- diff_col_name %>% 
      str_match_all("[^\\|]+")
    
    indicator <- bits%>% map_chr(3)
    diff_vs <- bits%>% map_chr(2)
    
    paste(indicator, "difference vs.", diff_vs)
  }
  rescramble_val_col <- function(val_col_name){
    bits <- val_col_name %>% 
      str_match_all("[^\\|]+")
    
    indicator <- bits %>% map_chr(2)
    year_str <- bits %>% map_chr(3)
    
    paste(indicator, "value at", year_str)
  }
  rescramble_roc_col <- function(roc_col_name){
    bits <- roc_col_name %>% 
      str_match_all("[^\\|]+")
    
    indicator <- bits%>% map_chr(2)
    
    paste(indicator, "rate of change from prev. period")
  }
  rename_pop <- function(pop_col_name){
    bits <-     bits <- pop_col_name %>% 
      str_match_all("[^\\|]+")
    
    pop_year <- bits %>% map_chr(2)
    paste(pop_year, "population")
  }
  
  
  data %>%
    rename_with(rescramble_diff_col, contains("diff")) %>% 
    rename_with(rescramble_val_col, contains("val||")) %>% 
    rename_with(rescramble_roc_col, contains("roc||")) %>%
    rename_with(rename_pop, contains("pop||")) %>% 
    rename(`iz name`=iz_name)
    
}





