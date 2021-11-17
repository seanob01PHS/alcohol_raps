year_type <- function(year_str){
  case_when(grepl("-", year_str) ~ "3 cal year agg",
            grepl(" to ", year_str) ~ "3 fin year agg",
            grepl("/", year_str) ~ "fin year",
            TRUE ~ NA_character_)
}


process_year <- function(data){
  data %>% 
    mutate(year_type=year_type(year),
           year_start=as.numeric(substr(year, 1, 4))) %>% 
    rename(year_str=year)

}




most_up_to_date_date <- function(data, indicator_f){
  data %>% filter(indicator==indicator_f) %>% pull(year_start) %>% max()
}





rate_pairings <- function(data, most_recent_dates){
  #list of [[recent_col_1, old_col_1], [recent_col_2, old_col_2]...]
  column_pairings <- list()
  for (indicator_f in data %>% pull(indicator) %>% unique()){
    cur_year_str <- data %>% 
      filter(indicator==indicator_f,
             year_start==most_recent_dates[indicator]) %>% 
      pull(year_str) %>% 
      .[[1]]
    
    prev_year_str <- data %>% 
      filter(indicator==indicator_f,
             year_start==unlist(most_recent_dates[indicator])-1) %>% 
      pull(year_str) %>% 
      .[[1]]
    
    column_pairings[[length(column_pairings)+1]] <- list(indicator_f,
                                                         paste(indicator_f, cur_year_str),
                                                         paste(indicator_f, prev_year_str))
  }
  column_pairings
}


make_profile_frame <- function(data){
  
  indicators <- data %>% pull(indicator) %>% unique()
  
  most_recent_dates <- list()
  for (indicator in indicators){
    most_recent_dates[[indicator]] <- most_up_to_date_date(data, indicator)
  }
  
  data_f <- data %>%
    #choosing the most recent and second most recent dates
    filter(year_start == most_recent_dates[indicator] |
           year_start == unlist(most_recent_dates[indicator]) - 1)
  
  data_piv <- data_f %>% 
    mutate(indicator_and_year_str = paste(indicator, year_str)) %>% 
    select(iz, iz_name, hscp, indicator_and_year_str, rate) %>% 
    pivot_wider(names_from = indicator_and_year_str, values_from = rate)
  
  
  rate_pairings <- rate_pairings(data_f, most_recent_dates)
  
  for (pair in rate_pairings){
    indicator_name <- pair[[1]]
    curr_col <- pair[[2]]
    old_col <- pair[[3]]
    
    new_var_name <- paste(indicator_name, "rate of change")
    
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
    
    new_var_name_scot <- paste(indicator, " difference compared to Scotland")
    new_var_name_ggc <- paste(indicator, " difference compared to NHS GGC")
    
    data_piv[[new_var_name_scot]] <- zero_safe_rate_of_change(data_piv[[curr_col]], scot_figs[[indicator]])
    data_piv[[new_var_name_ggc]] <- zero_safe_rate_of_change(data_piv[[curr_col]], ggc_figs[[indicator]])
    
  }
  
  
  data_piv_f <- data_piv %>%
    pivot_longer(cols = contains("compared to"), names_to = "full_comparison", values_to = "comparison_ratio") %>% 
    mutate(compared_to = full_comparison %>% str_split(" compared to ") %>% map_chr(2),
           comparison_indicator = full_comparison %>% str_split(" compared to ") %>% map_chr(1)) %>% 
    select(-full_comparison) %>% 
    pivot_wider(names_from = "comparison_indicator", values_from = "comparison_ratio")

  
  data_piv_f %>% select(-all_of(unlist(old_cols)))
}

first_non_na <- function(grp){
  first(na.omit(grp))
}


zero_safe_rate_of_change <- function(a,b){
  case_when(a>0 & b==0 ~ NA_real_,
            a==0 & b==0 ~ 0.,
            TRUE ~ a/b - 1)
}
