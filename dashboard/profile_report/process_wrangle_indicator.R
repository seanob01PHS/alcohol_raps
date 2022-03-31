

make_indicator_detail_frames <- function(data, indicator_name){
  data <- data %>%
   filter(indicator==indicator_name)
 
  latest_year <- data %>%
    arrange(year_str) %>% 
    pull(year_str) %>% 
    unique() %>% 
    tail(1)
  
  df_vals <- data %>%
   pivot_wider(id_cols = all_of(c("iz", "iz_name", "hscp", "indicator")),
               names_from = year_str,
               values_from = rate) %>%
    #arrange by values in latest year
    dplyr::arrange(desc(all_of(latest_year)))
 
  df_change_ini <- df_vals
  df_change_rolling <- df_vals

 
  # number of columns before the first column that contains rate data
  cols_before_first_rate <- 4
 
  ini <- df_vals[[cols_before_first_rate + 1]]
 
  for (i in seq_along(df_vals)){
    # +1 because cant compute change on first rate
    # I dont care that this is slightly wasteful.
    if (i>cols_before_first_rate+1){
      prev <- df_vals[[i-1]]
      cur <- df_vals[[i]]
      
      # rel initial year
      df_change_ini[i] <- zero_safe_rate_of_change(cur, ini)
      
      # rolling
      df_change_rolling[i] <- zero_safe_rate_of_change(cur, prev)
    }
  }
  
  df_change_rolling[cols_before_first_rate + 1] <- NA_real_
  df_change_ini[cols_before_first_rate + 1] <- NA_real_
  
  list("vals" = df_vals,
       "change_ini" = df_change_ini,
       "change_rolling" = df_change_rolling)
}
