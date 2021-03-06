#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Makes a relative change (rel first column) table from data
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
relative_change_table_data <- function(data, val_name){
  data_f <- data %>%
    format_and_pivot(val_name) %>% 
    # everything as percentage of first date (column 2)
    mutate_if(is.numeric, zero_safe_rate_of_change, .[[2]])
  data_f[2] <- NA
  
  data_f
}

#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Makes a relative change (rolling rel previous column) table from data
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
year_on_year_change <- function(data, val_name){
  data_i <- data %>% 
    format_and_pivot(val_name)
  
  data_f <- data_i
  for (i in seq_along(data_i)){
    #first column is places,
    #second is initial year (no previous)
    if (i>2) {
      prev <- data_i[i-1]
      cur <- data_i[i]
      data_f[i] <- (cur-prev)/prev
    }
  }
  data_f[2] <- NA  
  data_f
}

#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Makes the simplified table data
# from the input DataFrame
# Each column is a year and each row is
# an area. The cells represent the value in
# the column val_name
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
format_and_pivot <- function(data, val_name){
  big_areas <- c("Scotland", "NHS Greater Glasgow and Clyde", "NHS GGC")
  data %>%
    #convert date time to string based on template
    mutate(date_end = format(date_end, format="%d-%b-%y")) %>%
    select(c("area", "date_end", val_name)) %>%
    pivot_wider(names_from = "date_end", values_from = val_name) %>%
    #put big areas at bottom
    arrange(area %in% big_areas, area)
}


