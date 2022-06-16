#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Takes a table with first column as a variable
# and all subsequent columns are observations and
# returns a reactable table with the data and a 
# sparkline for each row
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
table_with_sparklines <- function(table_data,
                                  roc_table = FALSE,
                                  sparkline_type="line",
                                  n_digits = 1){
  
  big_areas <- c("Scotland", "NHS Greater Glasgow and Clyde", "NHS GGC")
  
  table_data_t <- table_data %>%
    pivot_longer(-area, names_to = "year_ending") %>% 
    pivot_wider(names_from = area, values_from = value)
  
  year_end_group <- names(table_data) %>% tail(-1)
  
  #c(pos_max, neg_max)
  abs_maxs <- get_abs_p_m_max(table_data)
  
  table_data <- table_data %>% 
    mutate(sparkline=NA)
  
  #inital columns that we don't need to iterate over
  col_defs <- list(
    area = colDef(sticky="left",
                  class ="border-right",
                  name = "Area",
                  width = 200, 
                  style = function(value) {
                    if (value %in% big_areas) {
                      font <- "bold"
                      background <- "#e6f2fb"
                    } else {
                      font <- "regular"
                      background <- "#ffffff"
                    }
                    list(background = background, fontWeight = font)
                  }),
    
    sparkline = colDef(name = "Trend",
                       sticky = "right",
                       class = "border-left",
                       align = "center",
                       cell = function(value, index) {
                         sparkline(table_data_t[[table_data[[index,1]]]] %>% round(2), sparkline_type, list(barColor="#d26146", 
                                                                                               negBarColor="#9cc951",
                                                                                               width = "10em",
                                                                                               barWidth = "6em"))
                       })
                        )
  
  #all the year end columns that are identical
  for (year_end_col in year_end_group){
    
    if (roc_table) {
      col_defs[[year_end_col]] <- make_roc_column(table_data[year_end_col],
                                                  col_name = year_end_col,
                                                  header_str = "")
    } else {
      col_defs[[year_end_col]] <- colDef(cell=make_plain_val_col_format(n_digits = n_digits))
    }

  }
  
  reactable(table_data,
            columns = col_defs,
            defaultColDef = colDef(
              cell = function(value){
                if (is.numeric(value)){
                  format(round(value, digits = 1), big.mark=",")
                } else{
                  value
                }
                }
            ),
            compact = TRUE,
            columnGroups = list(
              colGroup(name="Year Ending", columns=year_end_group)
              )
            )
}

#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Simple function currying of number of digits
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
make_plain_val_col_format <- function(n_digits){
  function (value){
    format_val(value, n_digits)
  }
}

