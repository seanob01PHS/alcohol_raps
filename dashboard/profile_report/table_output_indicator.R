indicator_table <- function(data,
                            data_shared,
                            type="val", # can be "val" or "change"
                            ini_or_year_on_year="ini", # relevant when type="change"
                                                      # changes some subheader strings
                            sparkline_type = "line"
                            ){
  
  # column names that define each row
  # the rest of the columns are `year_cols`
  id_cols <- c("iz", "iz_name", "hscp", "indicator")
  year_cols <- names(data) %>% .[!(. %in% id_cols)]
  latest_year <- year_cols %>% tail(1)
  
  col_defs <- default_cols_full(width_stretch=1.4)
  
  # For the column group label over all the year columns
  year_type <- year_cols[[1]] %>% find_year_type()
  
  # Finding the transpose of the year columns for the sparkline
  year_df_t <- data %>% 
    select(all_of(c("iz", year_cols))) %>% 
    pivot_longer(all_of(year_cols), names_to="year") %>% 
    pivot_wider(names_from = "iz", values_from="value")
  
  
  # Super header (column group) over all the years
  col_group_defs <- list()
  col_group_defs[[1]] <- colGroup(name = "years_group",
                                  columns = year_cols,
                                  header = function(header_name){
                                    div(class="col_group_head",
                                        year_type)
                                  })
  
  for (col_index in seq_along(year_cols)){
    col_name <- year_cols[[col_index]]
    
    # make leftmost year column have separating border on left
    cell_class <- NULL
    if (col_index==1){
      cell_class <- "border-left"
    }
    
    if (type=="val"){
      # val column type
      col_defs[[col_name]] <- make_val_column(data[col_name],
                                              col_name = col_name,
                                              header_str = "EASR",
                                              cell_class = cell_class)
    } else if (type=="change") {
      # roc (rate of change) column type
      
      subhead <- "error"
      if (ini_or_year_on_year=="ini"){
        subhead <- "% change"
      } else if (ini_or_year_on_year=="year_on_year"){
        subhead <- "% change"
      }
      
      col_defs[[col_name]] <-  make_roc_column(data[col_name],
                                               col_name = col_name,
                                               header_str = subhead,
                                               cell_class = cell_class)
      
    }
  }
  
  # columns needs to be present in the data
  data <- data %>%
    mutate(sparkline = NA)
  
  # sparkline
  col_defs[["sparkline"]] <- colDef(show = TRUE,
                                   name = "Trend",
                                   sticky = "right",
                                   align = "center",
                                   class = "border-left",
                                   cell = function(value, index){
    sparkline(year_df_t[[data[[index,1]]]] %>% round(2), sparkline_type, list(barColor="#d26146", 
                                                                           negBarColor="#9cc951",
                                                                           width = "10em",
                                                                           barWidth = "6em"))
                                   })
  
  default_sorting <- list()
  default_sorting[[latest_year]] <- "desc"
  
  reactable(data_shared,
            selection = "multiple",
            onClick = "select",
            columns = col_defs,
            defaultColDef = colDef(show = FALSE),
            columnGroups = col_group_defs,
            defaultSorted = default_sorting,
            searchable = FALSE,
            compact = TRUE,
            pagination = FALSE
  )
}


make_change_column <- function(col_data){
  
}

# Header string for year type
find_year_type <- function(sample_year_str){
  if (grepl("to", sample_year_str)){
    "3-Year Aggregate (Financial Years)"
  } else if (grepl("-", sample_year_str)){
    "3-Year Aggregate (Calendar Years)"
  } else if (grepl("/", sample_year_str)){
    "Financial Year"
  } else {
    "Year type error"
  }
}
