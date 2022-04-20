library(htmltools)

profile_table <- function(all_data, shared_table, all_default_cols=TRUE){
  
  populations <- all_data %>%
    select(contains("pop"), iz) %>% 
    rename_with(function(x) "pop", contains("pop"))
  
  population_year <- Filter(function(x) grepl("(pop)", x), names(all_data)) %>% 
    str_match_all("[^\\|]+") %>% 
    .[[1]] %>% .[[2]] %>% 
    as.numeric()
  
  table_data <- all_data %>% select(-contains("diff vs||"), -contains("pop"))
  
  
  big_areas <- c("Scotland", "NHS Greater Glasgow and Clyde", "NHS GGC")
  
  
  col_names <- table_data %>% colnames
  indicators <- Filter(function(x) grepl("^(val\\|\\|)", x), col_names) %>% 
    str_match_all("[^\\|]+") %>% 
    map_chr(2)

  default_sorting <- list()
   
  if (all_default_cols){
    my_col_defs <- default_cols_full()
    # sort table by first value column show
    ha_admissions_val_col <- col_names[[4]]
    default_sorting[[ha_admissions_val_col]] <- "desc"
  } else {
    my_col_defs <- default_cols_area()
  }
  
  my_col_groups <- list()
  
  # Main loop where all the value and roc columns are generated
  for (indicator in indicators){
    val_col <- col_names[[which(grepl(paste0("^(val\\|\\|",
                                             indicator, ")"),
                                      col_names))]]
    roc_col <- col_names[[which(grepl(paste0("^(roc\\|\\|",
                                             indicator, ")"),
                                      col_names))]]
    year_str <- col_names[[which(grepl(paste0("^(val\\|\\|",
                                              indicator, ")"),
                                       col_names))]] %>% 
      str_match_all("[^\\|]+") %>% map_chr(3)
    
    my_col_defs[[val_col]] <- make_val_column(table_data[val_col])
    my_col_defs[[roc_col]] <- make_roc_column(table_data[roc_col])
    
    
    my_col_groups[[length(my_col_groups)+1]] <- 
      colGroup(name = paste(indicator, year_str, sep = "||"), columns = c(val_col, roc_col),
               header=col_group_header)
  }
  

   
  reactable(shared_table,
            selection = "multiple",
            onClick = "select",
            columns = my_col_defs,
            defaultColDef = colDef(show = FALSE),
            searchable = FALSE,
            compact = TRUE,
            pagination = FALSE,
            columnGroups = my_col_groups,
            defaultSorted = default_sorting,
            details =   colDef(
              show=TRUE,
              details = details_generator(all_data, populations, population_year))
  )
}


#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Custom header generator for column group headings
# The year string is small and in grey
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
col_group_header <- function(header_name){
  parts <- header_name %>% str_match_all("[^\\|]+") %>% .[[1]]
  indicator <- parts[[1]]
  year_str <- parts[[2]]
  
  div(class="col_group_head",
      indicator,
      div(class="col_group_subhead",
          year_str))
  
}

#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# To get the absolute value of the extremes of a data set
# for colour scaling purposes
# Returns c(abs(max(x)), abs(min(x)))
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
get_abs_p_m_max <- function(table){
  # test if table is only NA return c(0,0)
  if (is.na(table[1,1])){
    #browser()
  }

  if (table %>%
      map(~ is.na(.x) %>% 
          reduce(`&`)) %>% 
      reduce(`&`)){
    return(c(1,1))
  }
  
  neg_max <- table %>% select_if(is.numeric) %>% min(na.rm = TRUE) %>% abs()
  pos_max <- table %>% select_if(is.numeric) %>% max(na.rm = TRUE) %>% abs()
  return(c(pos_max, neg_max))
}





#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Makes the function that returns the style of a comparison
# cell based on abs_max_vals
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
comparison_style_generator <- function(abs_max_vals, colour_pallette){
  function(value){
    pos_max <- abs_max_vals[[1]]
    neg_max <- abs_max_vals[[2]]
    
    if (value==0 | is.na(value)){
      scaled <- 0
    } else if (value > 0){
      scaled <- value/pos_max
    } else {
      scaled <- value/neg_max
    }
    
    list(color = "#111", background = colour_pallette(scaled), fontSize="9pt")
  }
}







#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# returns a function that returns different colours to positive
# and negative values given a pre scaled value input
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
make_p_m_color_pal <- function(plus_neg_zero_colours, bias = 1) {
  p_col <- plus_neg_zero_colours[[1]]
  n_col <- plus_neg_zero_colours[[2]]
  z_col <- plus_neg_zero_colours[[3]]
  
  get_colour_p <- colorRamp(c(z_col, p_col), bias = bias)
  get_colour_n <- colorRamp(c(z_col, n_col), bias = bias)
  
  function(x) {
    if (x==0){
      #usually white
      z_col
    } else if (x < 0){
      rgb(get_colour_n(abs(x)), maxColorValue = 255)
    } else {
      rgb(get_colour_p(x), maxColorValue = 255)
    }
  }
}


p_m_colours <- make_p_m_color_pal(c("#d26146", "#9cc951", "#ffffff"), bias=10)
val_colours <- make_p_m_color_pal(c("#EAC43A", "#9cc951", "#ffffff"), bias=1.5)


#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Returns a list of details for each row
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
details_generator <- function(data, populations, population_year){
  
  
  details_data <- data %>% select(contains("diff vs||"))
  details_df_list <- split(details_data, seq(nrow(details_data)))
  
  details_pivoter <- function(x){
    x %>% 
      pivot_longer(everything()) %>% 
      mutate(`Difference rel to` = name %>% str_match_all("[^\\|]+") %>% map_chr(2),
             indicator= name %>% str_match_all("[^\\|]+") %>% map_chr(3)) %>%
      select(-name) %>% 
      pivot_wider(names_from = "indicator") %>% 
      rename_with(str_trim, everything())
  }
  
  
  details_df_list <- lapply(details_df_list, details_pivoter)
  
  #now just for the abs_maxs
  details_pivoted_full <- do.call(rbind, details_df_list)
  
  
  diff_col_name <- details_df_list[[1]] %>% 
    names()
  diff_col_name <- diff_col_name[[which(str_contains(diff_col_name, "iff"))]]
  
  details_col_defs <- list()
  details_col_defs[[diff_col_name]] <- colDef(show=TRUE,
                                              html = TRUE,
                                              class = "details_diff_vs_col",
                                              width = 100,
                                              headerClass = "details_head")
  
  
  non_diff_col_names <- names(details_df_list[[1]])[-which(details_df_list[[1]] %>% names == diff_col_name)]
  
  for (col in non_diff_col_names){
    #THIS IS IMPORTANT
    # the environment of the variable decalred in a for loop
    # was not as local as I expected.
    # This was causing weird and annoying bugs
    my_col <- col
    details_col_defs[[my_col]] <- colDef(show = TRUE,
                                      format = colFormat(digits=2),
                                      html = TRUE,
                                      cell = details_format_generator(get_abs_p_m_max(details_pivoted_full[my_col])),
                                      headerClass = "details_head")
  }
  
  
  
  function(index){
    div(
      div(class = "details_pop",
          h2(paste(population_year, "Population")),
          format(round(populations[index, "pop"][[1]]), big.mark=",")),
      div(class = "details_table",
                   h1("% difference in rates relative to Scotland and NHS GGC"),
                   reactable(details_df_list[[index]],
                             columns = details_col_defs,
                             outlined = TRUE
                             )
          )
      )
  }
}

#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# For doing the renaming above
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
indicator_from_diff_col <- function(col_name){
  indicator <- col_name %>% 
    str_match_all("[^\\|]+") %>% 
    .[[1]] %>% .[[3]]
}


#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Nice formatting for details
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
details_format_generator <- function(abs_max_vals_i){
  pos_max <- abs_max_vals_i[[1]]
  neg_max <- abs_max_vals_i[[2]]
  function(value, row_index, col_name){
    
    if (value==0 | is.na(value)){
      scaled <- 0
    } else if (value > 0){
      scaled <- value/pos_max
    } else {
      scaled <- value/neg_max
    }

    out_str <- format_pct(value)
    div(class = "details_col", style = list(background = p_m_colours(scaled)), out_str)
  }
}