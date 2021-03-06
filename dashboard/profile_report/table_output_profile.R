#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Gathers all necessary components and returns a reactable table.
# Need to send it a tibble and SharedData (crosstalk) equivalent of 
# that table. All wrangling and processing is done on the tibble
# and the shared_table is what is actually displayed. This allowd for
# use of crosstalk in the parent envir of this function
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
profile_table <- function(all_data,
                          shared_table,
                          all_default_cols=TRUE){
  
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
    # \/ \/ \/ THIS IS IMPORTANT \/ \/ \/
    force(col)
    # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    # the environment of the variable decalred in a for loop
    # was not as local as I expected.
    # This was causing weird and annoying bugs
    force(col)
    details_col_defs[[col]] <- colDef(show = TRUE,
                                      format = colFormat(digits=2),
                                      html = TRUE,
                                      cell = details_format_generator(get_abs_p_m_max(details_pivoted_full[col])),
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