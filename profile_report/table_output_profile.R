library(htmltools)

profile_table <- function(all_data, populations, population_year=9999){
  table_data <- all_data %>% select(-contains("diff vs||"))
  
  
  big_areas <- c("Scotland", "NHS Greater Glasgow and Clyde", "NHS GGC")
  
  
  col_names <- table_data %>% names
  indicators <- Filter(function(x) grepl("^(val\\|\\|)", x), col_names) %>% 
    str_match_all("[^\\|]+") %>% 
    map_chr(2)
  
  
  my_col_defs <- default_cols()
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

  
   
  reactable(table_data,
            columns = my_col_defs,
            defaultColDef = colDef(show = FALSE),
            searchable = TRUE,
            compact = TRUE,
            pagination = FALSE,
            columnGroups = my_col_groups,
            details =   colDef(
              show=TRUE,
              details = details_generator(all_data, populations, population_year)
            )
  )
}



#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Generator for value columns
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
make_val_column <- function(col_data){
  abs_max_vals <- get_abs_p_m_max(col_data)
  colDef(
    show = TRUE,
    name = "Value",
    headerStyle = list(fontSize="9pt"),
    format=colFormat(digits=2),
    align = "right",
    class = "border-left",
    style = comparison_style_generator(abs_max_vals),
    html = TRUE,
    header = function(header_val, col_name){
      div(class = "val_and_roc_head",
          header_val,
          div(class="val_and_roc_subhead",
              "EASR"))
      }
  )
}

#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Generator for Rate Of Change columns
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
make_roc_column <- function(col_data){
  abs_max_vals <- get_abs_p_m_max(col_data)
  
  arrows_and_colours <- list(
    up=list(shiny::icon("arrow-up"), "#B8100D"),
    down=list(shiny::icon("arrow-down"), "#71B109"),
    zero=list(shiny::icon("grip-lines"), "#F4E61F"))
  
  roc_column_cell <- function(value){
    show_str <- format_pct(value)
    if (is.na(value) || value==0){
      icon_and_colour <- arrows_and_colours[["zero"]]
    } else if (value < 0){
      icon_and_colour <- arrows_and_colours[["down"]]
    } else {
      icon_and_colour <- arrows_and_colours[["up"]]
    }
    
    icon <-tagAppendAttributes(icon_and_colour[[1]],
                               style = paste("color:", icon_and_colour[[2]]))
    div(class="roc_col_left",
        role = "img",
        icon,
        div(class="roc_col",
            show_str))
  }
  
  colDef(
    show = TRUE,
    name = "Rate of Change",
    headerStyle = list(fontSize="9pt"),
    format=colFormat(digits=2),
    align = "right",
    cell = roc_column_cell,
    style = comparison_style_generator(abs_max_vals),
    html = TRUE,
    header = function(header_val, col_name){
      div(class = "val_and_roc_head",
          header_val,
          div(class="val_and_roc_subhead",
              "rel. prev. period"))
      }
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
  neg_max <- table %>% select_if(is.numeric) %>% min(na.rm = TRUE) %>% abs()
  pos_max <- table %>% select_if(is.numeric) %>% max(na.rm = TRUE) %>% abs()
  c(pos_max, neg_max)
}


#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Nice formatting of pct columns
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
format_pct <- function(value) {
  if (is.na(value)) "  \u2013  "    # en dash for NAs
  else {
    sign_numeric <- sign(value)
    sign_str <- case_when(sign_numeric==-1~"-",
                          sign_numeric==+1~"+",
                          TRUE~"")
    
    paste0(sign_str,
           formatC(paste0(round(abs(value) * 100), "%"), width = 6))
  }
}


#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Makes the function that returns the style of a comparison
# cell based on abs_max_vals
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
comparison_style_generator <- function(abs_max_vals){
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
    
    list(color = "#111", background = p_m_colours(scaled), fontSize="9pt")
  }
}


#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Sets up the defaults (basic info) columns
# Just to clear up space
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
default_cols <- function(){
  list(
    iz = colDef(sticky="left", name = "Intermediate Zone Code", width = 85, 
                show = TRUE,
                filterable = TRUE,
                style = function(value) {
                  if (value %in% big_areas) {
                    font <- "bold"
                    background <- "#e6f2fb"
                  } else {
                    font <- "regular"
                    background <- "#ffffff"
                  }
                  list(background = background, fontWeight = font, fontSize="9pt")
                },
                headerStyle = list(fontSize="9pt")),
    iz_name = colDef(sticky="left", name = "Intermediate Zone Name", width=220,
                     filterable = TRUE,
                     show = TRUE,
                     style=list(fontSize="9pt"),
                     headerStyle = list(fontSize="9pt")),
    
    hscp = colDef(sticky="left", name = "HSCP", width=125,
                  filterable = TRUE,
                  show = TRUE,
                  style=list(fontSize="9pt"),
                  headerStyle = list(fontSize="9pt"))
  )
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


p_m_colours <- make_p_m_color_pal(c("#d26146", "#9cc951", "#ffffff"), bias=1.5)



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
                   h1("Relative difference in rates compared to Scotland and NHS GGC"),
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
    # print("===========")
    # print(col_name)
    # print(value)
    # print(pos_max)
    # print(neg_max)
    
    if (value==0 | is.na(value)){
      scaled <- 0
    } else if (value > 0){
      scaled <- value/pos_max
    } else {
      scaled <- value/neg_max
    }
    
    # print(scaled)

    out_str <- format_pct(value)
    div(class = "details_col", style = list(background = p_m_colours(scaled)), out_str)
  }
}