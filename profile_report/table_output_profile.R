library(htmltools)

profile_table <- function(table_data, all_data){
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
    
    my_col_defs[[val_col]] <- make_val_column(table_data[val_col])
    my_col_defs[[roc_col]] <- make_roc_column(table_data[roc_col])
    
    
    my_col_groups[[length(my_col_groups)+1]] <- 
      colGroup(name = indicator, columns = c(val_col, roc_col))
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
              details = details_generator(all_data)
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
          div(class = "val_and_roc_subhead",
              "EASR")
          )}
  )
}

#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Generator for Rate Of Change columns
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
make_roc_column <- function(col_data){
  abs_max_vals <- get_abs_p_m_max(col_data)
  colDef(
    show = TRUE,
    name = "Rate of Change",
    headerStyle = list(fontSize="9pt"),
    format=colFormat(digits=2),
    align = "right",
    cell = format_pct,
    style = comparison_style_generator(abs_max_vals),
    html = TRUE,
    header = function(header_val, col_name){
      div(class = "val_and_roc_head",
          header_val,
          div(class = "val_and_roc_subhead",
              "rel. prev. period")
      )}
  )
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
  else formatC(paste0(round(value * 100), "%"), width = 6)
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
    
    list(color = "#111", background = p_m_colours(scaled), fontSize="8pt")
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
                  list(background = background, fontWeight = font, fontSize="8pt")
                },
                headerStyle = list(fontSize="9pt")),
    iz_name = colDef(sticky="left", name = "Intermediate Zone Name", width=220,
                     filterable = TRUE,
                     show = TRUE,
                     style=list(fontSize="8pt"),
                     headerStyle = list(fontSize="9pt")),
    
    hscp = colDef(sticky="left", name = "HSCP", width=125,
                  filterable = TRUE,
                  show = TRUE,
                  style=list(fontSize="8pt"),
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
details_generator <- function(data){
  details_data <- data %>% select(contains("diff vs||"))
  details_col_defs <- list()
  abs_max_col_vals <- list()
  
  
  for (col_x in details_data %>% names %>% sort){
    
    if (grepl("(Scotland)", col_x)){
      rel_to <- "Scotland"
      #print("yoyo")
    } else if (grepl("(NHS GGC)", col_x)){
      rel_to <- "NHS GGC"
    } else {
      rel_to <- "ERROR"
    }
    
    header_f <-  function(header_val, col_name){
      div(class = "details_head",
          header_val,
          div(class = "details_subhead",
              rel_to)
      )}
    
    
    details_col_defs[[col_x]] <- colDef(show = TRUE,
                                        name = paste(indicator_from_diff_col(col_x), "difference"),
                                        format = colFormat(digits=2),
                                        html = TRUE,
                                        cell = details_format_generator(get_abs_p_m_max(details_data[col_x])),
                                        header = header_f
                                        )
    
  }
  
  
  function(index){
    htmltools::div(style = "padding: 16px",
                   reactable(details_data[index, ],
                             columns = details_col_defs
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
details_format_generator <- function(abs_max_vals){
  function(value, row_index, col_name){
    pos_max <- abs_max_vals[[1]]
    neg_max <- abs_max_vals[[2]]
    
    
    pos_max <- 10
    neg_max <- 1
    
    if (value==0 | is.na(value)){
      scaled <- 0
    } else if (value > 0){
      scaled <- value/pos_max
    } else {
      scaled <- value/neg_max
    }
    
    
    # print("===========")
    # print(col_name)
    # print(value)
    # print(pos_max)
    # print(neg_max)
    # print(scaled)
    # 
    display_val <- format_pct(value)
    div(class = "details_col", style = list(background = p_m_colours(scaled)), display_val)
  }
}