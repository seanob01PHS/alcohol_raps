library(htmltools)

profile_table <- function(table_data, shared_table_data){
  big_areas <- c("Scotland", "NHS Greater Glasgow and Clyde", "NHS GGC")
  
  #inital columns that we don't need to iterate over
  col_defs <- list(
    iz = colDef(sticky="left", name = "Intermediate Zone Code", width = 100, 
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
    
    compared_to=colDef(show = FALSE),
    
    iz_name = colDef(sticky="left", name = "Intermediate Zone Name", width=220,
                     style=list(fontSize="8pt"),
                     headerStyle = list(fontSize="9pt")),
    
    hscp = colDef(sticky="left", name = "HSCP", width=150,
                  style=list(fontSize="8pt"),
                  headerStyle = list(fontSize="9pt"))
  )
  
  #difference columns c(abs_pos_max, abs_neg_min)
  comp_abs_maxs <- get_abs_p_m_max(table_data %>% select(contains("difference")))
  
  numbers_group <- table_data %>% 
    select(matches("(Alcohol-).*\\d")) %>% 
    colnames()
  
  for (col in numbers_group){
    col_defs[[col]] <- colDef(format=colFormat(digits=2), style=list(fontSize="8pt"), headerStyle = list(fontSize="9pt"))
  }
  
  
  roc_group <- table_data %>% 
    select(matches("(rate of change)")) %>% 
    colnames()
  
  difference_group <- table_data %>% 
    select(matches("(difference")) %>% 
    colnames()
  
  for (col in roc_group){
    col_defs[[col]] <- make_roc_column(comp_abs_maxs)
  }
  
  for (col in difference_group){
    col_defs[[col]] <- make_difference_column(comp_abs_maxs)
  }
  
  
  
  difference_col_group <- colGroup(columns = unlist(difference_group),
                                   name= "Difference rel. to NHS GGC or Scotland")
  
  roc_col_group <- colGroup(columns = unlist(roc_group),
                            name = "Rate of change rel. to previous year")
                                   
  reactable(shared_table_data,
            columns = col_defs,
            compact = TRUE,
            pagination = FALSE,
            columnGroups = list(
               difference_col_group,
               roc_col_group
            )
  )
}



#bit of a bodge. Doesn't handle stupid cases very well
get_abs_p_m_max <- function(table){
  neg_max <- table %>% select_if(is.numeric) %>% min() %>% abs()
  pos_max <- table %>% select_if(is.numeric) %>% max() %>% abs()
  c(pos_max, neg_max)
}

format_pct <- function(value) {
  if (is.na(value)) "  \u2013  "    # en dash for NAs
  else formatC(paste0(round(value * 100), "%"), width = 6)
}

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


make_roc_column <- function(abs_max_vals){
  colDef(
    headerStyle = list(fontSize="9pt"),
    format=colFormat(digits=2),
    align = "right",
    cell = format_pct,
    style = comparison_style_generator(abs_max_vals),
    html = TRUE,
    header =JS("
              function(colInfo) {
              return colInfo.column.name + '<div style=\"color: #999\">cm</div>'
              }")
    )
}



make_difference_column <- function(abs_max_vals){
  colDef(
    headerStyle = list(fontSize="9pt"),
    format=colFormat(digits=2),
    align = "right",
    cell = format_pct,
    style = comparison_style_generator(abs_max_vals),
    html = TRUE,
    header =JS("
              function(colInfo) {
              return colInfo.column.name + '<div style=\"color: #999\">cm</div>'
              }")
  )
}

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
