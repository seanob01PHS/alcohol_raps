#========================================================
# Takes a table with first column as a variable
# and all subsequent columns are observations and
# returns a reactable table with the data and a 
# sparkline for each row
#--------------------------------------------------------
table_with_sparklines <- function(table_data, p_m_colouring = FALSE, sparkline_type="line"){
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
    area = colDef(sticky="left", name = "Area", width = 200, 
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
                       align = "center",
                       cell = function(value, index) {
                         sparkline(table_data_t[[table_data[[index,1]]]], sparkline_type, list(barColor="#d26146", 
                                                                                               negBarColor="#9cc951",
                                                                                               width = "10em",
                                                                                               barWidth = "6em"))
                       })
                        )
  
  if (p_m_colouring) {
    #all the year end columns that are identical
    for (year_end_col in year_end_group){
      col_defs[[year_end_col]] <- make_year_end_column(abs_max_vals = abs_maxs)
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

make_year_end_column <- function(abs_max_vals){
  colDef(
    align = "right",
    cell = format_pct,
    style = function(value){
    pos_max <- abs_max_vals[[1]]
    neg_max <- abs_max_vals[[2]]
    
    if (value==0 | is.na(value)){
      scaled <- 0
    } else if (value > 0){
      scaled <- value/pos_max
    } else {
      scaled <- value/neg_max
    }
    
    list(color = "#111", background = p_m_colours(scaled))
  })
}

#bit of a bodge. Doesn't handle stupid cases very well
get_abs_p_m_max <- function(table){
  neg_max <- table %>% select_if(is.numeric) %>% min() %>% abs()
  pos_max <- table %>% select_if(is.numeric) %>% max() %>% abs()
  c(pos_max, neg_max)
}


format_pct <- function(value) {
  if (is.na(value)) "  \u2013  "    # en dash for NAs
  else {
    sign_numeric <- sign(value)
    sign_str <- case_when(sign_numeric==-1~"-",
                          sign_numeric==+1~"+",
                          TRUE~"")
    
    paste0(sign_str,
           formatC(paste0(round(abs(value) * 100, digits = 1), "%"), width = 6))
  }
}
