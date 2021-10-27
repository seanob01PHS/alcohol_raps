#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Makes the simplified table data
# from the input DataFrame
# Each column is a year and each row is
# an area. The cells represent the value in
# the column val_name
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
trend_table_data <- function(data, val_name){
  big_areas <- c("Scotland", "NHS Greater Glasgow and Clyde")
  data %>%
    #convert date time to string based on template
    mutate(date_end = format(date_end, format="%d-%b-%y")) %>%
    select(c("area", "date_end", val_name)) %>%
    pivot_wider(names_from = "date_end", values_from = val_name) %>%
    #put big areas at bottom
    arrange(area %in% big_areas, area)
}


#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Makes a relative change (percentage of first column) table from data
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
relative_change_table_data <- function(data, val_name){
  big_areas <- c("Scotland", "NHS Greater Glasgow and Clyde")
  data %>%
    #convert date time to string based on template
    mutate(date_end = format(date_end, format="%d-%b-%y")) %>%
    select(c("area", "date_end", val_name)) %>%
    pivot_wider(names_from = "date_end", values_from = val_name) %>%
    # everything as percentage of first date (column 2)
    mutate_if(is.numeric, change_percentage, .[[2]]) %>%
    mutate_if(is.numeric, round, 2) %>% 
    #put big areas at bottom
    arrange(area %in% big_areas, area)
}

change_percentage <- function(col, init_col){
  round((col/init_col - 1)*100,2)
}




#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Runs generate_trend_plots over each row in
# trend_table_data.
# This MUST be run in a chunk with the option include=FALSE
# This will allow for the plots to be generated but not shown
# as they only need to be shown in the table
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
make_all_plots <- function(data, val_name, table_generator){
  par(oma=c(0,0,0,0))
  par(mar=c(0,0,0,0))
  
  table_data <- data %>% table_generator(val_name)
  
  #generate the plots
  apply(table_data[,2:ncol(table_data)], 1, generate_trend_plots)
}


#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Generates and displays the final table with the plots
# embedded in the last column
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
make_trend_table <- function(trend_table_data){
  #add the plots (filepaths pointing to them) to the data
  out <- cbind(trend_table_data, sprintf("![](%s%s-%s.png){width=45%%}", opts_current$get("fig.path"), opts_current$get("label"), 1:nrow(trend_table_data)))
  
  #you NEED the column name to not be "![](%s%s-%s.png){width=45%%}" which is what it is automatically assigned as
  names(out) <- append(names(out)[1:length(names(out))-1], "Trend lines")
  
  out %>%
    kable() %>%
    kable_styling() %>% 
    column_spec(1, width = "15em") %>%
    row_spec(c(7,8), hline_after = TRUE, color = phs_colours("phs-teal-80"), background = phs_colours("phs-teal-10")) %>%
    column_spec(ncol(out), width="20em", border_left = TRUE, background="white") %>% 
    add_header_above(c(" " = 1, "Year Ending" = 9, "  "=1))
}




#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
make_relative_change_table <- function(relative_change_table_data){
  
  abs_max <- relative_change_table_data %>% 
    select_if(is.numeric) %>% 
    abs() %>% 
    max()
  
  out <- relative_change_table_data %>% 
    mutate_if(is.numeric, change_colourings, abs_max)
  
  #add the plots (filepaths pointing to them) to the data
  out <- cbind(out, sprintf("![](%s%s-%s.png){width=45%%}", opts_current$get("fig.path"), opts_current$get("label"), 1:nrow(out)))
  
  #you NEED the column name to not be "![](%s%s-%s.png){width=45%%}" which is what it is automatically assigned as
  names(out) <- append(names(out)[1:length(names(out))-1], "Trend lines")
  
  out %>%
    kable(escape=FALSE) %>%
    kable_styling() %>% 
    column_spec(1, width = "15em") %>%
    row_spec(c(7,8), hline_after = TRUE, color = phs_colours("phs-teal-80")) %>%
    column_spec(ncol(out), width="20em", border_left = TRUE) %>% 
    add_header_above(c(" " = 1, "Year Ending" = 9, "  "=1))
}

change_colourings <- function(x, abs_max){
  alphas <- abs(x/abs_max)
  cell_spec(x, background = case_when(!is.numeric(x)~"white",
                                      x>0 ~ rgb(199/255,57/255,24/255, alphas),
                                      x<0 ~ rgb(0/255,120/255,212/255, alphas),
                                      TRUE ~ "white"))
}





#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Makes a single trend plot from a row vector
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
generate_trend_plots <- function(col){
  print("yo")
  col_len = length(col)
  data = tibble(x=1:col_len, y=col)
  
  max_y = data %>% pull(y) %>% max()
  #first instance of row where y is maximum
  max_x = data %>% filter(y==max_y) %>% pull(x) %>% min()
  
  min_y = data %>% pull(y) %>% min()
  #first instance of row where y is minimum
  min_x = data %>% filter(y==min_y) %>% pull(x) %>% min()
  
  colours <- list(rep("black", col_len))
  colours[min_x] <- "blue"
  colours[max_x] <- "red"
  
  min_max <- data %>% slice(c(min_x,max_x)) %>% mutate(type=c("min", "max"))  
  
  data %>% ggplot(aes(x=x,y=y)) +
    geom_line(size=0.4, colour="#CAC6D1") +
    geom_point(data=min_max, aes(x=x,y=y, colour=type, shape="")) +
    
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          
          axis.line =element_blank(),
          
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          
          plot.margin = unit(c(0, 0, 0, 0), "null"),
          panel.margin = unit(c(0, 0, 0, 0), "null"),
          
          legend.position = "none",
          legend.margin = unit(0, "null"))
  
}





