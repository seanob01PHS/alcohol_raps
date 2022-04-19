#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Generator for value columns
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
make_val_column <- function(col_data,
                            col_name = "Value",
                            header_str = "EASR (per 10,000 pop)",
                            cell_class = "border-left"){
  abs_max_vals <- get_abs_p_m_max(col_data)
  
  greater_than_q75_colour <- "#F3DE90"
  cell_style <- value_style_generator(col_data, greater_than_q75_colour)
  
  colDef(
    show = TRUE,
    name = col_name,
    headerStyle = list(fontSize="9pt"),
    cell = val_cell_js(),
    align = "right",
    class = cell_class,
    style = cell_style,
    html = TRUE,
    header = function(header_val, col_name){
      div(class = "val_and_roc_head",
          header_val,
          div(class="val_and_roc_subhead",
              header_str))
    }
  )
}



#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Generator for Rate Of Change columns
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
make_roc_column <- function(col_data,
                            col_name = "% change",
                            header_str = "relative to prev. period",
                            cell_class = NULL){
  abs_max_vals <- get_abs_p_m_max(col_data)
  
  roc_column_cell <- function(value){
    show_str <- format_pct(value)
    div(class="roc_col",
        show_str)
  }
  
  colDef(
    show = TRUE,
    name = col_name,
    headerStyle = list(fontSize="9pt"),
    format=colFormat(digits=2),
    align = "right",
    class = cell_class,
    cell = roc_column_cell,
    style = comparison_style_generator(abs_max_vals, p_m_colours),
    html = TRUE,
    header = function(header_val, col_name){
      div(class = "val_and_roc_head",
          header_val,
          div(class="val_and_roc_subhead",
              header_str))
    }
  )
}


#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Sets up the defaults (basic info) columns
# Just to clear up space
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
default_cols_full <- function(width_stretch=1){
  list(
    iz = colDef(sticky="left", name = "Intermediate Zone Code", width = 95 * width_stretch, 
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
    iz_name = colDef(sticky="left", name = "Intermediate Zone Name", width=220 * width_stretch,
                     filterable = TRUE,
                     show = TRUE,
                     style=list(fontSize="9pt"),
                     headerStyle = list(fontSize="9pt")),
    
    hscp = colDef(sticky="left", name = "HSCP", width=125 * width_stretch,
                  filterable = FALSE,
                  show = TRUE,
                  style=list(fontSize="9pt"),
                  headerStyle = list(fontSize="9pt")),
    
    # no tick selector column for now
    .selection = colDef(show=FALSE)
  )
}


#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# Sets up the defaults (basic info) columns
# For when we only want area name (when showing big areas)
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
default_cols_area <- function(width_stretch = 1){
  list(
    iz = colDef(sticky="left", name = "Area", width = 300*width_stretch, 
                show = TRUE,
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
                headerStyle = list(fontSize="9pt"))
  )
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
           formatC(paste0(round(abs(value) * 100, digits = 1), "%"), width = 6))
  }
}


#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
# For colouring cells in 75th %ile+ for the whole table (static)
#{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
value_style_generator <- function(column_vals, colour){
  col_75th <- quantile(column_vals[[1]], 0.75)[[1]]
  function(value){
    background <- "#FFFFFF"
    if (value >= col_75th){
      background <- colour
    }
    
    list(background=background)
  }
}




# Javascript for adding dynamic quartile calculation to value cells
val_cell_js <- function(){
  cell_JS <- htmlwidgets::JS("function(cellInfo, state) {

                  var currCellVal = cellInfo.value
                  var thisColName = cellInfo.column.id;
                  var thisColDataCurr = state.pageRows.map(function(value,index) { return value[thisColName]; });
                  var thisColDataOrig = state.data.map(function(value,index) { return value[thisColName]; });            

                  const quantile = (arr, q) => {
                      const sorted = arr.sort((a, b) => a - b);
                      const pos = (sorted.length - 1) * q;
                      const base = Math.floor(pos);
                      const rest = pos - base;
                      if (sorted[base + 1] !== undefined) {
                          return sorted[base] + rest * (sorted[base + 1] - sorted[base]);
                      } else {
                          return sorted[base];
                      }
                  };

                  var q75Curr = quantile(thisColDataCurr, 0.75);

                  const makeInDiv = function(text, classF){
                    return '<div class=\"' + classF + '\">' + text + '</div>';
                  }

                  var varIcon = ''
                  
                  if (thisColDataCurr.length != thisColDataOrig.length){
                      if (currCellVal >= q75Curr){
                          varIcon += '<i class=\"fa fa-circle-o\" style=\"color: #d26146\" aria-hidden=\"true\"></i>';
                      } else {
                          varIcon += '<i class=\"fa fa-fw\" aria-hidden=\"true\"></i>';
                      }
                  }
                  return makeInDiv(varIcon + makeInDiv(currCellVal.toFixed(1), 'roc_col'), 'roc_col_left')
                }")
  cell_JS
}
