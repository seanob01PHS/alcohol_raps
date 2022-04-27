find_missing_qtr_data <- function(qtr_start, qtr_end){
  
  if( (!(is_quarter_end(qtr_start))) | (!(is_quarter_end(qtr_start))) ){
    stop("Both the start and end dates must be the last date in a quarter.")
  }
  
  all_days_between <- seq(qtr_start, qtr_end, by="day")
  all_qtr_ends_between <- all_days_between %>% 
    ceiling_date(unit = "quarter") %>% 
    unique %>% 
    `-`(1)
  
  # directory where necessary files are
  files_dir <- here("output", "admissions_qtr_data")
  
  # vector of necessary names HB
  file_names_HB <- paste0(files_dir,
                       "/Qtr alcohol admissions - HB (",
                       all_qtr_ends_between,
                       ").csv")
  
  # vector of necessary names HB
  file_names_HSCP <- paste0(files_dir,
                          "/Qtr alcohol admissions - HSCP (",
                          all_qtr_ends_between,
                          ").csv")
  
  dates_with_file_names <- list("dates" = all_qtr_ends_between,
                                "files_HB" = file_names_HB,
                                "files_HSCP" = file_names_HSCP)
  
  # returns qtr ends with filenames that are not present
  dates_with_file_names %>%
    # zips together the parts in to one list of lists
    pmap(~list(..1, ..2, ..3)) %>%
    # discards items that have both files existsing
    discard(~(file.exists(.x[[2]]) & file.exists(.x[[3]]))) %>%
    # discards files within those remaining that exist
    map( ~(.x %>% discard(~(substr(.x, 1, 1)=="/" &&
                            file.exists(.x)
                            )
                         )
          )
       )

}
