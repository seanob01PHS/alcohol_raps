source(here("extract", "EASR_qtr", "generate_new_qtr_data.R"))
source(here("extract", "EASR_qtr", "accumulate_qtr_extracts.R"))
source(here("extract", "EASR_qtr", "find_missing_qtr_data.R"))

coordinate_qtr_extract <- function(qtr_start, qtr_end){
  
  #find missing files
  missing_qtrs_and_files <- find_missing_qtr_data(qtr_start, qtr_end)
  
  # generate the latest data before checking for missing other missing data.
  if(qtr_end %in% (missing_qtrs_and_files %>% map(1))){
    message(paste0("Generating data for qtr end: ", qtr_end))
    generate_new_qtr_data(qtr_end)
  }

  # update the missing files
  missing_qtrs_and_files <- find_missing_qtr_data(qtr_start, qtr_end)

  # if other files are absent
  if(length(missing_qtrs_and_files)>0){
    
    missing_bits_str <- 
      missing_qtrs_and_files %>% 
      map_chr(~paste0(.x[[1]], ":\n",
                      tail(.x, -1) %>% 
                        map(~paste0("\t", .x, "\n")) %>% 
                        reduce(paste0))) %>% 
            reduce(paste0)
                  
    
    message(paste0("\nError: certain necessary qtr files are not present in the folder:\n\t",
                   here("output", "EASR_qtr_data"), "\n--------------------------------\n",
                   missing_bits_str, "\n"
                   ))
    
    response <- ""
    while(!(response == "y" | response == "n")){
      response <- readline(prompt = "Please input `y/n` if you want to generate these files now. If n, program will quit -> ")
    }
    
    if(response == "n"){
      stop("Stopping. Necessary files not present.")
    }
    
    # generate the files
    dates_to_generate <- missing_qtrs_and_files %>% 
      map(1)
    
    for(qtr_end in dates_to_generate){
      message(paste0("Generating data for qtr end: ", qtr_end))
      generate_new_qtr_data(qtr_end)
    }
    
  }
  
  accumulate_qtr_extracts()
  
  # filtering accumulated data to desired dates
  accumulated_data_dir <- here("output", "EASR_accumulated_data", "qtr_alc_adm_GGC.csv")
  accumulated_data <- read_csv(accumulated_data_dir, show_col_types = FALSE)
  accumulated_data %>% 
    filter(date_end %within% interval(qtr_start, qtr_end)) %>% 
    write_csv(accumulated_data_dir)
  
}

# Checks that both dates are the last day of a quarter
is_quarter_end <- function(date){
  date == date %>% 
    ceiling_date(unit = "quarter") %>% 
    `-`(1)
}
