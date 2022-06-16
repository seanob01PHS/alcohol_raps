source(here("extract", "admissions_qtr", "generate_new_qtr_data.R"))
source(here("extract", "admissions_qtr", "accumulate_qtr_extracts.R"))
source(here("extract", "admissions_qtr", "find_missing_qtr_data.R"))

coordinate_qtr_extract <- function(qtr_start,
                                   qtr_end,
                                   hscp_pop_lookup_path,
                                   hb_pop_lookup_path){
  
  #find missing files
  missing_qtrs_and_files <- find_missing_qtr_data(qtr_start, qtr_end)
  
  # generate the latest data before checking for missing other missing data.
  if(qtr_end %in% (missing_qtrs_and_files %>% map(1))){
    
    # Connect to SMRA tables using odbc connection
    # The suppressWarnings function prevents your password from
    # appearing in the console if the connection is unsuccessful
    channel <- suppressWarnings(
      dbConnect(odbc(),
                dsn = "SMRA",
                uid = Sys.info()[['user']],
                pwd = .rs.askForPassword("What is your LDAP password?"))
    )
    
    generate_new_qtr_data(qtr_end,
                          channel,
                          hscp_pop_lookup_path,
                          hb_pop_lookup_path)
  }

  # update the missing files
  missing_qtrs_and_files <- find_missing_qtr_data(qtr_start, qtr_end)

  # if other files are absent
  if(length(missing_qtrs_and_files)>0){
    
    missing_bits_str <- 
      missing_qtrs_and_files %>% 
      map_chr(~paste0(.x[[1]], ":\n",
                      tail(.x, -1) %>% 
                        map(~paste0("\t", basename(.x), "\n")) %>% 
                        reduce(paste0))) %>% 
            reduce(paste0)
                  

    message(paste0("\nError: certain necessary qtr files are not present in the folder:\n\t",
                   here("output", "admissions_qtr_data"), "\n--------------------------------\n",
                   missing_bits_str, "\n"
                   ))
    
    response <- ""
    while(!(response == "y" | response == "n")){
      response <- readline(prompt = "Please input `y/n` if you want to generate these files now. If n, program will quit -> ")
    }
    
    if(response == "n"){
      stop("Stopping. Necessary files not present.")
    }
    
    if (!exists("channel")){
      channel <- suppressWarnings(
        dbConnect(odbc(),
                  dsn = "SMRA",
                  uid = Sys.info()[['user']],
                  pwd = .rs.askForPassword("What is your LDAP password?"))
      )
    }
    
    # generate the files
    dates_to_generate <- missing_qtrs_and_files %>% 
      map(1)
    
    for(qtr_end in dates_to_generate){
      message(paste0("Generating data for qtr end: ", qtr_end))
      generate_new_qtr_data(qtr_end,
                            channel,
                            hscp_pop_lookup_path,
                            hb_pop_lookup_path)
    }
    
  }
  
  if (exists("channel")){
    dbDisconnect(channel)
  }
  
  # all the files dealt with above put in to one file filtered for
  # the relevant dates
  accumulated_data <- accumulate_qtr_extracts(qtr_start, qtr_end)
  
  # filtering accumulated data to desired dates
  accumulated_data_out <- here("output", "admissions_qtr_data", "accumulated", paste0("qtr_alc_adm_GGC_", qtr_start, "_to_", qtr_end, ".csv"))
  # write output
  accumulated_data %>% write_csv(accumulated_data_out)
}

# Checks that both dates are the last day of a quarter
is_quarter_end <- function(date){
  date == date %>% 
    ceiling_date(unit = "quarter") %>% 
    `-`(1)
}
