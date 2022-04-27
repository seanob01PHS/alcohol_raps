### Previously "Alcohol admits qtr - GGC.R"
### GK 09 Aug 2021
### Alcohol-related hospital admissions   
### 
###
### Produces 1 output file - for Scotland, NHS GGC and the 6 HSCPs


accumulate_qtr_extracts <-  function(qtr_start, qtr_end){
  ################################################################
  ##### Set file names and folders
  ################################################################
  
  # output (data.in - where individual qtr reports are saved; data.out - output of this script)
  data.in = here("output", "admissions_qtr_data")
  
  # set Area names
  area_names = c("NHS Greater Glasgow and Clyde",
                 "East Dunbartonshire",
                 "East Renfrewshire",
                 "Glasgow City",
                 "Inverclyde",
                 "Renfrewshire",
                 "West Dunbartonshire")
  
  #############################################
  # Data Processing
  #############################################
  
  ### read in CSVs
  
  # get list of files that match pattern
  files <- list.files(path = data.in, pattern = "Qtr alcohol admissions", full.names = TRUE)
  
  # import data
  data <- do.call(rbind, lapply(files, read.csv, skip = 1, header = FALSE))
  
  # assign column names
  colnames(data) <- c('area', 'pop',	'number',	'EASR',	'crude_rate', 'data_period', 'date_end')
  
  # select only GGC
  data2 <- data %>%
    filter (area %in% area_names)
  
  # select only Scotland
  data3 <- data %>%
    filter (area == "Scotland")
  
  # remove duplicates due to both HB and HSCP files 
  data3b <- data3 [duplicated(data3), ]
  
  # add Scotland to HB / HSCP
  data2 <- rbind(data2, data3b) %>%
    arrange(area, date_end)
  
  # add lookup column - date in Excel format
  data2 <- data2 %>%
    mutate(lookup = paste0(area,as.numeric(as.Date(date_end) -as.Date(0, origin="1899-12-30", tz='UTC')))) %>%
    select(lookup, everything())
  
  # return output
  data2 %>% 
    filter(as_date(date_end) %within% interval(qtr_start, qtr_end))
}

