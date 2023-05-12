library(tidyverse)
library(tidyhydat)
library(zoo)
library(lubridate)
library(caTools)
library(dataRetrieval)

# Various functions for calculating: IHA, Percent Change, and Ice Variables 
# Created by Jacqui Levy


#######PART 1 - MISC FUNCTIONS#####

# MISSING YEARS FUNCTION

  calc_missing_yrs <- function(df, Date) {
  #Find out if there are missing years in the data set. Will return "false" if there are no missing years, or a df of missing years. 
  #Date should be in yyyy-mm-dd
  #cal year not wy 
  years <- format(df$Date, "%Y")
  unique_years <- unique(years)
  all_years <- seq(min(unique_years), max(unique_years), by = 1)
  missing_years <- setdiff(all_years, unique_years)
  
  View(missing_years) #should be zero
  any(missing_years) #says if there is anything in the list
  print(missing_years)
  }
  
  
#RLE FUNCTION
  
  calc_rle <- function(df) {
    #function to return a df with rows with > 14 consec NA values 
    na_rows <- with(rle(is.na({{df}}$Value)), rep(values & lengths > 14, lengths))
    rtn <- {{df}}[na_rows,]
    return(rtn)
  }
  
#DAY OF THE WATER YEAR FUNCTION
  #Date should be in yyyy-mm-dd

  calc_day_of_wyear <- function(data){
    #function sequences by number of days in each water year
    
    df <- data.frame(waterYear = character(), sequence = character())
    
    for (i in unique({{data}}$waterYear)){
      df_subset <- {{data}}[{{data}}$waterYear == i,]
      days <- seq(1:nrow(df_subset))
      temp_df <- data.frame(waterYear = i, day_of_year = days)
      df <- rbind(df, temp_df)
    }
    df2 <- cbind(df, {{data}}) 
    df3 <- df2[, !duplicated(colnames(df2))]
    
    return(df3)
  }
  

#######PART 2 - IHA VARIABLES#####
  

#IHA FUNCTION
#Date must be in dd-mm-yyyy format - use mutate(Date = format(Date,"%d-%m-%Y"))

  
  calc_IHA <- function(data){
    
    flow_data <-  {{data}} %>%
      select(Date, Value)
    
    flow_data <- zoo(flow_data$Value, order.by = as.Date(as.character(flow_data$Date), format = "%d-%m-%Y"))
    ## Run IHA analyses
    group1_output <- group1(flow_data, year = "water", FUN = median)
    group2_output <- group2(flow_data, year = "water", mimic.tnc = TRUE)
    group3_output <- group3(flow_data, year = "water", mimic.tnc = FALSE)
    group4_output <- group4(flow_data, year = "water")
    group5_output <- group5(flow_data, year = "water")
    
    ## Convert outputs
    group1_output <- as.data.frame(group1_output)
    group2_output <- group2_output[,-1]
    group3_output <- as.data.frame(group3_output)
    group4_output <- as.data.frame(group4_output)
    group5_output <- as.data.frame(group5_output)
    
    #to deal with one less row for group4 variables 
    if (nrow(group4_output) < nrow(group1_output)) {
      group4_output <- group4_output %>%
        add_row()
    }
    ## Create output dataframe 
    IHA_output <- bind_cols(list(group1_output, group2_output, group3_output, group4_output, group5_output))
    
    #make the years the column names instead of the rownames
    IHA_output <- tibble::rownames_to_column(IHA_output, "Year") 
    
  }

  
#######PART 3 - ICE VARIABLES#####
  
#Used to calculate the freeze-up dates, ice break-up dates, and continuous ice coverage
#df must have col names: "day_of_year", "Value", "Symbol", "Date", "waterYear"
#Date must be in yyyy-mm-dd format for all ice variables functions
  
#GROUP 1 FUNCTION - ICE COVER 
  
  Group_1_ice_cover <- function(data) {
    #function returns a df for the length of ice coverage, per water year
    #works with one station, multiple years
    #for multiple stations, split-apply-combine station
    lst <- list()
    
    for (i in unique({{data}}$waterYear)) {
      
      length_B_date  <- max(rle({{data}}$Symbol[{{data}}$waterYear == i] == "B")[[1]]) 
      #append each value to a list
      length_B_date <- length_B_date - 1
      lst[[i]] <- length_B_date
    }
    
    Ice_coverage_wy <- data.frame(waterYear = names(lst), Ice_coverage_wy = unlist(lst))
    rownames(Ice_coverage_wy) <-NULL
    return(Ice_coverage_wy)
    
  }
  

#GROUP 2 FUNCTION - FREEZE AND THAW DATES
  
  Group_2_freeze_thaw <- function(data) {
    #This function calculates the freeze and thaw dates and their flow values, using the longest consecutive run of "B" Symbols in the df
    
    start_date_lst <- list()
    end_date_lst <- list()
    start_flow_lst <- list()
    end_flow_lst <- list()
    start_doy_lst <- list()
    end_doy_lst <- list()
    
    for (i in unique({{data}}$waterYear)){
      
      df_subset <- {{data}}[{{data}}$waterYear == i,]
      
      #calc rle for B symbol
      rle_m = rle(df_subset$Symbol == "B")
      
      #find index for max run of B symbols
      max_run_index <- which.max(rle_m$lengths)
      
      #find end start and index of max run of B symbols
      end <- cumsum(rle_m$lengths)[max_run_index]
      start <- end - rle_m$lengths[max_run_index] +1
      
      #find date at end, start index of the max run 
      date_end = df_subset$Date[end]
      date_start = df_subset$Date[start]
      
      #find flow at end, start index
      flow_end = df_subset$Value[end]
      flow_start = df_subset$Value[start]
      
      #find doy at end, start index
      doy_end = df_subset$day_of_year[end]
      doy_start = df_subset$day_of_year[start]
      
      #append dates to the list
      start_date_lst[[i]] <- date_start
      end_date_lst[[i]] <- date_end
      
      #append flows to the list
      start_flow_lst[[i]] <- flow_start
      end_flow_lst[[i]] <- flow_end
      
      #append doy to the list
      start_doy_lst[[i]] <- doy_start
      end_doy_lst[[i]] <- doy_end
      
    }
    
    Freeze_Date <- as.Date(unlist(start_date_lst))
    Thaw_Date <- as.Date(unlist(end_date_lst))
    Flow_Freeze <- unlist(start_flow_lst)
    Flow_Thaw <- unlist(end_flow_lst)
    Freeze_DOY <-unlist(start_doy_lst)
    Thaw_DOY <- unlist(end_doy_lst)
    
    df <- cbind.data.frame(Freeze_Date,Freeze_DOY,Flow_Freeze,Thaw_Date,Thaw_DOY, Flow_Thaw)
    
    Ice_coverage_dates_flow <- rownames_to_column(df, "waterYear")
    return(Ice_coverage_dates_flow)
  }
  
  
#GROUP 3 FUNCTION - ONSET OF FRESHET
  
  
  Group_3_freshet <- function(data) {
    index <- 0
    f_index <- 16
    date_lst <- list()
    flow_lst <- list()
    stn_nu <- list()
    doy_lst <- list()
    
    for (i in unique({{data}}$waterYear)) { #first loop
      #subset data by year, resetting at each year
      index = 0  
      f_index = 16 
      df_subset <- {{data}}[{{data}}$waterYear == i,]
      df_subset <- df_subset %>%
        #data tidying:delete dates before Feb 12, so rolling mean calc starts on March 1
        mutate(Date = as.Date(Date)) %>%
        #filter(month(Date) %in% c(3,4,5,6))
        mutate(new_col = format(Date,"%m-%d")) %>%
        filter(month(Date) >= 2 & month(Date) < 7) %>% 
        filter(!(new_col %in% c("02-01", "02-02", "02-03", "02-04", "02-05", "02-06", "02-07", "02-08", "02-09", "02-10", "02-11")))
      
      #calc rolling 16 day mean
      rollmn <- rollmean(df_subset$Value, k = 16, width = 16)
      #rollmn <- as.data.frame(rollmn)
      
      for (j in rollmn) { #second loop
        #increment index
        index = index + 1
        f_index = f_index + 1
        
        #find rolling mean value at the index and multiply by 1.5
        rollmnvalue <- rollmn[index] #roll mean = 0.81
        rollmnvalue1.5 <- rollmnvalue*1.5 #1.215
        
        #get the flow value at that index
        flowvalue <- df_subset$Value[f_index] #flow = 0.654
        
        if (flowvalue > rollmnvalue1.5 & f_index < 123 ) { #third loop
          #append date, flow value to a list using the index numbers
          dt <- df_subset$Date[f_index]
          fl <- df_subset$Value[f_index]
          st <- df_subset$STATION_NUMBER[f_index]
          doy <- df_subset$day_of_year[f_index]
          # print(dt) 
          #  print(fl)
          #  print("end") 
          date_lst[[i]] <- dt 
          flow_lst[[i]] <- fl 
          stn_nu[[i]] <- st
          doy_lst[[i]] <- doy
          Freshet_Date <- as.Date(unlist(date_lst))
          Freshet_Flow <- unlist(flow_lst)
          Station_Number <- unlist(stn_nu)
          Day_of_year <- unlist(doy_lst)
          df <- cbind.data.frame(Station_Number, Freshet_Date, Day_of_year, Freshet_Flow)
          break
        }
        
      }
      
    }
      Freshet_dates_flow <- rownames_to_column(df, "waterYear")
      return(Freshet_dates_flow)
  }  
  
  
  
#######PART 4 - MISC FUNCTIONS NO LONGER USING BUT STILL USEFUL#####
  
 # Find missing years
  

  #Find out if there are missing years in the data set. Will return "false" if there are no missing years, or a df of missing years. Date should be in yyyy-mm-dd
  
  yrs_missing <- function(df, Date) {
    years <- format(df$Date, "%Y")
    unique_years <- unique(years)
    all_years <- seq(min(unique_years), max(unique_years), by = 1)
    missing_years <- setdiff(all_years, unique_years)
    
    View(missing_years) #should be zero
    any(missing_years) #says if there is anything in the list
    print(missing_years)
  }

