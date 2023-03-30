library(tidyverse)
library(tidyhydat)
library(zoo)
library(lubridate)
library(caTools)
library(dataRetrieval)

#These functions can be used to calculate the freeze-up dates, 
#ice break-up dates, and continuous ice coverage for water stations from tidyhydat
#dataframe must have col names: "day_of_year", "Value", "Symbol", "Date", "waterYear"


#######Day of Water Year#####


day_of_year_calc <- function(data){
  
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

#to do: 
#add if/else for water year vs cal year and create input for alternative col titles


#######Group 1: Total Continuous Ice Coverage#####


Group_1_ice_cover <- function(data) {
  #function returns a df for the length of ice coverage, per calendar year
  #column title must be in "waterYear", use addWaterYear()
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
#to do: 
#add if/else for water year vs cal year and alternative col titles



#######Group 2: Freeze and Thaw Dates and Flow#####


Group_2_freeze_thaw <- function(data) {
  #This function calculates the freeze and thaw dates and their flow values
  #based on longest consecutive run of "B" Symbols in the df
  #column title must be in "waterYear",  use addWaterYear()
  #col title must be in "day_of_year", use above function
  
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


#to do: 
#add if/else for water year vs cal year and alternative col titles for water year

