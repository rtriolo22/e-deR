#' Ingests the data from your Power Data and puts in a more tractable format
#'
#' @description Power usage information for particular household
#'
#' @param data data containing power information
#' @return  A data frame containing a modified version of the power data
#'
#' @export
#' #https://stackoverflow.com/questions/23149873/when-creating-package-can-i-call-function-inside-package-while-writing-other-fu
#'
#' @import utils
#' @import magrittr
#' @import dplyr
#' @import stringr 
#' @import base
#' @import datetime
#' @import lubridate
#'
#' 
#' @examples 
#' household_data <- read.csv("POWER_USAGE_DATA")
#' ingestData(household_data)
#' @keywords utilities


ingestData <- function(power_data)
{

  #DELETE: power_data = read.csv("intervals_7288510005.csv")
  
  #Make sure data is properly formatted
  if(any(!(c("interval_start", "interval_end", "interval_kWh", "interval_kW") 
           %in% colnames(power_data))))
  {
    stop("Data must contain columns: 
         \"interval_start\", \"interval_end\", \"interval_kWh\", \"interval_kW\".
         (NOTE: Data may already have been properly ingested)")
  }
  
  ## THIS CODE INGESTS DATA AND MUTATES FRAME INTO MORE MANAGEABLE FORMAT
  
  #Subset to only necessary columns
  power_data <- power_data %>% subset(select = c("interval_start", "interval_end", 
                                                 "interval_kWh", "interval_kW"))
  
  #We will split the interval_start and interval_end columns into two columns: day // hour
  split_interval_start = str_split(power_data$interval_start, pattern = " ")
  day_start = sapply(1:nrow(power_data), function(x){split_interval_start[[x]][1]})
  time_start = sapply(1:nrow(power_data), function(x){split_interval_start[[x]][2]})
  
  split_interval_end = str_split(power_data$interval_end, pattern = " ")
  day_end = sapply(1:nrow(power_data), function(x){split_interval_end[[x]][1]})
  time_end = sapply(1:nrow(power_data), function(x){split_interval_end[[x]][2]})
  
  #Determine the interval length for each period
  interval_length_hours = difftime(as.POSIXct(power_data$interval_end, format = "%m/%d/%Y %H:%M"), 
                                   as.POSIXct(power_data$interval_start, format = "%m/%d/%Y %H:%M"), 
                                   units = "hours")
  
  #Create new columns that contain simplified information
  power_data <- power_data %>% mutate(day_start = day_start, time_start = time_start, 
                                      day_end = day_end, time_end = time_end,
                                      interval_length_hours = interval_length_hours) %>% 
    select(-interval_start, -interval_end)
  
  #Parse out the week/month/season
  power_data <- power_data %>% mutate(week = week(as.POSIXct(power_data$day_start, format = "%m/%d/%Y")))
  #fix slight bug which treats the day of 12/31 as a week 53 
  power_data[(month(as.POSIXct(power_data$day_start, format = "%m/%d/%Y")) == 12 & 
                day(as.POSIXct(power_data$day_start, format = "%m/%d/%Y")) == 31),]$week = 52 
  
  power_data <- power_data %>% mutate(month = month(as.POSIXct(power_data$day_start, format = "%m/%d/%Y")))
  
  #Helper function for calculation of the season
  getSeason <- function(input.date){
    numeric.date <- 100*month(input.date)+day(input.date)
    ## input Seasons upper limits
    cuts <- cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231)) 
    # rename the resulting groups
    levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
    return(cuts)
  }
  
  power_data <- power_data %>% mutate(season = getSeason(as.POSIXct(power_data$day_start, format = "%m/%d/%Y")))
  
  return(power_data)
}


