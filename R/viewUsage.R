#' Generate table showing usage infromation (in Kw)
#'
#' @description Power usage information for particular household
#'
#' @param data data containing power information
#' 
#' @param type an indication if you want total or average usage
#' 
#' @param window an indication if you want weekly, monthly, or seasonal usage
#' 
#' @return  A data frame containing a time series of usage requested information
#'
#' @export
#'
#' @import utils
#' @import magrittr
#' @import dplyr
#' @import stringr 
#' @import base
#' @import datetime
#'
#' 
#' @examples 
#' viewUsage(household_data, "total", "seasonal")
#' @keywords utilities


viewUsage <- function(power_data, type = c("total", "average"), 
                      window = c("weekly", "monthly", "seasonal"))
{
  #DELETE: power_data = read.csv("intervals_7288510005.csv")
  
  #Make sure data is properly formatted
  if(any(!(c("interval_start", "interval_end", "interval_kWh", "interval_kW") 
           %in% colnames(power_data))))
  {
    stop("Data must contain columns: 
         interval_start, interval_end, interval_kWh, interval_kW")
  }
  
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
}