#' Generate table showing usage infromation (in Kw)
#'
#' @description Power usage analytical statistics for particular household
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
#' @import base
#' @import stats
#'
#' 
#' @examples 
#' household_data <- ingestData(household_data)
#' viewUsage(household_data, "total", "seasonally")
#' @keywords utilities


viewUsage <- function(power_data, type = c("total", "average"), 
                      window = c("weekly", "monthly", "seasonally"))
{
  #DELETE: power_data = read.csv("intervals_7288510005.csv")
  
  ## THIS CODE CALCULATES THE LOAD ANALYTICS
  type = match.arg(type)
  window = match.arg(window)
  
  #Check if args are properly formatted
  if(type != "total " && type != "average")
  {
    stop("type must be:
         \"total\" or \"average\"")
  }
  if(window != "weekly " && window != "monthly" && window != "seasonally")
  {
    stop("window must be:
         \"weekly\", \"monthly\", \"seasonally\"")
  }
  

  if(window == "weekly")
  {
    if(type == "total")
    {
      aggregate(power_data$interval_kW, by = list(power_data$week), FUN = sum)
    }
    if(type == "average")
    {
      aggregate(power_data$interval_kW, by = list(power_data$week), FUN = mean)
    }
  }
  
  if(window == "monthly")
  {
    if(type == "total")
    {
      aggregate(power_data$interval_kW, by = list(power_data$month), FUN = sum)
    }
    if(type == "average")
    {
      aggregate(power_data$interval_kW, by = list(power_data$month), FUN = mean)
    }
  }
  
  if(window == "seasonally")
  {
    if(type == "total")
    {
      aggregate(power_data$interval_kW, by = list(power_data$season), FUN = sum)
    }
    if(type == "average")
    {
      aggregate(power_data$interval_kW, by = list(power_data$season), FUN = mean)
    }
  }
  
  
  
  
  
  
}