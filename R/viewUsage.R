#' Generate table showing usage infromation (in Kw)
#'
#' @description Power usage information for particular household
#'
#' @param data data containing 
#' 
#' @param type an indication if you want total or average usage
#' 
#' @param window an indication if you want weekly, monthly, or seasonal usage
#' 
#' @return  A data frame containing a time series of usage requested information
#'
#' @export
#'
#' @importFrom utils head
#' @importFrom ggplot2 ggplot aes coord_flip geom_bar scale_fill_manual scale_y_continuous
#' @importFrom scales percent_format 
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange desc group_by inner_join mutate n select slice summarize ungroup
#' @importFrom tibble tibble
#'
#' 
#' @examples 
#' viewUsage(householdX, "total", "seasonal")
#' @keywords dplot



viewUsage <- function(data, type, window)
{
  
}