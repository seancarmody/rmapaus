#' rmapaus.
#' 
#' @name rmapaus
#' @docType package
#' @import ggplot2 scales rgeos rgdal
NULL

#' Statistical division map data
#'  
#' A dataset containing map data for Australian statistical divisions (SD). 
#' 
#' @docType data
#' @keywords datasets
#' @name SD.full
#' @usage data(SD.full)
#' @format A data frame with 53940 rows and 10 variables
NULL

#' Statistical division map data (smoothed)
#'  
#' A dataset containing map data for Australian statistical divisions (SD). 
#' The regions have been adapted from the full ABS specification by smoothing
#' the boundaries to reduce the size of the data set and speed up plotting.
#' 
#' @docType data
#' @keywords datasets
#' @name SD.smoothed
#' @usage data(SD.smoothed)
#' @format A data frame with 53940 rows and 10 variables
NULL