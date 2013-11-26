#' rmapaus.
#' 
#' @name rmapaus
#' @docType package
#' @import ggplot2 scales rgeos rgdal RJSONIO
NULL

#' Statistical division map data
#'  
#' A dataset containing map data for Australian statistical divisions (SD). 
#' 
#' @docType data
#' @keywords datasets
#' @name SD.full
#' @usage data(sd)
#' @format A spatial polygon data frame with 61 regions
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
#' @usage data(sd)
#' @format A spatial polygon data frame with 61 regions
NULL

#' Statistical sub-division map data
#'  
#' A dataset containing map data for Australian statistical sub-divisions (SSD). 
#' 
#' @docType data
#' @keywords datasets
#' @name SSD.full
#' @usage data(ssd)
#' @format A spatial polygon data frame with 206 regions
NULL

#' Statistical sub-division map data (smoothed)
#'  
#' A dataset containing map data for Australian statistical sub-divisions (SSD). 
#' The regions have been adapted from the full ABS specification by smoothing
#' the boundaries to reduce the size of the data set and speed up plotting.
#' 
#' @docType data
#' @keywords datasets
#' @name SSD.smoothed
#' @usage data(ssd)
#' @format A spatial polygon data frame with 206 regions
NULL