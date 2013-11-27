#' rmapaus.
#' 
#' @name rmapaus
#' @docType package
#' @import ggplot2 scales rgeos rgdal 
NULL

#' Statistical division map data
#'  
#' A dataset containing map data for Australian statistical divisions (SD). 
#' Data is sourced from the Australian Bureau of Statistics.
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
#' Data is sourced from the Australian Bureau of Statistics.
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

#' State and Territory map data
#'  
#' A dataset containing map data for Australian states and territories.
#' 
#' @docType data
#' @keywords datasets
#' @name STE.full
#' @usage data(ste)
#' @format A spatial polygon data frame with 206 regions
NULL

#' State and Territory mapdata (smoothed)
#'  
#' A dataset containing map data for Australian states and territories
#' The regions have been adapted from the full ABS specification by smoothing
#' the boundaries to reduce the size of the data set and speed up plotting.
#' 
#' @docType data
#' @keywords datasets
#' @name STE.smoothed
#' @usage data(ste)
#' @format A spatial polygon data frame with 206 regions
NULL

#' State and Territory mapdata (caricature)
#'  
#' A dataset containing map data for Australian states and territories
#' The regions have been adapted from the full ABS specification by smoothing
#' the boundaries to reduce the size of the data set and speed up plotting.
#' These regions have a higher level of smoothing than the "smoothed" data set.
#' 
#' @docType data
#' @keywords datasets
#' @name STE.caricature
#' @usage data(ste)
#' @format A spatial polygon data frame with 206 regions
NULL

#' Statistical local area map data
#'  
#' A dataset containing map data for Australian statistical local areas (SLA).
#' Data is sourced from the Australian Bureau of Statistics.
#' 
#' @docType data
#' @keywords datasets
#' @name SLA.full
#' @usage data(sla)
#' @format A spatial polygon data frame with 61 regions
NULL