#' Load Australian maps
#' 
#' @param map a string specifying map type
#' @param accuracy sting specifying the accuracy of the map to load. Default 
#'   is \code{'smoothed'}.
#' @param crop a logical value determining whether map should be cropped
#'   to mainland Australia and Tasmania (i.e. excluding Cocos Keeling Islands,
#'   Christmas Island and Lord Howe Island).
#' @param messages logical value indicating whether additional logging messages
#'   should be displayed as warnings.
#' 
#' @export

get_mapaus <- function(map="SD", accuracy=c("smoothed", "full", "caricature"), 
                       crop=FALSE, messages=TRUE){
  map <- toupper(map)
  accuracy <- match.arg(accuracy)
  if (accuracy=="caricature" & map != "STE") stop("Caricature accuracy only available for STE.")
  if (accuracy=="smoothed" & map == "SLA") stop("SLA only available in full accuracy.")
  if (messages & accuracy=="full") warning("Full accuracy maps can result in slow plotting.")
  if (crop) warning("Cropping not currently implemented")
  data(list=list(tolower(map)), envir = environment(), package="rmapaus")
  map <- get(paste(map, accuracy, sep="."), envir=environment(), inherits=FALSE)
  map
}

#' @rdname get_mapaus
#' @export
getMapAus <- function(map="SD", crop=TRUE){
  warning("getMapAus is now deprecated and will not be supported in future releases. Please use get_mapaus instead.")
  get_mapaus(type, crop=crop)
}
