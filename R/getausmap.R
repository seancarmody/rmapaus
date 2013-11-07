#' Load Australian maps
#' 
#' @param map type
#' @param crop a logical value determining whether map should be cropped
#'   to mainland Australia and Tasmania (i.e. excluding Cocos Keeling Islands,
#'   Christmas Island and Lord Howe Island).
#' 
#' @export

getMapAus <- function(type="SD", crop=TRUE){
  data(aussd, envir = environment(), package="rmapaus")
  map <- get(paste("aus", type, sep="."), envir=environment(), inherits=FALSE)
  if (crop) mapCrop(map, bb.australia) else map
}