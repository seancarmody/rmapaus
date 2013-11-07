#' Load Australian maps
#' 
#' @param map type
#' @param crop a logical value determining whether map should be cropped
#'   to mainland Australia and Tasmania (i.e. excluding Cocos Keeling Islands,
#'   Christmas Island and Lord Howe Island).
#' 
#' @export

getAusMap <- function(type="SD", crop=TRUE){
  load("data/aus-sd.Rdata")
  if (crop) mapCrop(aus.SD, bb.australia) else aus.SD
}