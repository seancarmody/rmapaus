#' Load Australian maps
#' 
#' @param type a map type
#' @param crop a logical value determining whether map should be cropped
#'   to mainland Australia and Tasmania (i.e. excluding Cocos Keeling Islands,
#'   Christmas Island and Lord Howe Island).
#' 
#' @export

getMapAus <- function(type="SD", crop=TRUE){
  data(list=list(tolower(paste0("aus", type))), envir = environment(), package="rmapaus")
  map <- get(paste0("aus.", type), envir=environment(), inherits=FALSE)
#   if (crop) {
#     map <- mapCrop(map, bb.australia)
#   }
  map
}
