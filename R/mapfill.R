#' Draw map and fill regions
#'
#' Use ggplot2 to draw a map and fill regions of the map based on
#' values in a dataframe.
#' 
#' @param x data frame with fill values
#' @param map a map of type \code{SpatialPolygonsDataFrame}
#' @param fill name of field in \code{x} containing fill values
#' @param id name of field in \code{x} containing map region id values. Defaults 
#'   to the first field name \code{x} and \code{map} have in common
#' @param id.name name of field in \code{mapdata} containing region names
#'   used for (optional) region labelling
#' @param border colour of region borders
#' @param ticks a logical value indicating whether tick marks should be added
#'   to axes. Defaults to \code{FALSE}.
#' @param labels a logical value indicating whether regions should be labelled.
#'    Defaults to \code{FALSE}
#' @param label.filter string to be evaluated in the context of `x` which determines
#'    whether a given region will be labelled.
#' @param ... additional parameters passed to \code{geom_text}
#'    
#' @details Draws a map with regions shaded based on values provided in a dataframe.
#' 
#' @export

mapfill <- function(x, map, fill, id = intersect(names(x), names(map)), 
                    id.name = id, border="grey95", ticks=FALSE, 
                    labels=FALSE, label.filter=TRUE, ...) {
  x <- as.data.frame(x)
  
  if (length(id) > 1) {
    id <- id[1]
    warning(paste0("More than one id field. Using ", id, "."))
  }
  
  # Restrict to common regions
  ids <- intersect(map@data[, id], x[, id])
  if (length(ids) < 2) stop("At least two distinct regions required for mapping.")
  map <- map[map@data[,id] %in% ids, ]
  x <- x[x[, id] %in% ids, ]
  map_df <- fortify(map, region=id)
  
  # Draw map
  plt <- ggplot(x, aes_string(fill = fill)) +
    geom_map(aes_string(map_id = id), map=map_df, colour=border) +
    expand_limits(x = map_df$long, y = map_df$lat)
 if(labels) {
   # Check map data includes coordinates for labels
   if (!all(c("long", "lat") %in% names(map@data))){
     warning("Label coordinates 'long' and 'lat' not provided.")
   } else {
    # Apply filter to labels
    ids <- x[eval(substitute(label.filter), x, parent.frame()), id]
    map <- map[map@data[ ,id] %in% ids,]
    plt <- plt + geom_text(data=map@data,
                             aes_string(x="long", y="lat",
                             label=id.name, fill=NULL),  
                             ...)
   }
 }
if (!ticks) plt <- plt + theme(axis.ticks=element_blank()) +
    scale_x_continuous(labels=NULL) + scale_y_continuous(labels=NULL)

 plt + labs(x="", y="") + coord_fixed()
}
