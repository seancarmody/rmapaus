#' Create a box spatial object
#' 
#' @param coords coordinates
#' @param sp optional spatial reference object from which to inherit projection
#' @param id id of the (single) rectangular polygon. Defaults to \code{"1"}
#' @param CRS projection (coordinate reference system) string
#' 
#' @export

mapBox <- function(coords, sp=NULL, id="1", CRS="+proj=longlat +ellps=GRS80 +no_defs") {
  if (inherits(coords, "Spatial")) coords <- bbox(coords)
  if (!is.null(sp)) CRS <- proj4string(sp)
  if (class(coords)=="matrix") coords <- unlist(t(coords))
  bb <- data.frame(x=c(coords[1], coords[1], coords[2], coords[2], coords[1]),
                   y=c(coords[3], coords[4], coords[4], coords[3], coords[3]))
  SpatialPolygons(list(Polygons(list(Polygon(bb)), id)), 
                  proj4string=CRS(CRS))
}

#' Intersection of two maps
#' 
#' @param x first map
#' @param y second map (or other spatial object)
#' @param crop a logical value determined whether regions of \code{x} overlapping the
#'  boundary of \code{y} should be cropped. Defaults to \code{FALSE}
#'  
#' @details
#' Returns a map (\code{SpatialDataFrame}) which is a subset of \code{x} constrained
#' to lie within \code{y}. Regions of \code{x} overlapping the boundary of \code{y}
#' are not included, unless \code{crop} is set to \code{TRUE} in which case those
#' regions are cropped to the portion contained within \code{y}. Map data is inherited
#' from \code{x}.
#' 
#' @export

mapIntersect <- function(x, y, crop = FALSE){
  sp_data <- x@data
  if (crop) {
    ix <- gIntersects(x, y, byid = TRUE)[1,]
    x <- x[ix, ]
    x <- gIntersection(x, y, byid = TRUE)
  } else {
    ix <- gWithin(x, y, byid=TRUE)[1,]
    x <- x[ix,]
  }
  if (any(ix)) SpatialPolygonsDataFrame(x, data = droplevels(sp_data[ix,]),
                                        match.ID = FALSE)
  else NULL
}

#' Crop a spatial object to  within a specified rectangular region
#' 
#' @param sp spatial object
#' @param coords coordinates of rectangular region
#' @param crop a logical region which determines whether regions in \code{sp}
#'   overlapping the rectangular region should be cropped. Defaults to true.
#'   
#' @export

mapCrop <- function(sp, coords, crop=TRUE){
  bb <- mapBox(coords, sp) 
  mapIntersect(sp, bb, crop=crop)
}
