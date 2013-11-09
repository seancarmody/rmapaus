# https://stat.ethz.ch/pipermail/r-sig-geo/2012-July/015693.html

# Note: this function no longer words for +proj=longlat
# To get around this, use spTransform to project to a different
# projection, such as +proj=laea (Lambert Azimuthal Equal Area)

calc.labpt <- function(pols, colnames=c("long", "lat")) {
  require(rgeos)
  require(rgdal)
  
  # Force planar coordinates
  suppressWarnings(proj4string(pols) <- CRS("+proj=laea"))
  
  # Prepopulate the label point matrix with the centroid values
  coords<- coordinates(pols)
  
  # For each polygon in pols, calculate the appropriate label point
  for(i in seq_len(length(pols))) {
    
    # First fetch the polygon to process
    p <- pols[i,]
    
    init <- 0                     # Initial amount to shrink
    estep <- sqrt(gArea(p)/pi)/10 # Additional amount to shrink for each step
    
    # Try repeatedly shrinking the polygon until we’re left
    # with a polygon whose convex hulls fits inside 
    repeat {
      repeat {
        r = init + estep               # Amount to shrink
        p2 = gBuffer(p, width = -r)    # Shrink the polygon
        if( length(p2)==0 || gArea(p2) <= 0 )           # If the shrunken polygon is empty ...
          estep = estep/2 else break   # ... try again with a smaller value
      }
      
      # If we’re left with more than one polygon, choose the largest one
      areas=sapply(p2@polygons[[1]]@Polygons, function(x) x@area)
      if(length(areas) > 1) {
        # Note that we create a *new* SpatialPolygon containing the largest polygon.
        # I guess in theory we *could* have just replaced the @Polygons slot of p2,
        # but then gArea seems to crash R ... :(
        ind.max = which.max(areas)
        p2 = SpatialPolygons(list(Polygons(list(p2@polygons[[1]]@Polygons[ind.max][[1]]),
                                           ID="middle")), proj4string=CRS(proj4string(p2)))
      }
      
      # Calculate the convex hull of the inner polygon.
      # If this is wholly contained in the original polygon,
      # break out of the loop and set the label point to
      # the centroid of the inner polygon.
      if( gContains(p, gConvexHull(p2)) ) break else init=init+estep
    }
    coords[i,] <- coordinates(p2)
  }
  if (!is.null(colnames)) colnames(coords) <- colnames
  coords
}