# Generic map plotting functions

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
  map <- map[map@data[,id] %in% ids, ]
  x <- x[x[, id] %in% ids, ]
  map_df <- fortify(map, region=id)
  
  # Draw map
  plt <- ggplot(x, aes_string(fill = fill)) +
    geom_map(aes_string(map_id = id), map=map_df, colour=border) +
    expand_limits(x = map_df$long, y = map_df$lat)
 if(labels) {
   # Check map data includes coordinates for labels
   if (!all(c("long", "lat") %in% names(sd.map))){
     warning("Label coordinates 'long' and 'lat' not provided.")
   } else {
    # Apply filter to labels
    ids <- intersect(map@data[, id], with(x, x[eval(parse(text=label.filter)), id]))
    map <- map[map@data[ ,id] %in% ids,]
    plt <- plt + geom_text(data=map@data,
                             aes_string(x="long", y="lat", label=id.name, fill=NULL),  
                             ...)
   }
 }
if (!ticks) plt <- plt + theme(axis.ticks=element_blank()) +
    scale_x_continuous(labels=NULL) + scale_y_continuous(labels=NULL)

 plt + labs(x="", y="") + coord_fixed()
}
