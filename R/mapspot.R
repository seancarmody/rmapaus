mapspot <- function(x, map, fill, size, id = intersect(names(x), names(map)),
                    lab.size = 3, regions = "white", max_size = 6,
                    id.name = id, border="grey95", ticks=FALSE,
                    labels=FALSE, label.filter=TRUE, 
                    size.label = comma, ...) {
  # Check map data includes coordinates for points and labels
  stopifnot(all(c("long", "lat") %in% names(sd.map)))
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
  x <- join(map@data[, c(id, "long", "lat")], x, by=id)
  
  # Draw map
  plt <- ggplot(x) +
    geom_map(aes_string(map_id = id), map=map_df, colour=border, fill = regions) +
    expand_limits(x = map_df$long, y = map_df$lat) +
    geom_point(aes_string(x="long", y="lat", colour=fill, size=size), 
               data=x) +  
    scale_size_area(max_size=max_size, labels = size.label) 
  if(labels) {
    # Apply filter to labels
    ids <- intersect(map@data[, id], with(x, x[eval(parse(text=label.filter)), id]))
    map <- map[map@data[,id] %in% ids,]
    plt <- plt + geom_text(data=map@data, size = lab.size, 
                           aes_string(x="long", y="lat", label=id.name, fill=NULL),  
                           ...)
  }
  if (!ticks) plt <- plt + theme(axis.ticks=element_blank()) +
    scale_x_continuous(labels=NULL) + scale_y_continuous(labels=NULL)
  plt + labs(x="", y="") + coord_fixed()
}