#' Geocode using the Google Maps
#'
#' Interrogate the Google Maps API to geocode a specified location.
#' Geocoding provides longitude, latitude and some additional data.
#' including full address where available.
#' 
#' @param location vector of location strings.
#' @param output string specifying desired output.
#' @param region two letter country code to determine search bias.
#' 
#' @details 
#' The Google API attempts a fuzzy match of the specified location(s).
#' Where the location is ambiguous, a single result will be returned
#' and the specified search bias region will used to assist in making
#' a selection. 
#' 
#' This function uses RCurl and RJSONIO to download data from Google's API:
#' Latitude, longitude, location type (see explanation at the end), formatted address
#' Notice there is a limit of 2,500 calls per day. Currently, the function does
#' not enforce this limit, so exercise caution!
#'
#' Location type, for more info check here: https://developers.google.com/maps/documentation/directions/
#' "ROOFTOP" indicates that the returned result is a precise geocode for which we have location information accurate down to street address precision.
#' "RANGE_INTERPOLATED" indicates that the returned result reflects an approximation (usually on a road) interpolated between two precise points (such as intersections). Interpolated results are generally returned when rooftop geocodes are unavailable for a street address.
#' "GEOMETRIC_CENTER" indicates that the returned result is the geometric center of a result such as a polyline (for example, a street) or polygon (region).
#' "APPROXIMATE" indicates that the returned result is approximate.
#' 
#' @source
#' The code is adapted from 
#' \href{http://www.jose-gonzalez.org/using-google-maps-api-r/}{Jose Gonzales}.
#' This function is very similar to the \code{geocode} function in the \code{ggmap}
#' package.
#' 
#' @export

geo_code <- function(location, output=c("list", "longlat", "detail", "json"), region="AU") {
  stopifnot(is.character(location))
  output <- match.arg(output)
  if (length(location) > 1) return(lapply(location, geo_code, output=output))
  root <- "http://maps.google.com/maps/api/geocode/json?address="
  u <- URLencode(paste0(root, location, "&sensor=false", "&region=", region))
  conn <- url(u)
  doc <- paste(readLines(conn), collapse="\n")
  close(conn)
  result <- fromJSON(doc, simplify = FALSE)
  stopifnot(result$status=="OK") 
  result <- result$results[[1]]
  if (output=="json") return(doc)
  if (output=="detail") return(result)
  ret <- list()
  ret$long <- result$geometry$location$lng
  ret$lat <- result$geometry$location$lat
  if (output=="longlat") return(c(ret$long, ret$lat))
  ret$location_type <- result$geometry$location_type
  ret$formatted_address <- result$formatted_address
  # In Australia, the State is recorded as Area Level 1
  if (result$address_components[[4]]$types[[1]]=="administrative_area_level_1")
    ret$state <- result$address_components[[4]]$short_name
  if (result$address_components[[6]]$types[[1]] == "postal_code") 
    ret$postcode <- result$address_components[[6]]$short_name
  ret$ISO2 <- result$address_components[[5]]$short_name
  ret$country <- result$address_components[[5]]$long_name
  return(ret)
}

#' Find region for a point
#' 
#' @param point numeric vector of latitude and longitude
#' @param map SpatialPolygon object
#' 
#' @details
#' Checks each region in map to determine with it contains the
#' specified point. Returns as list of the ids of each region
#' containing the point.
#' 
#' @export

find_region_point <- function(point, map){
  stopifnot(length(point)==2)
  pt <- readWKT(paste("POINT (", point[1], point[2], ")"))
  proj4string(pt) <- proj4string(map) 
  rownames(map@data)[which(gContains(map, pt, byid=TRUE))]
}

#' Find region for a point
#' 
#' @param location vector of string locations
#' @param map SpatialPolygon object
#' 
#' @details
#' Checks each region in map to determine with it contains the
#' specified point. Returns as list of the ids of each region
#' containing the point.
#' 
#' @export
find_region <- function(location, map){
  point <- geo_code(location, output="longlat")
  if (length(location) == 1) point <- list(point)
  sapply(point, find_region_point, map=map)
}
                    
# Code below is from ggmap for comparison

altgeo <- function (location, output = c("latlon", "latlona", "more", "all"), 
                    messaging = FALSE, sensor = FALSE, override_limit = FALSE) 
{
  stopifnot(is.character(location))
  output <- match.arg(output)
  stopifnot(is.logical(messaging))
  if (length(location) > 1) {
    s <- "google restricts requests to 2500 requests a day."
    if (length(location) > 2500) 
      stop(s, call. = F)
    if (length(location) > 200 && messaging) 
      message(paste("Reminder", s, sep = " : "))
    if (output == "latlon" || output == "latlona" || output == 
          "more") {
      return(ldply(as.list(location), geocode, output = output, 
                   messaging = messaging))
    }
    else {
      return(llply(as.list(location), geocode, output = output, 
                   messaging = messaging))
    }
  }
  sensor4url <- paste("sensor=", tolower(as.character(sensor)), 
                      sep = "")
  loc <- location
  location <- gsub(" ", "+", location)
  posturl <- paste(location, sensor4url, sep = "&")
  url_string <- paste("http://maps.googleapis.com/maps/api/geocode/json?address=", 
                      posturl, sep = "")
  url_string <- URLencode(url_string)
  if (messaging) 
    message(paste("contacting ", url_string, "...", sep = ""), 
            appendLF = F)
  check_geocode_query_limit(url_string, elems = 1, override = override_limit, 
                            messaging = messaging)
  connect <- url(url_string)
  gc <- fromJSON(paste(readLines(connect), collapse = ""))
  if (messaging) 
    message(" done.")
  close(connect)
  if (output == "all") 
    return(gc)
  message(paste0("Information from URL : ", url_string))
  message("Google Maps API Terms of Service : http://developers.google.com/maps/terms")
  if (gc$status != "OK") {
    warning(paste("geocode failed with status ", gc$status, 
                  ", location = \"", location, "\"", sep = ""), call. = FALSE)
    return(data.frame(lon = NA, lat = NA))
  }
  if (length(gc$results) > 1 && messaging) {
    message(paste("more than one location found for \"", 
                  loc, "\", using address\n  \"", tolower(gc$results[[1]]$formatted_address), 
                  "\"\n", sep = ""))
  }
  NULLtoNA <- function(x) {
    if (is.null(x)) 
      return(NA)
    x
  }
  gcdf <- with(gc$results[[1]], {
    data.frame(lon = NULLtoNA(geometry$location$lng), lat = NULLtoNA(geometry$location$lat), 
               type = tolower(NULLtoNA(types[1])), loctype = tolower(NULLtoNA(geometry$location_type)), 
               address = tolower(NULLtoNA(formatted_address)), north = NULLtoNA(geometry$viewport$northeast$lat), 
               south = NULLtoNA(geometry$viewport$southwest$lat), 
               east = NULLtoNA(geometry$viewport$northeast$lng), 
               west = NULLtoNA(geometry$viewport$southwest$lng))
  })
  if (output == "latlon") 
    return(gcdf[, c("lon", "lat")])
  if (output == "latlona") 
    return(gcdf[, c("lon", "lat", "address")])
  attrdf <- ldply(gc$results[[1]]$address_components, function(l) {
    as.data.frame(l, stringsAsFactors = FALSE)[1, ]
  })
  attrdf <- attrdf[, c("types", "long_name")]
  gcdf <- within(gcdf, {
    point_of_interest <- tolower(NULLtoNA(attrdf$long_name[attrdf$types == 
                                                             "point_of_interest"]))
    streetNo <- as.numeric(NULLtoNA(attrdf$long_name[attrdf$types == 
                                                       "street_number"]))
    street <- tolower(NULLtoNA(attrdf$long_name[attrdf$types == 
                                                  "route"]))
    locality <- tolower(NULLtoNA(attrdf$long_name[attrdf$types == 
                                                    "locality"]))
    administrative_area_level_1 <- tolower(NULLtoNA(attrdf$long_name[attrdf$types == 
                                                                       "administrative_area_level_1"]))
    administrative_area_level_2 <- tolower(NULLtoNA(attrdf$long_name[attrdf$types == 
                                                                       "administrative_area_level_2"]))
    country <- tolower(NULLtoNA(attrdf$long_name[attrdf$types == 
                                                   "country"]))
    postal_code <- tolower(NULLtoNA(attrdf$long_name[attrdf$types == 
                                                       "postal_code"]))
  })
  gcdf$query <- loc
  return(gcdf)
}
                     
check_geocode_query_limit <- function (url_string, elems, override, messaging)  {
  .GoogleGeocodeQueryCount <- NULL
  rm(.GoogleGeocodeQueryCount)
  if (exists(".GoogleGeocodeQueryCount", .GlobalEnv)) {
    .GoogleGeocodeQueryCount <<- subset(.GoogleGeocodeQueryCount, 
                                         time >= Sys.time() - 24 * 60 * 60)
    if (sum(.GoogleGeocodeQueryCount$elements) + elems >  2500) { 
      message("query max exceeded, see ?geocode.  current total = ",
              sum(.GoogleGeocodeQueryCount$elements)) 
      if (!override) stop("google query limit exceeded.", call. = FALSE) 
    } 
    if (with(.GoogleGeocodeQueryCount, sum(elements[time >= Sys.time() - 10]) + elems > 10)) {
      message(".", appendLF = F) 
      Sys.sleep(1) 
    } 
    .GoogleGeocodeQueryCount <<- rbind(.GoogleGeocodeQueryCount,
                                       data.frame(time = Sys.time(),
                                                  url = url_string,
                                                  elements = elems, 
                                                  stringsAsFactors = FALSE)) 
  }
  else {
    .GoogleGeocodeQueryCount <<- data.frame(time = Sys.time(), 
                                            url = url_string,
                                            elements = elems,
                                            stringsAsFactors = FALSE) 
  } 
}