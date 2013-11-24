\name{find_region_point}
\alias{find_region_point}
\title{Find region for a point}
\usage{
  find_region_point(point, map)
}
\arguments{
  \item{point}{numeric vector of latitude and longitude}

  \item{map}{SpatialPolygon object}
}
\description{
  Find region for a point
}
\details{
  Checks each region in map to determine with it contains
  the specified point. Returns as list of the ids of each
  region containing the point.
}

