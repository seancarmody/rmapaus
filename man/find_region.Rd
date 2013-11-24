\name{find_region}
\alias{find_region}
\title{Find region for a point}
\usage{
  find_region(location, map)
}
\arguments{
  \item{location}{vector of string locations}

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

