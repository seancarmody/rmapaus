\name{mapBox}
\alias{mapBox}
\title{Create a box spatial object}
\usage{
mapBox(coords, sp = NULL, id = "1",
  CRS = "+proj=longlat +ellps=GRS80 +no_defs")
}
\arguments{
  \item{coords}{coordinates}

  \item{sp}{optional spatial reference object from which to
  inherit projection}

  \item{id}{id of the (single) rectangular polygon.
  Defaults to \code{"1"}}

  \item{CRS}{projection (coordinate reference system)
  string}
}
\description{
  Create a box spatial object
}

