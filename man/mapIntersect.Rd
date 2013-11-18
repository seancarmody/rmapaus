\name{mapIntersect}
\alias{mapIntersect}
\title{Intersection of two maps}
\usage{
mapIntersect(x, y, crop = FALSE)
}
\arguments{
  \item{x}{first map}

  \item{y}{second map (or other spatial object)}

  \item{crop}{a logical value determined whether regions of
  \code{x} overlapping the boundary of \code{y} should be
  cropped. Defaults to \code{FALSE}}
}
\description{
  Intersection of two maps
}
\details{
  Returns a map (\code{SpatialDataFrame}) which is a subset
  of \code{x} constrained to lie within \code{y}. Regions
  of \code{x} overlapping the boundary of \code{y} are not
  included, unless \code{crop} is set to \code{TRUE} in
  which case those regions are cropped to the portion
  contained within \code{y}. Map data is inherited from
  \code{x}.
}

