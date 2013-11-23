\name{mapfill}
\alias{mapfill}
\title{Draw map and fill regions}
\usage{
  mapfill(x, map, fill,
    id = intersect(names(x), names(map)), id.name = id,
    border = "grey95", ticks = FALSE, labels = FALSE,
    label.filter = TRUE, ...)
}
\arguments{
  \item{x}{data frame with fill values}

  \item{map}{a map of type \code{SpatialPolygonsDataFrame}}

  \item{fill}{name of field in \code{x} containing fill
  values}

  \item{id}{name of field in \code{x} containing map region
  id values. Defaults to the first field name \code{x} and
  \code{map} have in common}

  \item{id.name}{name of field in \code{mapdata} containing
  region names used for (optional) region labelling}

  \item{border}{colour of region borders}

  \item{ticks}{a logical value indicating whether tick
  marks should be added to axes. Defaults to \code{FALSE}.}

  \item{labels}{a logical value indicating whether regions
  should be labelled.  Defaults to \code{FALSE}}

  \item{label.filter}{string to be evaluated in the context
  of `x` which determines whether a given region will be
  labelled.}

  \item{...}{additional parameters passed to
  \code{geom_text}}
}
\description{
  Use ggplot2 to draw a map and fill regions of the map
  based on values in a dataframe.
}
\details{
  Draws a map.
}

