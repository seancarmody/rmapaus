\name{mapspot}
\alias{mapspot}
\title{Draw map and add bubbles}
\usage{
mapspot(x, map, fill, size, id = intersect(names(x), names(map)),
  id.name = id, border = "grey95", ticks = FALSE, labels = FALSE,
  label.filter = TRUE, lab.size = 3, regions = "white", max_size = 6,
  size.label = comma, ...)
}
\arguments{
  \item{x}{data frame with fill values and bubble sizes}

  \item{map}{a map of type \code{SpatialPolygonsDataFrame}}

  \item{fill}{name of field in \code{x} containing fill
  values}

  \item{size}{name of field in \code{x} containing the size
  values (mapped to size aesthetic).}

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

  \item{lab.size}{size of region label text}

  \item{regions}{colour of map regions}

  \item{max_size}{maximum size of bubbles}

  \item{size.label}{label formatter defaults to
  \code{comma}}

  \item{...}{additional parameters passed to
  \code{geom_text}}
}
\description{
Use ggplot2 to draw a map and fill regions of the map based
on values in a dataframe.
}
\details{
Draws a map.
}

