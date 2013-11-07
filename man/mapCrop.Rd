\name{mapCrop}
\alias{mapCrop}
\title{Crop a spatial object to  within a specified rectangular region}
\usage{
  mapCrop(sp, coords, crop = TRUE)
}
\arguments{
  \item{sp}{spatial object}

  \item{coords}{coordinates of rectangular region}

  \item{crop}{a logical region which determines whether
  regions in \code{sp} overlapping the rectangular region
  should be cropped. Defaults to true.}
}
\description{
  Crop a spatial object to within a specified rectangular
  region
}

