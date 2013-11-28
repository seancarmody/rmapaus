\name{get_mapaus}
\alias{getMapAus}
\alias{get_mapaus}
\title{Load Australian maps}
\usage{
get_mapaus(map = "SD", accuracy = c("smoothed", "full", "caricature"),
  crop = FALSE, messages = TRUE)

getMapAus(map = "SD", crop = TRUE)
}
\arguments{
  \item{map}{a string specifying map type}

  \item{accuracy}{sting specifying the accuracy of the map
  to load. Default is \code{'smoothed'}.}

  \item{crop}{a logical value determining whether map
  should be cropped to mainland Australia and Tasmania
  (i.e. excluding Cocos Keeling Islands, Christmas Island
  and Lord Howe Island).}

  \item{messages}{logical value indicating whether
  additional logging messages should be displayed as
  warnings.}
}
\description{
Load Australian maps
}

