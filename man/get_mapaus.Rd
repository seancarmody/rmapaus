\name{get_mapaus}
\alias{getMapAus}
\alias{get_mapaus}
\title{Load Australian maps}
\usage{
  get_mapaus(map = "SD",
    accuracy = c("smoothed", "full", "caricature"),
    crop = FALSE, messages = TRUE)

  getMapAus(type = "SD", crop = TRUE)
}
\arguments{
  \item{type}{a map type}

  \item{crop}{a logical value determining whether map
  should be cropped to mainland Australia and Tasmania
  (i.e. excluding Cocos Keeling Islands, Christmas Island
  and Lord Howe Island).}
}
\description{
  Load Australian maps
}

