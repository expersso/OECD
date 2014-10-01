\name{oecd-package}
\alias{oecd-package}
\alias{oecd}
\docType{package}
\title{Search and extract data from the OECD's API}
\description{
Search and extract data from the Organization for Economic Cooperation and 
Development (OECD)'s API. Contains convenience functions for searching
datasets, exploring dataset dimensions, and reading metadata.
}
\details{
\tabular{ll}{
Package: \tab oecd\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-09-29\cr
License: \tab GPL-3\cr
}
\code{getDatasets()}, \code{searchDatasets()}, \code{getSeries()}, 
\code{getDimensions()}, \code{getDesc()}, \code{browseMetadata()}
}
\author{Eric Persson}
\examples{
datasets <- getDatasets()
searchSeries(datasets, "unemployment")
series <- "MIG_UNEMP_GENDER"
df <- getSeries(series)
head(df)
getDimensions(series)
desc <- getDesc(series)
}