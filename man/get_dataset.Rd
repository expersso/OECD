% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{get_dataset}
\alias{get_dataset}
\title{Download OECD data sets.}
\usage{
get_dataset(
  dataset,
  filter = NULL,
  start_time = NULL,
  end_time = NULL,
  last_n_observations = NULL,
  ...
)
}
\arguments{
\item{dataset}{A string with the code for the desired data set}

\item{filter}{A character vectors specifying the filter to be applied to
each dimension of the dataset (see \code{examples} below).}

\item{start_time}{Starting time/period for data. If left blank, no time filter is
applied (i.e. all observations since the earliest available observation are
downloaded).}

\item{end_time}{End time/period for data.}

\item{last_n_observations}{Number of most recent observations to download.}

\item{...}{Additional parameters passed to \code{read_sdmx}}
}
\value{
A data frame
}
\description{
Returns a data frame with the requested data, downloaded through the OECD's API.
}
\examples{
# Get entire dataset
\dontrun{
df <- get_dataset("OECD.SDD.NAD.SEEA/DSD_NAT_RES@DF_NAT_RES,1.0", "AUS+CAN.A....")
}
\dontrun{
head(df, 10)
}

}
