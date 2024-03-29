% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatial.R
\name{gtfs_transform}
\alias{gtfs_transform}
\title{Transform or convert coordinates of a gtfs feed}
\usage{
gtfs_transform(gtfs_obj, crs)
}
\arguments{
\item{gtfs_obj}{gtfs feed (tidygtfs object)}

\item{crs}{target coordinate reference system, used by sf::st_transform}
}
\value{
tidygtfs object with transformed stops and shapes sf dataframes
}
\description{
Transform or convert coordinates of a gtfs feed
}
