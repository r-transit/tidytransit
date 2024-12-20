% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_types.R
\name{convert_types}
\alias{convert_types}
\title{Convert columns between gtfsio types to tidytransit types according to GTFS reference}
\usage{
convert_types(gtfs_list, conversion_table, conversion_function)
}
\arguments{
\item{gtfs_list}{gtfs object}

\item{conversion_table}{data.frame containing a column \code{file} and \code{Field_Name}, generally
from internal \code{gtfs_reference_types} dataset}

\item{conversion_function}{function to convert columns}
}
\value{
gtfs_list with converted (overwritten) columns in tables
}
\description{
Convert columns between gtfsio types to tidytransit types according to GTFS reference
}
\keyword{internal}
