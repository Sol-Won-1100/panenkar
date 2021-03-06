% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/depr_get_metadata.R
\name{as_tibble_metadata}
\alias{as_tibble_metadata}
\title{As Tibble Metadata}
\usage{
as_tibble_metadata(metadata)
}
\arguments{
\item{metadata}{see ?get_metadata}
}
\description{
Helper function - turns the metadata list into a tibble
}
\examples{
metadata_tibble <- get_metadata() \%>\% as_tibble_metadata()
}
