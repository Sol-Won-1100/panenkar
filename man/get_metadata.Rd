% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/depr_get_metadata.R
\name{get_metadata}
\alias{get_metadata}
\title{Get Metadata}
\usage{
get_metadata(
  competition_ids = NA,
  latest_european_season = "2020_2021",
  output_format = "list"
)
}
\arguments{
\item{competition_ids}{Each competition has a unique competition_id to
identify it. If left as NA, the default, all competitions will be returned.
From here you can inspect all competition_ids for future use.}

\item{latest_european_season}{e.g. "2019_2020", stick to that format}

\item{output_format}{"list" or "tibble" default is a list}
}
\value{
A list containing the metadata on that competition.

Each element of the output list will be named with the
competition_id so you can easily access the required part. Each sub-element
is structured as follows:

\itemize{
  \item \code{competition_id}, as described above
  \item \code{competition_region}, typically the country the competition is
  played in or the region e.g. Europe, World, etc
  \item \code{competition_name}, full name of the competition
  \item \code{competition_alias}, some competitions change names and are
  still refered to as their old name
  \item \code{competition_type}, one of league, cup, friendly
  \item \code{competition_confederation}, uefa, concaf, etc
  \item \code{competition_tier}, if a league type system then this is the
  position of the league within the structure
  \item \code{competition_promoted_into}, if a league type system then this
  is the competition_id of the league that teams are promoted into
  \item \code{competition_relegated_into}, if a league type system then this
  is the competition_id of the league that teams are relegated into
}

\cr
There are some other special sub-elements
\itemize{
  \item \code{football_data}, contains metadata for download results and odds
  data from www.football-data.co.uk
}

\cr
Within \code{football_data} there are several sub2-elements:
\itemize{
  \item \code{url_suffix}, each league on the website has a unique code which
  is used to construct its url
  \item \code{start_season, end_season}, see \code{\link{sequence_seasons}}
  for season format details
  \item \code{data_type}, either main_league or extra_league. The website had
  a set of main leagues for years which are stored as individual csv files.
  Extra leagues were added at some point in time but are stored in a single
  file per sheet.

}

More element details will be added to metadata as data sources and coverage
expands.
}
\description{
This functions stores and returns metadata on each competition.
}
\examples{
metadata <- get_metadata()
metadata_subset <- get_metadata(c("eng_pl", "eng_champ"))
}
\seealso{
\link[panenkar]{sequence_seasons}
}
