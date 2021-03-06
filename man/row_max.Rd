% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/row_max_min.R
\name{row_max}
\alias{row_max}
\alias{row_min}
\title{Row Maximum / Minimum}
\usage{
row_max(...)

row_min(...)
}
\arguments{
\item{...}{columns of the tibble to compute the rowwise maximum on, must be numeric}
}
\description{
Return the row maximum / minimum for columns ... in a tibble x using tidy evaluation
}
\note{
passes the max function to row_numeric_function which applys the max functions to rows
}
\examples{


library(tidyverse)

x <- tribble(~x1,    ~x2,      ~x3,
             "dog",   2,        4,
             "cat",   NA_real_, 5.5,
             "mouse", 40,       20)

row_max(x, x2, x3, new_col_name = "max_x2_x3")
row_min(x, x2, x3, append_col = FALSE)
}
