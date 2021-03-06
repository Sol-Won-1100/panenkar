% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poisson_model.R
\name{poisson_time_weights}
\alias{poisson_time_weights}
\title{Calculate Time Weights Exponential}
\usage{
poisson_time_weights(match_dates, current_date, xi = 0.0016)
}
\arguments{
\item{match_dates}{dates of football matches, date object}

\item{current_date}{what date to calculate the exponential time weight from}

\item{xi}{the time weight parameter}
}
\value{
vector of weights
}
\description{
Uses an exponetial time weighting for modelling, weighting more recent matches more strongly. This is derived from 
the time weighting method used by Dixon-Coles in their version of the double Poisson model
}
