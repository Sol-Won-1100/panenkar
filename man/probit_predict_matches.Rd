% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/probit_model.R
\name{probit_predict_matches}
\alias{probit_predict_matches}
\title{Probit Predict Matches}
\usage{
probit_predict_matches(fixtures, fit, market = "result")
}
\arguments{
\item{fixtures}{a tibble columns of which must be "home_team", "away_team", "result"}

\item{fit}{the fitted probit model, output from probit_fit}
}
\value{
outcome probabilities for the fixtures
}
\description{
Predict multiple matches using ordered probit model
}
