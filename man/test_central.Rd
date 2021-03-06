% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FC_Rationality_Tests.R
\name{test_central}
\alias{test_central}
\title{Tests mean median and mode rationality}
\usage{
test_central(
  x,
  y,
  instruments = c("1", "X"),
  vcov = iid,
  bw = NULL,
  other_data = NULL,
  id_mode = id_modegaussian
)
}
\arguments{
\item{x}{forecast}

\item{y}{observation}

\item{instruments}{lists of character variables describing the instruments
(also takes a list of lists if several instruments choices should be considered simultanuously)}

\item{vcov}{covariance estimator}

\item{bw}{standard NULL (then id_modegaussion function chooses bw_rule)}

\item{other_data}{matrix can be provided to use additional instruments}
}
\value{
returns data.frame of p-values
}
\description{
Tests mean median and mode for given instruments. Allows to specifiy instruments, covariance matrix estimation, an asymptotic identification function, and the bandwidth choice.
}
