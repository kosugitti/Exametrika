% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Ch02TestItemAnalysis.R
\name{ItemOdds}
\alias{ItemOdds}
\title{Item Odds}
\usage{
ItemOdds(U, na = NULL, Z = NULL, w = NULL)
}
\arguments{
\item{U}{U is a data matrix of the type matrix or data.frame.}

\item{na}{na argument specifies the numbers or characters to be treated as missing values.}

\item{Z}{Z is a missing indicator matrix of the type matrix or data.frame}

\item{w}{w is item weight vector}
}
\description{
Item Odds are defined as
\eqn{O_j = \frac{p_j}{1-p_j}}.
Thus, this index represents the ratio of Correct Response Rate to
Incorrect Response Rate.
}
