% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Ch03CTT.R
\name{CTT}
\alias{CTT}
\title{Classical Test Theory}
\usage{
CTT(U, na = NULL, Z = NULL, w = NULL)
}
\arguments{
\item{U}{U is a data matrix of the type matrix or data.frame.}

\item{na}{na argument specifies the numbers or characters to be treated as missing values.}

\item{Z}{Z is a missing indicator matrix of the type matrix or data.frame}

\item{w}{w is item weight vector}
}
\description{
This function calculates the overall alpha and omega coefficients for
the given data matrix. It also computes the alpha coefficient for
each item, assuming that item is excluded.
}
