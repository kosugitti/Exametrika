% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch08BNM.R
\name{BNM}
\alias{BNM}
\title{Bayesian Network Model}
\usage{
BNM(
  U,
  Z = NULL,
  w = NULL,
  na = NULL,
  g = NULL,
  adj_file = NULL,
  adj_matrix = NULL
)
}
\arguments{
\item{U}{U is either a data class of Exametrika, or raw data. When raw data is given,
it is converted to the Exametrika class with the \link{dataFormat} function.}

\item{Z}{Z is a missing indicator matrix of the type matrix or data.frame}

\item{w}{w is item weight vector}

\item{na}{na argument specifies the numbers or characters to be treated as missing values.}

\item{g}{Specify a graph object suitable for the igraph class.}

\item{adj_file}{specify CSV file where the graph structure is specified.}

\item{adj_matrix}{specify adjacency matrix.}
}
\value{
\describe{
\item{nobs}{Sample size. The number of rows in the dataset.}
\item{testlength}{Length of the test. The number of items included in the test.}
\item{crr}{correct response ratio}
\item{TestFitIndices}{Overall fit index for the test.See also \link{TestFit}}
\item{adj}{Adjacency matrix}\
\item{param}{Learned Parameters}
\item{CCRR_table}{Correct Response Rate tables}
}
}
\description{
performs Bayseia Network Model with specified graph stracture
}
\details{
This function performs a Bayesian network analysis on the relationships
between items. This corresponds to Chapter 8 of the text. It uses the igraph
package for graph visualization and checking the adjacency matrix.
You need to provide either a graph object or a CSV file where the graph
structure is specified.
}
