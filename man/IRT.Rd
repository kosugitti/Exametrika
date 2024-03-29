% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Ch04ParameterEstimation.R
\name{IRT}
\alias{IRT}
\title{Estimating Item parmateres using EM algorithm}
\usage{
IRT(U, model = 2, na = NULL, Z = NULL, w = NULL)
}
\arguments{
\item{U}{U is either a data class of Exametrika, or raw data. When raw data is given,
it is converted to the Exametrika class with the \code{\link{dataFormat}} function.}

\item{model}{This argument takes the number of item parameters to be
estimated in the logistic model. It is limited to values 2, 3, or 4.}

\item{na}{na argument specifies the numbers or characters to be treated as missing values.}

\item{Z}{Z is a missing indicator matrix of the type matrix or data.frame}

\item{w}{w is item weight vector}
}
\value{
\describe{
\item{model}{number of item parameters you set.}
\item{testlength}{Length of the test. The number of items included in the test.}
\item{nobs}{Sample size. The number of rows in the dataset.}
\item{params}{Matrix containing the estimated item parameters}
\item{Q3mat}{Q3-matrix developed by Yen(1984)}
\item{itemPSD}{Posterior standard deviation of the item parameters}
\item{ability}{Estimated parameters of students ability}
\item{ItemFitIndices}{Fit index for each item.See also \code{\link{ItemFit}}}
\item{TestFitIndices}{Overall fit index for the test.See also \code{\link{TestFit}}}
}
}
\description{
A function for estimating item parameters using the EM algorithm.
}
\details{
Apply the 2, 3, and 4 parameter logistic models to estimate the item and subject populations.
The 4PL model can be described as follows.
\deqn{P(\theta,a_j,b_j,c_j,d_j)= c_j + \frac{d_j -c_j}{1+exp\{-a_j(\theta - b_j)\}}}
\eqn{a_j, b_j, c_j}, and \eqn{d_j} are parameters related to item j, and are parameters that
adjust the logistic curve.
\eqn{a_j} is called the slope parameter, \eqn{b_j} is the location, \eqn{c_j} is the lower asymptote,
and \eqn{d_j} is the upper asymptote paramter.
The model includes lower models, and among the 4PL models, the case where \eqn{d=1} is the 3PL model,
and among the 3PL models, the case where \eqn{c=0} is the 2PL model.
}
\references{
Yen, W. M. (1984) Applied Psychological Measurement, 8, 125-145.
}
