% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch07Biclustering.R
\name{Biclustering}
\alias{Biclustering}
\title{Biclustering and Ranklustering}
\usage{
Biclustering(
  U,
  ncls = 2,
  nfld = 2,
  Z = NULL,
  w = NULL,
  na = NULL,
  method = "B",
  conf = NULL,
  mic = FALSE,
  maxiter = 100,
  verbose = TRUE
)
}
\arguments{
\item{U}{U is either a data class of Exametrika, or raw data. When raw data is given,
it is converted to the Exametrika class with the \link{dataFormat} function.}

\item{ncls}{number of classes}

\item{nfld}{number of fields}

\item{Z}{Z is a missing indicator matrix of the type matrix or data.frame}

\item{w}{w is item weight vector}

\item{na}{na argument specifies the numbers or characters to be treated as missing values.}

\item{method}{Specify either "B"iclustering or "R"unklustering.}

\item{conf}{For the confirmatory parameter, you can input either a vector with
items and corresponding fields in sequence, or a field membership profile
matrix. In the case of the former, the field membership profile matrix will be generated internally.
When providing a membership profile matrix, it needs to be either matrix or data.frame.
The number of fields(nfld) will be overwrite to the number of columns of this matrix.
The default is NULL, and the field membership
matrix will be estimated according to the specified number of classes(ncls) and fields(nfld).}

\item{mic}{Monotonic increasing IRP option. The default is FALSE.}

\item{maxiter}{Maximum number of iterations. default is 100.}

\item{verbose}{verbose output Flag. default is TRUE}
}
\value{
\describe{
\item{nobs}{Sample size. The number of rows in the dataset.}
\item{testlength}{Length of the test. The number of items included in the test.}
\item{Nclass}{number of classes you set}
\item{BRM}{Bicluster Reference Matrix}
\item{FRP}{Field Reference Profile}
\item{FRPIndex}{Index of FFP includes the item location parameters B and Beta,
the slope parameters A and Alpha, and the monotonicity indices C and Gamma.}
\item{TRP}{Test Reference Profile}
\item{FMP}{Field Membership Profile}
\item{Students}{Class/Rank Membership Profile matrix.The s-th row vector of \eqn{\hat{M}_R}, \eqn{\hat{m}_R}, is the
rank membership profile of Student s, namely the posterior probability distribution representing the student's
belonging to the respective latent classes. It also includes the rank with the maximum estimated membership probability,
as well as the rank-up odds and rank-down odds.}
\item{LRD}{Latent Rank Distribution. see also \link{plot.Exametrika}}
\item{LCD}{Latent Class Distribution. see also \link{plot.Exametrika}}
\item{LFD}{Latent Field Distribuiton. see also \link{plot.Exametrika}}
\item{RMD}{Rank Membership Distribution.}
\item{TestFitIndices}{Overall fit index for the test.See also \link{TestFit}}
}
}
\description{
performs biclustering, rankclustering, and their confirmative models.
}
