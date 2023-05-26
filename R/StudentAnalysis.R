#' @title nrs
#' @description \code{nrs}
#' The Number-right score (NRS) function returns the count of the items passed to it.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @return This function counts and returns the number of correct answers from the data matrix.
#' @export
#' @examples
#' # nrs(x)
nrs <- function(U, Z = NULL, w = NULL) {
  nc <- ncol(U)
  nr <- nrow(U)
  ## if Missing indicator not given
  if (is.null(Z)) {
    Z <- matrix(1, ncol = nc, nrow = nr)
  }
  # if item weight vector not given
  if (is.null(w)) {
    w <- rep(1, length = nc)
  }
  tW <- (Z * U) %*% w
  return(tW)
}

#' @title passage
#' @description
#' Passage rate of Student s is NRS divided by the number of presented items.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @return This function returns passage rate for each students
#' @export
#' @examples
#' # passage(x)
passage <- function(U, Z = NULL, w = NULL) {
  nc <- ncol(U)
  nr <- nrow(U)
  tw <- nrs(U, Z, w)
  ## if Missing indicator not given
  if (is.null(Z)) {
    Z <- matrix(1, ncol = nc, nrow = nr)
  }
  # if item weight vector not given
  if (is.null(w)) {
    w <- rep(1, length = nc)
  }
  rW <- tw / (Z %*% w)
  return(rW)
}

#' @title sscore
#' @description
#' The standardized score indicates how high or low the student's ability is
#' placed in the standard normal distribution.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @return This function returns standardized score for each students.
#' @export
#' @examples
#' # sscore(x)
sscore <- function(U, Z = NULL, w = NULL) {
  S <- nrow(U)
  J <- ncol(U)
  OneS <- rep(1, length = S)
  ## if Missing indicator not given
  if (is.null(Z)) {
    Z <- matrix(1, ncol = J, nrow = S)
  }
  # if item weight vector not given
  if (is.null(w)) {
    w <- rep(1, length = J)
  }
  rW <- passage(U, Z, w)
  rBarW <- (t(OneS) %*% rW) / S
  Var_rW <- t(rW - c(rBarW) * OneS) %*% (rW - c(rBarW) * OneS) / (S - 1)
  Zeta_W <- (rW - c(rBarW) * OneS) / (sqrt(c(Var_rW)) * OneS)
  return(Zeta_W)
}
