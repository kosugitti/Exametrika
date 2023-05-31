#' @title crr
#' @description
#' THe correct response rate (CRR) is one of the most basic and important
#'  statistics for item analysis. This is an index of item difficulty and
#'  a measure of how many students out of those who tried an item correctly
#'  responded to it.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @export

crr <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  OneS <- rep(1, length = nrow(U))
  Una <- ifelse(is.na(tmp$U), 0, tmp$U)
  p <- t(tmp$Z * Una) %*% OneS / t(tmp$Z) %*% OneS
  pW <- tmp$w * p
  return(pW)
}

#' @title Item Odds
#' @description
#' Item Odds are defined as
#' \eqn{O_j = \frac{p_j}{1-p_j}}.
#' Thus, this index represents the ratio of Correct Response Rate to
#' Incorrect Response Rate.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @export

ItemOdds <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  p <- crr(U = tmp$U, Z = tmp$Z)
  o <- p / (1 - p)
  return(o)
}

#' @title Item Threshold
#' @description
#' Itemthreshold is a measure of difficulty based on a standard normal distribuiton.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @importFrom stats qnorm
#' @export

ItemThreshold <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  p <- crr(U = tmp$U, Z = tmp$Z)
  Tau <- qnorm(1 - p)
  return(Tau)
}

#' @title Item Entropy
#' @description
#' The item entropy is an indicator of the variability or randomness
#' of the responses.
#' @details
#' \eqn{e_j = -p_j\log_2p_j-(1-p_j)\log_2(1-p_j)}
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @export

ItemEntropy <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  p <- crr(U = tmp$U, Z = tmp$Z)
  itemE <- -p * log(p, base = 2) - (1 - p) * log(1 - p, base = 2)
  return(itemE)
}

#' @title Item-Total Correlation
#' @description
#' Item-Total correlation(ITC) is a Peason's correlation of an item with
#' the NRS/total score.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @export

ItemTotalCorr <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  OneS <- rep(1, length = nrow(U))
  OneJ <- rep(1, length = ncol(U))
  p <- crr(U = tmp$U)
  Zeta <- sscore(U = tmp$U)
  Una <- ifelse(is.na(tmp$U), 0, tmp$U)
  C <- t(tmp$Z * (Una - OneS %*% t(p))) %*% (tmp$Z * (Una - OneS %*% t(p))) / (t(tmp$Z) %*% tmp$Z - OneJ %*% t(OneJ))
  rho_Zi <- t(tmp$Z * (Una - OneS %*% t(p))) %*% Zeta / (t(tmp$Z) %*% OneS - OneJ) / sqrt(diag(C))
  return(rho_Zi)
}
