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
  OneS <- rep(1, length = nrow(tmp$U))
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
  p <- crr(U, na = na)
  Zeta <- sscore(U, na = na)
  TBL <- matrix(rep(p, each = NROW(U)), nrow = NROW(U), byrow = F)
  Una <- ifelse(is.na(tmp$U), 0, tmp$U)
  dev <- tmp$Z * (Una - TBL)
  V <- colSums(dev^2) / (colSums(tmp$Z) - 1)
  SD <- sqrt(V)
  rho_Zi <- t(dev) %*% Zeta / SD / colSums(tmp$Z)
  return(rho_Zi)
}

#' @title Biserial Correlation
#' @description
#' A biserial correlation is a correlation between dichotomous-ordinal and
#' continuous variables.
#' @param i i is a dichotomous-ordinal variables. x and y can also be the other way around.
#' @param t t is a continuous variables. x and y can also be the other way around.
#' @export

Biserial_Correlation <- function(i, t) {
  # Count unique values
  unique_i <- length(unique(na.omit(i)))
  unique_t <- length(unique(na.omit(t)))
  # Check if one is binary and the other is continuous
  if (!((unique_i == 2 && unique_t > 2) | (unique_t == 2 && unique_i > 2))) {
    stop("One argument must be binary and the other must be continuous.")
  }
  ## if switched...
  if (unique_i > 2) {
    tmp <- i
    i <- t
    t <- tmp
  }
  ## calcs correlation
  tau_j <- qnorm(1 - mean(i, na.rm = T))
  ll <- function(rho, tau_j, i, t) {
    tmp <- (1 - i) %*% (log(pnorm(tau_j, mean = rho * t, sd = sqrt(1 - rho^2)))) +
      i %*% (log(1 - pnorm(tau_j, mean = rho * t, sd = sqrt(1 - rho^2))))
  }
  pairwise <- !is.na(i + t)
  ret <- optim(
    par = 0, # initial value
    fn = function(x) {
      -ll(rho = x, tau_j, i[pairwise], t[pairwise])
    },
    lower = -1, # lower limit
    upper = 1, # upper limit
    method = "Brent" # one-dimensional optimization method
  )
  return(ret$par)
}

#' @title Item-Total Biserial Correlation
#' @description
#' The Item-Biserial Correlation computes the biserial correlation
#' between an item and the total score.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @export

ITBiserial <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  Una <- ifelse(is.na(tmp$U), NA, tmp$U)
  Zeta <- sscore(U, na = na)
  ITB <- rep(NA, ncol(tmp$U))
  for (i in 1:ncol(tmp$U)) {
    tmptmp <- Una[, i]
    ITB[i] <- Biserial_Correlation(tmptmp, Zeta)
  }
  return(ITB)
}
