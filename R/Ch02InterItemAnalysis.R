#' @title Joint Sample Size
#' @description
#' The joint sample size is a matrix whose elements are the number of
#' individuals who responded to each pair of items.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @export

JointSampleSize <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  S_jk <- t(tmp$Z) %*% tmp$Z
  return(S_jk)
}

#' @title Joint Correct Response Rate
#' @description
#' The joint correct response rate(JCRR) is the rate of students who passed
#' both items.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @export

JCRR <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  U <- ifelse(is.na(tmp$U), 0, tmp$U)
  Z <- tmp$Z
  P_J <- t(Z * U) %*% (Z * U) / (t(Z) %*% Z)
  return(P_J)
}


#' @title Conditional Correct Repsonse Rate
#' @description
#' The conditional correct response rate(CCRR) represents the ratio of the studnets
#' who passed Item C(consequent item) to those who passed Item A(antecedent item)
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @export

CCRR <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  U <- ifelse(is.na(tmp$U), 0, tmp$U)
  Z <- tmp$Z
  OneJ <- rep(1, ncol(U))
  Pj <- JCRR(tmp$U)
  p <- crr(tmp$U)
  P_C <- Pj / (p %*% t(OneJ))
  return(P_C)
}


#' @title Item Lift
#' @description
#' The lift is a commonly used index in a POS data analysis.
#' The item lift of Item $k$ to Item $J$ is defined as follow:
#' \eqn{ l_{jk} = \frac{p_{k\mid j}}{p_k} }
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @references Brin, S., Motwani, R., Ullman, J., & Tsur, S. (1997). Dynamic itemset counting and
#' implication rules for market basket data. In Proceedings of ACM SIGMOD International Conference
#' on Management of Data (pp. 255–264).https://dl.acm.org/doi/10.1145/253262.253325
#' @export

ItemLift <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  OneJ <- rep(1, ncol(U))
  Pc <- CCRR(tmp$U)
  p <- crr(tmp$U)
  P_L <- Pc / (OneJ %*% t(p))
  return(P_L)
}

#' @title Mutual Information
#' @description
#' Mutual Information is a measure that represents the degree of interdependence
#' between two items
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @export

MutualInformation <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  Z <- tmp$Z
  U <- ifelse(is.na(tmp$U), 0, tmp$U)
  p <- crr(tmp$U)
  # Calculate joint response matrix
  S <- list()
  S$S_11 <- t(Z * U) %*% (Z * U)
  S$S_10 <- t(Z * U) %*% (Z * (1 - U))
  S$S_01 <- t(Z * (1 - U)) %*% (Z * U)
  S$S_00 <- t(Z * (1 - U)) %*% (Z * (1 - U))

  # Calculate joint probability matrix
  P <- lapply(S, function(x) x / (t(Z) %*% Z))

  # Calculate lift matrix
  L <- list()
  L$L_11 <- P$S_11 / (p %*% t(p))
  L$L_10 <- P$S_10 / (p %*% t(1 - p))
  L$L_01 <- P$S_01 / ((1 - p) %*% t(p))
  L$L_00 <- P$S_00 / ((1 - p) %*% t(1 - p))

  # Calculate mutual information
  MI <- P$S_00 * log(L$L_00, base = 2) + P$S_01 * log(L$L_01, base = 2) +
    P$S_10 * log(L$L_10, base = 2) + P$S_11 * log(L$L_11, base = 2)
  diag(MI) <- diag(P$S_00 * log(L$L_00, base = 2) + P$S_11 * log(L$L_11, base = 2))
  return(MI)
}

#' @title Phi-Coefficient
#' @description
#' The phi coefficient is the Peason's product moment correlation coefficient
#' between two binary items.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @export

PhiCoefficient <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  Z <- tmp$Z
  U <- ifelse(is.na(tmp$U), 0, tmp$U)
  p <- crr(tmp$U)
  OneS <- rep(1, nrow(U))
  OneJ <- rep(1, ncol(U))
  C <- t(Z * (U - OneS %*% t(p))) %*% (Z * (U - OneS %*% t(p))) / (t(Z) %*% Z - OneJ %*% t(OneJ))
  v <- diag(C)
  phi <- C / sqrt(v) %*% t(sqrt(v))
  return(phi)
}

#' @title Tetrachoric Correlation
#' @description
#' Tetrachoric Correlation is superiror to the phi coefficient as a measure of the
#' relation of an item pair. See Divgi, 1979; Olsson, 1979;Harris, 1988.
#' @references Divgi, D. R. (1979). Calculation of the tetrachoric correlation coefficient.
#' Psychometrika, 44, 169–172.
#' @references Olsson, U. (1979). Maximum likelihood estimation of the polychoric correlation
#'  coefficient. Psychometrika,44, 443–460.
#' @references Harris, B. (1988). Tetrachoric correlation coefficient. In L. Kotz, & N. L. Johnson
#'  (Eds.), Encyclopedia of statistical sciences (Vol. 9, pp. 223–225). Wiley.
#' @param x binary vector x
#' @param y binary vector y
#' @importFrom mvtnorm pmvnorm
#' @importFrom stats qnorm
#' @importFrom stats pnorm
#' @importFrom stats optimize
#' @export

tetrachoric <- function(x, y) {
  pairwise <- !is.na(x + y)
  # count 2x2 cells
  tbl <- table(x[pairwise], y[pairwise])
  S00 <- tbl[1, 1]
  S10 <- tbl[2, 1]
  S01 <- tbl[1, 2]
  S11 <- tbl[2, 2]
  if (S00 == 0) {
    S00 <- 0.5
  }
  if (S10 == 0) {
    S10 <- 0.5
  }
  if (S01 == 0) {
    S01 <- 0.5
  }
  if (S11 == 0) {
    S11 <- 0.5
  }
  # calcs tau
  tau_j <- qnorm(1 - mean(x, na.rm = TRUE))
  tau_k <- qnorm(1 - mean(y, na.rm = TRUE))
  ## BVN funcs
  BVN11 <- function(rho, tau_j, tau_k) {
    pmvnorm(upper = c(-tau_j, -tau_k), corr = matrix(c(1, rho, rho, 1), ncol = 2))
  }
  BVN01 <- function(rho, tau_j, tau_k) {
    pnorm(tau_j) - pmvnorm(upper = c(tau_j, tau_k), corr = matrix(c(1, rho, rho, 1), ncol = 2))
  }
  BVN10 <- function(rho, tau_j, tau_k) {
    pnorm(tau_k) - pmvnorm(upper = c(tau_j, tau_k), corr = matrix(c(1, rho, rho, 1), ncol = 2))
  }
  BVN00 <- function(rho, tau_j, tau_k) {
    pmvnorm(upper = c(tau_j, tau_k), corr = matrix(c(1, rho, rho, 1), ncol = 2))
  }
  ## LL
  log_likelihood_phi <- function(rho, tau_j, tau_k, S00, S11, S10, S01) {
    S00 * log(BVN00(rho, tau_j, tau_k)) + S01 * log(BVN01(rho, tau_j, tau_k)) +
      S10 * log(BVN10(rho, tau_j, tau_k)) + S11 * log(BVN11(rho, tau_j, tau_k))
  }
  ret <- optim(
    par = 0, # initial value
    fn = function(x) {
      -log_likelihood_phi(rho = x, tau_j, tau_k, S00, S11, S10, S01)
    },
    lower = -1, # lower limit
    upper = 1, # upper limit
    method = "Brent" # one-dimensional optimization method
  )
  return(ret$par)
}

#' @title Tetrachoric Correlation Matrix
#' @description
#' This function returns the tetrachoric correlation for all item pairs.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @export

TetrachoricCorrelationMatrix <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  Z <- tmp$Z
  U <- ifelse(is.na(tmp$U), 0, tmp$U)
  m <- ncol(U)
  mat <- matrix(NA, ncol = m, nrow = m)
  colnames(mat) <- tmp$ItemLabel
  rownames(mat) <- tmp$ItemLabel
  for (i in 1:(m - 1)) {
    for (j in (i + 1):m) {
      x <- tmp$U[, i]
      y <- tmp$U[, j]
      mat[i, j] <- tetrachoric(x, y)
      mat[j, i] <- mat[i, j]
    }
  }
  diag(mat) <- 1
  return(mat)
}
