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
  ret <- structure(S_jk, class = c("Exametrika", "matrix"))
  return(ret)
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
  P_J <- t(tmp$Z * tmp$U) %*% (tmp$Z * tmp$U) / (t(tmp$Z) %*% tmp$Z)
  ret <- structure(P_J, class = c("Exametrika", "matrix"))
  return(ret)
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
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  Z <- tmp$Z
  OneJ <- rep(1, ncol(tmp$U))
  Pj <- JCRR(tmp)
  p <- crr(tmp)
  P_C <- Pj / (p %*% t(OneJ))
  ret <- structure(P_C, class = c("Exametrika", "matrix"))
  return(ret)
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
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  OneJ <- rep(1, ncol(tmp$U))
  Pc <- CCRR(tmp)
  p <- crr(tmp)
  P_L <- Pc / (OneJ %*% t(p))
  ret <- structure(P_L, class = c("Exametrika", "matrix"))
  return(ret)
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
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  p <- crr(tmp)
  # Calculate joint response matrix
  S <- list()
  S$S_11 <- t(tmp$Z * tmp$U) %*% (tmp$Z * tmp$U)
  S$S_10 <- t(tmp$Z * tmp$U) %*% (tmp$Z * (1 - tmp$U))
  S$S_01 <- t(tmp$Z * (1 - tmp$U)) %*% (tmp$Z * tmp$U)
  S$S_00 <- t(tmp$Z * (1 - tmp$U)) %*% (tmp$Z * (1 - tmp$U))

  # Calculate joint probability matrix
  P <- lapply(S, function(x) x / (t(tmp$Z) %*% tmp$Z))

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
  ret <- structure(MI, class = c("Exametrika", "matrix"))
  return(ret)
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
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  p <- crr(tmp)
  OneS <- rep(1, nrow(tmp$U))
  OneJ <- rep(1, ncol(tmp$U))
  C <- t(tmp$Z * (tmp$U - OneS %*% t(p))) %*% (tmp$Z * (tmp$U - OneS %*% t(p))) / (t(tmp$Z) %*% tmp$Z - OneJ %*% t(OneJ))
  v <- diag(C)
  phi <- C / sqrt(v) %*% t(sqrt(v))
  ret <- structure(phi, class = c("Exametrika", "matrix"))
  return(ret)
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
  x.fac <- factor(x[pairwise], levels = 0:1)
  y.fac <- factor(y[pairwise], levels = 0:1)
  tbl <- table(x.fac, y.fac)
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
  ret <- structure(ret$par, class = c("Exametrika"))
  return(ret)
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
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  tmp$U[tmp$Z == 0] <- NA
  Una <- tmp$U
  m <- ncol(Una)
  mat <- matrix(NA, ncol = m, nrow = m)
  colnames(mat) <- tmp$ItemLabel
  rownames(mat) <- tmp$ItemLabel
  for (i in 1:(m - 1)) {
    for (j in (i + 1):m) {
      x <- Una[, i]
      y <- Una[, j]
      mat[i, j] <- tetrachoric(x, y)
      mat[j, i] <- mat[i, j]
    }
  }
  diag(mat) <- 1
  ret <- structure(mat, class = c("Exametrika", "matrix"))
  return(ret)
}


#' @title Inter-Item Analysis
#' @description
#' Inter-Item Analysis returns various metrics such as JSS, JCRR, CCR,
#' IL, MI, Phi, and Tetrachoric correlations in the form of a matrix.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @export

InterItemAnalysis <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  JSS <- JointSampleSize(U = tmp$U, Z = tmp$Z, w = tmp$z)
  JCRR <- JCRR(U = tmp$U, Z = tmp$Z, w = tmp$z)
  IL <- ItemLift(U = tmp$U, Z = tmp$Z, w = tmp$z)
  MI <- MutualInformation(U = tmp$U, Z = tmp$Z, w = tmp$z)
  Phi <- PhiCoefficient(U = tmp$U, Z = tmp$Z, w = tmp$z)
  Tet <- TetrachoricCorrelationMatrix(U = tmp$U, Z = tmp$Z, w = tmp$z)
  ret <- structure(list(
    JSS = JSS, JCRR = JCRR, IL = IL, MI = MI, Phi = Phi, Tetrachoric = Tet
  ), class = c("Exametrika", "IIAnalysis"))
  return(ret)
}
#' @title Correct Response Rate
#' @description
#' The correct response rate (CRR) is one of the most basic and important
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
  p <- t(tmp$Z * tmp$U) %*% OneS / t(tmp$Z) %*% OneS
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
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  p <- crr(tmp)
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
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  p <- crr(tmp)
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
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  p <- crr(tmp)
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
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  p <- crr(tmp)
  Zeta <- sscore(tmp)
  TBL <- matrix(rep(p, each = NROW(tmp$U)), nrow = NROW(tmp$U), byrow = F)
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
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  Zeta <- sscore(tmp)
  tmp$U[tmp$Z == 0] <- NA
  ITB <- rep(NA, ncol(tmp$U))
  for (i in 1:ncol(tmp$U)) {
    tmptmp <- tmp$U[, i]
    ITB[i] <- Biserial_Correlation(tmptmp, Zeta)
  }
  return(ITB)
}
#' @title Number Right Score
#' @description \code{nrs}
#' The Number-right score (NRS) function returns the count of the items passed to it.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @return This function counts and returns the number of correct answers from the data matrix.
#' @export
#' @examples
#' # nrs(U)
nrs <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  tW <- (tmp$Z * tmp$U) %*% tmp$w
  return(tW)
}

#' @title Passage Rate of student
#' @description
#' Passage rate of Student s is NRS divided by the number of presented items.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @return This function returns passage rate for each students
#' @export
#' @examples
#' # passage(U)
passage <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  tw <- nrs(tmp)
  Js <- NCOL(tmp$U) - rowSums(1 - tmp$Z)
  rW <- tw / Js
  return(rW)
}

#' @title Standardized Score
#' @description
#' The standardized score indicates how high or low the student's ability is
#' placed in the standard normal distribution.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @return This function returns standardized score for each students.
#' @export
#' @examples
#' # sscore(U)
sscore <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  S <- nrow(tmp$U)
  OneS <- rep(1, length = S)
  rW <- passage(tmp)
  rBarW <- (t(OneS) %*% rW) / S
  Var_rW <- t(rW - c(rBarW) * OneS) %*% (rW - c(rBarW) * OneS) / (S - 1)
  Zeta_W <- (rW - c(rBarW) * OneS) / (sqrt(c(Var_rW)) * OneS)
  return(Zeta_W)
}

#' @title percentile
#' @description
#' The percentile function returns the corresponding score percentile,
#' out of 100 divisions, for each student.
#' @importFrom stats ecdf
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @return This function returns standardized score for each students.
#' @export
#' @examples
#' # percentile(U)
percentile <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  sstmp <- sscore(tmp)
  empiricalZeta <- ecdf(sstmp)
  ret <- ceiling(empiricalZeta(sstmp) * 100)
  return(ret)
}

#' @title Stanine
#' @description
#' The Stanine scoring system divides students into nine groups.
#' These groups correspond to the following percentile ranges:
#' the lowest 4%, the subsequent 7%, the following 12%, the next 17%,
#' the middle 20%, the subsequent 17%, the following 12%,
#' the next 7%, and the highest 4%.
#' @import stats
#' @references
#' Angoff, W. H. (1984). Scales, norms, and equivalent scores. Educational Testing Service.
#' (Reprint of chapter in R. L. Thorndike (Ed.) (1971) Educational Measurement (2nd Ed.).
#' American Councilon Education.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @return This function returns Stanine Rank for each students.
#' @export
#' @examples
#' # stanine(U)
stanine <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  sttmp <- nrs(tmp)
  pbs <- cumsum(c(0.04, 0.07, 0.12, 0.17, 0.20, 0.17, 0.12, 0.07))
  stanine_prob <- quantile(sttmp, pbs, na.rm = TRUE)
  sttmp2 <- percentile(tmp)
  stanine_prob_ss <- quantile(sttmp2, pbs)
  stanine_scores <- cut(sttmp2, breaks = c(-Inf, stanine_prob_ss, Inf), right = F)
  stanine_scores <- factor(stanine_scores, labels = 1:9)
  return(list(stanine = stanine_prob, stanineScore = stanine_scores))
}

#' @title StudentAnalysis
#' @description
#' The StudentAnalysis function returns descriptive statistics for each individual student.
#' Specifically, it provides the number of responses, the number of correct answers,
#' the passage rate, the standardized score, the percentile, and the stanine.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @export
#'

StudentAnalysis <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }

  NRS <- nrs(U = tmp$U, Z = tmp$Z, w = tmp$w)
  NR <- NCOL(tmp$U) - rowSums(is.na(tmp$U))
  PR <- passage(U = tmp$U, Z = tmp$Z, w = tmp$w)
  SS <- sscore(U = tmp$U, Z = tmp$Z, w = tmp$w)
  Ptile <- percentile(U = tmp$U, Z = tmp$Z, w = tmp$w)
  ST <- stanine(U = tmp$U, Z = tmp$Z, w = tmp$w)
  ret <- data.frame(
    ID = tmp$ID,
    NR = NR,
    NRS = NRS,
    PR = PR,
    SS = SS,
    Percentile = Ptile,
    Stanine = ST$stanineScore
  )
  return(ret)
}
#' @title Simple Test Statistics
#' @description
#' Statistics regarding the total score.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @return
#' \describe{
#' \item{TestLength}{Length of the test. The number of items included in the test.}
#' \item{SampleSize}{Sample size. The number of rows in the dataset.}
#' \item{Mean}{Average number of correct answers.}
#' \item{SEofMean}{Standard error of mean}
#' \item{Variance}{Variance}
#' \item{SD}{Standard Deviation}
#' \item{Skewness}{Skewness}
#' \item{Kurtosis}{Kurtosis}
#' \item{Min}{Minimum score}
#' \item{Max}{Max score}
#' \item{Range}{Range of score}
#' \item{Q1}{First quartile. Same as the 25th percentile.}
#' \item{Median}{Median.Same as the 50th percentile.}
#' \item{Q3}{Third quartile. Same as the 75th percentile.}
#' \item{IQR}{Interquartile range. It is calculated by subtracting the first quartile from the third quartile.}
#' \item{Stanine}{see [stanine]}
#' }
#' @export

TestStatistics <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  tW <- nrs(tmp)
  TestLength <- NCOL(tmp$Z)
  SampleSize <- NROW(tmp$Z)
  Mean <- mean(tW)
  SEofMean <- sd(tW) / sqrt(SampleSize)
  Variance <- var(tW)
  SD <- sd(tW)
  SDs <- sqrt(mean((tW - Mean)^2))
  tmpZ <- (tW - Mean) / SDs
  Skewness <- mean(tmpZ^3)
  Kurtosis <- mean(tmpZ^4) - 3
  Min <- min(tW)
  Max <- max(tW)
  Range <- Max - Min
  Q1 <- quantile(tW, probs = 0.25, na.rm = TRUE)
  Median <- quantile(tW, probs = 0.5, na.rm = TRUE)
  Q3 <- quantile(tW, probs = 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  Stanine <- stanine(tmp)
  ret <-
    structure(list(
      TestLength = TestLength,
      SampleSize = SampleSize,
      Mean = Mean,
      SEofMean = SEofMean,
      Variance = Variance,
      SD = SD,
      Skewness = Skewness,
      Kurtosis = Kurtosis,
      Min = Min,
      Max = Max,
      Range = Range,
      Q1 = Q1,
      Median = Median,
      Q3 = Q3,
      IQR = IQR,
      Stanine = Stanine$stanine
    ), class = c("Exametrika", "TestStatistics"))
  return(ret)
}

#' @title Dimensionality
#' @description
#' The dimensionality is the number of components
#' the test is measuring.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @export

Dimensionality <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  R <- TetrachoricCorrelationMatrix(tmp)
  Esystem <- eigen(R)
  Eval <- Esystem$values
  EvalVariance <- Esystem$values / length(Eval) * 100
  CumVari <- cumsum(EvalVariance)
  ret <-
    structure(
      list(
        Component = seq(1:length(Eval)),
        Eigenvalue = Eval,
        PerOfVar = EvalVariance,
        CumOfPer = CumVari
      ),
      class = c("Exametrika", "Dimensionality")
    )

  return(ret)
}


ItemStatistics <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }

  ret <-
    structure(list(
      ItemLabel = tmp$ItemLabel,
      NumberOfRespondents = colSums(tmp$Z),
      CorrectResponseRate = crr(tmp),
      ItemThreshold = ItemThreshold(tmp),
      ItemTotalCrr = ItemTotalCorr(tmp)
    ), class = c("Exametrika", "ItemStatistics"))
  return(ret)
}
