#' @title dataFormat
#' @description
#' This function returns the sample size, data matrix, missing value identifier matrix,
#' and weight matrix from the given data matrix.
#' @param U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @return
#'
dataFormat <- function(U, na = NULL, Z = NULL, w = NULL) {
  # Check if U is either a matrix or a dataframe, otherwise stop the execution
  if (!is.matrix(U) && !is.data.frame(U)) {
    stop("U must be matrix or data.frame")
  }
  # Convert U to matrix if it's not already
  U <- as.matrix(U)
  # Check U matrix
  if (!all(U %in% c(0, 1, NA, na))) {
    stop("U can only contain the values 0, 1, NA, and the specified missing value")
  }
  # Check if Z is indicator matrix,or not.
  if (!is.null(Z)) {
    if (!all(Z %in% c(0, 1))) {
      stop("The missing indicator matrix must contains only 0 or 1")
    }
  }

  ### This function finally makes each matrix as follow:
  # U is a matrix composed solely of 0s,1s and NA.
  # Z is the missing identifier matrix composed solely of 0s and 1s.
  if (!is.null(na)) {
    ## na value specified
    U <- ifelse(U == na, NA, U)
  }

  Z <- ifelse(is.na(U), 0, 1)

  # If w is not specified, create a vector of 1s with length equal to the number of columns in U
  if (is.null(w)) {
    w <- rep(1, NCOL(U))
  }

  # Return the resulting U, Una, Z, and w
  return(list(U = U, Z = Z, w = w))
}


#' @title nrs
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
  tmp <- dataFormat(U = U, na = na, Z = Z, w = w)
  tmp$U <- ifelse(is.na(tmp$U), 0, tmp$U)
  tW <- (tmp$Z * tmp$U) %*% tmp$w
  return(tW)
}

#' @title passage
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
  tmp <- dataFormat(U = U, na = na, Z = Z, w = w)
  tw <- nrs(U = tmp$U, Z = tmp$Z, w = tmp$w)
  Js <- NCOL(U) - rowSums(is.na(tmp$U))
  rW <- tw / Js
  return(rW)
}

#' @title sscore
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
  S <- nrow(U)
  OneS <- rep(1, length = S)
  tmp <- dataFormat(U = U, na = na, Z = Z, w = w)
  rW <- passage(U = tmp$U, Z = tmp$Z, w = tmp$w)
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
  tmp <- dataFormat(U = U, na = na, Z = Z, w = w)
  sstmp <- sscore(U = tmp$U, Z = tmp$Z, w = tmp$w)
  empiricalZeta <- ecdf(sstmp)
  ret <- ceiling(empiricalZeta(sstmp) * 100)
  return(ret)
}

#' @title stanine
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
  tmp <- dataFormat(U = U, na = na, Z = Z, w = w)
  sttmp <- nrs(U = tmp$U, Z = tmp$Z, w = tmp$w)
  pbs <- cumsum(c(0.04, 0.07, 0.12, 0.17, 0.20, 0.17, 0.12, 0.07))
  stanine_prob <- quantile(sttmp, pbs)
  sttmp2 <- percentile(U = tmp$U, Z = tmp$Z, w = tmp$w)
  stanine_prob_ss <- quantile(sttmp2, pbs)
  stanine_scores <- cut(sttmp2, breaks = c(-Inf, stanine_prob_ss, Inf), right = F)
  stanine_scores <- factor(stanine_scores, labels = 1:9)
  return(list(stanine=stanine_prob,stanineScore = stanine_scores))
}


