#' @title emclus funciton
#' @description
#' This function is used for both LCA (Latent Class Analysis) and
#' LRA (Latent Rank Analysis). LCA is considered a special case of LRA,
#' in which the filtering matrix is reduced to the identity matrix.
#' This function takes a dataset, the number of classes, and a filtering
#' matrix as input and returns the latent rank.
#' @param U response matrix U of the examData class.
#' @param Z missing indicator matrix Z of the examData class.
#' @param Fil Filter matrix
#' @param ncls number of latent class
#' @param beta1 beta distribution parameter1 as prior density of rank reference matrix
#' @param beta2 beta distribution parameter2 as prior density of rank reference matrix
#' @param maxiter Maximum number of iterations.
#'
#'

emclus <- function(U, Z, ncls, Fil, beta1, beta2, maxiter = 100) {
  # Initialize
  testlength <- NCOL(U)
  const <- exp(-testlength)
  testEll <- -1 / const
  oldtestEll <- -2 / const
  itemEll <- rep(testEll / testlength, testlength)
  classRefMat <- matrix(rep(1:ncls / (ncls + 1), testlength), ncol = testlength)

  ## EM algorithm
  emt <- 0
  FLG <- TRUE

  while (FLG) {
    emt <- emt + 1
    oldtestEll <- testEll

    llmat <- U %*% t(log(classRefMat + const)) + (Z * (1 - U)) %*% t(log(1 - classRefMat + const))
    exp_llmat <- exp(llmat)
    postDist <- exp_llmat / rowSums(exp_llmat)

    smoothPost <- postDist %*% Fil
    correct_cls <- t(smoothPost) %*% U
    incorrect_cls <- t(smoothPost) %*% (Z * (1 - U))

    old_classRefMat <- classRefMat
    classRefMat <- (correct_cls + beta1 - 1) / (correct_cls + incorrect_cls + beta1 + beta2 - 2)

    itemEll <- colSums(correct_cls * log(classRefMat + const) + incorrect_cls * log(1 - classRefMat + const))
    testEll <- sum(itemEll)

    if (testEll - oldtestEll <= 0) {
      classRefMat <- old_classRefMat
      FLG <- FALSE
    }
    if ((testEll - oldtestEll) <= 0.0001 * abs(oldtestEll)) {
      FLG <- FALSE
    }
    if (emt == maxiter) {
      message("Reached the maximum number of iterations")
      FLG <- FALSE
    }
  }

  ret <- list(
    iter = emt,
    itemEll = itemEll,
    postDist = postDist,
    classRefMat = classRefMat
  )
  return(ret)
}
