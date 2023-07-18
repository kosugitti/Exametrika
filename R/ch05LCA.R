#' @title Latent Class Analysis
#' @description
#' A function for estimating LCA using the EM algorithm.
#' @param U U is either a data class of Exametrika, or raw data. When raw data is given,
#' it is converted to the Exametrika class with the [dataFormat] function.
#' @param ncls number of latent class
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @return
#' \describe{
#'  \item{nobs}{Sample size. The number of rows in the dataset.}
#'  \item{testlength}{Length of the test. The number of items included in the test.}
#'  \item{Nclass}{number of classes you set}
#'  \item{TRP}{Test Reference Profile matrix. The TRP is the column sum vector of estimated class reference matrix,
#' \eqn{\hat{\Pi}_c}}
#'  \item{LCD}{Latent Class Dstribution table.see also [plot.Exametrika]}
#'  \item{CMD}{Class Membership Dstribution table. see also [plot.Exametrika]}
#'  \item{Students}{Class Membership Profile matrix.The s-th row vector of \eqn{\hat{M}_c}, \eqn{\hat{m}_c}, is the
#' class membership profile of Student s, namely the posterior probability distribution representing the student's
#' belonging to the respective latent classes. The last column indicates the latent class estimate.}
#'  \item{IRP}{Item Reference Profile matrix.The IRP of item j is the j-th row vector in the class reference matrix,
#' \eqn{\hat{\pi}_c}}
#'  \item{ItemFitIndices}{Fit index for each item.See also [ModelFit]}
#'  \item{TestFitIndices}{Overall fit index for the test.See also [ModelFit]}
#' }
#' @export
#'

LCA <- function(U, ncls = 2, na = NULL, Z = NULL, w = NULL) {
  # data format
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  U <- ifelse(is.na(tmp$U), 0, tmp$U) * tmp$Z

  if (ncls < 2 | ncls > 20) {
    stop("[Caution!] An invalid number of classes was specified.")
  }

  # Initialize
  testlength <- NCOL(tmp$U)
  beta1 <- 1
  beta2 <- 1
  const <- exp(-testlength)
  testEll <- -1 / const
  oldtestEll <- -2 / const
  itemEll <- rep(testEll / testlength, testlength)
  classRefMat <- matrix(rep(1:ncls / (ncls + 1), testlength), ncol = testlength)

  ## EM algorithm
  emt <- 0
  maxemt <- 100
  FLG <- TRUE

  while (FLG) {
    emt <- emt + 1
    oldtestEll <- testEll

    llmat <- tmp$U %*% t(log(classRefMat + const)) + (tmp$Z * (1 - tmp$U)) %*% t(log(1 - classRefMat + const))
    exp_llmat <- exp(llmat)
    postDist <- exp_llmat / rowSums(exp_llmat)

    correct_cls <- t(postDist) %*% tmp$U
    incorrect_cls <- t(postDist) %*% (tmp$Z * (1 - tmp$U))

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
    if (emt == maxemt) {
      FLG <- FALSE
    }
  }

  ## Returns
  #### Class Information
  TRP <- classRefMat %*% tmp$w
  bMax <- matrix(rep(apply(postDist, 1, max), ncls), ncol = ncls)
  clsNum <- apply(postDist, 1, which.max)
  cls01 <- sign(postDist - bMax) + 1
  LCD <- colSums(cls01)
  CMD <- colSums(postDist)
  StudentClass <- cbind(postDist, clsNum)
  colnames(StudentClass) <- c(paste("Membership", 1:ncls), "Estimate")
  ### Item Information
  IRP <- t(classRefMat)
  colnames(IRP) <- paste0("IRP", 1:ncls)

  ### Model Fit
  # each Items
  ell_A <- itemEll
  FitIndices <- ModelFit(tmp$U, tmp$Z, ell_A, ncls)

  ret <- structure(list(
    testlength = testlength,
    nobs = nobs,
    Nclass = ncls,
    N_EM_Cycle = emt,
    TRP = as.vector(TRP),
    LCD = as.vector(LCD),
    CMD = as.vector(CMD),
    Students = StudentClass,
    IRP = IRP,
    ItemFitIndices = FitIndices$item,
    TestFitIndices = FitIndices$test
  ), class = c("Exametrika", "LCA"))
}
