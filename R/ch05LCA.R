#' @title Latent Clas Analysis
#' @description
#' A function for estimating LCA using the EM algorithm.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param ncls number of latent class
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
  pj <- crr(tmp$U)
  pj_mat <- matrix(rep(pj, nrow(tmp$U)), ncol = testlength, byrow = T)
  ell_N <- colSums(tmp$U * log(pj_mat) + (tmp$Z * (1 - tmp$U)) * log(1 - pj_mat))
  # Benchmark model
  nobs <- NROW(tmp$Z)
  total <- rowSums(tmp$U)
  totalList <- sort(unique(total))
  totalDist <- as.vector(table(total))
  ntotal <- length(totalList)
  ## Group Membership Profile Matrix
  MsG <- matrix(0, ncol = ntotal, nrow = nobs)
  for (i in 1:nobs) {
    MsG[i, which(totalList == total[i])] <- 1
  }
  ## PjG
  PjG <- t(MsG) %*% (tmp$Z * tmp$U) / t(MsG) %*% tmp$Z
  U1gj <- t(MsG) %*% (tmp$Z * tmp$U)
  U0gj <- t(MsG) %*% (tmp$Z * (1 - tmp$U))
  ell_B <- colSums(U1gj * log(PjG + const) + U0gj * log(1 - PjG + const))

  df_A <- ntotal - ncls
  df_B <- ntotal - 1
  ItemFitIndices <- Model_Fit(ell_A, ell_B, ell_N, df_A, df_B, nobs)
  # Test Total
  testEllmodel <- sum(ell_A)
  testEllbench <- sum(ell_B)
  testEllNull <- sum(ell_N)

  TestFitIndices <- Model_Fit(
    ell_A = testEllmodel, ell_B = testEllbench, ell_N = testEllNull,
    df_A = df_A * testlength,
    df_B = df_B * testlength, nobs
  )

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
    ItemFitIndices = ItemFitIndices,
    TestFitIndices = TestFitIndices
  ), class = c("Exametrika", "LCA"))
}
