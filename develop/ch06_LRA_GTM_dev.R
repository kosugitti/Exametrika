rm(list = ls())
library(tidyverse)
dat <- read_csv("tests/testthat/sampleData/J15S500.csv")
library(Exametrika)
tmp <- dataFormat(dat, na = -99)

testlength <- NCOL(tmp$U)
samplesize <- NROW(tmp$U)
const <- exp(-testlength)

ncls <- 6


# Function --------------------------------------------------------


# test EMclus -----------------------------------------------------

emclus(tmp$U, tmp$Z, ncls = 6, Fil = diag(rep(1, 6)), beta1 = 1, beta2 = 1)

# LRA_GTM ---------------------------------------------------------

f0 <- ifelse(ncls < 6, 1.05 - 0.05 * ncls,
  ifelse(ncls < 11, 1.00 - 0.04 * ncls,
    0.80 - 0.02 * ncls
  )
)
f1 <- diag(0, ncls)
f1[row(f1) == col(f1) - 1] <- (1 - f0) / 2
Filter <- diag(rep(f0, ncls)) + t(f1) + f1
Filter[, 1] <- Filter[, 1] / sum(Filter[, 1])
Filter[, ncls] <- Filter[, ncls] / sum(Filter[, ncls])

emclus(tmp$U, tmp$Z, ncls = 6, Fil = Filter, 1, 1)

# test ------------------------------------------------------------

LRA <- function(U, ncls = 2, na = NULL, Z = NULL, w = NULL,
                method = c("SOM", "GTM"),
                mic = FALSE,
                maxiter = 100,
                BIC.check = FALSE) {
  # data format
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  U <- ifelse(is.na(tmp$U), 0, tmp$U) * tmp$Z
  testlength <- ncol(tmp$U)
  if (ncls < 2 | ncls > 20) {
    stop("[Caution!] An invalid number of classes was specified.")
  }


  if (method == "SOM") {
    print("SOM")
    somt <- 0
    alpha1 <- 1
    alphaT <- 0.01
    sigma1 <- 1
    sigmaT <- 0.12

    alpha_list <- ((maxiter - 1:maxiter) * alpha1 + (1:maxiter - 1) * alphaT) / (maxiter - 1)
    sigma_list <- ((maxiter - 1:maxiter) * sigma1 + (1:maxiter - 1) * sigmaT) / (maxiter - 1)

    kappa1 <- 0.01
    kappaT <- 0.0001

    kappa_list <- ((maxiter - 1:maxiter) * kappa1 + (1:maxiter - 1) * kappaT) / (maxiter - 1)

    prior_list <- rep(1 / ncls, ncls)

    r_list <- seq(-ncls + 1, ncls - 1)
    hhhmat <- array(NA, c(maxiter, length(r_list)))
    for (t in 1:maxiter) {
      hhhmat[t, ] <- alpha_list[t] * ncls / samplesize * exp(-(r_list)^2 / (2 * ncls^2 * sigma_list[t]^2))
    }
    clsRefMat <- matrix(rep(1:ncls / (ncls + 1), testlength), ncol = testlength)
    RefMat <- t(clsRefMat)
    oldBIC <- 1e5

    ### SOM iteration
    FLG <- TRUE
    while (FLG) {
      somt <- somt + 1

      if (somt <= maxiter) {
        h_count <- somt
      } else {
        h_cout <- maxiter
      }

      loglike <- 0
      set.seed(sum(tmp$U) + somt)
      is <- order(runif(samplesize, 1, 100))

      for (s in 1:samplesize) {
        ss <- is[s]
        mlrank <- tmp$U[ss, ] %*% log(RefMat + const) + (1 - tmp$U[ss, ]) %*% log(1 - RefMat + const) + log(prior_list)
        winner <- which.max(mlrank)
        loglike <- loglike + mlrank[winner]
        hhh <- matrix(rep(hhhmat[h_count, (ncls + 1 - winner):(2 * ncls - winner)], testlength),
          nrow = testlength, byrow = T
        )
        RefMat <- RefMat + hhh * (tmp$U[ss, ] - RefMat)
        prior_list <- prior_list + (kappa_list[h_count] / ncls)
        prior_list[winner] <- prior_list[winner] - kappa_list[h_count]
        prior_list[prior_list > 1] <- 1
        prior_list[prior_list < const] <- const
      }
      if (mic) {
        print("mic")
        RefMat <- t(apply(RefMat, 1, sort))
      }
      llmat <- tmp$U %*% t(log(t(RefMat) + const)) + (tmp$Z * (1 - tmp$U)) %*%
        t(log(1 - t(RefMat) + const))
      expllmat <- exp(llmat)
      postdist <- expllmat / rowSums(expllmat)

      if (BIC.check) {
        if (somt > maxiter * 10) {
          message("Reached ten times the maximum number of iterations.")
          FLG <- FALSE
        }
        correctcls <- t(postdist) %*% tmp$U
        incorrectcls <- t(postdist) %*% (tmp$Z * (1 - tmp$U))
        item_ell <- correctcls * log(t(RefMat) + const) + incorrectcls * (log(1 - t(RefMat) + const))
        item_ell <- colSums(item_ell)
        FI <- ModelFit(tmp$U, tmp$Z, item_ell, ncls)
        diff <- abs(oldBIC - FI$test$BIC)
        print(paste("iter", somt, "BIC ", FI$test$BIC))
        oldBIC <- FI$test$BIC
        if (diff < 1e-4) {
          FLG <- FALSE
        }
      } else {
        if (somt == maxiter) {
          FLG <- FALSE
        }
      }
    }

    correctcls <- t(postdist) %*% tmp$U
    incorrectcls <- t(postdist) %*% (tmp$Z * (1 - tmp$U))
    classRefMat <- (correctcls) / (correctcls + incorrectcls)
    item_ell <- correctcls * log(t(RefMat) + const) + incorrectcls * (log(1 - t(RefMat) + const))
    item_ell <- colSums(item_ell)

    fit <- list(
      iter = somt,
      itemEll = item_ell,
      postDist = postdist,
      classRefMat = classRefMat
    )
  } else {
    print("GTM")
    # GTM.
    f0 <- ifelse(ncls < 6, 1.05 - 0.05 * ncls,
      ifelse(ncls < 11, 1.00 - 0.04 * ncls,
        0.80 - 0.02 * ncls
      )
    )
    f1 <- diag(0, ncls)
    f1[row(f1) == col(f1) - 1] <- (1 - f0) / 2
    Filter <- diag(rep(f0, ncls)) + t(f1) + f1
    Filter[, 1] <- Filter[, 1] / sum(Filter[, 1])
    Filter[, ncls] <- Filter[, ncls] / sum(Filter[, ncls])

    fit <- emclus(tmp$U, tmp$Z, ncls = ncls, Fil = Filter, 1, 1)
  }

  #### Class Information
  TRP <- fit$classRefMat %*% tmp$w
  bMax <- matrix(rep(apply(fit$postDist, 1, max), ncls), ncol = ncls)
  clsNum <- apply(fit$postDist, 1, which.max)
  cls01 <- sign(fit$postDist - bMax) + 1
  LCD <- colSums(cls01)
  CMD <- colSums(fit$postDist)
  StudentClass <- cbind(fit$postDist, clsNum)
  colnames(StudentClass) <- c(paste("Membership", 1:ncls), "Estimate")
  ### Item Information
  IRP <- t(fit$classRefMat)
  colnames(IRP) <- paste0("IRP", 1:ncls)

  ### Model Fit
  # each Items
  ell_A <- fit$itemEll
  FitIndices <- ModelFit(tmp$U, tmp$Z, ell_A, ncls)

  ret <- structure(list(
    testlength = testlength <- NCOL(tmp$U),
    nobs = NROW(tmp$U),
    Nclass = ncls,
    N_EM_Cycle = fit$iter,
    TRP = as.vector(TRP),
    LCD = as.vector(LCD),
    CMD = as.vector(CMD),
    Students = StudentClass,
    IRP = IRP,
    ItemFitIndices = FitIndices$item,
    TestFitIndices = FitIndices$test
  ), class = c("Exametrika", "LRA"))
}
# do the test ------------------------------------------------------------

LRA(tmp$U, method = "GTM", ncls = 6) -> unGTM
LRA(tmp$U, method = "SOM", ncls = 6, BIC.check = T, mic = T, maxiter = 300) -> unSOM
