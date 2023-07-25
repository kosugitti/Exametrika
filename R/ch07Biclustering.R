#' @title Biclustering and Ranklustering
#' @description
#' performs biclustering, rankclustering, and their confirmative models.
#' @param U U is either a data class of Exametrika, or raw data. When raw data is given,
#' it is converted to the Exametrika class with the [dataFormat] function.
#' @param ncls number of classes
#' @param nfld number of fields
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param mic Monotonic increasing IRP option. The default is FALSE.
#' @param method Specify either "B"iclustering or "R"unklustering.
#' #' @param maxiter Maximum number of iterations. default is 100.
#' @return
#' \describe{
#'  \item{nobs}{Sample size. The number of rows in the dataset.}
#'  \item{testlength}{Length of the test. The number of items included in the test.}
#'  \item{Nclass}{number of classes you set}
#'  \item{BRM}{Bicluster Reference Matrix}
#'  \item{FRP}{Field Reference Profile}
#'  \item{FRPIndex}{Index of FFP includes the item location parameters B and Beta,
#'  the slope parameters A and Alpha, and the monotonicity indices C and Gamma.}
#'  \item{TRP}{Test Reference Profile}
#'  \item{FMP}{Field Membership Profile}
#'  \item{Students}{Rank Membership Profile matrix.The s-th row vector of \eqn{\hat{M}_R}, \eqn{\hat{m}_R}, is the
#' rank membership profile of Student s, namely the posterior probability distribution representing the student's
#' belonging to the respective latent classes. It also includes the rank with the maximum estimated membership probability,
#' as well as the rank-up odds and rank-down odds.}
#'  \item{LRD}{Latent Rank Distribution. see also [plot.Exametrika]}
#'  \item{LFD}{Latent Field Distribuiton. see also [plot.Exametrika]}
#'  \item{RMD}{Rank Membership Distribution.}
#'  \item{ItemFitIndices}{Fit index for each item.See also [ModelFit]}
#'  \item{TestFitIndices}{Overall fit index for the test.See also [ModelFit]}
#' }

Biclustering <- function(U, ncls = 2, nfld = 2,
                         Z = NULL, w = NULL, na = NULL,
                         method = "B",
                         mic = FALSE,
                         maxiter = 100){
  # data format
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  U <- tmp$U * tmp$Z
  testlength <- NCOL(tmp$U)
  nobs <- NROW(tmp$U)
  const <- exp(-testlength)

  if (method == "B" | method == "Biclustering") {
    print("Biclustering is chosen.")
    model <- 1
  }else if(method = "R" | method == "Ranklustering"){
    print("Ranklustering is chosen.")
    model <- 2
  }else{
    stop("The method must be selected as either Biclustering or Ranklustering.")
  }

  if (ncls < 2 | ncls > 20) {
    stop("Please set the number of classes to a number between 2 and less than 20.")
  }

  ### Algorithm
  beta1 <- 1
  beta2 <- 1
  testell <- -1 / const
  oldtestell <- -2 / const
  emt <- 0
  maxemt <- 100

  fld0 <- ceiling(1:testlength / (testlength / nfld))
  crr_order <- order(crr(dat), decreasing = TRUE)
  fld <- fld0[match(1:testlength, crr_order)]
  fldmemb <- matrix(0, nrow = testlength, ncol = nfld)
  for (i in 1:testlength) {
    fldmemb[i, fld[i]] <- 1
  }

  PiFR <- matrix(NA, nrow = nfld, ncol = ncls)
  for (i in 1:nfld) {
    for (j in 1:ncls) {
      PiFR[i, j] <- (nfld - i + j) / (nfld + ncls)
    }
  }

  if(model==1){
    Fil <- diag(rep(1, ncls))
  }else{
    f0 <- ifelse(ncls < 5, 1.05 - 0.05 * ncls,
                 ifelse(ncls < 10, 1.00 - 0.04 * ncls,
                        0.80 - 0.02 * ncls
                 )
    )
    f1 <- diag(0, ncls)
    f1[row(f1) == col(f1) - 1] <- (1 - f0) / 2
    Fil <- diag(rep(f0, ncls)) + t(f1) + f1
    Fil[, 1] <- Fil[, 1] / sum(Fil[, 1])
    Fil[, ncls] <- Fil[, ncls] / sum(Fil[, ncls])
  }


  FLG <- TRUE
  while (FLG) {
    if (testell - oldtestell < 1e-4 * abs(oldtestell)) {
      FLG <- FALSE
      break
    }
    if (emt == maxemt) {
      FLG <- FALSE
      message("max iteration")
    }
    emt <- emt + 1
    oldtestell <- testell
    csr <- tmp$U %*% fldmemb
    fsr <- (tmp$Z * (1 - tmp$U)) %*% fldmemb
    llsr <- csr %*% log(PiFR + const) + fsr %*% log(1 - PiFR + const)
    minllsr <- apply(llsr, 1, min)
    expllsr <- exp(llsr - minllsr)
    clsmemb <- round(expllsr / rowSums(expllsr), 1e8)

    smoothed_memb <- clsmemb %*% Fil

    cjr <- t(tmp$U) %*% smoothed_memb
    fjr <- t(tmp$Z * (1 - tmp$U)) %*% smoothed_memb
    lljf <- cjr %*% log(t(PiFR) + const) + fjr %*% log(t(1 - PiFR) + const)

    max_log_lljf <- apply(lljf,1,max)
    log_lljf_adj <- lljf - max_log_lljf
    log_fldmemb <- log_lljf_adj - log(rowSums(exp(log_lljf_adj)))
    fldmemb <- exp(log_fldmemb)

    cfr <- t(fldmemb) %*% t(tmp$U) %*% smoothed_memb
    ffr <- t(fldmemb) %*% t(tmp$Z * (1 - tmp$U)) %*% smoothed_memb
    oldPiFR <- PiFR
    if(mic){PiFR <- t(apply(PiFR,1,sort))}
    PiFR <- (cfr + beta1 - 1) / (cfr + ffr + beta1 + beta2 - 2)
    testell <- sum(cfr * log(PiFR + const) + ffr * log(1 - PiFR + const))
    print(paste("iter", emt, " logLik", testell))
    if (testell - oldtestell <= 0) {
      PiFR <- oldPiFR
      break
    }
  }

  cls <- apply(clsmemb, 1, which.max)
  fld <- apply(fldmemb, 1, which.max)
  fldmemb01 <- sign(fldmemb - apply(fldmemb,1,max))+1
  flddist <- colSums(fldmemb01)
  clsmemb01 <- sign(clsmemb - apply(clsmemb,1,max))+1
  clsdist <- colSums(clsmemb01)
  TRP <- colSums(PiFR * flddist)
  StudentRank <- clsmemb
  rownames(StudentRank) <- tmp$ID
  RU <- ifelse(cls + 1 > ncls, NA, cls + 1)
  RD <- ifelse(cls - 1 < 1, NA, cls - 1)
  RUO <- StudentRank[cbind(1:nobs, RU)] / StudentRank[cbind(1:nobs, cls)]
  RDO <- StudentRank[cbind(1:nobs, RD)] / StudentRank[cbind(1:nobs, cls)]
  StudentRank <- cbind(StudentRank, cls, RUO, RDO)
  colnames(StudentRank) <- c(
    paste("Membership", 1:ncls), "Estimate",
    "Rank-Up Odds", "Rank-Down Odds"
  )

  FRP = PiFR
  colnames(FRP) <- paste0("Rank",1:ncls)
  rownames(FRP) <- paste0("Field",1:nfld)
  # item location index
  Beta <- apply(abs(FRP - 0.5), 1, which.min)
  B <- FRP[cbind(1:nfld,Beta)]
  # item slope index and item monotonicity index
  A <- Alpha <- rep(NA, nfld)
  C <- Gamma <- rep(0, nfld)
  for (i in 1:nfld) {
    vec <- FRP[i, ]
    lags <- vec - c(NA, vec[1:(ncls - 1)])
    A[i] <- max(lags, na.rm = T)
    Alpha[i] <- which.max(lags) - 1
    C[i] <- sum(lags[lags < 0], na.rm = T)
    if (C[i] != 0) {
      Gamma[i] <- (length(lags[lags < 0]) - 1) / (ncls - 1)
    }
  }
  FRPIndex <- cbind(Alpha, A, Beta, B, Gamma, C)
  TRPlag <- TRP[2:nfld]
  SOAC <- sum(TRPlag - TRP[1:(nfld-1)]<0)
  WOAC <- sum(C)
  if (SOAC ==0 & WOAC ==0) {
    message("Strongly ordinal alignment condition was satisfied.")
  }
  if(SOAC ==0 * WPAC !=0){
    message("Weakly ordinal alignment condition was satisfied.")
  }
  ### Model Fit
  cfr <- t(fldmemb) %*% t(tmp$U) %*% clsmemb
  ffr <- t(fldmemb) %*% t(tmp$Z * (1-tmp$U)) %*% clsmemb
  testell <-  sum(cfr * log(PiFR+const) + ffr * log(1-PiFR+const))
  nparam <- ifelse(model==1,ncls*nfld,sum(diag(Fil))*nfld)
  FitIndices <- ModelFit(tmp$U,tmp$Z,testell,nparam)


  ret <- structure(list(
    testlength = testlength,
    nobs = nobs,
    Nclass = ncls,
    N_Cycle = emt,
    BRM = PiFR,
    LFD = flddist,
    LRD = clsdist,
    FRP = FRP,
    FRPIndex = FRPIndex,
    TRP = TRP,
    FMP = fldmemb,
    Students = StudentRank,
    ItemFitIndices = FitIndices$item,
    TestFitIndices = FitIndices$test
  ), class = c("Exametrika", "LRA"))

}
