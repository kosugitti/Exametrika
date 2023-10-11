rm(list = ls())
library(Exametrika)
dat <- J35S515
tmp <- dataFormat(dat)

# initialize ------------------------------------------------------

nfld <- 11
ncls <- 15
model <- 1
testlength <- NCOL(tmp$U)
nobs <- NROW(tmp$U)
const <- exp(-testlength)
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

Fil <- diag(rep(1, ncls))

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

mic <- F


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

  max_log_lljf <- apply(lljf, 1, max)
  log_lljf_adj <- lljf - max_log_lljf
  log_fldmemb <- log_lljf_adj - log(rowSums(exp(log_lljf_adj)))
  fldmemb <- exp(log_fldmemb)

  cfr <- t(fldmemb) %*% t(tmp$U) %*% smoothed_memb
  ffr <- t(fldmemb) %*% t(tmp$Z * (1 - tmp$U)) %*% smoothed_memb
  oldPiFR <- PiFR
  PiFR <- (cfr + beta1 - 1) / (cfr + ffr + beta1 + beta2 - 2)
  if (mic) {
    PiFR <- t(apply(PiFR, 1, sort))
  }
  has_nan <- any(is.nan(cfr))
  if (has_nan) {
    stop("The calculation diverged during the process. Please adjust your settings appropriately")
  }
  testell <- sum(cfr * log(PiFR + const) + ffr * log(1 - PiFR + const))
  print(paste("iter", emt, " logLik", testell))
  if (testell - oldtestell <= 0) {
    PiFR <- oldPiFR
    break
  }
}



cls <- apply(clsmemb, 1, which.max)
fld <- apply(fldmemb, 1, which.max)
fldmemb01 <- sign(fldmemb - apply(fldmemb, 1, max)) + 1
flddist <- colSums(fldmemb01)
clsmemb01 <- sign(clsmemb - apply(clsmemb, 1, max)) + 1
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

FRP <- PiFR
if (model == 1) {
  msg1 <- "Class"
} else {
  msg1 <- "Rank"
}
FRP <- PiFR
print(FRP)
print(paste0(msg1, 1:ncls))
colnames(FRP) <- paste0(msg1, 1:ncls)
rownames(FRP) <- paste0("Field", 1:nfld)
# item location index
Beta <- apply(abs(FRP - 0.5), 1, which.min)
B <- FRP[cbind(1:nfld, Beta)]
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
TRPlag <- TRP[2:ncls]
TRPmic <- sum(TRPlag[1:(ncls-1)] - TRP[1:(ncls - 1)] < 0, na.rm = TRUE)
FRPmic <- sum(abs(C))
SOACflg <- WOACflg <- FALSE
if(TRPmic == 0){
  WOACflg <- TRUE
  if(FRPmic == 0){
    SOACflg <- TRUE
  }
}
if (SOACflg & WOACflg) {
  message("Strongly ordinal alignment condition was satisfied.")
}
if (!SOACflg & WOACflg) {
  message("Weakly ordinal alignment condition was satisfied.")
}


### Model Fit
cfr <- t(fldmemb) %*% t(tmp$U) %*% clsmemb
ffr <- t(fldmemb) %*% t(tmp$Z * (1 - tmp$U)) %*% clsmemb
testell <- sum(cfr * log(PiFR + const) + ffr * log(1 - PiFR + const))
nparam <- ifelse(model == 1, ncls * nfld, sum(diag(Fil)) * nfld)
FitIndices <- TestFit(tmp$U, tmp$Z, testell, nparam)
colSums(clsmemb)



# Plot ------------------------------------------------------------

par(mfrow = c(1, 2))
stepx <- 300 / testlength
stepy <- 600 / nobs
## Original Data
plot(0, 0,
  type = "n", xlim = c(0, 300), ylim = c(0, 600),
  xlab = "", ylab = "", xaxt = "n", yaxt = "n",
  frame.plot = TRUE,
  main = "Original Data"
)

for (i in 1:nobs) {
  for (j in 1:testlength) {
    x1 <- (j - 1) * stepx
    y1 <- (i - 1) * stepy
    x2 <- j * stepx
    y2 <- i * stepy
    if (tmp$U[i, j] == 1) {
      rect(x1, y1, x2, y2, col = "black")
    }
  }
}

## Clusterd Plot
plot(0, 0,
  type = "n", xlim = c(0, 300), ylim = c(0, 600),
  xlab = "", ylab = "", xaxt = "n", yaxt = "n",
  frame.plot = TRUE,
  main = "Clusterd Plot"
)


cls <- apply(clsmemb, 1, which.max)
fld <- apply(fldmemb, 1, which.max)

sorted <- tmp$U[, order(fld, decreasing = FALSE)]
sorted <- sorted[order(cls, decreasing = TRUE), ]
for (i in 1:nobs) {
  for (j in 1:testlength) {
    x1 <- (j - 1) * stepx
    y1 <- (i - 1) * stepy
    x2 <- j * stepx
    y2 <- i * stepy
    if (sorted[i, j] == 1) {
      rect(x1, y1, x2, y2, col = "black")
    }
  }
}

vl <- cumsum(table(sort(fld)))
for (i in 1:(nfld - 1)) {
  lines(x = c(vl[i] * stepx, vl[i] * stepx), y = c(0, 600), col = "red")
}
hl <- nobs - cumsum(table(sort(cls)))
for (j in 1:(ncls - 1)) {
  lines(x = c(0, 300), y = c(hl[j] * stepy, hl[j] * stepy), col = "red")
}

# FieldAnalysis ---------------------------------------------------
dat <- J35S515
tmp <- dataFormat(dat)
Bic <- Biclustering(tmp, ncls = 11, nfld = 12, method = "B", mic = T)
plot(Bic,type="FRP",nr=3,nc=4)
Bic <- Biclustering(tmp, ncls = 15, nfld = 11, method = "R", mic = F)
Bic
Bic$TestFitIndices$BIC

digits <- 5

FieldAnalysis(Bic) -> unko
unko
unko$FieldAnalysisMatrix


dat <- J35S515
tmp <- dataFormat(dat)
minBIC <- 100
bestC <- 100
bestF <- 100
retMat <- matrix(NA,nrow=11,ncol=7)
for (ncls in 11:17) {
  for (nfld in 5:15) {
    print(paste("ncls:", ncls, "nfld:", nfld))
    Bic <- Biclustering(tmp, ncls = ncls, nfld = nfld, method = "R", mic = F)
    bic <- Bic$TestFitIndices$BIC
    if(!Bic$WOACflg){bic <- 0}
    print(paste("BIC", bic))
    if (bic < minBIC) {
      minBIC <- bic
      bestC <- ncls
      bestF <- nfld
    }
    retMat[nfld-4,ncls-10] <- bic
  }
}

colnames(retMat) <- paste0("nRank",11:17)
rownames(retMat) <- paste0("nField",5:15)
retMat



print(paste("Best nField", bestF))
print(paste("Best nClass", bestC))
print(minBIC)
Bic$TestFitIndices



# Confirmatory ----------------------------------------------------

conf <- read.csv("develop/mtmk14forVer13/FixFieldBiclus.csv")
conf_vec <- conf[,2]
conf <- conf_vec


Biclustering(tmp,ncls=6,nfld=1,conf=conf_vec,mic=F,method="R") -> jjj
jjj

plot(jjj,"LRD",nc=1,nr=1)

jjj$WOACflg
jjj$TRP
plot(jjj,"Array")
jjj$FRP

TRPlag <- jjj$FRP[2:6]
TRPlag
