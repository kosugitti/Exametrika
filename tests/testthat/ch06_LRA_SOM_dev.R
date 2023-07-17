rm(list = ls())
library(tidyverse)
dat <- read_csv("tests/testthat/sampleData/J15S500.csv")
library(Exametrika)
tmp <- dataFormat(dat, na = -99)

testlength <- NCOL(tmp$U)
samplesize <- NROW(tmp$U)
const <- exp(-testlength)

nclus <- 6
clsRefMat <- matrix(rep(1:nclus / (nclus + 1), testlength), ncol = testlength)

## 先にhの勾配つけ得た行列を作っておく（最大反復回数まで）
maxT <- 1000
alpha1 <- 1
alphaT <- 0.01
sigma1 <- 1
sigmaT <- 0.12

alpha_list <- ((maxT - 1:maxT) * alpha1 + (1:maxT - 1) * alphaT) / (maxT - 1)
sigma_list <- ((maxT - 1:maxT) * sigma1 + (1:maxT - 1) * sigmaT) / (maxT - 1)

kappa1 <- 0.01
kappaT <- 0.0001

kappa_list <- ((maxT - 1:maxT) * kappa1 + (1:maxT - 1) * kappaT) / (nclus*(maxT - 1))

prior_list <- rep(1 / nclus, nclus)

r_list <- seq(-nclus + 1, nclus - 1)
hhhmat <- array(NA, c(maxT, length(r_list)))
for (t in 1:maxT) {
  hhhmat[t, ] <- alpha_list[t] * nclus / samplesize * exp(-(r_list)^2 / (2 * nclus^2 * sigma_list[t]^2))
}

RefMat <- t(clsRefMat)
oldRefMat <- RefMat
record <- array(maxT)
somt <- 0
### 反復開始
FLG <- TRUE
while (FLG) {
  somt <- somt + 1
  if (somt == maxT) {
    FLG <- FALSE
  }
  loglike <- 0
  ## 並べ替え
  set.seed(sum(tmp$U) + somt)
  is <- order(runif(samplesize, 1, 100))
  for (s in 1:samplesize) {
    ss <- is[s]
    mlrank <- tmp$U[ss, ] %*% log(RefMat + const) + (1 - tmp$U[ss, ]) %*% log(1 - RefMat + const) + log(prior_list)
    winner <- which.max(mlrank)
    loglike <- loglike + mlrank[winner]
    hhh <- matrix(rep(hhhmat[somt, (nclus + 1 - winner):(2 * nclus - winner)], testlength),
      nrow = testlength, byrow = T
    )

    RefMat <- RefMat + hhh * (tmp$U[ss, ] - RefMat)
    prior_list <- prior_list + (kappa_list[somt] / nclus)
    prior_list[winner] <- prior_list[winner] - kappa_list[somt]
    prior_list[prior_list > 1] <- 1
    prior_list[prior_list < const] <- const
  }
  # record[somt] <- loglike
  llmat <- tmp$U %*% t(log(t(RefMat) + const)) + (tmp$Z * (1 - tmp$U)) %*%
    t(log(1 - t(RefMat) + const))
  expllmat <- exp(llmat)
  postdist <- expllmat / rowSums(expllmat)

  correctcls <- t(postdist) %*% tmp$U
  incorrectcls <- t(postdist) %*% (tmp$Z * (1 - tmp$U))
  item_ell <- correctcls * log(t(RefMat) + const) + incorrectcls * (log(1 - t(RefMat) + const))
  item_ell <- colSums(item_ell)

  diff <- sum(abs(oldRefMat - RefMat))
  record[somt] <- diff
  oldRefMat <- RefMat
  if (diff / nclus / testlength < 1e-3) {
    FLG <- FALSE
  }
}
somt
plot(record)

llmat <- tmp$U %*% t(log(t(RefMat) + const)) + (tmp$Z * (1 - tmp$U)) %*%
  t(log(1 - t(RefMat) + const))
expllmat <- exp(llmat)
postdist <- expllmat / rowSums(expllmat)

correctcls <- t(postdist) %*% tmp$U
incorrectcls <- t(postdist) %*% (tmp$Z * (1 - tmp$U))
item_ell <- correctcls * log(t(RefMat) + const) + incorrectcls * (log(1 - t(RefMat) + const))
item_ell <- colSums(item_ell)
sum(item_ell)
