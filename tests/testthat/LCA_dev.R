rm(list = ls())
dat <- read.csv("tests/testthat/sampleData/J15S500.csv")
tmp <- Exametrika::dataFormat(dat)

ncls <- 5
testlength <- NCOL(tmp$U)
beta1 <- 1
beta2 <- 1


const <- exp(-testlength)
testEll <- -1 / const
oldtestEll <- -2 / const
itemEll <- rep(testEll / testlength, testlength)


classRefMat <- t(matrix(rep(1:ncls / (ncls + 1), testlength), ncol = testlength))

s <- 1
den <- array(1, ncls)
for (cl in 1:ncls) {
  for (j in 1:testlength) {
    den[cl] <- den[cl] * classRefMat[j, cl]^tmp$U[s, j] * (1 - classRefMat[j, cl])^(1 - tmp$U[s, j])
  }
}

plot(den / sum(den), type = "b", ylim = c(0, 0.8))

llmat <- tmp$U %*% log(classRefMat + const) + (tmp$Z * (1 - tmp$U)) %*% log(1 - classRefMat + const)
exp_llmat <- exp(llmat)
postDist <- exp_llmat / rowSums(exp_llmat)

correct_cls <- t(postDist) %*% tmp$U
incorrect_cls <- t(postDist) %*% (tmp$Z * (1 - tmp$U))

classRefMat <- (correct_cls + beta1 - 1) / (correct_cls + incorrect_cls + beta1 + beta2 - 2)

itemEll <- colSums(correct_cls * log(classRefMat + const) + incorrect_cls * log(1 - classRefMat + const))
testEll <- sum(itemEll)

# EM-loop ---------------------------------------------------------


dat <- read.csv("tests/testthat/sampleData/J15S500.csv")
tmp <- Exametrika::dataFormat(dat)

ncls <- 5
testlength <- NCOL(tmp$U)
beta1 <- 1
beta2 <- 1


const <- exp(-testlength)
testEll <- -1 / const
oldtestEll <- -2 / const
itemEll <- rep(testEll / testlength, testlength)


classRefMat <- matrix(rep(1:ncls / (ncls + 1), testlength), ncol = testlength)

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
  print(testEll)

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

t(classRefMat)

# 5.4 Main Output -------------------------------------------------

TRP <- classRefMat %*% tmp$w
TRP

## メンバーの所属確率
bMax <- matrix(rep(apply(postDist,1,max),ncls),ncol=ncls)
cls01 <- sign(postDist - bMax)+1
