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
## クラスごとのメンバ数
colSums(cls01)
colSums(postDist)

## IRP グラフ
library(tidyverse)
classRefMat |> t() |> as.data.frame() |>
  rownames_to_column("item") |>
  pivot_longer(-item) |>
  mutate(class = str_extract(name,pattern="[0-9]") |> as.numeric()) |>
  ggplot(aes(x=class,y=value))+geom_line()+geom_point()+facet_wrap(~item,ncol=3)

## CLASS reference vector
classRefMat |> t() |> as.data.frame() |>
  rownames_to_column("item") |>
  pivot_longer(-item) |>
  mutate(class = str_extract(name,pattern="[0-9]") |> as.numeric()) |>
  mutate(item = as.factor(item)) |>
  mutate(itemID = as.numeric(item)) |>
  ggplot(aes(x=itemID,y=value,color=as.factor(class)))+geom_line()+geom_point()

## CLASS membership profiles
postDist |> as.data.frame() |>
  rowid_to_column("Students") |>
  pivot_longer(-Students) |>
  filter(Students < 16) |>
  mutate(class = str_extract(name,pattern="[0-9]") |> as.numeric()) |>
  ggplot(aes(x=class,y=value))+geom_line()+geom_point()+facet_wrap(~Students,ncol=3)


# 5.5 Model Fit ---------------------------------------------------

ell_A <- itemEll

pj <- Exametrika::crr(tmp$U)
pj_mat <- matrix(rep(pj,nrow(tmp$U)),ncol=testlength,byrow=T)
ell_N <- colSums(tmp$U * log(pj_mat) + (tmp$Z * (1-tmp$U)) * log(1-pj_mat))

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
Exametrika::Model_Fit(ell_A,ell_B,ell_N,df_A,df_B,nobs)

testEllmodel <- sum(ell_A)
testEllbench <- sum(ell_B)
testEllNull <- sum(ell_N)

Exametrika::Model_Fit(ell_A = testEllmodel,ell_B=testEllbench,ell_N = testEllNull,
                      df_A = df_A * testlength,
                      df_B = df_B * testlength,nobs)
