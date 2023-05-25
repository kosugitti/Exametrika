rm(list = ls())
library(tidyverse)

# DataMatrix --------------------------------------------------------------

dat <- read_csv("sampleData/J20S400.csv", na = "-99") %>%
  mutate(Student = as.factor(Student))

summary(dat)

U <- as.matrix(dat[, -1])
Z <- ifelse(is.na(U), 0, 1)

## NAに数値を与えなければならない
U <- ifelse(is.na(U), -99, U)
## 計算に必要な数字を準備
S <- nrow(U)
J <- ncol(U)
OneJ <- rep(1, length = J)
OneS <- rep(1, length = S)

# Student Analysis --------------------------------------------------------

## NRS:Number-Right Score
W <- rep(1, length = ncol(U))
t <- (Z * U) %*% W
tW <- (Z * U) %*% W

### ch03CTT output
mean(t)
var(t)
sd(t)
moments::skewness(t)
moments::kurtosis(t)
min(t)
max(t)
## Passage Rate
r <- t / (Z %*% OneJ)
rW <- tW / (Z %*% W)

## Standardized Score
rBar <- (t(OneS) %*% r) / S
rBar <- rBar %>% array(dim = 1)
Var_r <- t(r - c(rBar) * OneS) %*% (r - c(rBar) * OneS) / (S - 1)
rBarW <- (t(OneS) %*% rW) / S
Var_rW <- t(rW - c(rBarW) * OneS) %*% (rW - c(rBarW) * OneS) / (S - 1)
Xi_sW <- (rW - c(rBarW)) / sqrt(c(Var_rW))

Xi <- (r - c(rBar) * OneS) / (sqrt(c(Var_r)) * OneS)
Xi_W <- (rW - c(rBarW) * OneS) / (sqrt(c(Var_rW)) * OneS)

## Parcentile Rank
## Stanine
pbs <- cumsum(c(0.04, 0.07, 0.12, 0.17, 0.20, 0.17, 0.12, 0.07, 0.04))
quantile(t, pbs)

# Single Item Analysis ----------------------------------------------------

### Number of Respondents
apply(Z, 2, sum)
### CRR:Correct Response Rate/ index of Easiness
p <- t(Z * U) %*% OneS / t(Z) %*% OneS
p_W <- W * p
### Item Odds
o <- p / (1 - p)
### Item Threshold/ index of Difficulty
Tau <- qnorm(1 - p)
### Item Entropy
E <- -p * log(p, base = 2) - (1 - p) * log(1 - p, base = 2)


# Interitem Correct Reponse Rate Analysis ---------------------------------

### JCRR:Joint correct response rate
S_jk <- t(Z) %*% Z
S_bar_jk <- t(Z * U) %*% (Z * U)
p_jk <- S_bar_jk / S_jk
## 別解
P_J <- t(Z * U) %*% (Z * U) / (t(Z) %*% Z)

### CCRR:Conditional correct response rate
P_C <- P_J / (p %*% t(OneJ))

### Item Lift
P_L <- P_C / (OneJ %*% t(p))

### Mutual Information: degree of interdependence between two items
## 正答した人U
## 誤答した人1-U
## Jに正答してKに正答した人
S_11 <- t(Z * U) %*% (Z * U)
## Jに正答してKに誤答した人
S_10 <- t(Z * U) %*% (Z * (1 - U))
## Jに誤答してKに正答した人
S_01 <- t(Z * (1 - U)) %*% (Z * U)
## 両方に誤答した人
S_00 <- t(Z * (1 - U)) %*% (Z * (1 - U))
### すべて割合に
P_11 <- S_11 / (t(Z) %*% Z)
P_10 <- S_10 / (t(Z) %*% Z)
P_01 <- S_01 / (t(Z) %*% Z)
P_00 <- S_00 / (t(Z) %*% Z)
### すべてItem Liftに
L_11 <- P_11 / (p %*% t(p))
L_10 <- P_10 / (p %*% t(1 - p))
L_01 <- P_01 / ((1 - p) %*% t(p))
L_00 <- P_00 / ((1 - p) %*% t(1 - p))
### MIに
MI <- P_00 * log(L_00, base = 2) + P_01 * log(L_01, base = 2) +
  P_10 * log(L_10, base = 2) + P_11 * log(L_11, base = 2)
diag(MI) <- diag(P_00 * log(L_00, base = 2) + P_11 * log(L_11, base = 2))


# Interitem Correlation Analysis ------------------------------------------
## Phi Coefficient
C <- t(Z * (U - OneS %*% t(p))) %*% (Z * (U - OneS %*% t(p))) / (t(Z) %*% Z - OneJ %*% t(OneJ))
v <- diag(C)
phi <- C / sqrt(v) %*% t(sqrt(v))

## Tetrachoric Coefficient
psych::tetrachoric(x = c(20, 20, 6, 40))

### Standard Bivariate Normal Distribution Func
library(mvtnorm)
tau_j <- 0.5
tau_k <- 0.6
rho <- 0.9
Upper <- c(tau_j, tau_k)
Sigma <- matrix(c(1, rho, rho, 1), ncol = 2)
mvtnorm::pmvnorm(lower = -Inf, Upper, mean = c(0, 0), Sigma)



sBVN_11 <- function(rho, tau_j, tau_k) {
  tmp <- mvtnorm::pmvnorm(
    lower = c(tau_j, tau_k), upper = c(Inf,Inf),
    mean = c(0, 0), corr = matrix(c(1, rho, rho, 1), ncol = 2),
    keepAttr = FALSE,
    algorithm = Miwa
  )
  return(tmp)
}
sBVN_01 <- function(rho, tau_j, tau_k) {
  tmp <- mvtnorm::pmvnorm(
    lower = c(-Inf, tau_k), upper = c(tau_j, Inf),
    mean = c(0, 0), corr = matrix(c(1, rho, rho, 1), ncol = 2),
    keepAttr = FALSE,
    algorithm = Miwa

  )
  return(tmp)
}
sBVN_10 <- function(rho, tau_j, tau_k) {
  tmp <- mvtnorm::pmvnorm(
    lower = c(tau_j, -Inf), upper = c(Inf, tau_k),
    mean = c(0, 0), corr = matrix(c(1, rho, rho, 1), ncol = 2),
    keepAttr = FALSE,
    algorithm = Miwa


  )
  return(tmp)
}
sBVN_00 <- function(rho, tau_j, tau_k) {
  tmp <- mvtnorm::pmvnorm(
    lower = c(-Inf,-Inf), upper = c(tau_j, tau_k),
    mean = c(0, 0), corr = matrix(c(1, rho, rho, 1), ncol = 2),
    keepAttr = FALSE,
    algorithm = Miwa

  )
  return(tmp)
}

# 展開後 ---------------------------------------------------------------------

sBVN_11_2 <- function(rho,tau_j,tau_k){
  tmp <- mvtnorm::pmvnorm(upper=c(-tau_j,-tau_k),corr=matrix(c(1,rho,rho,1),ncol=2))
  return(tmp)
}
sBVN_01_2 <- function(rho,tau_j,tau_k){
  tmp <- pnorm(tau_j) - mvtnorm::pmvnorm(upper=c(tau_j,tau_k),corr=matrix(c(1,rho,rho,1),ncol=2))
  return(tmp)
}
sBVN_10_2 <- function(rho,tau_j,tau_k){
  tmp <- pnorm(tau_k) - mvtnorm::pmvnorm(upper=c(tau_j,tau_k),corr=matrix(c(1,rho,rho,1),ncol=2))
  return(tmp)
}
sBVN_00_2 <- function(rho,tau_j,tau_k){
  return(mvtnorm::pmvnorm(upper=c(tau_j,tau_k),corr=matrix(c(1,rho,rho,1),ncol=2)))
}

log_likelihood_phi <- function(rho, tau_j, tau_k, S_00, S_11, S_10, S_01) {
  S_00 * log(sBVN_00_2(rho, tau_j, tau_k)) + S_01 * log(sBVN_01_2(rho, tau_j, tau_k)) +
    S_10 * log(sBVN_10_2(rho, tau_j, tau_k)) + S_11 * log(sBVN_11_2(rho, tau_j, tau_k))
}

log_likelihood_phi(rho = 0.5, tau_j = -0.518, tau_k = 0.088, S_00 = 20, S_11 = 40, S_10 = 20, S_01 = 6)
log_likelihood_phi(rho = 0.0, tau_j = -0.518, tau_k = 0.088, S_00 = 20, S_11 = 40, S_10 = 20, S_01 = 6)

qnorm(1-(60/86))
qnorm(1-(46/86))

pnorm(0)
qnorm(0.5)

optimize(
  function(x) {
    log_likelihood_phi(
      rho = x, tau_j = qnorm(1-(60/86)), tau_k = qnorm(1-(46/86)),
      S_00 = 20, S_11 = 40, S_10 = 20, S_01 = 6
    )
  },
  interval = c(1, -1), maximum = T
)


optimize(
  function(x) {
    log_likelihood_phi(
      rho = x, tau_j = qnorm(1-(60/86)), tau_k = qnorm(1-(40/86)),
      S_00 = 20, S_11 = 40, S_10 = 20, S_01 = 6
    )
  },
  interval = c(1, -1), maximum = T
)


log_likelihood_phi(rho = 0.619, tau_j = qnorm(1-(60/86)), tau_k = qnorm(1-(46/86)),S_00 = 20, S_11 = 40, S_10 = 20, S_01 = 6)
log_likelihood_phi(rho = 0.598, tau_j = qnorm(1-(60/86)), tau_k = qnorm(1-(46/86)),S_00 = 20, S_11 = 40, S_10 = 20, S_01 = 6)


# テトラコリック相関を関数化 -----------------------------------------------------------
### 2x2データからロウデータをつくる
tenkai <- function(f)
{
  list(x=rep(row(f), f), y=rep(col(f), f))
}

ret <- tenkai(matrix(c(20, 6, 20, 40),ncol=2))
ret$x <- ret$x -1
ret$y <- ret$y -1
ret %>% as.data.frame() %>% table()
x <- ret$x
y <- ret$y

### 自作関数
tetrachoricCorrelation <- function(x,y){
  # data format check
  if(length(x) != sum(x==0|x==1)){stop("X should be 1/0.")}
  if(length(y) != sum(y==0|y==1)){stop("Y should be 1/0.")}
  if(length(x) != length(y)){stop("The length of X and Y are different.")}
  # count 2x2 cells
  tbl <- table(x,y)
  S00 <- tbl[1,1]
  S10 <- tbl[2,1]
  S01 <- tbl[1,2]
  S11 <- tbl[2,2]
  # calcs tau
  tau_j <- qnorm(1-mean(x))
  tau_k <- qnorm(1-mean(y))
  ## BVN funcs
  BVN11 <- function(rho,tau_j,tau_k){
    mvtnorm::pmvnorm(upper=c(-tau_j,-tau_k),corr=matrix(c(1,rho,rho,1),ncol=2))
  }
  BVN01 <- function(rho,tau_j,tau_k){
    pnorm(tau_j) - mvtnorm::pmvnorm(upper=c(tau_j,tau_k),corr=matrix(c(1,rho,rho,1),ncol=2))
  }
  BVN10 <- function(rho,tau_j,tau_k){
    pnorm(tau_k) - mvtnorm::pmvnorm(upper=c(tau_j,tau_k),corr=matrix(c(1,rho,rho,1),ncol=2))
  }
  BVN00 <- function(rho,tau_j,tau_k){
    mvtnorm::pmvnorm(upper=c(tau_j,tau_k),corr=matrix(c(1,rho,rho,1),ncol=2))
  }
  ## LL
  log_likelihood_phi <- function(rho, tau_j, tau_k, S00, S11, S10, S01) {
    S00 * log(BVN00(rho, tau_j, tau_k)) + S01 * log(BVN01(rho, tau_j, tau_k)) +
      S10 * log(BVN10(rho, tau_j, tau_k)) + S11 * log(BVN11(rho, tau_j, tau_k))
  }
  ret <- optimize(
    function(x) {
      log_likelihood_phi(rho = x,tau_j, tau_k, S00, S11, S10, S01)
    },
    interval = c(1, -1), maximum = T
  )
  return(ret$maximum)
}

tetrachoricCorrelation(x,y)

tetrachoricCorrelationMatrix<- function(data){
  m <- ncol(data)
  mat <- matrix(NA,ncol=m,nrow=m)
  colnames(mat)<- colnames(data)
  rownames(mat) <- colnames(data)
  for(i in 1:(m-1)){
    for(j in (i+1):m){
      mat[i,j] <- tetrachoricCorrelation(x= data[,i],y=data[,j])
      mat[j,i] <- mat[i,j]
    }
  }
  diag(mat) <- 1
  return(mat)
}


# Item-Total Correlation --------------------------------------------------


