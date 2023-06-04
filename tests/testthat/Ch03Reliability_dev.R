rm(list = ls())
library(tidyverse)

# Ch03 Classical Test Theory ----------------------------------------------

## Alpha Coefficient(Tau-equivalent Measure)

dat <- read_csv("tests/testthat/sampleData/J20S400.csv") %>%
  mutate(Student = as.factor(Student))

tmp <- Exametrika::dataFormat(dat, na = -99)
U <- tmp$U

# Alpha -------------------------------------------------------------------


alphaCoefficient <- function(V) {
  J <- NCOL(V)
  alphaConst <- J / (J - 1)
  tr <- sum(diag(V))
  alpha <- (sum(V) - tr) / (sum(V)) * alphaConst
  return(alpha)
}

V <- cov(tmp$U, use = "pairwise")
alphaCoefficient(V)
V <- PhiCoefficient(U = tmp$U)
alphaCoefficient(V)
V <- TetrachoricCorrelationMatrix(U = tmp$U)
alphaCoefficient(V)

alphaIfDel <- function(V) {
  J <- NCOL(V)
  alphaIfDel <- vector(length = J)
  for (j in 1:J) {
    mat <- V[-j, -j]
    alphaIfDel[j] <- alphaCoefficient(mat)
  }
  return(alphaIfDel)
}
alphaIfDel(V)


# Omega -------------------------------------------------------------------

V <- cov(tmp$U, use = "pairwise")
V <- Exametrika::TetrachoricCorrelationMatrix(tmp$U)

Esys <- eigen(V)
Eval <- Esys$values[1]
Evec <- Esys$vectors[, 1]
Load <- sqrt(Eval) * Evec
V <- Load %*% t(Load)
diag(V) <- 0

aj2 <- sum(Load)^2
e2 <- sum(1 - diag(Load %*% t(Load)))
aj2 / (aj2 + e2)
psych::omega(V)
# psych::omega(tmp$U)
result.fa <- psych::fa.poly(tmp$U, nfactors = 1)
faLoads <- result.fa$fa$loadings %>% as.vector()
mean(Load / faLoads)
e2 <- 1 - result.fa$fa$communality

sum(faLoads^2) / (sum(faLoads^2) + sum(e2))


omegaCoefficient <- function(V) {
  J <- NCOL(V)
  offdiagonal <- matrix(1, ncol = ncol(V), nrow = nrow(V)) - diag(nrow = nrow(V))
  lam <- runif(J)
  # Define the function to minimize
  objective_function <- function(lam) {
    lammat <- outer(lam, lam)
    sum(offdiagonal * (lammat - V)^2)
  }
  # optimize
  result <- optim(lam, objective_function, method = "BFGS")
  # Error check
  if (result$convergence != 0) {
    stop(result$message)
  }
  lamest <- result$par
  Numerator <- sum(lamest)^2
  Denominator <- Numerator + sum(diag(V - outer(lamest, lamest)))
  omega <- Numerator / Denominator
  return(omega)
}

V <- cov(tmp$U, use = "pairwise")
omegaCoefficient(V)
V <- PhiCoefficient(U = tmp$U)
omegaCoefficient(V)
V <- TetrachoricCorrelationMatrix(U = tmp$U)
omegaCoefficient(V)

eps <- 1e-15
FLG <- TRUE
iterMax <- 100
iter <- 0
Vtmp <- V
Ltmp <- rep(1,20)
while(FLG & iter< iterMax){
  iter <- iter + 1
  Esys <- eigen(Vtmp)
  Eval <- Esys$values[1]
  Evec <- Esys$vectors[, 1]
  Load <- sqrt(Eval) * Evec
  Load <- sign(sum(sign(Load))) * Load
  Vtmp2 <- Load %*% t(Load)
  print(paste(iter,Eval))
  if(sum((Load-Ltmp)^2) > eps){
    Vtmp <- V
    Ltmp <- Load
    diag(Vtmp) <- diag(Vtmp2)
  }else{
    FLG = FALSE
  }
}

