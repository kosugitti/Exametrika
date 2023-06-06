rm(list = ls())
library(tidyverse)

# MLE for Ability ---------------------------------------------------------
## IRT functions
LogisticModel <- function(a = 1, b, c = 0, d = 0, theta) {
  p <- c + (d - c) / (1 + exp(-a * (theta - b)))
  return(p)
}

## or
PL <- function(lambda, theta) {
  p <- lambda[3] + (lambda[4] - lambda[3]) / (1 + exp(-lambda[1] * (theta - lambda[2])))
  return(p)
}

## LLprob
LLtheta <- function(theta, Lambda, u, z) {
  # theta; Unknown parameter
  # Lambda; Item paramteres matrix
  # U; resp vector
  # Z; missing index vector
  ll <- 0
  for (j in 1:length(u)) {
    ll <- ll + z[j] * (u[j] * log(PL(Lambda[j, ], theta)) + (1 - u[j]) * log(1 - PL(Lambda[j, ], theta)))
  }
  return(ll)
}


## Data from Table 4.4
a <- rep(2, 9)
b <- seq(-2.4, 2.4, 0.6)
c <- rep(0, 9)
d <- rep(1, 9)
Lambda <- matrix(c(a, b, c, d), ncol = 4)

U <- matrix(c(
  1, 1, 0, 0, 0, 0, 0, 0, 0,
  1, 1, 1, 1, 0, 0, 0, 0, 0,
  1, 1, 1, 1, 1, 1, 1, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0,
  1, 1, 1, 1, 1, 1, 1, 1, 1
), nrow = 5, ncol = 9, byrow = T)

Z <- matrix(1, ncol = ncol(U), nrow = nrow(U))

x <- seq(-4, 4, 0.03)

df <- data.frame(theta = x) %>%
  mutate(
    s1 = LLtheta(theta = theta, Lambda = Lambda, u = U[1, ], z = Z[1, ]),
    s2 = LLtheta(theta = theta, Lambda = Lambda, u = U[2, ], z = Z[2, ]),
    s3 = LLtheta(theta = theta, Lambda = Lambda, u = U[3, ], z = Z[3, ])
  )
df %>%
  pivot_longer(-theta) %>%
  ggplot(aes(x = theta, y = value, color = name)) +
  geom_point()



## or
PL <- function(lambda, theta) {
  p <- lambda[3] + (lambda[4] - lambda[3]) / (1 + exp(-lambda[1] * (theta - lambda[2])))
  return(p)
}

## LLprob
LLtheta <- function(theta, Lambda, u, z) {
  # theta; Unknown parameter
  # Lambda; Item paramteres matrix
  # U; resp vector
  # Z; missing index vector
  ll <- 0
  for (j in 1:length(u)) {
    ll <- ll + z[j] * (u[j] * log(PL(Lambda[j, ], theta)) + (1 - u[j]) * log(1 - PL(Lambda[j, ], theta)))
  }
  return(ll)
}



optimize(
  function(x) {
    LLtheta(
      theta = x, Lambda = Lambda, u = U[1, ], z = Z[1, ]
    )
  },
  interval = c(-4, 4), maximum = T
)

# GPT4 refined!--------------------------------------------------------------------

PL <- function(lambda, theta) {
  p <- lambda[, 3] + (lambda[, 4] - lambda[, 3]) / (1 + exp(-lambda[, 1] * (theta - lambda[, 2])))
  return(p)
}

LLtheta <- function(theta, Lambda, u, z) {
  p <- PL(Lambda, theta)
  ll <- sum(z * (u * log(p) + (1 - u) * log(1 - p)))
  return(ll)
}

LLthetaMAP <- function(theta, Lambda, u, z) {
  p <- PL(Lambda, theta)
  ll <- sum(z * (u * log(p) + (1 - u) * log(1 - p))) - theta^2 / 2
  return(ll)
}



result <- mapply(
  function(u, z) {
    optimize(
      function(x) {
        LLthetaMAP(
          theta = x, Lambda = Lambda, u = u, z = z
        )
      },
      interval = c(-5, 5), maximum = TRUE
    )$maximum
  },
  split(U, seq(nrow(U))), split(Z, seq(nrow(Z))),
  SIMPLIFY = TRUE
)



# EAP ---------------------------------------------------------------------
pr <- function(theta, Lambda, u, z) {
  tmp <- 0
  for (j in 1:NROW(Lambda)) {
    p <- Lambda[j, 3] + (Lambda[j, 4] - Lambda[j, 3]) / (1 + exp(-Lambda[j, 1] * (theta - Lambda[j, 2])))
    tmp <- tmp + u[j] * log(p) + (1 - u[j]) * log(1 - p)
  }
  ll <- tmp - theta^2 / 2
  llexp <- exp(ll)
  return(llexp)
}

prMat <- function(theta, Lambda, u, z) {
  f <- function(x) {
    p <- Lambda[, 3] + (Lambda[, 4] - Lambda[, 3]) / (1 + exp(-Lambda[, 1] * (x - Lambda[, 2])))
    ll <- sum(u * log(p) + (1 - u) * log(1 - p)) - x^2 / 2
    llexp <- exp(ll)
    return(llexp)
  }
  f_vec <- Vectorize(f)
  return(f_vec(theta))
}

pracma::quadv(function(x) prMat(x, Lambda, u = U[1, ], z = Z[1, ]), -5, 5)
integrate(function(x) pr(theta = x, Lambda = Lambda, u = U[1, ], z = Z[1, ]), lower = -5, upper = 5)$value

for (s in 1:NROW(U)) {
  u <- U[s, ]
  z <- Z[s, ]
  denomi <- integrate(function(x) pr(theta = x, Lambda = Lambda, u = u, z = z), lower = -5, upper = 5)$value
  numer <- integrate(function(x) x * pr(theta = x, Lambda = Lambda, u = u, z = z), lower = -5, upper = 5)$value
  denomi2 <- pracma::quadv(function(x) prMat(x, Lambda, u = u, z = z), -5, 5)$Q
  numer2 <- pracma::quadv(function(x) x * prMat(x, Lambda, u = u, z = z), -5, 5)$Q
  print(paste(numer / denomi, numer2 / denomi2))
}

## 関数化(PSDと一緒に)

EAPs <- function(Lambda, U, Z) {
  Ns <- NROW(U)
  ret <- vector(length = Ns)
  PSD <- vector(length = Ns)
  for (s in 1:Ns) {
    u <- U[s, ]
    z <- Z[s, ]
    denominator <- integrate(function(x) pr(theta = x, Lambda = Lambda, u = u, z = z), lower = -5, upper = 5)$value
    numerator <- integrate(function(x) x * pr(theta = x, Lambda = Lambda, u = u, z = z), lower = -5, upper = 5)$value
    ret[s] <- numerator / denominator
    tmp <- integrate(function(x) (x - ret[s])^2 * pr(theta = x, Lambda = Lambda, u = u, z = z), lower = -5, upper = 5)$value
    PSD[s] <- sqrt(tmp / denominator)
  }
  ret <- list(ret, PSD)
  return(ret)
}

EAPs(Lambda, U, Z)

# 積分関数を自作して ---------------------------------------------------------------

prMat2 <- function(theta, Lambda, U, Z) {
  f <- function(x) {
    p <- Lambda[, 3] + (Lambda[, 4] - Lambda[, 3]) / (1 + exp(-Lambda[, 1] * (x - Lambda[, 2])))
    ll <- sum(u * log(p) + (1 - u) * log(1 - p)) - x^2 / 2
    ll <- U %*% log(p) + (1 - U) %*% log(1 - p) - x^2 / 2
    return(exp(ll))
  }
  f_vec <- Vectorize(f)
  return(f_vec(theta))
}

EAP_PSD <- function(Lambda, U, Z) {
  dev <- 0.01
  xdev <- seq(-5, 5, dev)
  N <- length(xdev)

  tmp1 <- prMat2(xdev[1:N - 1], Lambda, U, Z)
  tmp2 <- prMat2(xdev[2:N], Lambda, U, Z)

  denominator <- rowSums((tmp1 + tmp2) * dev / 2)
  numerator <- rowSums((tmp1 %*% diag(xdev[1:N - 1]) + tmp2 %*% diag(xdev[2:N])) * dev / 2)
  EAP <- numerator / denominator

  weight <- ((EAP %*% t(rep(1, length(xdev)))) - ((rep(1, length(EAP))) %*% t(xdev)))^2
  numerator <- rowSums((tmp1 * weight[, 1:N - 1] + tmp2 * weight[, 2:N]) * dev / 2)
  PSDs <- sqrt(numerator / denominator)

  return(list(EAP = EAP, PSDs = PSDs))
}

EAP_PSD(Lambda, U, Z)
