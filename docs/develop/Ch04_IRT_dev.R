rm(list = ls())
library(tidyverse)

# MLE for Ability ---------------------------------------------------------
## IRT functions
LogisticModel <- function(a = 1, b, c = 0, d = 1, theta) {
  p <- c + ((d - c) / (1 + exp(-a * (theta - b))))
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


# 4.5 EM algorithm --------------------------------------------------------
library(Exametrika)
dat <- read_csv("develop/sampleData/J12S5000.csv") %>%
  mutate(Student = as.factor(Student))

tmp <- Exametrika::dataFormat(dat, na = -99)
U <- ifelse(is.na(tmp$U), 0, tmp$U) * tmp$Z

rho <- Exametrika::ItemTotalCorr(U)
tau <- Exametrika::ItemThreshold(U)

testlength <- NCOL(U)

### Initialize
model <- 2
slope <- 2 * rho
loc <- 2 * tau
if (model >= 3) {
  loasym <- rep(0.05, testlength)
} else {
  loasym <- rep(0, testlength)
}
if (model >= 4) {
  upasym <- rep(0.95, testlength)
} else {
  upasym <- rep(1, testlength)
}

paramset <- matrix(c(slope, loc, loasym, upasym), ncol = 4)

quadrature <- seq(-3.2, 3.2, 0.4)

const <- exp(-testlength)

loglike <- -1 / const
oldloglike <- -2 / const


itemloglike <- rep(loglike / testlength, testlength)

log(LogisticModel(a = paramset[2, 1], b = paramset[2, 2], c = paramset[2, 3], d = paramset[2, 4], theta = -3.2) + const)


### Expectation
lpj <- matrix(NA, nrow = testlength, ncol = length(quadrature))
for (j in 1:testlength) {
  lpj[j, ] <- log(LogisticModel(
    a = paramset[j, 1],
    b = paramset[j, 2],
    c = paramset[j, 3],
    d = paramset[j, 4],
    theta = quadrature
  ) + const)
}
lpj

lqj <- matrix(NA, nrow = testlength, ncol = length(quadrature))
for (j in 1:testlength) {
  lqj[j, ] <- log(1 - LogisticModel(
    a = paramset[j, 1],
    b = paramset[j, 2],
    c = paramset[j, 3],
    d = paramset[j, 4],
    theta = quadrature
  ) + const)
}
lqj


posttheta_numerator <- exp(
  (tmp$Z * tmp$U) %*% lpj +
    (tmp$Z * (1 - tmp$U)) %*% lqj -
    matrix(rep(quadrature^2 / 2, NROW(tmp$U)), nrow = NROW(tmp$U), byrow = T)
)

post_theta <- posttheta_numerator / rowSums(posttheta_numerator)

marginal_posttheta <- colSums(post_theta)

qjtrue <- t(tmp$Z * tmp$U) %*% post_theta
qjfalse <- t(tmp$Z * (1 - tmp$U)) %*% post_theta

### Maximize

slopeprior <- function(a, m, s) {
  -(log(max(a, const)) - m)^2 / (2 * s^2) - log(max(a, const)) - log(s)
}

asymprior <- function(c, alp, bet) {
  (alp - 1) * log(c) + (bet - 1) * log(1 - c)
}

objective_function <- function(params, j) {
  a <- params[1]
  b <- params[2]
  c <- params[3]
  exloglike <- sum(
    qjtrue[j, ] * log(LogisticModel(a = a, b = b, c = c, d = 1, theta = quadrature)) +
      qjfalse[j, ] * log(1 - LogisticModel(a = a, b = b, c = c, d = 1, quadrature))
  )
  exloglike <- exloglike - (b / 2)^2 / 2 + slopeprior(a, 0, 0.5) + asymprior(c, 2, 5)
  return(exloglike)
}


# 初期値の設定
initial_values <- c(1, 0.1, 0.1) # a, bの初期値
for (j in 1:testlength) {
  result <- optim(
    initial_values,
    objective_function,
    method = "BFGS",
    control = list(fnscale = -1),
    j = j
  )
  print(paste(j, result$value))
}


# EMalgorithm -----------------------------------------------------
dat <- read_csv("develop/sampleData/J15S500.csv") %>%
  mutate(Student = as.factor(Student))

tmp <- Exametrika::dataFormat(dat, na = -99)
U <- ifelse(is.na(tmp$U), 0, tmp$U) * tmp$Z

rho <- Exametrika::ItemTotalCorr(U)
tau <- Exametrika::ItemThreshold(U)

testlength <- NCOL(U)

model <- 2


### Function Definition
LogisticModel <- function(a = 1, b, c = 0, d = 1, theta) {
  p <- c + ((d - c) / (1 + exp(-a * (theta - b))))
  return(p)
}

slopeprior <- function(a, m, s) {
  -(log(max(a, const)) - m)^2 / (2 * s^2) - log(max(a, const)) - log(s)
}

asymprior <- function(c, alp, bet) {
  (alp - 1) * log(c) + (bet - 1) * log(1 - c)
}

objective_function2 <- function(params, j) {
  a <- params[1]
  b <- params[2]
  exloglike <- sum(
    qjtrue[j, ] * log(LogisticModel(a = a, b = b, c = 0, d = 1, theta = quadrature)) +
      qjfalse[j, ] * log(1 - LogisticModel(a = a, b = b, c = 0, d = 1, quadrature))
  )
  exloglike <- exloglike - (b / 2)^2 / 2 + slopeprior(a, 0, 0.5)
  return(exloglike)
}

objective_function3 <- function(params, j) {
  a <- params[1]
  b <- params[2]
  c <- params[3]
  exloglike <- sum(
    qjtrue[j, ] * log(LogisticModel(a = a, b = b, c = c, d = 1, theta = quadrature)) +
      qjfalse[j, ] * log(1 - LogisticModel(a = a, b = b, c = c, d = 1, quadrature))
  )
  exloglike <- exloglike - (b / 2)^2 / 2 + slopeprior(a, 0, 0.5) + asymprior(c, 2, 5)
  return(exloglike)
}

objective_function4 <- function(params, j) {
  a <- params[1]
  b <- params[2]
  c <- params[3]
  d <- params[4]
  exloglike <- sum(
    qjtrue[j, ] * log(LogisticModel(a = a, b = b, c = c, d = d, theta = quadrature)) +
      qjfalse[j, ] * log(1 - LogisticModel(a = a, b = b, c = c, d = d, quadrature))
  )
  exloglike <- exloglike - (b / 2)^2 / 2 + slopeprior(a, 0, 0.5) + asymprior(c, 2, 5) + asymprior(d, 10, 2)
  return(exloglike)
}


### Initialize

slope <- 2 * rho
loc <- 2 * tau
if (model >= 3) {
  loasym <- rep(0.05, testlength)
} else {
  loasym <- rep(0, testlength)
}
if (model >= 4) {
  upasym <- rep(0.95, testlength)
} else {
  upasym <- rep(1, testlength)
}

paramset <- matrix(c(slope, loc, loasym, upasym), ncol = 4)

quadrature <- seq(-3.2, 3.2, 0.4)

const <- exp(-testlength)

loglike <- -1 / const
oldloglike <- -2 / const


itemloglike <- rep(loglike / testlength, testlength)


emt <- 0
maxemt <- 25
FLG <- TRUE

if (model == 2) {
  opt_func <- objective_function2
} else if (model == 3) {
  opt_func <- objective_function3
} else if (model == 4) {
  opt_func <- objective_function4
} else {
  stop("The model must set either 2, 3, or 4")
}

Hessian <- list()
while (FLG) {
  if (loglike - oldloglike <= 1e-7 * abs(oldloglike)) {
    FLG <- FALSE
  }
  emt <- emt + 1
  oldloglike <- loglike
  if (emt > maxemt) {
    FLG <- FALSE
  }
  ### Expectation
  lpj <- matrix(NA, nrow = testlength, ncol = length(quadrature))
  for (j in 1:testlength) {
    lpj[j, ] <- log(LogisticModel(
      a = paramset[j, 1],
      b = paramset[j, 2],
      c = paramset[j, 3],
      d = paramset[j, 4],
      theta = quadrature
    ) + const)
  }

  lqj <- matrix(NA, nrow = testlength, ncol = length(quadrature))
  for (j in 1:testlength) {
    lqj[j, ] <- log(1 - LogisticModel(
      a = paramset[j, 1],
      b = paramset[j, 2],
      c = paramset[j, 3],
      d = paramset[j, 4],
      theta = quadrature
    ) + const)
  }

  posttheta_numerator <- exp(
    (tmp$Z * tmp$U) %*% lpj +
      (tmp$Z * (1 - tmp$U)) %*% lqj -
      matrix(rep(quadrature^2 / 2, NROW(tmp$U)), nrow = NROW(tmp$U), byrow = T)
  )

  post_theta <- posttheta_numerator / rowSums(posttheta_numerator)

  marginal_posttheta <- colSums(post_theta)
  #
  qjtrue <- t(tmp$Z * tmp$U) %*% post_theta
  qjfalse <- t(tmp$Z * (1 - tmp$U)) %*% post_theta

  ### Maximize
  if (model == 2) {
    initial_values <- c(0.5, 0.5) # a, bの初期値
  } else if (model == 3) {
    initial_values <- c(0.5, 0.5, 0.05) # a, b, cの初期値
  } else if (model == 4) {
    initial_values <- c(0.5, 0.5, 0.05, 0.05) # a, b, c, dの初期値
  }

  totalLogLike <- 0
  for (j in 1:testlength) {
    result <- optim(
      initial_values,
      opt_func,
      control = list(fnscale = -1),
      hessian = TRUE,
      j = j
    )
    totalLogLike <- totalLogLike + result$value
    if (model == 2) {
      newparams <- c(result$par, 0, 1)
    } else if (model == 3) {
      newparams <- c(result$par, 1)
    } else if (model == 4) {
      newparams <- result$par
    }
    paramset[j, ] <- newparams
    Hessian[[j]] <- result$hessian
  }
  print(paste("iter", emt, "LogLik", totalLogLike))
  loglike <- totalLogLike
}

paramset
eapscore <- post_theta %*% quadrature

tmpA <- matrix(rep(quadrature, NROW(tmp$U)), nrow = NROW(tmp$U), byrow = T)
tmpB <- matrix(rep(eapscore, length(quadrature)), nrow = NROW(tmp$U), byrow = F)

psd <- sqrt(diag(post_theta %*% t((tmpA - tmpB)^2)))

EAPs(paramset, tmp$U, tmp$Z)[[2]] - EAP_PSD(paramset, tmp$U, tmp$Z)$PSD

# 4.5.8 Posterior Standard Deviation ------------------------------
### model3
# Mathematica <- read_excel("tests/testthat/mtmk_v13/Chapter04IRT_3.xlsx", sheet = "Item")
# Goal_params3 <- Mathematica[, 7:9]
#
#
# lambda_1MAP <- paramset[1, 1:3]
# lambda_1MAP_Goal <- Goal_params3[1, 1:3] %>%
#   as.vector() %>%
#   unname() %>%
#   unlist()
#
# # prior slope Log_normal(0,0.5)
# # prior location normal(0,2)
# # prior lower_asym Beta(2,5)
#
# Ipr_a <- function(a) {
#   (1 - 0.5^2 - log(a)) / (a^2 * 0.5^2)
# }
#
# Ipr_cd <- function(c) {
#   1 / c^2 + 4 / (1 - c)^2
# }
#
# Ipr_a(lambda_1MAP_Goal[1])
# Ipr_a(lambda_1MAP[1])
# Ipr_b <- 1 / 2^2
# Ipr_cd(lambda_1MAP_Goal[3])
#
#
# a <- lambda_1MAP_Goal[1]
# b <- lambda_1MAP_Goal[2]
# c <- lambda_1MAP_Goal[3]
#
# p <- LogisticModel(a = a, b = b, c = c, d = 1, theta = quadrature)
# q <- 1 - p
#
# num <- (quadrature - b)^2 * (p - c)^2 * q
# den <- ((1 - c)^2 * p)
#
# (num / den * marginal_posttheta) %>% sum()
#
#
# num <- lambda_1MAP_Goal[1] * (quadrature - lambda_1MAP_Goal[2]) * (p - lambda_1MAP_Goal[3])^2 * q
# den <- (1 - lambda_1MAP_Goal[3])^2 * p
# -(num / den * marginal_posttheta) %>% sum()

### 関数化

I_pr_lambda <- function(m, params) {
  a <- params[1]
  b <- params[2]
  c <- params[3]
  d <- params[4]
  I_pr_lambda <- diag(rep(NA, m))
  I_pr_lambda[1, 1] <- (1 - 0.5^2 - log(a)) / (a^2 * 0.5^2)
  I_pr_lambda[2, 2] <- 1 / 2^2
  if (m > 2) {
    I_pr_lambda[3, 3] <- 1 / c^2 + 4 / (1 - c)^2
  }
  if (m > 3) {
    I_pr_lambda[4, 4] <- 1 / d^2 + 4 / (1 - d)^2
  }
  return(I_pr_lambda)
}


I_F_lambda <- function(m, params, quadrature, marginal_posttheta) {
  a <- params[1]
  b <- params[2]
  c <- params[3]
  d <- params[4]
  p <- LogisticModel(a = a, b = b, c = c, d = d, theta = quadrature)
  q <- 1 - p
  I_F_lambda <- matrix(rep(NA, m * m), ncol = m)
  den <- (d - c)^2 * p * q
  ## aa
  num <- (quadrature - b)^2 * (p - c)^2 * (d - p)^2
  I_F_lambda[1, 1] <- sum((num / den) * marginal_posttheta)
  ## ba
  num <- a * (quadrature - b) * (p - c)^2 * (d - p)^2
  I_F_lambda[1, 2] <- I_F_lambda[2, 1] <- -1 * sum(num / den * marginal_posttheta)
  ## bb
  num <- a^2 * (p - c)^2 * (d - p)^2
  I_F_lambda[2, 2] <- sum((num / den) * marginal_posttheta)

  if (m > 2) {
    ## ca
    num <- (quadrature - b) * (p - c) * (d - p)^2
    I_F_lambda[1, 3] <- I_F_lambda[3, 1] <- sum((num / den) * marginal_posttheta)
    ## cb
    num <- a * (p - c) * (d - p)^2
    I_F_lambda[2, 3] <- I_F_lambda[3, 2] <- -1 * sum((num / den) * marginal_posttheta)
    ## cc
    num <- (d - p)^2
    I_F_lambda[3, 3] <- sum(num / den * marginal_posttheta)
  }

  if (m > 3) {
    ## da
    num <- (quadrature - b) * (p - c)^2 * (d - p)
    I_F_lambda[1, 4] <- I_F_lambda[4, 1] <- sum((num / den) * marginal_posttheta)
    ## db
    num <- a * (p - c)^2 * (d - p)
    I_F_lambda[2, 4] <- I_F_lambda[4, 2] <- -1 * sum((num / den) * marginal_posttheta)
    ## dc
    num <- (p - c) * (d - p)
    I_F_lambda[3, 4] <- I_F_lambda[4, 3] <- sum((num / den) * marginal_posttheta)
    ## dd
    num <- (p - c)^2
    I_F_lambda[4, 4] <- sum((num / den) * marginal_posttheta)
  }

  return(I_F_lambda)
}

#
# Ij <- I_F_lambda(3, c(lambda_1MAP_Goal, 1), quadrature, marginal_posttheta) + I_pr_lambda(3, c(lambda_1MAP_Goal, 1))
# Ij
# solve(Ij) %>%
#   diag() %>%
#   sqrt()

sqrt(diag(solve(I_pr_lambda(4, paramset[1, ]) + I_F_lambda(4, params = paramset[1, ], quadrature, marginal_posttheta))))
sqrt(diag(solve(-1 * Hessian[[1]])))

PSD_item_params <- function(model, Lambda, quadrature, marginal_posttheta) {
  J <- NROW(Lambda)
  ret <- array(NA, dim = c(J, model))
  for (j in 1:J) {
    a <- Lambda[j, 1]
    b <- Lambda[j, 2]
    c <- Lambda[j, 3]
    d <- Lambda[j, 4]
    ### I_pr
    I_pr_lambda <- diag(rep(NA, model))
    I_pr_lambda[1, 1] <- (1 - 0.5^2 - log(a)) / (a^2 * 0.5^2)
    I_pr_lambda[2, 2] <- 1 / 2^2
    if (model > 2) {
      I_pr_lambda[3, 3] <- 1 / c^2 + 4 / (1 - c)^2
    }
    if (model > 3) {
      I_pr_lambda[4, 4] <- 1 / d^2 + 4 / (1 - d)^2
    }
    ## I_F
    p <- LogisticModel(a = a, b = b, c = c, d = d, theta = quadrature)
    q <- 1 - p
    I_F_lambda <- matrix(rep(NA, model * model), ncol = model)
    den <- (d - c)^2 * p * q
    ## aa
    num <- (quadrature - b)^2 * (p - c)^2 * (d - p)^2
    I_F_lambda[1, 1] <- sum((num / den) * marginal_posttheta)
    ## ba
    num <- a * (quadrature - b) * (p - c)^2 * (d - p)^2
    I_F_lambda[1, 2] <- I_F_lambda[2, 1] <- -1 * sum(num / den * marginal_posttheta)
    ## bb
    num <- a^2 * (p - c)^2 * (d - p)^2
    I_F_lambda[2, 2] <- sum((num / den) * marginal_posttheta)

    if (model > 2) {
      ## ca
      num <- (quadrature - b) * (p - c) * (d - p)^2
      I_F_lambda[1, 3] <- I_F_lambda[3, 1] <- sum((num / den) * marginal_posttheta)
      ## cb
      num <- a * (p - c) * (d - p)^2
      I_F_lambda[2, 3] <- I_F_lambda[3, 2] <- -1 * sum((num / den) * marginal_posttheta)
      ## cc
      num <- (d - p)^2
      I_F_lambda[3, 3] <- sum(num / den * marginal_posttheta)
    }

    if (model > 3) {
      ## da
      num <- (quadrature - b) * (p - c)^2 * (d - p)
      I_F_lambda[1, 4] <- I_F_lambda[4, 1] <- sum((num / den) * marginal_posttheta)
      ## db
      num <- a * (p - c)^2 * (d - p)
      I_F_lambda[2, 4] <- I_F_lambda[4, 2] <- -1 * sum((num / den) * marginal_posttheta)
      ## dc
      num <- (p - c) * (d - p)
      I_F_lambda[3, 4] <- I_F_lambda[4, 3] <- sum((num / den) * marginal_posttheta)
      ## dd
      num <- (p - c)^2
      I_F_lambda[4, 4] <- sum((num / den) * marginal_posttheta)
    }

    ##
    Ij <- I_F_lambda + I_pr_lambda
    ret[j, ] <- sqrt(diag(solve(Ij)))
    ## Return
  }
  return(PSD = ret)
}

PSD_item_params(model = 4, paramset, quadrature, marginal_posttheta)


