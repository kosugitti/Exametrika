rm(list = ls())
library(tidyverse)
library(Exametrika)
dat <- read_csv("tests/testthat/sampleData/J15S500.csv") %>%
  mutate(Student = as.factor(Student))

tmp <- Exametrika::dataFormat(dat, na = -99)
U <- ifelse(is.na(tmp$U), 0, tmp$U) * tmp$Z

rho <- Exametrika::ItemTotalCorr(U)
tau <- Exametrika::ItemThreshold(U)

testlength <- NCOL(U)


# Mathematica out -------------------------------------------------

library(readxl)
### model2
Mathematica <- read_excel("tests/testthat/mtmk_v13/Chapter04IRT_2.xlsx", sheet = "Item")
Goal_params2 <- Mathematica[, 7:8]
ellA2 <- Mathematica$`Log-Likelihood(Analysis Model)`
### model3
Mathematica <- read_excel("tests/testthat/mtmk_v13/Chapter04IRT_3.xlsx", sheet = "Item")
Goal_params3 <- Mathematica[, 7:9]
ellA3 <- Mathematica$`Log-Likelihood(Analysis Model)`
### model4
Mathematica <- read_excel("tests/testthat/mtmk_v13/Chapter04IRT_4.xlsx", sheet = "Item")
Goal_params4 <- Mathematica[, 7:10]
ellA4 <- Mathematica$`Log-Likelihood(Analysis Model)`

# EMalgorithm -----------------------------------------------------



### Function Definition
LogisticModel <- function(a = 1, b, c = 0, d = 1, theta) {
  c + ((d - c) / (1 + exp(-a * (theta - b))))
}

slopeprior <- function(a, m, s) {
  -(log(max(a, const)) - m)^2 / (2 * s^2) - log(max(a, const)) - log(s)
}

asymprior <- function(c, alp, bet) {
  (alp - 1) * log(c) + (bet - 1) * log(1 - c)
}

objective_function <- function(params, j) {
  a <- params[1]
  b <- params[2]
  c <- ifelse(length(params) > 2, params[3], 0)
  d <- ifelse(length(params) > 3, params[4], 1)

  exloglike <- sum(
    qjtrue[j, ] * log(LogisticModel(a = a, b = b, c = c, d = d, theta = quadrature)) +
      qjfalse[j, ] * log(1 - LogisticModel(a = a, b = b, c = c, d = d, theta = quadrature))
  )

  exloglike <- exloglike - ((b / 2)^2 / 2) + slopeprior(a, 0, 0.5)

  if (model >= 3) {
    exloglike <- exloglike + asymprior(c, 2, 5)
  }

  if (model == 4) {
    exloglike <- exloglike + asymprior(d, 10, 2)
  }

  return(exloglike)
}

objective_function_IRT <- function(lambda, model, qjtrue, qjfalse, quadrature) {
  a <- lambda[1]
  b <- lambda[2]
  c <- ifelse(model > 2, lambda[3], 0)
  d <- ifelse(model > 3, lambda[4], 1)

  exloglike <- sum(
    qjtrue * log(LogisticModel(a = a, b = b, c = c, d = d, theta = quadrature)) +
      qjfalse * log(1 - LogisticModel(a = a, b = b, c = c, d = d, theta = quadrature))
  )

  exloglike <- exloglike - ((b / 2)^2 / 2) + slopeprior(a, 0, 0.5)

  if (model >= 3) {
    exloglike <- exloglike + asymprior(c, 2, 5)
  }

  if (model == 4) {
    exloglike <- exloglike + asymprior(d, 10, 2)
  }

  return(exloglike)
}


### Initialize

model <- 4

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
itemloglike <- array(NA, testlength)

while (FLG) {
  if (abs(loglike - oldloglike) < 0.00001 * abs(oldloglike)) {
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
  totalLogLike <- 0
  for (j in 1:testlength) {
    ## 初期値微動,上限下限設定
    initial_values <- paramset[j, 1:model] + rnorm(model, 0, 0.01)
    initial_values <- c(0.5, 0.5, 0, 1)[1:model]
    lowers <- c(-Inf, -5, 1e-10, 1e-10)[1:model]
    uppers <- c(Inf, 5, 1 - 1e-10, 1 - 1e-10)[1:model]

    optim_flg <- FALSE
    max_attempt <- 100
    attempt <- 0
    while (!optim_flg & attempt < max_attempt) {
      attempt <- attempt + 1
      tryCatch(
        {
          # result <- optim(
          #   par = initial_values,
          #   fn = objective_function,
          #   method = "L-BFGS-B",
          #   lower = lowers,
          #   upper = uppers,
          #   control = list(fnscale = -1, factr = 1e-10),
          #   j = j
          result <- optim(
            par = initial_values,
            fn = objective_function_IRT,
            method = "L-BFGS-B",
            lower = lowers,
            upper = uppers,
            control = list(fnscale = -1, factr = 1e-10),
            ### args for objective_function_IRT
            model = model,
            qjtrue = qjtrue[j, ],
            qjfalse = qjfalse[j, ],
            quadrature = quadrature,
            # j=j
          )
          optim_flg <- TRUE
        },
        error = function(e) {
          message(paste("initial reset, item ", j))
          initial_values <<- paramset[j, 1:model] + rnorm(model, 0, 0.1)
        }
      )
    }
    itemloglike[j] <- result$value
    totalLogLike <- totalLogLike + itemloglike[j]
    if (model == 2) {
      newparams <- c(result$par, 0, 1)
    } else if (model == 3) {
      newparams <- c(result$par, 1)
    } else if (model == 4) {
      newparams <- result$par
    }
    paramset[j, ] <- newparams
  }
  loglike <- totalLogLike
  print(paste("iter", emt, "LogLik", totalLogLike, "itemloglike", itemloglike[2]))
}


item_model_loglike <- itemloglike + (paramset[, 2] / 2)^2 / 2 - slopeprior(paramset[, 1], 0, 0.5)

if (model >= 3) {
  item_model_loglike <- item_model_loglike - asymprior(paramset[, 3], 2, 5)
}

if (model == 4) {
  item_model_loglike <- item_model_loglike - asymprior(paramset[, 4], 10, 2)
}

ell_A <- item_model_loglike


mean(abs(ell_A - ellA3))

paramset



# Model Fit -------------------------------------------------------


item_model_loglike <- itemloglike + (paramset[, 2] / 2)^2 / 2 - slopeprior(paramset[, 1], 0, 0.5)

if (model >= 3) {
  item_model_loglike <- item_model_loglike - asymprior(paramset[, 3], 2, 5)
}

if (model == 4) {
  item_model_loglike <- item_model_loglike - asymprior(paramset[, 4], 10, 2)
}

ell_A <- item_model_loglike


## Null Model
nrs <- colSums(tmp$Z)
crr <- colSums(tmp$U) / nrs
const <- exp(-testlength)
nobs <- NROW(tmp$Z)

ell_N <- nobs * crr * log(crr + const) + nobs * (1 - crr) * log(1 - crr + const)

## Benchmark Model

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

#### Total

null_loglike <- sum(ell_N)
bench_loglike <- sum(ell_B)
model_loglike <- sum(ell_A)


### chisq
#### Analysis_model
chi_A <- 2 * (ell_B - ell_A)
df_A <- ntotal - model
### Null_model
chi_B <- 2 * (ell_B - ell_N)
df_B <- ntotal - 1

## Standardized Fit Indeces
NFI <- 1 - (chi_A / chi_B)
RFI <- 1 - ((chi_A / df_A) / (chi_B / df_B))
IFI <- 1 - ((chi_A - df_A) / (chi_B - df_A))
TLI <- 1 - ((chi_A / df_A) - 1) / ((chi_B / df_B) - 1)
CFI <- 1 - ((chi_A - df_A) / (chi_B - df_B))
RMSEA <- sqrt((chi_A - df_A) / (df_A * nobs - 1))

## Information Criteria
AIC <- chi_A - 2 * df_A
CAIC <- chi_A - df_A * log(nobs - 1)
BIC <- chi_A - df_A * log(nobs)
