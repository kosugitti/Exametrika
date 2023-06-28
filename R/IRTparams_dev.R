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
Mathematica <- read_excel("tests/testthat/mtmk_v13/Chapter04IRT_2.xlsx",sheet = "Item")
Goal_params2 <- Mathematica[,7:8]

### model3
Mathematica <- read_excel("tests/testthat/mtmk_v13/Chapter04IRT_3.xlsx",sheet = "Item")
Goal_params3 <- Mathematica[,7:9]

### model4
Mathematica <- read_excel("tests/testthat/mtmk_v13/Chapter04IRT_4.xlsx",sheet = "Item")
Goal_params4 <- Mathematica[,7:10]

# EMalgorithm -----------------------------------------------------



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

objective_function <- function(params, j) {
  a <- params[1]
  b <- params[2]
  c <- ifelse(length(params) > 2, params[3], 0)
  d <- ifelse(length(params) > 3, params[4], 1)

  exloglike <- sum(
    qjtrue[j, ] * log(LogisticModel(a = a, b = b, c = c, d = d, theta = quadrature)) +
      qjfalse[j, ] * log(1 - LogisticModel(a = a, b = b, c = c, d = d, theta = quadrature))
  )

  exloglike <- exloglike - (b / 2)^2 / 2 + slopeprior(a, 0, 0.5)

  if (model >= 3) {
    exloglike <- exloglike + asymprior(c, 2, 5)
  }

  if (model == 4) {
    exloglike <- exloglike + asymprior(d, 10, 2)
  }

  return(exloglike)
}




### Initialize
iter <- 10
sim_result <- array(NA, dim = c(iter, testlength * 4))

model <- 4

for (i in 1:iter) {
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

  while (FLG) {
    if ((loglike - oldloglike) <= 1e-7 * abs(oldloglike)) {
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
        objective_function,
        control = list(fnscale = -1),
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
    }
    loglike <- totalLogLike
  }
  print(paste("iter", i, "LogLik", totalLogLike))

  sim_result[i, ] <- paramset %>% as.vector()

}

paramset
apply(sim_result, 2, mean)
max(abs(apply(sim_result, 2, max) - apply(sim_result, 2, min)))
apply(sim_result, 2, sd)
