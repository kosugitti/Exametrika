rm(list = ls())
pacman::p_load(
  tidyverse,
  cmdstanr,
  job,
  psych,
  ggforce,
  MASS,
  bayestestR,
  bayesplot,
  ggrepel,
  patchwork
)

source("develop/sim_jobSource.R")
# ごくわかりやすい2次元mixモデルをやってみる -----------------------------------------
set.seed(1433)
nitems <- 40
nobs <- 200
p <- 2
ncls <- 2
nfld <- 4
phi <- 4
psi <- 0.2
Radious <- 2
sigma_dev <- 0.2
sigma_e <- 0.25
Scale.min <- 1
Scale.max <- 7

dataSet <-
  makeDataset(
    p, nobs, nitems, ncls, nfld, phi, psi,
    Radious, sigma_dev, sigma_e, Scale.min, Scale.max
  )

dataSet$X %>%
  ggplot(aes(x = V1, y = V2)) +
  geom_point() +
  geom_point(
    data = data.frame(dataSet$mu_cls),
    mapping = aes(
      x = X1, y = X2,
      color = "red"
    )
  ) +
  xlim(-4, 4) +
  ylim(-4, 4)

dataSet$Y %>%
  ggplot(aes(x = V1, y = V2)) +
  geom_point() +
  geom_point(
    data = data.frame(dataSet$mu_fld),
    mapping = aes(
      x = X1, y = X2,
      color = "red"
    )
  ) +
  xlim(-4, 4) +
  ylim(-4, 4)
# 推定 --------------------------------------------------------------
Nchains <- 4
model <- cmdstanr::cmdstan_model("develop/mix_2dim.stan")
dataSet.tmp <- list(
  nobs = dataSet$nobs,
  p = dataSet$p,
  ncls = dataSet$ncls,
  x = dataSet$X[, 1:2]
)

## 初期値を決める一発目
result.simple <- model$sample(
  data = dataSet.tmp,
  chains = 4,
  parallel_chains = 4
)


result.simple$draws("lp__") |>
  apply(2, mean) %>% print

mlp <- result.simple$draws("lp__") |>
  apply(2, mean) |>
  which.max()

best <- result.simple$draws()[, mlp, ]

initial.values <- best %>%
  posterior::as_draws_df() %>%
  rowid_to_column("iter") %>%
  pivot_longer(-iter) %>%
  group_by(name) %>%
  summarise(EAP = mean(value)) %>%
  dplyr::select(name,EAP)

rawThetaAlpha <- initial.values %>% filter(str_detect(name,"raw_theta_alpha")) %>% pull(EAP)
radiusAlpha <- initial.values %>% filter(str_detect(name,"radius_alpha")) %>% pull(EAP)
initList.seed <- list(raw_theta_alpha = rawThetaAlpha,
                      radius_alpha =radiusAlpha)

initList <- replicate(Nchains, initList.seed, simplify = FALSE)

result.simple <- model$sample(
  data = dataSet.tmp,
  chains = 4,
  parallel_chains = 4,
  init = initList
)

result.simple
result.simple$draws("raw_theta_alpha") %>% bayesplot::mcmc_combo()
result.simple$draws("alpha") %>% posterior::summarise_draws()
result.simple$draws("theta1") %>% posterior::summarise_draws()
result.simple$draws("class_membership") %>% posterior::summarise_draws() %>% tail()

# xyに拡張 -----------------------------------------------------------

model <- cmdstanr::cmdstan_model("develop/mix_2dim_xy.stan")
dataSet.tmp <- list(
  nobs = dataSet$nobs,
  nitems = dataSet$nitems,
  p = dataSet$p,
  ncls = dataSet$ncls,
  nfld = dataSet$nfld,
  x = dataSet$X[, 1:2],
  y = dataSet$Y[, 1:2]
)

result.simple <- model$sample(
  data = dataSet.tmp,
  chains = 4,
  parallel_chains = 4
)

result.simple$draws("lp__") |>
  apply(2, mean) %>% print

mlp <- result.simple$draws("lp__") |>
  apply(2, mean) |>
  which.max()

best <- result.simple$draws()[, mlp, ]

initial.values <- best %>%
  posterior::as_draws_df() %>%
  rowid_to_column("iter") %>%
  pivot_longer(-iter) %>%
  group_by(name) %>%
  summarise(EAP = mean(value)) %>%
  dplyr::select(name,EAP)

rawThetaAlpha <- initial.values %>% filter(str_detect(name,"raw_theta_alpha")) %>% pull(EAP)
radiusAlpha <- initial.values %>% filter(str_detect(name,"radius_alpha")) %>% pull(EAP)
rawThetaBeta <- initial.values %>% filter(str_detect(name,"raw_theta_beta")) %>% pull(EAP)
radiusBeta <- initial.values %>% filter(str_detect(name,"radius_beta")) %>% pull(EAP)


initList.seed <- list(raw_theta_alpha = rawThetaAlpha,
                      radius_alpha =radiusAlpha,
                      raw_theta_beta = rawThetaBeta,
                      rdius_beta = radiusBeta)

initList <- replicate(Nchains, initList.seed, simplify = FALSE)

result.simple <- model$sample(
  data = dataSet.tmp,
  chains = 4,
  parallel_chains = 4,
  init = initList
)

result.simple
result.simple$draws("raw_theta_alpha") %>% bayesplot::mcmc_combo()
result.simple$draws("raw_theta_beta") %>% bayesplot::mcmc_combo()
result.simple$draws("raw_theta_beta") %>% posterior::summarise_draws()
result.simple$draws("theta_beta") %>% posterior::summarise_draws()
result.simple$draws("alpha") %>% posterior::summarise_draws()
result.simple$draws("theta1") %>% posterior::summarise_draws()
result.simple$draws("beta") %>% posterior::summarise_draws()
result.simple$draws("theta2") %>% posterior::summarise_draws()
