rm(list = ls())
pacman::p_load(Exametrika, tidyverse, cmdstanr, bayesplot, bayestestR)

ret <- Exametrika::Biclustering(J35S515, ncls = 13, nfld = 12, method = "R")
ret$FRP



# SVD -------------------------------------------------------------

source("http://aoki2.si.gunma-u.ac.jp/R/src/dual.R", encoding="euc-jp")
source("http://aoki2.si.gunma-u.ac.jp/R/src/summary.dual.R", encoding="euc-jp")
source("http://aoki2.si.gunma-u.ac.jp/R/src/plot.dual.R", encoding="euc-jp")


result <- dual(1-ret$FRP)
summary(result)
plot(result,weighted = T)


# Unfolding -------------------------------------------------------

model <- cmdstanr::cmdstan_model("develop/unfolding.stan")
dataSet <- list(FLD = 12, RNK = 13, Y = (5-5*as.matrix(ret$FRP)))

fit <- model$sample(
  data = dataSet,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 10000
)

mcmc_rhat(rhat(fit))
mcmc_dens(fit$draws(c("delta")))
mcmc_dens(fit$draws(c("theta")))

## MAP関数
map_estimation <- function(z) {
  density(z)$x[which.max(density(z)$y)]
}


fit.df <- fit$draws() %>%
  posterior::as_draws_df() %>%
  tibble::as_tibble() %>%
  dplyr::select(-lp__, -.draw, -.chain, -.iteration) %>%
  tibble::rowid_to_column("iter") %>%
  tidyr::pivot_longer(-iter) %>%
  group_by(name) %>%
  summarise(
    MAP = map_estimation(value),
    SD = sd(value),
    U95 = quantile(value, prob = 0.975),
    L95 = quantile(value, prob = 0.025)
  ) %>%
  filter(str_detect(name, "theta") | str_detect(name, "delta")) %>%
  mutate(
    variables = str_sub(name, 1, 5),
    varID = as.integer(str_extract(name, "(?<=\\[)\\d+")),
    dimension = as.integer(str_extract(name, "(?<=,)\\d+"))
  ) %>%
  mutate(
    Var = ifelse(variables == "delta", "R", "F"),
    Var = paste0(Var, varID)
  ) %>%
  dplyr::select(Var, dimension, MAP) %>%
  pivot_wider(id_cols = Var, names_from = dimension, values_from = MAP, names_prefix = "dim")

fit.df %>%
  mutate(G = str_sub(Var,1,1)) %>%
  ggplot(aes(x = dim1, y = dim2, label = Var, color = G)) +
  geom_point() +
  geom_label()



