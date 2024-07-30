rm(list = ls())
source("develop/projectHeader.R")
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


# dual model ------------------------------------------------------

dual.model <- cmdstanr::cmdstan_model("develop/dualMDSunfold.stan")
job({
  result.dual <- dual.model$sample(
    data = dataSet,
    chains = 4,
    parallel_chains = 4,
    output_dir = "develop/simResult/MainEstimation/",
    output_basename = paste0("dualMDS", dataSet$nitems, "S", dataSet$nobs, "mix")
  )
})

fitX <- result.dual %>% getEstimate(str = "rotX")
fitY <- result.dual %>% getEstimate(str = "y")

result.dual$draws("scaling") %>% posterior::summarise_draws()
result.dual$draws("moving") %>% posterior::summarise_draws()
result.dual$draws("A") %>% posterior::summarise_draws()
result.dual$draws(c("sigmaX", "sigmaY", "sigmaU")) %>% posterior::summarise_draws()

fitX %>%
  as.data.frame() %>%
  rowid_to_column("id") %>%
  ggplot(aes(x = V1, y = V2, label = id)) +
  geom_point() +
  geom_text_repel()
dataSet$X %>%
  rowid_to_column("id") %>%
  ggplot(aes(x = V1, y = V2, label = id)) +
  geom_point() +
  geom_text_repel()

fitY %>%
  as.data.frame() %>%
  rowid_to_column("id") %>%
  ggplot(aes(x = V1, y = V2, label = id)) +
  geom_point() +
  geom_text_repel()
dataSet$Y %>%
  rowid_to_column("id") %>%
  ggplot(aes(x = V1, y = V2, label = id)) +
  geom_point() +
  geom_text_repel()
