rm(list=ls())
library(tidyverse)
read.csv("pool01.csv") |>
  select(-GID) |>
  pivot_longer(-ID) |>
  mutate(ID = paste0("Student",ID)) |>
  na.omit() |>
  write_csv(file="pool01long.csv")
dat <- read_csv("pool01long.csv")
