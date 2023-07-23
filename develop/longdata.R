rm(list = ls())
library(tidyverse)
read.csv("pool01.csv") |>
  select(-GID) |>
  pivot_longer(-ID) |>
  mutate(ID = paste0("Student", ID)) |>
  na.omit() |>
  write_csv(file = "pool01long.csv")
dat <- read_csv("pool01long.csv")

dat <- read.csv("pool01.csv")
dat <- dat[, -1]
library(Exametrika)
TestStatistics(dat)

U <- suppressMessages(read_csv("sampleData/J20S400.csv"))
dat <- dataFormat(U, na = -99, id = 1)
dataFormat(dat$U,na = -1)
dataFormat(data=dat$U,Z=dat$Z,na=-99)
