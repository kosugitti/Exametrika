rm(list = ls())
library(tidyverse)
read.csv("develop/pool01.csv") |>
  select(-GID) |>
  pivot_longer(-ID) |>
  mutate(ID = paste0("Student", ID)) |>
  na.omit() |>
  write_csv(file = "develop/pool01long.csv")
dat <- read_csv("develop/pool01long.csv")

dat <- read.csv("develop/pool01.csv")
dat <- dat[, -1]
library(Exametrika)
TestStatistics(dat)

dat <- J15S500

IRT(dat,na=-99,model=4) -> unko
plot(unko,type="ICC",nc=3,nr=5)

dat <- read.csv("develop/k2022.csv")
dat <- dat |> select(-GID)
dataFormat(dat)

irt2 <- IRT(dat,model=2)
irt3 <- IRT(dat,model=3)
irt4 <- IRT(dat,model=4)
plot(irt2,type="ICC",nc=3,nr=5)
lca <- LCA(dat,4)
plot(lca,type="IRP",items = 1:15,nr=3,nc=5)
lca

lra <- LRA(dat,4,method="SOM")
lra
plot(lra,type="IRP",items = 1:15,nr=3,nc=5)

Bic <- Biclustering(dat,ncls=8,nfld=8,mic=T)
Bic
plot(Bic,"Array")
plot(Bic,type="TRP",items = 1:15,nr=1,nc=1)

Bic <- Biclustering(dat,ncls=8,nfld=8,mic=T,method="R")
Bic
plot(Bic,"Array")
plot(Bic,type="TRP",items = 1:15,nr=1,nc=1)
plot(Bic,type="LRD",items = 1:15,nr=1,nc=1)

