rm(list = ls())
library(tidyverse)
library(Exametrika)

### GOALS
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


###

dat <- read_csv("tests/testthat/sampleData/J15S500.csv") %>%
  mutate(Student = as.factor(Student))

tmp <- Exametrika::dataFormat(dat, na = -99)
U <- ifelse(is.na(tmp$U), 0, tmp$U) * tmp$Z

result <- IRT(model=4,U=U)
result$params
result$item_log_like
