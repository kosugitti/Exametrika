rm(list = ls())
library(tidyverse)
library(Exametrika)

### GOALS
library(readxl)
### model2
pl2Test <- read_excel("Chapter04IRT2pl.xlsx", sheet = "Test")
pl2Item <- read_excel("Chapter04IRT2pl.xlsx", sheet = "Item")
pl2Student <- read_excel("Chapter04IRT2pl.xlsx", sheet = "Student")

dat <- read_csv("sampleData/J15S500.csv") %>%
  mutate(Student = as.factor(Student))

tmp <- Exametrika::dataFormat(dat, na = -99)
U <- ifelse(is.na(tmp$U), 0, tmp$U) * tmp$Z

result2 <- IRT(model = 2, U = U)

test_that("2PL model Test Info", {
  expect <- pl2Test[13:28, 2] %>%
    unlist() %>%
    unname() %>%
    as.numeric()
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8, 9, 10, 11, 12, 13, 14, 15, 16)]
  result <- result2$TestFitIndices %>% as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})
