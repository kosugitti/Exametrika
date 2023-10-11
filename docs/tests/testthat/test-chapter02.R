### Read Output of Mathematica
library(tidyverse)
library(readxl)
library(Exametrika)

Ch03CTT <- suppressMessages(read_excel("../../develop/Chapter03CTT.xlsx",
  sheet = "Student"
)) %>%
  select_if(is.numeric) %>%
  as.data.frame()
## read same data
dat <- suppressMessages(read_csv("../../develop/sampleData/J20S400.csv"))
U <- as.matrix(dat[, -1])
Z <- ifelse(is.na(U), 0, 1)

test_that("NRS", {
  result <- nrs(U, na = -99) %>% as.vector()
  expect <- Ch03CTT$`Number-Right Score` %>% as.vector()
  expect_equal(result, expect)
})

test_that("passage", {
  result <- passage(U, na = -99) %>% as.vector()
  expect <- Ch03CTT$`Passage Rate` %>% as.vector()
  expect_equal(result, expect)
})

test_that("sscore", {
  result <- sscore(U, na = -99) %>% as.vector()
  expect <- Ch03CTT$`Standardized Score` %>% as.vector()
  expect_equal(result, expect)
})

test_that("percentile", {
  result <- percentile(U, na = -99) %>%
    as.vector() %>%
    ceiling()
  expect <- Ch03CTT$`Percentile Rank` %>% as.vector()
  expect_equal(result, expect)
})

test_that("stanine", {
  result <- stanine(U, na = -99)
  test <- result$stanineScore %>% as.numeric()
  expect <- Ch03CTT$Stanine %>% as.vector()
  expect_equal(test, expect)
})
