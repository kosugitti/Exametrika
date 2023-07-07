# Prepare Section ---------------------------------------------------------
rm(list = ls())
library(tidyverse, quietly = TRUE)
library(readxl)
library(Exametrika)

# Ch03Tests <- read_excel("J20S400_Ch03CTT.xlsx",
Ch03Tests <- suppressMessages(read_excel("J20S400_Ch03CTT.xlsx",
  sheet = "Test"
))

SimpleStatistics <- Ch03Tests[1:23, 1:2] %>%
  rename(name = 1, value = 2) %>%
  mutate(value = as.numeric(value))

Ch03Tests2 <- suppressMessages(read_excel("Chapter03CTT.xlsx"))

Dimensionality <- suppressMessages(read_excel("Chapter03CTT.xlsx",
  sheet = "Dimensionality"
))

CTTexpect <- Ch03Tests[1:6, 3:4] %>%
  rename(name = 1, value = 2) %>%
  mutate(value = as.numeric(value))

Ch03Items <- suppressMessages(read_excel("J20S400_Ch03CTT.xlsx",
  sheet = "Item",
  skip = 1
)) %>%
  select_if(is.numeric) %>%
  as.data.frame()

## read same data
# dat <- read_csv("tests/testthat/sampleData/J20S400.csv")
dat <- suppressMessages(read_csv("sampleData/J20S400.csv"))
U <- as.matrix(dat[, -1])
Z <- ifelse(U == -99, 0, 1)

# Test Section ------------------------------------------------------------

test_that("Simple Test Statistics", {
  result <- Exametrika::TestStatistics(U, na = -99)
  expect <- Ch03Tests2[1:23, 2] %>%
    unlist() %>%
    as.vector()
  result <- unclass(result) %>% unlist() %>% as.vector()
  expect_equal(object = result, expected = expect)
})

test_that("Dimenosnality Analysis", {
  result <- Exametrika::Dimensionality(U, na = -99) %>%
    unclass() %>% unlist() %>% matrix(ncol=4)
  expect <- Dimensionality %>%
    as.matrix() %>%
    unname()
  expect_equal(object = result[, 2], expected = expect[, 2], tolerance = 1e-4)
  expect_equal(object = result[, 3]/100, expected = expect[, 3], tolerance = 1e-4)
  expect_equal(object = result[, 4]/100, expected = expect[, 4], tolerance = 1e-4)
})


test_that("Reliability", {
  result <- CTT(U, na = -99)
  result <- result$Reliability[,2] %>% as.matrix()
  expect <- CTTexpect[, 2] %>%
    as.matrix() %>%
    unname()
  expect_equal(object = result, expected = expect, tolerance = 1e-4)
})

test_that("Item Del Reliability", {
  result <- CTT(U, na = -99)
  result <- result$ReliabilityExcludingItem[, -1] %>% unname()
  expect <- Ch03Items[, 8:10] %>% unname()
  expect_equal(object = result, expected = expect, tolerance = 1e-4)
})
