# Prepare Section ---------------------------------------------------------
rm(list=ls())
library(tidyverse)
library(readxl)

Ch03Tests <- read_excel("J20S400_Ch03CTT.xlsx",
  sheet = "Test"
)
SimpleStatistics <- Ch03Tests[1:23, 1:2] %>%
  rename(name = 1, value = 2) %>%
  mutate(value = as.numeric(value))

Ch03Tests2 <- read_excel("Chapter03CTT.xlsx")

Dimensionality <- read_excel("Chapter03CTT.xlsx",
  sheet = "Dimensionality"
)

ClassicalTestTheory <- Ch03Tests[, 3:4] %>%
  rename(name = 1, value = 2) %>%
  mutate(value = as.numeric(value))
## read same data
dat <- read_csv("sampleData/J20S400.csv")
U <- as.matrix(dat[, -1])
Z <- ifelse(U == -99, 0, 1)

# Test Section ------------------------------------------------------------

test_that("Simple Test Statistics", {
  result <- TestStatistics(U, na = -99)
  expect <- Ch03Tests2[1:23, 2] %>%
    unlist() %>%
    as.vector()
  result <- result$value %>% unname()
  expect_equal(object = result, expected = expect)
})

test_that("Dimenosnality Analysis", {
  result <- DimensonalityAnalysis(U, na = -99) %>%
    as.matrix() %>%
    unname()
  expect <- Dimensionality %>%
    as.matrix() %>%
    unname()
  expect_equal(object = result[,2], expected = expect[,2], tolerance = 1e-4)
  expect_equal(object = result[,3], expected = expect[,3], tolerance = 1e-4)
  expect_equal(object = result[,4], expected = expect[,4], tolerance = 1e-4)
})
