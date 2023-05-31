### Read Output of Mathematica
library(tidyverse)
library(readxl)
Ch03Items <- read_excel("Chapter03CTT.xlsx",
  sheet = "Item"
) %>%
  select_if(is.numeric) %>%
  as.data.frame()
Ch03JSS <- read_excel("Chapter03CTT.xlsx",
  sheet = "Joint Sample Size"
) %>%
  select_if(is.numeric) %>%
  as.matrix()
Ch03JCRR <- read_excel("Chapter03CTT.xlsx",
  sheet = "Joint Correct Response Rate"
) %>%
  select_if(is.numeric) %>%
  as.matrix()
Ch03CCRR <- read_excel("Chapter03CTT.xlsx",
  sheet = "Conditioal Correct Response Rat"
) %>%
  select_if(is.numeric) %>%
  as.matrix()
Ch03IL <- read_excel("Chapter03CTT.xlsx",
  sheet = "Item Lift"
) %>%
  select_if(is.numeric) %>%
  as.matrix()
Ch03MI <- read_excel("Chapter03CTT.xlsx",
  sheet = "Mutual Information"
) %>%
  select_if(is.numeric) %>%
  as.matrix()
Ch03Phi <- read_excel("Chapter03CTT.xlsx",
  sheet = "Phi Coefficient"
) %>%
  select_if(is.numeric) %>%
  as.matrix()
Ch03Tet <- read_excel("Chapter03CTT.xlsx",
  sheet = "Tetrachoric Correlation"
) %>%
  select_if(is.numeric) %>%
  as.matrix()

## read same data
dat <- read_csv("sampleData/J20S400.csv")
U <- as.matrix(dat[, -1])
Z <- ifelse(is.na(U), 0, 1)

# Test Section ------------------------------------------------------------

test_that("crr", {
  result <- crr(U, na = -99) %>% as.vector()
  expect <- Ch03Items$`Correct Response Rate` %>% as.vector()
  expect_equal(result, expect)
})


test_that("Item Odds", {
  result <- ItemOdds(U, na = -99) %>% as.vector()
  expect <- Ch03Items$`Item Odds` %>% as.vector()
  expect_equal(result, expect)
})

test_that("Item Threshold", {
  result <- ItemThreshold(U, na = -99) %>% as.vector()
  expect <- Ch03Items$`Item Threshold` %>% as.vector()
  expect_equal(result, expect)
})

test_that("Item Entropy", {
  result <- ItemEntropy(U, na = -99) %>% as.vector()
  expect <- Ch03Items$`Item Entropy` %>% as.vector()
  expect_equal(result, expect)
})

# not yet -----------------------------------------------------------------
#
# test_that("Item Total Correlation", {
#   result <- ItemTotalCorr(U, na = -99) %>% as.vector()
#   expect <- Ch03Items$`Item-Total Correlation` %>% as.vector()
#   expect_equal(result, expect)
# })
# test_that("Item Total Biserial Correlation", {
#   result <- ItemEntropy(U, na = -99) %>% as.vector()
#   expect <- Ch03Items$`Item-Total Bisereal Correlation` %>% as.vector()
#   expect_equal(result, expect)
# })

# Between Items Section ---------------------------------------------------

test_that("Joint sample size", {
  result <- JointSampleSize(U, na = -99) %>% unname()
  expect <- Ch03JSS %>% unname()
  expect_equal(result, expect)
})

test_that("Joint Correct Response Rate", {
  result <- JCRR(U, na = -99) %>% unname()
  expect <- Ch03JCRR %>% unname()
  expect_equal(result, expect)
})


test_that("Conditional Correct Repsonse Rate", {
  result <- CCRR(U, na = -99) %>% unname()
  expect <- Ch03CCRR %>% unname()
  expect_equal(result, expect)
})


test_that("Item Lift", {
  result <- ItemLift(U, na = -99) %>% unname()
  expect <- Ch03IL %>% unname()
  expect_equal(result, expect)
})

test_that("Mutual Informaiton", {
  result <- MutualInformation(U, na = -99) %>% unname()
  expect <- Ch03MI %>% unname()
  expect_equal(result, expect)
})

test_that("Phi Coefficient", {
  result <- PhiCoefficient(U, na = -99) %>% unname()
  expect <- Ch03Phi %>% unname()
  expect_equal(result, expect)
})

test_that("Tetrachoric Correlation Matrix", {
  result <- TetrachoricCorrelationMatrix(U, na = -99) %>% unname()
  Una <- ifelse(U == -99, NA, U)
  result2 <- psych::tetrachoric(Una, global = T)
  ret2 <- result2$rho %>% unname()
  expect <- Ch03Tet %>% unname()
  expect_equal(expected = expect, object = result, tolerance = 0.01)
})
