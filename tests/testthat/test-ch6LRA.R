library(tidyverse)
### GOALS
library(readxl)
test <- read_excel("Chapter06LRA(GTMmic0).xlsx", sheet = "Test")
class <- read_excel("Chapter06LRA(GTMmic0).xlsx", sheet = "Rank")
items <- read_excel("Chapter06LRA(GTMmic0).xlsx", sheet = "Item")
student <- read_excel("Chapter06LRA(GTMmic0).xlsx", sheet = "Student")


### Target
dat <- read_csv("sampleData/J15S500.csv") %>%
  mutate(Student = as.factor(Student))

tmp <- Exametrika::dataFormat(dat, na = -99)
U <- ifelse(is.na(tmp$U), 0, tmp$U) * tmp$Z

model <- LRA(tmp, ncls = 6)
plot(model,type="IRP")
plot(model,type="TRP")
plot(model,type="LCD")
plot(model,type="CMP",students = 1:15,nr=5,nc=3)
### test
test_that("LCA Test Info", {
  expect <- test[15:30, 2] %>%
    unlist() %>%
    unname() %>%
    as.numeric()
  expect <- expect[c(5, 1, 2, 6, 3, 7, 4, 8:16)]
  result <- model$TestFitIndices %>% as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("LRA Class Info", {
  ## TRP
  expect <- class[1, 2:7] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$TRP |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## LCD
  expect <- class[2, 2:7] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$LCD |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## CMD
  expect <- class[3, 2:7] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$CMD |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("LRA Item Info", {
  ## IRP
  expect <- items[, 6:11] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$IRP |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## IRP index
  expect <- items[, 12:17] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$IRPIndex |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## FitIndex
  expect <- items[, c(22, 18, 19, 23, 20, 24, 21, 25:33)] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$ItemFitIndices |>
    unlist() |>
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})

test_that("LRA Student Info", {
  ##
  expect <- items[, 6:11] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$IRP |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## IRP index
  expect <- items[, 12:17] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$IRPIndex |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## FitIndex
  expect <- items[, c(22, 18, 19, 23, 20, 24, 21, 25:33)] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$ItemFitIndices |>
    unlist() |>
    as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
})


test_that("LCA Students", {
  ## Membership
  expect <- student[, 6:12] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$Students[, 1:7] |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-4)
  ## RUO/RDO
  expect <- student[, 13:14] |>
    unlist() |>
    unname() |>
    as.numeric()
  result <- model$Students[, 8:9] |> as.numeric()
  expect_equal(result, expect, tolerance = 1e-3)
})
