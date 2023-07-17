#' @title Simple Test Statistics
#' @description
#' Statistics regarding the total score.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @return
#' \describe{
#' \item{TestLength}{Length of the test. The number of items included in the test.}
#' \item{SampleSize}{Sample size. The number of rows in the dataset.}
#' \item{Mean}{Average number of correct answers.}
#' \item{SEofMean}{Standard error of mean}
#' \item{Variance}{Variance}
#' \item{SD}{Standard Deviation}
#' \item{Skewness}{Skewness}
#' \item{Kurtosis}{Kurtosis}
#' \item{Min}{Minimum score}
#' \item{Max}{Max score}
#' \item{Range}{Range of score}
#' \item{Q1}{First quartile. Same as the 25th percentile.}
#' \item{Median}{Median.Same as the 50th percentile.}
#' \item{Q3}{Third quartile. Same as the 75th percentile.}
#' \item{IQR}{Interquartile range. It is calculated by subtracting the first quartile from the third quartile.}
#' \item{Stanine}{see [stanine]}
#' }
#' @export

TestStatistics <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  tW <- nrs(U = tmp$U, Z = tmp$Z, w = tmp$w)
  TestLength <- NCOL(tmp$Z)
  SampleSize <- NROW(tmp$Z)
  Mean <- mean(tW)
  SEofMean <- sd(tW) / sqrt(SampleSize)
  Variance <- var(tW)
  SD <- sd(tW)
  SDs <- sqrt(mean((tW - Mean)^2))
  tmpZ <- (tW - Mean) / SDs
  Skewness <- mean(tmpZ^3)
  Kurtosis <- mean(tmpZ^4) - 3
  Min <- min(tW)
  Max <- max(tW)
  Range <- Max - Min
  Q1 <- quantile(tW, probs = 0.25)
  Median <- quantile(tW, probs = 0.5)
  Q3 <- quantile(tW, probs = 0.75)
  IQR <- Q3 - Q1
  Stanine <- stanine(U = tmp$U, Z = tmp$Z, w = tmp$w)
  ret <-
    structure(list(
      TestLength = TestLength,
      SampleSize = SampleSize,
      Mean = Mean,
      SEofMean = SEofMean,
      Variance = Variance,
      SD = SD,
      Skewness = Skewness,
      Kurtosis = Kurtosis,
      Min = Min,
      Max = Max,
      Range = Range,
      Q1 = Q1,
      Median = Median,
      Q3 = Q3,
      IQR = IQR,
      Stanine = Stanine$stanine
    ), class = c("Exametrika", "TestStatistics"))
  return(ret)
}

#' @title Dimensionality
#' @description
#' The dimensionality is the number of components
#' the test is measuring.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @export

Dimensionality <- function(U, na = NULL, Z = NULL, w = NULL) {
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  R <- TetrachoricCorrelationMatrix(U = tmp$U, Z = tmp$Z, w = tmp$z)
  Esystem <- eigen(R)
  Eval <- Esystem$values
  EvalVariance <- Esystem$values / length(Eval) * 100
  CumVari <- cumsum(EvalVariance)
  ret <-
    structure(
      list(
        Component = seq(1:length(Eval)),
        Eigenvalue = Eval,
        PerOfVar = EvalVariance,
        CumOfPer = CumVari
      ),
      class = c("Exametrika", "Dimensionality")
    )

  return(ret)
}
