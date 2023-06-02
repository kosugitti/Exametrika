#' @title Simple Test Statistics
#' @description
#' Statistics regarding the total score.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @export

TestStatistics <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
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
  Kurtosis <- mean(tmpZ^4)
  Min <- min(tW)
  Max <- max(tW)
  Range <- Max - Min
  Q1 <- quantile(tW, probs = 0.25)
  Median <- quantile(tW, probs = 0.5)
  Q3 <- quantile(tW, probs = 0.75)
  IQR <- Q3 - Q1
  Stanine <- stanine(U = tmp$U, Z = tmp$Z, w = tmp$w)
  df <- as.data.frame(list(
    name = c(
      "Test Length", "Sample Size",
      "Mean", "SE of Mean", "Variance",
      "SD", "Skewness", "Kurtosis",
      "Min", "Max", "Range",
      "Q1(25%)", "Median(50%)", "Q3(75%)",
      "Inter Quantile Range",
      "Q2(Stanine)", "Q3(Stanine)", "Q4(Stanine)",
      "Q5(Stanine)", "Q6(Stanine)", "Q7(Stanine)",
      "Q8(Stanine)", "Q9(Stanine)"
    ),
    value = c(
      TestLength, SampleSize,
      Mean, SEofMean, Variance, SD, Skewness, Kurtosis,
      Min, Max, Range, Q1, Median, Q3, IQR, Stanine$stanine
    )
  ))
  return(df)
}

#' @title Dimensionality Analysis
#' @description
#' The dimensionality is the number of contens ( or conponents)
#' the test is measuring.
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @export

DimensonalityAnalysis <- function(U, na = NULL, Z = NULL, w = NULL) {
  tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  R <- TetrachoricCorrelationMatrix(U = tmp$U, Z = tmp$Z, w = tmp$z)
  Esystem <- eigen(R)
  Eval <- Esystem$values
  EvalVariance <- Esystem$values / length(Eval)
  CumVari <- cumsum(Esystem$values / 20)
  df <- data.frame(list(
    Component = seq(1:length(Eval)),
    Eigenvalue = Eval,
    PercentageOfVariance = EvalVariance,
    CumulativeVariance = CumVari
  ))
  return(df)
}
