#' @title print.exametrika
#' @description
#' Output format for Exametria Class
#' @param x Exametrika Class object
#' @param digits printed digits
#' @param ... othre options
#' @importFrom utils tail
#' @export

print.Exametrika <- function(x, digits = 3, ...) {
  value <- if (length(class(x)) > 1) tail(class(x), 1) else "all"

  switch(value,
    TestStatistics = {
      cat("Test Statics\n")
      tmp <- as.data.frame(unlist(x))
      colnames(tmp) <- "value"
      print(tmp)
    },
    Dimensionality = {
      cat("Dimensionality Analyeis\n")
      cat("Eigenvalues\n")
      print(x$Eigenvalue)
      cat("Percentage Of Variance\n")
      print(x$PerOfVar)
      cat("Cummurative Percentage\n")
      print(x$CumOfPer)
      xdim <- x$Component
      ydim <- x$Eigenvalue
      plot(xdim, ydim,
        xlab = "Number of Components",
        ylab = "Eigenvalue", type = "b"
      )
    },
    examData = {
      cat("Resp Pattern\n")
      print(x$U)
      cat("Missing Pattern\n")
      print(x$Z)
      cat("Weight\n")
      print(x$w)
    },
    IIAnalysis = {
      cat("Joint Sample Size\n")
      print(x$JSS, digits = digits)
      cat("\nJoint Correct Response Rate\n")
      print(x$JCRR, digits = digits)
      cat("\nItem Lift\n")
      print(x$IL, digits = digits)
      cat("\nMutual Information\n")
      print(x$MI, digits = digits)
      cat("\nPhi coefficient\n")
      print(x$Phi, digits = digits)
      cat("\nCorrelation Matrix\n")
      print(x$Tetrachoric, digits = digits)
    },
    CTT = {
      cat("Realiability\n")
      print(x$Reliability, digits = digits)
      cat("\nReliability Excluding Item\n")
      print(x$ReliabilityExcludingItem, digits = digits)
    },
    IRT_EAP_PSD = {
      cat("Ability")
      print(x$EAP, digits = digits)
    },
    IRT = {
      cat("Item Parameters\n")
      y <- cbind(x$params, x$itemPSD)
      print(y, digits = digits)
      cat("\nItem Fit Indices\n")
      y <- unclass(x$ItemFitIndices)
      y <- as.data.frame(y)
      print(round(y, digits))
      cat("\nTest Fit Indices\n")
      y <- unclass(x$TestFitIndices)
      y <- t(as.data.frame(y))
      colnames(y) <- "value"
      print(round(y, digits))
    },
    LCA = {
      cat("Item Reference Profile\n")
      print(x$IRP, digits = digits)
      cat("\nTest Profile\n")
      y <- rbind(x$TRP, x$LCD, x$CMD)
      rownames(y) <- c(
        "Test Reference Profile",
        "Latent Class Ditribution",
        "Class Membership Distribuiton"
      )
      colnames(y) <- paste("Class", 1:x$Nclass)
      print(round(y, digits))
      cat("\nItem Fit Indices\n")
      y <- unclass(x$ItemFitIndices)
      y <- as.data.frame(y)
      print(round(y, digits))
      cat("\nTest Fit Indices\n")
      cat(paste("Number of Latent class:", x$Nclass))
      cat(paste("\nNumber of EM cycle:", x$N_Cycle, "\n"))
      y <- unclass(x$TestFitIndices)
      y <- t(as.data.frame(y))
      colnames(y) <- "value"
      print(round(y, digits))
    },
    ModelFit = {
      tmp <- data.frame(unclass(x))
      print(tmp)
    },
    matrix = {
      class(x) <- "matrix"
      print(x, digits = digits)
    },
    all = {
      class(x) <- "list"
      print(x, digits = digits)
    }
  )
}
