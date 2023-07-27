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
      cat("\nModel Fit Indices\n")
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
      cat("\nModel Fit Indices\n")
      cat(paste("Number of Latent class:", x$Nclass))
      cat(paste("\nNumber of EM cycle:", x$N_Cycle, "\n"))
      y <- unclass(x$TestFitIndices)
      y <- t(as.data.frame(y))
      colnames(y) <- "value"
      print(round(y, digits))
    },
    LRA = {
      cat(paste("estimating method is ", x$method))
      if (x$mic) {
        cat("\n Monotonic increasing IRP option is TRUE.\n")
      }
      cat("Item Reference Profile\n")
      print(x$IRP, digits = digits)
      cat("\nItem Reference Profile Indices\n")
      print(x$IRPIndex, digits = digits)
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
      cat("\nModel Fit Indices\n")
      cat(paste("Number of Latent class:", x$Nclass))
      cat(paste("\nNumber of EM cycle:", x$N_Cycle, "\n"))
      y <- unclass(x$TestFitIndices)
      y <- t(as.data.frame(y))
      colnames(y) <- "value"
      print(round(y, digits))
    },
    Biclustering = {
      if (x$model == 1) {
        model <- "Biclustering"
        msg1 <- "Bicluster"
        msg2 <- "Class"
      } else {
        model <- "Ranklustering"
        msg1 <- "Rankluster"
        msg2 <- "Rank"
      }
      if (x$mic) {
        model <- paste(model, "with MIC option.")
      }
      cat(paste(msg1, "Matrix Profile\n"))
      print(x$FRP, digits = digits)

      cat("\nField Reference Profile Indices\n")
      print(x$FRPIndex, digits = digits)
      cat("\n")

      y <- rbind(x$TRP, x$LRD, x$CMD)
      rownames(y) <- c(
        "Test Reference Profile",
        paste("Latent", msg2, "Ditribution"),
        paste(msg2, "Membership Distribuiton")
      )
      colnames(y) <- paste(msg2, 1:x$Nclass)
      print(round(y, digits))

      if (x$model == 2) {
        cat("\nField Membership Profile\n")
        y <- format(
          round(as.data.frame(x$FieldMembership),digits)
          ,nsmall=digits)
        print(y)
      }

      cat("Latent Field Distribution\n")
      y <- matrix(x$LFD, byrow = T, nrow = 1)
      rownames(y) <- "N of Items"
      colnames(y) <- paste("Field", 1:x$Nfield)
      print(round(y, digits))
      cat("\nModel Fit Indices\n")
      cat(paste("Number of Latent", msg2, ":", x$Nclass))
      cat(paste("\nNumber of Latent Field:", x$Nfield))
      cat(paste("\nNumber of EM cycle:", x$N_Cycle, "\n"))
      y <- unclass(x$TestFitIndices)
      y <- t(as.data.frame(y))
      colnames(y) <- "value"
      print(round(y, digits))
      if (x$SOACflg) {
        cat("Strongly Ordinal Alignment Condition is Satisfied.\n")
      }
      if (x$WOACflg) {
        cat("Weakly Ordinal Alignment Condition is Satisfied.\n")
      }
    },
    FieldAnalysis = {
      cat("Field Analysis Matrix\n")
      nr <- NROW(x$FieldAnalysisMatrix)
      nc <- NCOL(x$FieldAnalysisMatrix)
      rnames <- rownames(x$FieldAnalysisMatrix)
      cnames <- colnames(x$FieldAnalysisMatrix)
      yy <- x$FieldAnalysisMatrix
      crr <- round(yy[,1],digits)
      LFE <- yy[,2]
      Fields <- round(yy[,-(1:2)],digits)
      y <- cbind(crr,LFE,Fields)
      print(y)
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

