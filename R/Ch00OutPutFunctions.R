#' @title print.exametrika
#' @description
#' Output format for Exametria Class
#' @param x Exametrika Class object
#' @param digits printed digits
#' @param ... othre options
#' @export

print.Exametrika <- function(x, digits = 3, ...) {
  if (length(class(x)) > 1) {
    value <- class(x)[2]
  } else {
    value <- "all"
  }
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
    all = {
      print(x)
    }
  )
}
