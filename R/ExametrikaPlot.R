#' @title Plot Exametrika Output
#' @description
#' Plot format for Exametria Class
#' @param x Exametrika Class object
#' @param type Plot type
#' @param items Plot item number
#' @param ... othre optios
#' @importFrom graphics curve
#' @importFrom utils tail
#' @export

plot.Exametrika <- function(x, type = c("IIC", "ICC", "TIC"), items = NULL, ...) {
  value <- if (length(class(x)) > 1) tail(class(x), 1) else "None"
  testlength <- nrow(x$params)

  if (missing(type)) {
    stop("The 'type' argument must be specified.")
  }

  plotItemID <- if (!is.null(items)) {
    if (!is.numeric(items) || length(items) > testlength || any(items < 0 | items > testlength)) {
      stop("'items' must be a numeric vector of length at most ", testlength, " and contain numbers between 0 and ", testlength)
    }
    items
  } else {
    1:testlength
  }

  if ((type %in% c("IIC", "TIC") && any(plotItemID == 0)) || (type == "ICC" && all(plotItemID == 0))) {
    type <- "TIC"
    plotItemID <- 1:testlength
  } else if (type == "ICC" && any(plotItemID == 0)) {
    plotItemID <- plotItemID[plotItemID != 0]
    if (length(plotItemID) == 0) {
      plotItemID <- 1:testlength
    }
  }

  ### IRT curve function
  plotIRTCurve <- function(params, curveFunc, titleBase, ylab) {
    for (i in 1:nrow(params)) {
      a <- params[i, 1]
      b <- params[i, 2]
      c <- if (x$model > 2) params[i, 3] else 0
      d <- if (x$model > 3) params[i, 4] else 1
      title <- paste0(titleBase, ", item ", plotItemID[i])
      curve(curveFunc(a, b, c, d, theta = x),
        from = -4,
        to = 4,
        xlab = "ability", ylab = ylab,
        main = title
      )
    }
  }


  switch(value,
    IRT = {
      params <- x$params[plotItemID, ]
      if (type == "ICC") {
        plotIRTCurve(params, Exametrika::LogisticModel, "Item Characteristic Curve", "probability")
      }
      if (type == "IIC") {
        plotIRTCurve(params, Exametrika::ItemInformationFunc, "Item Information Curve", "information")
      }
      if (type == "TIC") {
        curve(Exametrika::TestInformationFunc(params, theta = x),
          from = -4,
          to = 4,
          xlab = "ability", ylab = "Information",
          main = "Test Informaiton Curve"
        )
      }
    },
    none = {
      cat("Sorry, this is not an object that can be plotted.")
    }
  )
}
