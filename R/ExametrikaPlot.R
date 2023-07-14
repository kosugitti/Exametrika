#' @title Plotting functions for the Exametrika package of class "Exametrika"
#' @description
#' Combines several plotting functions into one for objects of class "Exametrika".
#' This can be used to plot the results of [IRT], [LCA].
#' @param x Exametrika Class object
#' @param type Plot type.Specify one of the following: "IIC", "ICC", "TIC", "IRP", "TRP", "LCD", "CMP".
#' "IIC", "ICC", "TIC" plot the results of [IRT] (Item Response Theory),
#' while "IRP", "TRP", "LCD", "CMP" plot the results of [LCA] (Latent Class Analysis)."
#' @param items Specify the items you want to plot as a vector. If not specifically designated,
#' all items will be included.When the type is IIC, if the specified item is 0, it returns a TIC
#' representing the entire test.
#' @param students Specify the numbers of the students you want to plot as a vector.
#' If not specifically designated, all students will be included.
#' @param nc Specifying the number of columns when there are many plots to be drawn. The default is 1.
#' @param nr Specifying the number of rows when there are many plots to be drawn. The default is 1.
#' @param ... other options
#' @details
#' Each model has its own plot.
#' In [IRT] (Item Response Theory), we can visually check the characteristics of items and the entire test
#' by observing the item response curve("ICC"), item information curve("IIC"), and test information curve("TIC").
#'
#' In [LCA] (Latent Class Analysis), an Item Reference Profile("IRP") that represents the correspondence
#' between items and latent classes is plotted. This places the latent classes on the x-axis, and the
#' proportion of students belonging to that class who answer the item correctly on the y-axis.
#' The Test Reference Profile("TRP") and Latent Class Distribution("LCD") are plots that display two types of
#' information simultaneously. One is a bar graph showing the number of members in each class, and the other
#' is a line graph showing the average score taken by students in each membership class
#' and the class membership distribution.
#' The Class Membership Profile("CMP") visualizes the probability of a student belonging to each latent class.
#'
#' @importFrom graphics curve
#' @importFrom utils tail
#' @importFrom graphics axis barplot mtext par text
#' @export

plot.Exametrika <- function(x,
                            type = c(
                              "IIC", "ICC", "TIC",
                              "IRP", "TRP", "LCD", "CMP"
                            ),
                            items = NULL,
                            students = NULL,
                            nc = 1,
                            nr = 1, ...) {
  value <- if (length(class(x)) > 1) tail(class(x), 1) else "None"
  par(mfrow = c(nr, nc))
  testlength <- x$testlength
  nobs <- x$nobs

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

  plotStudentID <- if (!is.null(students)) {
    if (!is.numeric(students) || length(students) > nobs || any(students < 0 | students > nobs)) {
      stop("'students' must be a numeric vector of length at most ", nobs, " and contain numbers between 0 and ", nobs)
    }
    students
  } else {
    1:nobs
  }

  switch(value,
    IRT = {
      if ((type %in% c("IIC", "TIC") && any(plotItemID == 0)) ||
        (type == "ICC" && all(plotItemID == 0))) {
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

      params <- x$params[plotItemID, ]
      if (type == "ICC") {
        plotIRTCurve(
          params, Exametrika::LogisticModel,
          "Item Characteristic Curve", "probability"
        )
      }
      if (type == "IIC") {
        plotIRTCurve(
          params, Exametrika::ItemInformationFunc,
          "Item Information Curve", "information"
        )
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
    LCA = {
      if (type == "IRP") {
        # Item Reference Profile ----------------------------------------
        params <- x$IRP[plotItemID, ]
        for (i in 1:nrow(params)) {
          y <- params[i, ]
          plot(y,
            type = "b",
            ylab = "Correct Response Rate",
            xlab = "Latent Class",
            ylim = c(0, 1),
            main = paste("Item", i)
          )
        }
      }
      if (type == "TRP") {
        # Test Reference Profile ----------------------------------------
        old_par <- par(no.readonly = TRUE)
        par(mar = c(5, 4, 4, 4) + 0.1)
        bp <- barplot(x$LCD,
          names.arg = 1:x$Nclass,
          ylim = c(0, max(x$LCD) + 10),
          xlim = c(0, x$Nclass + 1),
          xlab = "Latent Class",
          ylab = "Number of Students"
        )
        text(x = bp, y = x$LCD, label = x$LCD, pos = 1, cex = 1.2)
        par(new = TRUE)
        plot(bp, x$TRP,
          type = "b", pch = 19, lty = 1,
          axes = FALSE, xaxt = "n", xlab = "", ylab = "",
          bty = "n",
          ylim = c(0, testlength),
          xlim = c(0, x$Nclass + 1),
        )
        axis(4, at = pretty(range(0, testlength)))
        mtext("Expected Score", side = 4, line = 3)
        par(old_par)
      }
      if (type == "LCD") {
        # Latent Class Distribution ----------------------------------------
        old_par <- par(no.readonly = TRUE)
        par(mar = c(5, 4, 4, 4) + 0.1)
        bp <- barplot(x$LCD,
          names.arg = 1:x$Nclass,
          ylim = c(0, max(x$LCD) + 10),
          xlim = c(0, x$Nclass + 1),
          xlab = "Latent Class",
          ylab = "Number of Students"
        )
        text(x = bp, y = x$LCD, label = x$LCD, pos = 1, cex = 1.2)
        par(new = TRUE)
        plot(bp, x$CMD,
          type = "b", pch = 19, lty = 1,
          axes = FALSE, xaxt = "n", xlab = "", ylab = "",
          bty = "n",
          ylim = c(0, max(x$LCD) + 10),
          xlim = c(0, x$Nclass + 1),
        )
        axis(4, at = pretty(range(0, max(x$LCD) + 10)))
        mtext("Frequency", side = 4, line = 3)
        par(old_par)
      }
      if (type == "CMP") {
        # Class Membership Profile ----------------------------------------
        params <- x$Students[plotStudentID, 1:x$Nclass]
        for (i in 1:NROW(params)) {
          y <- params[i, ]
          plot(y,
            type = "b",
            xlab = "Latent Class",
            ylab = "Membership",
            ylim = c(0, 1),
            main = paste("Student", plotStudentID[i])
          )
        }
      }
    },
    none = {
      cat("Sorry, this is not an object that can be plotted.")
    }
  )
}
