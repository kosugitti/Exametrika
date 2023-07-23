#' @title dataFormat
#' @description
#' This function serves the role of formatting the data prior to the analysis.
#' @param data is a data matrix of the type matrix or data.frame.
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param id id indicates the column number containing the examinee ID. The default is 1.
#' If the answer pattern is contained in the first column, it is treated as if there is no ID vector.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @return
#' \describe{
#' \item{U}{Data matrix. A matrix with rows representing the sample size and columns
#'  representing the number of items, where elements are either 0 or 1. \eqn{u_{ij}=1} indicates
#'  that student i correctly answered item j, while \eqn{u_{ij}=0} means that student i answered
#'  item j incorrectly. If the data contains NA values, any value can be filled in the matrix U,
#'  represented by the following missing value index matrix Z. However, in this funciton,
#'  -1 is assigned.}
#' \item{ID}{The ID label given by the designated column or function."}
#' \item{ItemLabel}{The item names given by the provided column names or function.}
#' \item{Z}{Missing indicator matrix.\eqn{z_{uj}=1} indicates that item j is presented to Student i,
#' while \eqn{z_{ij}=0} indicates item j is NOT presented to Student i.}
#' \item{w}{item weight vector}
#' }
#' @export
#'
dataFormat <- function(data, na = NULL, id = 1, Z = NULL, w = NULL) {
  value <- if (length(class(data)) > 1) {
    tail(class(data), 1)
  } else {
    class(data)
  }
  if (value != "examData") {
    # Check if U is either a matrix or a dataframe, otherwise stop the execution
    if (!is.matrix(data) && !is.data.frame(data)) {
      stop("Data must be matrix or data.frame")
    }

    # get ID vector
    if (is.null(rownames(data))) {
      ID <- rownames(data)
    } else {
      ID <- data[, id]
    }
    if (all(ID %in% c(0, 1, NA, na))) {
      ID <- paste0("Student", seq(1, NROW(data)))
      U <- data
    } else {
      U <- data[, -id]
    }
    # get Item-labels
    ItemLabel <- colnames(U)
    if (is.null(ItemLabel)) {
      ItemLabel <- paste0("Item", seq(1, NCOL(U)))
    }

    U <- as.matrix(U)
    # Check U matrix
    if (!all(U %in% c(0, 1, NA, na))) {
      stop("Data matrix can only contain the values 0, 1, NA, and the specified missing value")
    }
    # Check if Z is indicator matrix,or not.
    if (!is.null(Z)) {
      if (!all(Z %in% c(0, 1))) {
        stop("The missing indicator matrix must contains only 0 or 1")
      }
    }

    ### This function finally makes each matrix as follow:
    # U is a matrix composed solely of 0s,1s and NA.
    # Z is the missing identifier matrix composed solely of 0s and 1s.
    if (!is.null(na)) {
      ## na value specified
      U <- ifelse(U == na, -1, U)
    }

    Z <- ifelse(U == -1, 0, 1)

    # If w is not specified, create a vector of 1s with length equal to the number of columns in U
    if (is.null(w)) {
      w <- rep(1, NCOL(U))
    }

    # check sd for each items
    sd.check <- apply(U, 2, function(x) sd(x, na.rm = T))
    if (sum(is.na(sd.check)) != 0) {
      message("The following items with no variance.Excluded from the data.")
      cat(ItemLabel[is.na(sd.check)])
      cat("\n\n")
      U <- U[, !is.na(sd.check)]
      Z <- Z[, !is.na(sd.check)]
      ItemLabel <- ItemLabel[!is.na(sd.check)]
      w <- w[!is.na(sd.check)]
    }

    # Return
    ret <- structure(
      list(
        U = U, ID = ID, ItemLabel = ItemLabel,
        Z = Z, w = w
      ),
      class = c("Exametrika", "examData")
    )
    return(ret)
  } else {
    return(data)
  }
}


#' @title dataFormatter for long-type data
#' @description
#' A function to reshape long data into a dataset suitable for Exametrika.
#' @param data is a data matrix of the type matrix or data.frame.This must
#' contain at least three columns to identify the student, the item, and
#' the response. Additionally, it can include a column for the weight of
#' the items.
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param Sid Specify the column number containing the studnet ID label vector.
#' @param Qid Specify the column number containing the Question label vector.
#' @param Resp Specify the column number containing the Response value vector.
#' @param w Specify the column number containing the weight vector.
#' @return
#' \describe{
#' \item{U}{Data matrix. A matrix with rows representing the sample size and columns
#'  representing the number of items, where elements are either 0 or 1. \eqn{u_{ij}=1} indicates
#'   that student i correctly answered item j, while \eqn{u_{ij}=0} means that student i answered
#'    item j incorrectly.}
#' \item{ID}{The ID label given by the designated column or function."}
#' \item{ItemLabel}{The item names given by the provided column names or function.}
#' \item{Z}{Missing indicator matrix.\eqn{z_{uj}=1} indicates that item j is presented to Student i,
#' while \eqn{z_{ij}=0} indicates item j is NOT presented to Student i.}
#' \item{w}{item weight vector}
#' }
#' @export
#'
dataFormat.long <- function(data, na = NULL,
                            Sid = NULL, Qid = NULL,
                            Resp = NULL, w = NULL) {
  value <- if (length(class(data)) > 1) {
    tail(class(data), 1)
  } else {
    class(data)
  }

  if (value != "examData") {
    # Check if U is either a matrix or a dataframe, otherwise stop the execution
    if (!is.matrix(data) && !is.data.frame(data)) {
      stop("Data must be matrix or data.frame")
    }

    if (is.null(Sid)) {
      stop("column number for identifier for Student must be specified.")
    }

    if (is.null(Qid)) {
      stop("column number for identifier for Quesitons must be specified.")
    }

    if (is.null(Resp)) {
      stop("column number for response pattern must be specified.")
    }

    if (is.data.frame(data)) {
      Sid_vec <- data[[Sid]]
      Qid_vec <- data[[Qid]]
      Resp_vec <- data[[Resp]]
    } else {
      Sid_vec <- data[, Sid]
      Qid_vec <- data[, Qid]
      Resp_vec <- data[, Resp]
    }

    if (!is.numeric(Sid_vec)) {
      Sid_vec <- as.factor(Sid_vec)
      Sid_label <- unique(levels(Sid_vec))
      Sid_num <- as.numeric(Sid_vec)
    } else {
      Sid_num <- Sid_vec
      Sid_label <- unique(paste0("Student", Sid_num))
    }

    if (!is.numeric(Qid_vec)) {
      Qid_vec <- as.factor(Qid_vec)
      Qid_label <- unique(levels(Qid_vec))
      Qid_num <- as.numeric(Qid_vec)
    } else {
      Qid_num <- Qid_vec
      Qid_label <- unique(paste0("Q", Qid_vec))
    }

    Resp_vec <- as.numeric(Resp_vec)
    if (!is.null(na)) {
      Resp_vec[Resp_vec == na] <- NA
    }

    U <- matrix(NA, ncol = max(Qid_num), nrow = max(Sid_num))
    for (i in 1:length(Resp_vec)) {
      U[Sid_num[i], Qid_num[i]] <- Resp_vec[i]
    }
    if (!is.null(na)) {
      ## na value specified
      U <- ifelse(U == na, -1, U)
    }

    Z <- ifelse(U == -1, 0, 1)

    if (is.null(w)) {
      w <- rep(1, NCOL(U))
    } else {
      if (is.data.frame(data)) {
        w_vec <- data[[w]]
      } else {
        w_vec <- data[, w]
      }
      w <- w_vec[unique(Qid_num)]
    }

    # Return
    ret <- structure(
      list(
        U = U, ID = Sid_label, ItemLabel = Qid_label,
        Z = Z, w = w
      ),
      class = c("Exametrika", "examData")
    )
    return(ret)
  } else {
    return(ret)
  }
}
