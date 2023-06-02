#' @title dataFormat
#' @description
#' @param data is a data matrix of the type matrix or data.frame.
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param id id indicates the column number containing the examinee ID. The default is 1.
#' If the answer pattern is contained in the first column, it is treated as if there is no ID vector.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @export
#' @return
#'
dataFormat <- function(data, na = NULL, id = 1, Z = NULL, w = NULL) {
  # Check if U is either a matrix or a dataframe, otherwise stop the execution
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be matrix or data.frame")
  }

  # get ID vector
  ID <- data[, id]
  if (all(ID %in% c(0, 1, NA, na))) {
    ID <- paste0("Student", seq(1:NROW(data)))
    U <- data
  } else {
    U <- data[, -id]
  }
  # get Item-labels
  ItemLabel <- colnames(U)
  if (is.null(ItemLabel)) {
    ItemLabel <- paste0("Item", seq(1:NCOL(U)))
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
    U <- ifelse(U == na, NA, U)
  }

  Z <- ifelse(is.na(U), 0, 1)

  # If w is not specified, create a vector of 1s with length equal to the number of columns in U
  if (is.null(w)) {
    w <- rep(1, NCOL(U))
  }

  # Return the resulting U, Una, Z, and w
  return(list(U = U, ID = ID, ItemLabel = ItemLabel, Z = Z, w = w))
}
