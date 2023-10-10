#' @title Bayesian Network Model
#' @description
#' performs Bayseia Network Model with specified graph stracture
#' @details
#' This function performs a Bayesian network analysis on the relationships
#' between items. This corresponds to Chapter 8 of the text. It uses the igraph
#' package for graph visualization and checking the adjacency matrix.
#' You need to provide either a graph object or a CSV file where the graph
#'  structure is specified.
#' @param U U is either a data class of Exametrika, or raw data. When raw data is given,
#' it is converted to the Exametrika class with the [dataFormat] function.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @param na na argument specifies the numbers or characters to be treated as missing values.
#' @param DAG Specify a graph object suitable for the igraph class.
#' @param DAG_file specify CSV file where the graph structure is specified.
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph get.adjacency
#' @importFrom utils read.csv
#' @importFrom igraph V
#' @return
#' \describe{
#'  \item{nobs}{Sample size. The number of rows in the dataset.}
#'  \item{testlength}{Length of the test. The number of items included in the test.}
#'  \item{crr}{correct response ratio}
#'  \item{TestFitIndices}{Overall fit index for the test.See also [TestFit]}
#'  \item{adj}{Adjacency matrix}\
#'  \item{param}{Learned Parameters}
#'  \item{CCRR_table}{Correct Response Rate tables}
#' }
#' @export

BNM <- function(U, Z = NULL, w = NULL, na = NULL, DAG = NULL, DAG_file = NULL) {
  # data format
  if (class(U)[1] != "Exametrika") {
    tmp <- dataFormat(data = U, na = na, Z = Z, w = w)
  } else {
    tmp <- U
  }
  U <- tmp$U * tmp$Z
  testlength <- NCOL(tmp$U)
  nobs <- NROW(tmp$U)

  # graph check
  if (is.null(DAG) && is.null(DAG_file)) {
    stop("You need to provide either a graph object or a CSV file.")
  }
  if (!is.null(DAG)) {
    value <- class(DAG)
    if (value != "igraph") {
      stop("The provided graph is not compatible with the igraph class.")
    } else {
      g <- DAG
    }
  } else {
    g <- graph_from_data_frame(read.csv(DAG_file, header = FALSE))
  }
  graph_label <- V(g)$name
  if (!any(graph_label %in% tmp$ItemLabel)) {
    stop("Some labels in the graph do not align with the item labels.")
  }

  # get Adj matrix
  adj <- as.matrix(get.adjacency(g))

  ### Adj mat check
  adjU <- adj + t(adj)
  simpleFLG <- ifelse(max(adjU) <= 1, 1, 0)
  testlength <- ncol(adj)
  acyclicFLG <- 0
  connectedFLG <- 0
  for (i in 1:(testlength - 1)) {
    acyclicFLG <- acyclicFLG + sum(diag(adj^i))
    connectedFLG <- connectedFLG + min(sum(adjU^i))
  }
  acyclicFLG <- ifelse(acyclicFLG == 0, 1, 0)
  connectedFLG <- ifelse(connectedFLG > 0, 1, 0)
  dag <- simpleFLG * acyclicFLG
  cdag <- dag * connectedFLG

  # Initialize
  beta0 <- beta1 <- 1
  npa <- colSums(adj)

  pir <- lapply(1:length(npa), function(i) {
    if (npa[i] > 0) {
      mat <- as.matrix(tmp$U[, adj[, i] == 1])
      colnames(mat) <- colnames(tmp$U)[adj[, i] == 1]
      return(mat)
    } else {
      mat <- as.matrix(rep(0, nrow(tmp$U)))
      colnames(mat) <- "No Parents"
      return(mat)
    }
  })

  PIRP_mat <- matrix(nrow = nobs, ncol = testlength)
  for (s in 1:nobs) {
    for (j in 1:testlength) {
      # For each element of the matrix, calculate the decimal value from
      # the binary representation represented by the row of the pir list
      # at the corresponding index rev() reverses the binary vector,
      # which() identifies the indices of 1s,
      # and sum(2^(indices - 1)) converts binary to decimal
      PIRP_mat[s, j] <- sum(2^(which(rev(pir[[j]][s, ]) == 1) - 1)) + 1
    }
  }

  item_pattern_max <- 2^max(npa)
  PIRP_array <- array(0, dim = c(nobs, testlength, item_pattern_max))
  for (s in 1:nobs) {
    for (j in 1:testlength) {
      PIRP_array[s, j, PIRP_mat[s, j]] <- 1
    }
  }

  n_PIRP_1 <- matrix(nrow = testlength, ncol = item_pattern_max)
  n_PIRP_0 <- matrix(nrow = testlength, ncol = item_pattern_max)
  for (i in 1:item_pattern_max) {
    n_PIRP_1[, i] <- colSums(tmp$U * PIRP_array[, , i])
    n_PIRP_0[, i] <- colSums(tmp$Z * (1 - tmp$U) * PIRP_array[, , i])
  }

  deno <- n_PIRP_0 + n_PIRP_1 + beta0 + beta1 - 2
  denom0 <- sign(n_PIRP_1 + n_PIRP_0)

  param <- (n_PIRP_1 + beta1 - 1) / deno
  rownames(param) <- tmp$ItemLabel
  colnames(param) <- paste("PIRP", 1:ncol(param))

  # Model Fit
  const <- exp(-testlength)
  model_loglike <- sum(n_PIRP_1 * log(param * denom0 + const) + n_PIRP_0 * log(1 - param * denom0 + const), na.rm = T)
  model_nparam <- sum(denom0)
  FitIndices <- TestFit(tmp$U, tmp$Z, model_loglike, model_nparam)

  ## for output
  item_ptn <- 2^colSums(adj)
  for (j in 1:testlength) {
    for (i in 1:item_pattern_max) {
      if (i > item_ptn[j]) {
        param[j, i] <- NA
      }
    }
  }

  CCRR_table <- as.data.frame(matrix(NA, nrow = sum(item_ptn), ncol = 5))
  colnames(CCRR_table) <- c("Child Item", "N of Parents", "Parent Items", "PIRP", "Conditional CRR")
  CCRR_table[, 1] <- rep(tmp$ItemLabel, item_ptn)
  CCRR_table[, 2] <- rep(colSums(adj), item_ptn)
  parent_items <- lapply(pir, function(mat) {
    paste(colnames(mat), collapse = ", ")
  })
  CCRR_table[, 3] <- rep(unlist(parent_items), item_ptn)
  ### Inner function to make Bit Pattern
  BitRespPtn <- function(n) {
    if (n == 0) {
      ptn <- "No Pattern"
    } else {
      ptn <- sapply(0:(2^n - 1), function(x) {
        binary_str <- as.integer(intToBits(x)[1:n])
        paste0(rev(binary_str), collapse = "")
      })
    }
    return(ptn)
  }
  CCRR_table[, 4] <- unlist(sapply(colSums(adj), BitRespPtn))

  vec <- numeric(sum(item_ptn))
  cnt <- 0
  for (j in 1:testlength) {
    for (i in 1:item_ptn[j]) {
      cnt <- cnt + 1
      vec[cnt] <- sprintf("%.7f", as.numeric(param[j, i]))
      if (is.nan(param[j, i])) {
        vec[cnt] <- "NaN(0/0)"
      }
    }
  }
  CCRR_table[, 5] <- vec

  ret <- structure(list(
    U = U,
    testlength = testlength,
    nobs = nobs,
    crr = crr(U),
    ItemLabel = tmp$ItemLabel,
    adj = adj,
    g = g,
    acyclicFLG = acyclicFLG,
    connectedFLG = connectedFLG,
    param = param,
    TestFitIndices = FitIndices,
    CCRR_table = CCRR_table
  ), class = c("Exametrika", "BNM"))
  return(ret)
}
