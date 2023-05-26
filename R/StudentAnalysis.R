#' @title nrs
#' @description \code{nrs} The Number-right score (NRS) function returns the count of the items passed to it.
#'
#' @param U U is a data matrix of the type matrix or data.frame.
#' @param Z Z is a missing indicator matrix of the type matrix or data.frame
#' @param w w is item weight vector
#' @return This function counts and returns the number of correct answers from the data matrix.
#' @export
#' @examples
#' # nrs(x)

nrs <- function(U,Z=NULL,w=NULL){
  nc = ncol(U)
  nr = nrow(U)
  ## if Missing indicator not given
  if(is.null(Z)){
    Z = matrix(1,ncol=nc,nrow=nr)
  }
  # if item weight vector notogiven
  if(is.null(w)){
    w = rep(1, length = nc)
  }
  return((Z * U) %*% w)
}
