rm(list = ls())
library(tidyverse)
library(Exametrika)
library(igraph)

DAG <-
  matrix(
    c(
      "Item01", "Item02",
      "Item02", "Item03",
      "Item02", "Item04",
      "Item03", "Item05",
      "Item04", "Item05"
    ),
    ncol = 2, byrow = T
  )

g <- igraph::graph_from_data_frame(DAG)
#
# graph_file <- "develop/EdgesBNM_R.csv"
# g <- read.csv("develop/EdgesBNM_R.csv", header = FALSE) %>% graph_from_data_frame()


plot(g)
adj <- get.adjacency(g) %>% as.matrix()

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


# PIRP ; Parent Item Response Pattern ------------------------------------------------------------

dat <- dataFormat(J5S10)
tmp <- dataFormat(dat)
testlength <- ncol(dat$U)
nobs <- nrow(dat$U)
#
#
# dat <- dataFormat(J15S500)
# tmp <- dataFormat(dat)
# testlength <- ncol(dat$U)
# nobs <- nrow(dat$U)
# g <- GA_g

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
    # For each element of the matrix, calculate the decimal value from the binary representation
    # represented by the row of the pir list at the corresponding index
    # rev() reverses the binary vector, which() identifies the indices of 1s,
    # and sum(2^(indices - 1)) converts binary to decimal
    PIRP_mat[s, j] <- sum(2^(which(rev(pir[[j]][s, ]) == 1) - 1)) + 1
  }
}


item_pattern_max <- 2^max(npa)
PIRP_array <- array(0,dim=c(nobs,testlength,item_pattern_max))
for(s in 1:nobs){
  for(j in 1:testlength){
    PIRP_array[s,j,PIRP_mat[s,j]] <- 1
  }
}

n_PIRP_1 <- matrix(nrow=testlength,ncol=item_pattern_max)
n_PIRP_0 <- matrix(nrow=testlength,ncol=item_pattern_max)
for(i in 1:item_pattern_max){
  n_PIRP_1[,i] <- colSums(dat$U * PIRP_array[,,i])
  n_PIRP_0[,i] <- colSums(dat$Z * (1-dat$U) * PIRP_array[,,i])
}


deno <- n_PIRP_0 + n_PIRP_1 + beta0 + beta1 -2
denom0 <- sign(n_PIRP_1 + n_PIRP_0)

param <- (n_PIRP_1 + beta1 - 1)/deno


# Model Fit -------------------------------------------------------

const <- exp(-testlength)
model_loglike <- sum(n_PIRP_1 * log(param*denom0+const) + n_PIRP_0 * log(1-param*denom0+const),na.rm=T)
model_nparam <- sum(denom0)
ret <- TestFit(dat$U,dat$Z,model_loglike,model_nparam)
ret


## for output
item_ptn <- 2^colSums(adj)
CCRR_table <- as.data.frame(matrix(NA,nrow = sum(item_ptn),ncol=5))
colnames(CCRR_table) <- c("Child Item","N of Parents","Parent Items","PIRP","Conditional CRR")
CCRR_table[,1] <- rep(tmp$ItemLabel,item_ptn)
CCRR_table[,2] <- rep(colSums(adj),item_ptn)
parent_items <- lapply(pir, function(mat) {
  paste(colnames(mat), collapse = ", ")
})
CCRR_table[,3] <- rep(unlist(parent_items),item_ptn)

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

CCRR_table[,4] <- unlist(sapply(colSums(adj),BitRespPtn))

vec <- numeric(sum(item_ptn))
cnt <- 0
for(j in 1:testlength){
  for(i in 1:item_ptn[j]){
    cnt <- cnt +1
    vec[cnt] <- param[j,i]
    if(is.nan(param[j,i])){vec[cnt] <- "NaN(0/0)"}
  }
}
CCRR_table[,5] <- vec
