rm(list = ls())
library(Exametrika)
library(igraph)

dat <- dataFormat(J5S10)
# dat <- dataFormat(J15S500)
# dat <- dataFormat(J35S515)
tmp <- dataFormat(dat)
testlength <- ncol(dat$U)
nobs <- nrow(dat$U)

# Simple GA -------------------------------------------------------

crr <- crr(tmp)
sort_list <- order(crr, decreasing = TRUE)
adj_sort <- data.frame(item = tmp$ItemLabel, crr = crr)
adj <- matrix(0, ncol = testlength, nrow = testlength)
adj[upper.tri(adj)] <- 1
colnames(adj) <- rownames(adj) <- tmp$ItemLabel[sort_list]
gene_length <- sum(upper.tri(adj))

### Initialize
population <- 20
Rs <- 0.3 # survival rate
Rm <- 0.002 # mutation rate
RsI  <- round(population * Rs)

adj <- matrix(0, ncol = testlength, nrow = testlength)
colnames(adj) <- rownames(adj) <- tmp$ItemLabel[sort_list]

# set.seed(12345)
gene_list <- matrix(0, nrow = population, ncol = gene_length)

# maximum indegree per item(parents) is 2
maxParents <- 2

# maxParents function
maxParents_penalty <- function(vec,testlength,maxParents){
  adj_check <- matrix(0, ncol = testlength, nrow = testlength)
  adj_check[upper.tri(adj_check)] <- vec
  for(i in 2:testlength){
    vec <- adj_check[,i]
    if(sum(vec)>maxParents){
      ones_index <- which(vec==1)
      selection <- sample(ones_index,maxParents)
      vec[-selection] <- 0
      adj_check[,i] <- vec
    }
  }
  vec <- adj_check[upper.tri(adj_check)]
  return(vec)
}


for (j in 1:population) {
  adj_gene <- rbinom(gene_length,1,0.5)
  gene_list[j, ] <- maxParents_penalty(adj_gene,testlength,maxParents)
}

# Type of crossover. optional. Uniform(0), Single-point(1), Two-points(2)
crossover <- 1
# elitism selection 1
elitism <- 3
if (elitism < 0) {
  elitism <- 0
} else if (elitism > population * Rs * 0.5) {
  elitism <- round(population * Rs * 0.5)
  print(paste("Too many elites. Limit to ", elitism))
}

max_generation <- 100
bestfit <- 1e+100
limit_count <- 0
successive_limit <- 5

fit_log <- numeric(max_generation)

GA_FLG <- TRUE
generation <- 0
while (GA_FLG) {
  generation <- generation + 1
  print(paste("gen.", generation, "best BIC", bestfit, "limit count", limit_count))
  fitness <- numeric(population)
  for (i in 1:population) {
    # make adj from genes
    adj[upper.tri(adj)] <- gene_list[i, ]
    g <- graph_from_adjacency_matrix(adj)
    # Fitness
    ret <- BNM(tmp, DAG = g)
    fitness[i] <- ret$TestFitIndices$BIC
  }

  # Termination check
  if (generation > max_generation) {
    print("The maximum generation has been reached")
    GA_FLG <- FALSE
  }
  if (sort(fitness)[1] == bestfit) {
    limit_count <- limit_count + 1
  } else {
    limit_count <- 0
    bestfit <- sort(fitness)[1]
  }
  if (limit_count >= successive_limit) {
    print(paste("The BIC has not changed for", successive_limit, " times."))
    GA_FLG <- FALSE
  }

  fit_log[generation] <- sort(fitness)[1]

  # Selection
  new_gene_list <- matrix(NA, nrow = population, ncol = gene_length)
  survivers <- order(fitness)[1:RsI]
  new_gene_list[1:RsI, ] <- gene_list[survivers, ]

  # elitism
  if (elitism > 0) {
    new_gene_list[(RsI + 1):(RsI + elitism), ] <-
      new_gene_list[1:elitism, ]
  }
  # new gene
  for (i in (RsI + elitism + 1):population) {
    parents_num <- sample(1:(RsI + elitism), size = 2)
    parent1 <- new_gene_list[parents_num[1], ]
    parent2 <- new_gene_list[parents_num[2], ]
    # CrossOver
    if (crossover == 2) {
      # two-points crossover
      combinations <- combn(1:gene_length, 2)
      # Filter out combinations where the two numbers are adjacent
      combinations <- combinations[, abs(combinations[1, ] - combinations[2, ]) > 1]
      # Exclude combinations that start with 1 and 9
      combinations <- combinations[, combinations[1, ] != 1]
      combinations <- combinations[, combinations[2, ] != (gene_length)]
      cut_off <- combinations[, sample(1:ncol(combinations), size = 1)]
      child <- c(
        parent1[1:(cut_off[1] - 1)],
        parent2[cut_off[1]:cut_off[2]],
        parent1[(cut_off[2] + 1):gene_length]
      )
    } else if (crossover == 1) {
      # single point crossover
      cut_off <- sample(2:(gene_length - 1), 1)
      child <- c(parent1[1:cut_off], parent2[(cut_off + 1):gene_length])
    } else {
      # uniform crossover
      inherent <- sample(parents_num, size = gene_length, replace = TRUE)
      child <- numeric(gene_length)
      for (pos in 1:gene_length) {
        child[pos] <- new_gene_list[inherent[pos], pos]
      }
    }
    # Mutation
    Bm <- rbinom(gene_length, 1, Rm)
    child <- (1 - Bm) * child + Bm * (1 - child)

    # Omit Null Model
    if(sum(child)==0){
      pos <- sample(1:gene_length,1)
      child[pos] <- 1
    }

    child <- maxParents_penalty(child,testlength,maxParents)

    ## new_gene
    new_gene_list[i, ] <- child
  }

  gene_list <- new_gene_list
  old_fitness <- fitness
}

## Estimates GA
adj_best <- gene_list[1, ]
adj[upper.tri(adj)] <- adj_best
GA_g <- graph_from_adjacency_matrix(adj)
ret1 <- BNM(tmp$U, DAG = GA_g)



# PBIL ------------------------------------------------------------

crr <- crr(tmp)
sort_list <- order(crr, decreasing = TRUE)
adj_sort <- data.frame(item = tmp$ItemLabel, crr = crr)
adj_sort[sort_list, ]
adj <- matrix(0, ncol = testlength, nrow = testlength)
adj[upper.tri(adj)] <- 1
colnames(adj) <- rownames(adj) <- tmp$ItemLabel[sort_list]
gene_length <- sum(upper.tri(adj))

gene <- rep(0.5,gene_length)
adj_gene <- matrix(0, ncol = testlength, nrow = testlength)
adj_gene[upper.tri(adj_gene)] <- gene
# Initialize
population <- 100
Rs <- 0.3
RsI <- round(population * Rs)
alpha <- 0.05
Rm <- 0.002
adj_t <- matrix(0, nrow = population, ncol = gene_length)

# maximum indegree per item(parents) is 2
maxParents <- 3

max_generation <- 100
bestfit <- 1e+100
limit_count <- 0
successive_limit <- 5

fit_log2 <- numeric(max_generation)

elitism <- 1
if (elitism < 0) {
  elitism <- 0
} else if (elitism > population * Rs * 0.5) {
  elitism <- round(population * Rs * 0.5)
  print(paste("Too many elites. Limit to ", elitism))
}

GA_FLG <- TRUE
generation <- 0
for(i in 1:population){
  adj_t[i,] <- rbinom(gene_length,1,prob=gene)
}
best_individual <- adj_t[1,]

while (GA_FLG) {
  generation <- generation + 1
  print(paste("gen.", generation, "best BIC", bestfit, "limit count", limit_count))
  fitness <- numeric(population)
  for (i in 1:population) {
    # out of elite
    if(i > elitism){
      vec <- rbinom(gene_length,1,prob=gene)
      # Omit Null Model
      if(sum(vec)==0){
        pos <- sample(1:gene_length,1)
        vec[pos] <- 1
      }
      adj_t[i,] <- maxParents_penalty(vec,testlength,maxParents)
    }
    adj[upper.tri(adj)] <- adj_t[i,]
    # make adj from genes
    g <- graph_from_adjacency_matrix(adj)
    # Fitness
    ret <- BNM(tmp, DAG = g)
    fitness[i] <- ret$TestFitIndices$BIC
  }
  sort_list <- order(fitness)
  adj_t <- adj_t[sort_list,]

  ### Update Gne
  ar <- round(colMeans(adj_t[1:RsI,]))
  gene <- gene + (alpha * (ar - gene))

  ### mutation
  u <- runif(gene_length,min=0,max=1)
  Bm <- rbinom(gene_length,1,Rm)
  gene <- (1-Bm) * gene + Bm * (gene+u)/2

  # Termination check
  if (generation > max_generation) {
    print("The maximum generation has been reached")
    GA_FLG <- FALSE
  }
  if ( all(best_individual == adj_t[1,]) ) {
    limit_count <- limit_count + 1
  } else {
    bestfit <- sort(fitness)[1]
    limit_count <- 0
    best_individual <- adj_t[1,]
  }
  if (limit_count >= successive_limit) {
    print(paste("The BIC has not changed for", successive_limit, " times."))
    GA_FLG <- FALSE
  }

  fit_log2[generation] <- sort(fitness)[1]
}

adj[upper.tri(adj)] <- colMeans(adj_t[1:10,])
GA_g <- graph_from_adjacency_matrix(adj)
ret2 <- BNM(tmp$U, DAG = GA_g)

adj[upper.tri(adj)] <- round(gene)
GA_g <- graph_from_adjacency_matrix(adj)
ret3 <- BNM(tmp$U, DAG = GA_g)

plot(fit_log)
plot(fit_log2)

# # 全パターンで検証 --------------------------------------------------------
#
# # expand.gridを使用して、長さ10のベクトルのすべての0/1の組み合わせを生成
# binary_combinations <- expand.grid(rep(list(c(0, 1)), 10)) %>% as.matrix
#
# fitness <- numeric(length(binary_combinations))
# for(i in 1:nrow(binary_combinations)){
#   adj[upper.tri(adj)] <- binary_combinations[i,]
#   g <- graph_from_adjacency_matrix(adj)
#   ret <- BNM(tmp,DAG=g)
#   fitness[i] <- ret$TestFitIndices$BIC
#   print(paste(i,"/",nrow(binary_combinations),"Fit",fitness[i]))
# }
#
# fitting_order <- data.frame(ID=1:length(fitness),BIC=fitness)
# fitting_order <- fitting_order[order(fitting_order$BIC),]
# fitting_order$Rank <- 1:NROW(fitting_order)
#
# fitting_order[fitting_order$BIC==ret1$TestFitIndices$BIC,]
# fitting_order[fitting_order$BIC==ret2$TestFitIndices$BIC,]
# fitting_order[fitting_order$BIC==ret3$TestFitIndices$BIC,]

