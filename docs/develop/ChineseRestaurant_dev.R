rm(list = ls())
library(Exametrika)
dat <- J35S515
# dat <- J15S500
# datCSV <- read.csv("develop/mtmk14forVer13/J35S515.csv")
tmp <- dataFormat(dat)

# Initialize
sameFieldCount <- 0
maxSameFieldCount <- 5
const <- 1e-100
testlength <- NCOL(tmp$U)
nobs <- NROW(tmp$U)
gamp <- 1
gamma_c <- 1
gamma_f <- 1

TG <- 17

t <- 1
maxt <- 50

pattern <- sort(unique(nrs(tmp)))
zeroG <- ifelse(pattern[1] == 0, 1, 0)
fullG <- ifelse(pattern[length(pattern)] == testlength, 1, 0)



## Initial Class
ncls <- length(pattern)
cls01 <- matrix(0, ncol = length(pattern), nrow = nobs)
for (i in 1:nobs) {
  cls01[i, which((pattern == nrs(tmp)[i]))] <- 1
}
colnames(cls01) <- paste("Class", 1:ncls)
rownames(cls01) <- tmp$ID
cls <- cls01 %*% (1:ncls)
Nc <- colSums(cls01)
## Initial Field
nfld <- testlength
field <- 1:testlength
crr <- crr(tmp$U)
df_tmp <- data.frame(field, crr)
df_tmp <- df_tmp[order(crr, decreasing = T), ]
sorted_list <- df_tmp$field
field <- order(sorted_list)
fld01 <- matrix(0, ncol = testlength, nrow = nfld)
for (i in 1:nfld) {
  fld01[i, field[i]] <- 1
}
colnames(fld01) <- paste("Field", 1:nfld)
rownames(fld01) <- tmp$ItemLabel
Nf <- colSums(fld01)


### Initial value for parameter set and the model fit
Ccf <- t(cls01) %*% tmp$U %*% fld01
Fcf <- t(cls01) %*% (tmp$Z * (1 - tmp$U)) %*% fld01
Ncf <- Ccf + Fcf
Pcf <- (Ccf + gamp - 1) / (Ncf + 2 * gamp - 2)
if (zeroG == 1) {
  Pcf[1, ] <- 0
}
if (fullG == 1) {
  Pcf[nrow(Pcf), ] <- 1
}


# IRM Iteration ------------------------------------------------------


### Gibbs sampler i
Cif <- tmp$U %*% fld01 # SxF.　sがfで正答した数
Fif <- (tmp$Z * (1 - tmp$U)) %*% fld01 # SxF. sがfで誤答した数
iRand <- sample(1:nobs, nobs, replace = F)


for (loop in 1:1) {
  target <- iRand[loop]
  # delete selected member
  Nc[cls[target]] <- Nc[cls[target]] - 1
  Ncf[cls[target], ] <- Ncf[cls[target], ] - Nf
  Ccf[cls[target], ] <- Ccf[cls[target], ] - Cif[target, ]
  Fcf[cls[target], ] <- Fcf[cls[target], ] - Fif[target, ]
  CcfPlus <- Ccf + matrix(rep(Cif[target, ], ncls), nrow = ncls, byrow = T)
  FcfPlus <- Fcf + matrix(rep(Fif[target, ], ncls), nrow = ncls, byrow = T)

  if (Nc[cls[target]] == 0) {
    ## if the class disappeared...
    ncls <- ncls - 1
    delCls <- cls[target]
    cls01 <- cls01[, -delCls]
    cls <- cls01 %*% (1:ncls)
    Nc <- Nc[-delCls]
    Ncf <- Ncf[-delCls, ]
    Ccf <- Ccf[-delCls, ]
    Fcf <- Fcf[-delCls, ]
    CcfPlus <- CcfPlus[-delCls, ]
    FcfPlus <- FcfPlus[-delCls, ]
  }

  # Table Choice!
  Nc_tmp <- Nc[-1]
  Nc_tmp <- Nc_tmp[-(length(Nc_tmp))]
  exist_tab_tmp <- log((Nc_tmp / (nobs - 1 + gamma_c)) + const)
  vec1 <- numeric(ncls - 2)
  vec2 <- numeric(ncls - 2)
  vec3 <- numeric(ncls - 2)
  for (i in 2:(ncls - 1)) {
    # for(i in 4:8){
    Ctmp <- 0
    Ftmp <- 0
    den <- 0
    for (j in 1:nfld) {
      ct1 <- (CcfPlus[i, j] - Ccf[i, j] - 1)
      if (ct1 >= 0) {
        for (s in 0:ct1) {
          Ctmp <- Ctmp + log(CcfPlus[i, j] + gamp - 1 - s + const)
        }
      }
      ct2 <- (FcfPlus[i, j] - Fcf[i, j] - 1)
      if (ct2 >= 0) {
        for (s in 0:ct2) {
          Ftmp <- Ftmp + log(FcfPlus[i, j] + gamp - 1 - s + const)
        }
      }
      ct3 <- CcfPlus[i, j] + FcfPlus[i, j] - Ccf[i, j] - Fcf[i, j] - 1
      if (ct3 >= 0) {
        for (s in 0:ct3) {
          den <- den +
            (log(CcfPlus[i, j] + FcfPlus[i, j] + 2 * gamp - 1 - s + const))
        }
      }
    }
    vec1[i - 1] <- Ctmp
    vec2[i - 1] <- Ftmp
    vec3[i - 1] <- den
  }
  exist_tab <- exist_tab_tmp + vec1 + vec2 - vec3

  ## New Table
  new_tab_tmp <- log((gamma_c / (nobs - 1 + gamma_c)) + const)
  num <- 0
  den <- 0
  for (j in 1:nfld) {
    maxS <- Cif[target, j] - 1
    if (maxS >= 0) {
      for (s in 0:maxS) {
        num <- num + log(Cif[target, j] + gamp - 1 - s + const)
      }
    }
    maxS <- Nf[j] - Cif[target, j] - 1
    if (maxS >= 0) {
      for (s in 0:maxS) {
        num <- num + log(Nf[j] - Cif[target, j] + gamp - 1 - s + const)
      }
    }
    maxS <- Nf[j] - 1
    if (maxS >= 0) {
      for (s in 0:maxS) {
        den <- den + log(Nf[j] + 2 * gamp - 1 - s + const)
      }
    }
  }
  new_tab <- num - den + new_tab_tmp

  # 全ての尤度テーブルから確率に
  ptab <- c(exist_tab, new_tab)
  mintab <- min(ptab)
  exptab <- exp(ptab - mintab)
  ptab <- exptab / sum(exptab)
  ptab <- ptab / sum(ptab)

  sampled_value <- rmultinom(1, 1, ptab)
  newclass01 <- c(0, sampled_value, 0)

  if (newclass01[ncls] == 0) {
    # 既存のどこかのテーブルについた
    # print(paste("class",which.max(sampled_value),"に移動"))
    cls01[target, ] <- newclass01[-ncls]
    cls[target] <- which.max(cls01[target, ])
  } else {
    # 新しいテーブルについた
    ncls <- ncls + 1
    print(paste("新しいテーブルについた。クラス数は", ncls))
    cls01 <- lapply(cls01, function(x) append(x, 0, after = length(x) - 1))
    Ccf <- lapply(Ccf, function(x) append(x, 0, after = length(x) - 1))
    Fcf <- lapply(Fcf, function(x) append(x, 0, after = length(x) - 1))
    Ncf <- lapply(Ncf, function(x) append(x, 0, after = length(x) - 1))
    cls01[target, ] <- newclass01
    cls <- cls01 %*% (1:ncls)
  }

  # クラス数，正答数などをアップデート
  Nc <- colSums(cls01)
  Ccf[cls[target]] <- Ccf[cls[target]] + Cif[target]
  Fcf[cls[target]] <- Fcf[cls[target]] + Fif[target]
  Ncf[cls[target]] <- Ccf[cls[target]] + Fcf[cls[target]]
}


# 別のルートで考えてみる。 ----------------------------------------------------

beta0 <- 1
beta1 <- 1

start_time <- Sys.time()

for (loop in 1:nobs) {
  target <- iRand[loop]
  # print(paste(i,"番目；",target,"が class",cls[target],"に所属"))

  Ccf <- t(cls01) %*% tmp$U %*% fld01
  Fcf <- t(cls01) %*% (tmp$Z * (1 - tmp$U)) %*% fld01
  Cif <- tmp$U %*% fld01 # SxF.　sがfで正答した数
  Fif <- (tmp$Z * (1 - tmp$U)) %*% fld01 # SxF. sがfで誤答した数\

  # delete selected member
  Nc[cls[target]] <- Nc[cls[target]] - 1
  Ncf[cls[target], ] <- Ncf[cls[target], ] - Nf
  Ccf[cls[target], ] <- Ccf[cls[target], ] - Cif[target, ]
  Fcf[cls[target], ] <- Fcf[cls[target], ] - Fif[target, ]
  CcfPlus <- Ccf + matrix(rep(Cif[target, ], ncls), nrow = ncls, byrow = T)
  FcfPlus <- Fcf + matrix(rep(Fif[target, ], ncls), nrow = ncls, byrow = T)

  if (Nc[cls[target]] == 0) {
    ## if the class disappeared...
    ncls <- ncls - 1
    delCls <- cls[target]
    cls01 <- cls01[, -delCls]
    cls <- cls01 %*% (1:ncls)
    Nc <- Nc[-delCls]
    Ncf <- Ncf[-delCls, ]
    Ccf <- Ccf[-delCls, ]
    Fcf <- Fcf[-delCls, ]
    CcfPlus <- CcfPlus[-delCls, ]
    FcfPlus <- FcfPlus[-delCls, ]
  }

  # ターゲットが新しいテーブルにつく尤度

  den <- nfld * log(beta(beta0, beta1) + const)
  num <- sum(log(beta(Fif[target, ] + beta0, Cif[target, ] + beta1) + const))
  base <- log((gamma_c / (nobs - 1 + gamma_c)) + const)
  new_tab <- num - den + base

  ## ターゲットが既存の他のテーブルにつく尤度
  exist_tab <- numeric(ncls - 2)
  num_vec <- numeric(ncls - 2)
  den_vec <- numeric(ncls - 2)

  # Sが店を出ることに注意して
  for (i in 2:(ncls - 1)) {
    # Sが店にいない時の正答・誤答数
    U1 <- t(cls01[-target, i]) %*% tmp$U[-target, ] %*% fld01
    U0 <- t(cls01[-target, i]) %*% (tmp$Z[-target, ] * (1 - tmp$U[-target, ])) %*% fld01
    den_tmp <- sum(log(beta(U0 + beta0, U1 + beta1) + const))
    # Sが既存テーブルiに移動
    cls01m <- cls01
    cls01m[target, ] <- replace(rep(0, ncls), i, 1)
    U1s <- t(cls01m[, i]) %*% tmp$U %*% fld01
    U0s <- t(cls01m[, i]) %*% (tmp$Z * (1 - tmp$U)) %*% fld01
    num_tmp <- sum(log(beta(U0s + beta0, U1s + beta1) + const))
    num_vec[i - 1] <- num_tmp
    den_vec[i - 1] <- den_tmp
  }

  Nc_tmp <- Nc[-1]
  Nc_tmp <- Nc_tmp[-(length(Nc_tmp))]
  base <- log((Nc_tmp / (nobs - 1 + gamma_c)) + const)
  exist_tab <- base + num_vec - den_vec

  # 全ての尤度テーブルから確率に
  ptab <- c(exist_tab, new_tab)
  mintab <- min(ptab)
  exptab <- exp(ptab - mintab)
  ptab <- exptab / sum(exptab)
  ptab <- ptab / sum(ptab)

  sampled_value <- rmultinom(1, 1, ptab)
  newclass01 <- c(0, sampled_value, 0)

  if (newclass01[ncls] == 0) {
    # 既存のどこかのテーブルについた
    # print(paste("class",which.max(sampled_value),"に移動"))
    cls01[target, ] <- newclass01[-ncls]
    cls[target] <- which.max(cls01[target, ])
  } else {
    # 新しいテーブルについた
    ncls <- ncls + 1
    print(paste("新しいテーブルについた。クラス数は", ncls))
    cls01 <- lapply(cls01, function(x) append(x, 0, after = length(x) - 1))
    cls01[target, ] <- newclass01
    cls <- cls01 %*% (1:ncls)
  }

  # クラス数，正答数などをアップデート
  Nc <- colSums(cls01)
}

# 終了時間の記録
end_time <- Sys.time()
# 実行時間の計算
execution_duration2 <- end_time - start_time

print(paste("Shojima takes",execution_duration1))
print(paste("Kosugitti takes",execution_duration2))
