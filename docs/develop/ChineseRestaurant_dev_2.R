rm(list = ls())
library(Exametrika)
dat <- J35S515
# dat <- J15S500
# datCSV <- read.csv("develop/mtmk14forVer13/J35S515.csv")
tmp <- dataFormat(dat)


Pcf_sort <- function(Pcf, cls01, fld01, ncls, nfld) {
  sort_list <- order(colSums(Pcf), decreasing = TRUE)
  Pcf <- Pcf[, sort_list]
  fld01 <- fld01[, sort_list]
  colnames(fld01) <- paste("Field", 1:nfld)
  field <- t((1:nfld) %*% t(fld01))

  sort_list2 <- order(rowSums(Pcf), decreasing = FALSE)
  Pcf <- Pcf[sort_list2, ]
  cls01 <- cls01[, sort_list2]
  colnames(cls01) <- paste("Class", 1:ncls)
  cls <- cls01 %*% (1:ncls)
  return(list(Pcf = Pcf, cls01 = cls01, fld01 = fld01, cls = cls, field = field))
}

## Inner Function for Ccf/Fcf/Ncf/Pcf
Ccf_Fcf_Pcf <- function(U, Z, cls01, fld01, gamp, zeroG, fullG, type = 1) {
  Ccf <- t(cls01) %*% tmp$U %*% fld01
  Fcf <- t(cls01) %*% (tmp$Z * (1 - tmp$U)) %*% fld01
  Ncf <- Ccf + Fcf
  Pcf <- (Ccf + gamp - 1) / (Ncf + 2 * gamp - (type * 2))
  if (zeroG == 1) {
    Pcf[1, ] <- 0
  }
  if (fullG == 1) {
    Pcf[nrow(Pcf), ] <- 1
  }
  return(list(Ccf = Ccf, Fcf = Fcf, Ncf = Ncf, Pcf = Pcf))
}

# Initialize
sameFieldCount <- 0
maxSameFieldCount <- 5
minClassSize <- 20

const <- 1e-100
testlength <- NCOL(tmp$U)
nobs <- NROW(tmp$U)
gamp <- 1
gamma_c <- 0
gamma_f <- 0


iter <- 1
max_iter <- 500

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
fld01 <- matrix(0, nrow = testlength, ncol = nfld)
for (i in 1:testlength) {
  fld01[i,field[i]] <- 1
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


# IRM Iteration ------------------------------------------------------

IRM_FLG <- TRUE

while (IRM_FLG) {
  ### Gibbs sampler i
  Cif <- tmp$U %*% fld01 # SxF.　sがfで正答した数
  Fif <- (tmp$Z * (1 - tmp$U)) %*% fld01 # SxF. sがfで誤答した数
  iRand <- sample(1:nobs, nobs, replace = F)

  for (loop in 1:nobs) {
    target <- iRand[loop]

    if (cls[target] >= 2 && cls[target] <= ncls - 1) {
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
      ptab <- round(ptab + const, digits = -log10(const))

      sampled_value <- rmultinom(1, 1, ptab)
      newclass01 <- c(0, sampled_value, 0)
      delpos <- length(newclass01) - 1
      if (newclass01[delpos] == 0) {
        # 既存のどこかのテーブルについた
        # print(paste("class",which.max(sampled_value),"に移動"))
        cls01[target, ] <- newclass01[-delpos]
        cls[target] <- which.max(cls01[target, ])
      } else {
        # 新しいテーブルについた
        ncls <- ncls + 1
        new_class <- which.max(newclass01)
        cls01_left <- cls01[, 1:(ncls - 2)]
        cls01_right <- cls01[, (ncls - 1)]
        zeros <- rep(0, nrow(cls01))
        cls01 <- cbind(cls01_left, zeros, cls01_right)
        cls01[target, ] <- newclass01

        cls <- cls01 %*% (1:ncls)
        Ccf <- t(cls01) %*% tmp$U %*% fld01
        Fcf <- t(cls01) %*% (tmp$Z * (1 - tmp$U)) %*% fld01
        Ncf <- Ccf + Fcf
      }

      # クラス数，正答数などをアップデート
      Nc <- colSums(cls01)
      Ccf[cls[target], ] <- Ccf[cls[target], ] + Cif[target, ]
      Fcf[cls[target], ] <- Fcf[cls[target], ] + Fif[target, ]
      Ncf[cls[target], ] <- Ccf[cls[target], ] + Fcf[cls[target], ]
    }
  }

  # Gibbs sampler j
  oldField <- field # J x C # 項目jがクラスCでどれぐらい正答しているか
  Cjc <- t(tmp$U) %*% cls01
  Fjc <- t(tmp$Z * (1 - tmp$U)) %*% cls01
  jRand <- sample(1:testlength, testlength, replace = F)

  for (loop in 1:testlength) {
    target <- jRand[loop]

    Nf[field[target]] <- Nf[field[target]] - 1
    Ccf[, field[target]] <- Ccf[, field[target]] - t(Cjc[target, ])
    Fcf[, field[target]] <- Fcf[, field[target]] - t(Fjc[target, ])
    Ncf <- Ccf + Fcf
    CcfPlus <- Ccf + matrix(rep(Cjc[target, ], nfld), nrow = ncls, byrow = F)
    FcfPlus <- Fcf + matrix(rep(Fjc[target, ], nfld), nrow = ncls, byrow = F)

    ## ## if the field disappeared...
    if (Nf[field[target]] == 0) {
      nfld <- nfld - 1
      delFld <- field[target]
      fld01 <- fld01[, -delFld]
      field <- fld01  %*% (1:nfld)
      Nf <- Nf[-delFld]
      Ncf <- Ncf[, -delFld]
      Ccf <- Ccf[, -delFld]
      Fcf <- Fcf[, -delFld]
      CcfPlus <- CcfPlus[, -delFld]
      FcfPlus <- FcfPlus[, -delFld]
    }

    # Table Choice!
    log_tab_tmp <- log((Nf / (testlength - 1 + gamma_f)) + const)
    vec1 <- rep(0, nfld)
    vec2 <- rep(0, nfld)
    vec3 <- rep(0, nfld)
    for (j in 1:nfld) {
      Ctmp <- 0
      Ftmp <- 0
      den <- 0
      for (i in 1:ncls) {
        ct1 <- CcfPlus[i, j] - Ccf[i, j] - 1
        if (ct1 >= 0) {
          for (s in 0:ct1) {
            Ctmp <- Ctmp + log(CcfPlus[i, j] + gamp - 1 - s + const)
          }
        }
        ct2 <- FcfPlus[i, j] - Fcf[i, j] - 1
        if (ct2 >= 0) {
          for (s in 0:ct2) {
            Ftmp <- Ftmp + log(FcfPlus[i, j] + gamp - 1 - s + const)
          }
        }
        ct3 <- CcfPlus[i, j] + FcfPlus[i, j] - Ccf[i, j] - Fcf[i, j] - 1
        if (ct3 >= 0) {
          for (s in 0:ct3) {
            den <- den + (log(CcfPlus[i, j] + FcfPlus[i, j] + 2 * gamp - 1 - s + const))
          }
        }
      }
      vec1[j] <- Ctmp
      vec2[j] <- Ftmp
      vec3[j] <- den
    }
    log_tab <- log_tab_tmp + vec1 + vec2 - vec3
    minLog <- min(log_tab)
    log_exp_tab <- log_tab - minLog
    maxLogExp <- max(log_exp_tab)
    exptab <- exp(log_exp_tab - maxLogExp)
    ptab <- exptab / sum(exptab)
    ptab <- round(ptab + const, digits = -log10(const))

    selected_fld <- rmultinom(1, 1, ptab)
    fld01[target, ] <- selected_fld
    field[target] <- which.max(selected_fld)
    Nf[field[target]] <- Nf[field[target]] + 1
    Ccf[, field[target]] <- Ccf[, field[target]] + t(Cjc[target, ])
    Fcf[, field[target]] <- Fcf[, field[target]] + t(Fjc[target, ])
    Ncf[, field[target]] <- Ccf[, field[target]] + Fcf[, field[target]]
  }

  sameFieldCount <- if (sum(abs(oldField - field)) == 0) {
    sameFieldCount <- sameFieldCount + 1
  } else {
    sameFieldCount <- 0
  }
  print(paste("iter", iter, "Exact match count of field elements.", sameFieldCount, "nfld", nfld, "ncls", ncls))
  if (sameFieldCount == maxSameFieldCount || iter == max_iter) {
    IRM_FLG <- FALSE
  } else {
    iter <- iter + 1
  }
}



# クラス数を調整する -------------------------------------------------------
# save(cls01,fld01,nfld,ncls,file="20221002tmp.Rdata")
# load("20221002tmp.Rdata")
# Pcf
Ccf <- t(cls01) %*% tmp$U %*% fld01
Fcf <- t(cls01) %*% (tmp$Z * (1 - tmp$U)) %*% fld01
Ncf <- Ccf + Fcf
Pcf <- (Ccf + gamp - 1) / (Ncf + 2 * gamp - 2)
if (zeroG == 1) {
  Pcf[1, ] <- rep(0, nfld)
}
if (fullG == 1) {
  Pcf[ncls, ] <- rep(1, nfld)
}

# model Fit
llm <- sum(Ccf * log(Pcf + const) + Fcf * log(1 - Pcf + const))
df <- nobs * testlength - ncls * nfld
bic <- -2 * llm - df * log(nobs)
indices <- c(llm, df, bic)

# sort
sort_list <- order(colSums(Pcf), decreasing = TRUE)
Pcf <- Pcf[, sort_list]
fld01 <- fld01[, sort_list]
colnames(fld01) <- paste("Field", 1:nfld)
field <- t((1:nfld) %*% t(fld01))

sort_list2 <- order(rowSums(Pcf), decreasing = FALSE)
Pcf <- Pcf[sort_list2, ]
cls01 <- cls01[, sort_list2]
colnames(cls01) <- paste("Class", 1:ncls)
cls <- cls01 %*% (1:ncls)

###
Cif <- tmp$U %*% fld01
Fif <- (tmp$Z * (1 - tmp$U)) %*% fld01
Ccf <- t(cls01) %*% Cif
Fcf <- t(cls01) %*% Fif
Ncf <- Ccf + Fcf
Pcf <- (Ccf + gamp) / (Ncf + 2 * gamp)


# Reorganizing small-sized classses -------------------------------

zero_position <- which(rowSums(tmp$Z * tmp$U, na.rm = T) == 0)
full_position <- which(rowSums(tmp$Z * tmp$U, na.rm = T) == testlength)

delt <- 0
delrep <- ncls
EMrep <- 20
const <- 1e-100

DelRepFLG <- TRUE
while (DelRepFLG) {
  bestfit <- 10^10
  Nc <- colSums(cls01)
  NcTable <- matrix(Nc)
  NcTable <- cbind(NcTable, 1:ncls)
  if (zeroG == 1) {
    NcTable <- NcTable[-1, ]
  }
  if (fullG == 1) {
    NcTable <- NcTable[-nrow(NcTable), ]
  }
  minclass <- NcTable[order(NcTable[, 1]), ][1, ]
  if (minclass[1] < minClassSize) {
    delt <- delt + 1
    print("The minimum class member count is under the setting value.")
    print(paste("bic", bic, "nclass", ncls))
  } else {
    DelRepFLG <- FALSE
    break
  }

  ncls <- ncls - 1
  delc <- minclass[2]
  Pcf <- Pcf[-delc, ]

  EMt <- 1
  zeta <- log(rep(1, ncls) / ncls)

  EMrepFLG <- TRUE
  while (EMrepFLG) {
    log_num_Zic <- Cif %*% t(log(Pcf + const)) + Fif %*% t(log(1 - Pcf + const)) + matrix(rep(zeta, nobs), nrow = nobs)
    numeZic <- exp(log_num_Zic)
    if (zeroG == 1) {
      numeZic[, 1] <- rep(0, nobs)
    }
    if (fullG == 1) {
      numeZic[, ncol(numeZic)] <- rep(0, nobs)
    }
    denomZi <- rowSums(numeZic)

    Zic <- numeZic / denomZi
    Zic_max_list <- apply(Zic, 1, max)

    cls01 <- sign(Zic - Zic_max_list) + 1
    zero_scorer <- rep(0, ncls)
    zero_scorer[1] <- 1
    full_scorer <- rep(0, ncls)
    full_scorer[ncls] <- 1
    cls01[zero_position, ] <- matrix(rep(zero_scorer, length(zero_position)), nrow = length(zero_position), byrow = T)
    cls01[full_position, ] <- matrix(rep(full_scorer, length(full_position)), nrow = length(full_position), byrow = T)

    cls <- cls01 %*% (1:ncls)

    Ccf <- t(cls01) %*% Cif
    Fcf <- t(cls01) %*% Fif
    Ncf <- Ccf + Fcf
    Pcf <- (Ccf + gamp) / (Ncf + 2 * gamp)

    if (zeroG == 1) {
      Pcf[1, ] <- rep(0, nfld)
    }
    if (fullG == 1) {
      Pcf[ncls, ] <- rep(1, nfld)
    }

    llm <- sum(Ccf * log(Pcf + const) + Fcf * log(1 - Pcf + const))
    df <- nobs * testlength - ncls * nfld
    bic <- -2 * llm - df * log(nobs)
    indices <- c(llm, df, bic)
    fit <- bic
    if (fit < bestfit) {
      EMt <- EMt + 1
      bestfit <- fit
      best_index <- indices
      bestPcf <- Pcf
      bestclass <- class
    } else {
      Pcf <- bestPcf
      class <- bestclass
      EMrepFLG <- FALSE
      break
    }
  }
}


# Model Fit Finally -----------------------------------------------
ncls <- max(cls)
nfld <- max(field)
cls01 <- matrix(0, ncol = ncls, nrow = nobs)
for (i in 1:nobs) {
  cls01[i, cls[i]] <- 1
}
colnames(cls01) <- paste("Class", 1:ncls)
rownames(cls01) <- tmp$ID

fld01 <- matrix(0, nrow = testlength, ncol = nfld)
for (i in 1:testlength) {
  fld01[i, field[i]] <- 1
}
colnames(fld01) <- paste("Field", 1:nfld)
rownames(fld01) <- tmp$ItemLabel

Ccf <- t(cls01) %*% tmp$U %*% fld01
Fcf <- t(cls01) %*% (tmp$Z * (1-tmp$U)) %*% fld01
Ncf <- Ccf + Fcf
Pcf <- (Ccf + gamp -1)/(Ncf+2*gamp-2)
if(zeroG==1){
  Pcf[1,] <- 0
}
if(fullG==1){
  Pcf[ncls,] <- 1
}

llm <- sum(Ccf * log(Pcf + const) + Fcf * log(1-Pcf+const))
TestFit(tmp$U,tmp$Z,llm,ncls * nfld) -> unko

# Sort ------------------------------------------------------------

ret <- Pcf_sort(Pcf, cls01, fld01, ncls, nfld)
Pcf <- ret$Pcf
cls01 <- ret$cls01
fld01 <- ret$fld01
cls <- ret$cls
field <- ret$field

# sort_list <- order(colSums(Pcf), decreasing = TRUE)
# Pcf <- Pcf[, sort_list]
# fld01 <- fld01[, sort_list]
# colnames(fld01) <- paste("Field", 1:nfld)
# colnames(Pcf) <- paste("Field", 1:nfld)
# field <- t((1:nfld) %*% t(fld01))
#
# sort_list2 <- order(rowSums(Pcf), decreasing = FALSE)
# Pcf <- Pcf[sort_list2, ]
# cls01 <- cls01[, sort_list2]
# colnames(cls01) <- paste("Class", 1:ncls)
# cls <- cls01 %*% (1:ncls)
#
# round(t(Pcf),5)
# unko
