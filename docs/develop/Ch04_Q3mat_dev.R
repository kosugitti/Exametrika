rm(list=ls())
dat <- read_csv("develop/sampleData/J12S5000.csv") %>% Exametrika::dataFormat()
ret <- IRT(dat)
ret
ret$params
ret$ability

paramset <- ret$params
eapscore <- ret$ability[,2]

J <- ret$testlength
pij <- matrix(nrow=ret$nobs,ncol=ret$testlength)
for(j in 1:J){
  pij[,j] =
  Exametrika::LogisticModel(a= paramset[j,1],
                            b= paramset[j,2],
                            theta = eapscore)
}

eij = dat$Z * (dat$U - pij)
eij_mean <- colSums(eij) / colSums(dat$Z)
eij_dev <- dat$Z * (eij-eij_mean)
eij_var <- colSums(eij_dev^2) / colSums(dat$Z)
eij_sd <- sqrt(eij_var)
eij_cov <- t(eij_dev) %*% eij_dev / JointSampleSize(dat)

Q3mat <- eij_cov / (eij_sd %*% t(eij_sd))
