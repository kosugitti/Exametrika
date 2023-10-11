data{
  int<lower=1> FLD;
  int<lower=1> RNK;
  matrix[FLD,RNK] Y;
}

parameters{
  array[FLD] vector<lower=-4,upper=4>[2] theta;
  array[RNK] vector<lower=-4,upper=4>[2] delta;
  real<lower=-2,upper=2> alpha;
  real<lower=0> beta;
  real<lower=0> sigma;
}

model{
  matrix[FLD,RNK] mu;
  for(i in 1:FLD){
    for(j in 1:RNK){
      mu[i,j] = alpha - beta *dot_self(theta[i] - delta[j])^0.5;
    }
  }

  for(i in 1:FLD){
    for(j in 1:RNK){
      Y[i,j] ~ normal(mu[i,j], sigma);
    }
  }

  theta[1] ~ normal(0,1);
  theta[2] ~ normal(0,1);
  delta[1] ~ normal(0,1);
  delta[2] ~ normal(0,1);
  alpha ~ normal(0,5);
  beta ~ cauchy(0,5);
  sigma ~ cauchy(0,5);

}

