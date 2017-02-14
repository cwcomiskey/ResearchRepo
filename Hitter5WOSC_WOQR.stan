data {                             
  int<lower=0> N;                 // Number of observations
  int<lower=0> p;                 // number of predictors
  matrix[N,p] X;                  // Covariates
  int<lower=0, upper=1> hit[N];   // 0/1 outcomes; array of integers
}
parameters {                
  real beta0;                       // intercept 
  vector[p] beta;
}
model {  
      beta0 ~ normal(0,5);
      beta ~ normal(0, 5);
      hit ~ bernoulli_logit(beta0 + X*beta);
}
