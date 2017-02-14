data {                             
  int<lower=0> N;                 // Number of observations
  int<lower=0> p;                 // number of predictors
  matrix[N,p] Q;                  // QR decomp - Q
  matrix[p,p] R;                  // QR decomp - R
  int<lower=0, upper=1> hit[N];   // 0/1 outcomes; array of integers
  vector[p] theta_SDs;            // theta prior SDs

}
transformed data{
  matrix[p,p] R_inv;
  R_inv = inverse(R);
}
parameters {                
  real beta0;                       // intercept 
  vector[p] theta;
}
transformed parameters {
      vector[p] beta;
      beta = R_inv*theta;
}
model {  
      beta0 ~ normal(0,5);
      
      theta[1] ~ normal(0, theta_SDs[1]);
      theta[2] ~ normal(0, theta_SDs[2]);
      theta[3] ~ normal(0, theta_SDs[3]);
      theta[4] ~ normal(0, theta_SDs[4]);
      theta[5] ~ normal(0, theta_SDs[5]);
      theta[6] ~ normal(0, theta_SDs[6]);

      hit ~ bernoulli_logit(beta0 + Q*theta);
}
