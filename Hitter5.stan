data {                             
  int<lower=0> N;                 // Number of observations
  int<lower=0> p;                 // number of predictors
  matrix[N,p] Q;                  // QR decomp - Q
  matrix[p,p] R;                  // QR decomp - R
  int<lower=0, upper=1> hit[N];   // 0/1 outcomes; array of integers
  vector[2] px_pz[N];             // N-dim array of 2-dim vectors
}
transformed data{
  matrix[p,p] R_inv;
  R_inv = inverse(R);
}
parameters {                
  real<lower=0> l;                  // length-scale parameter
  real<lower = 0> sigma;            // scale parameter
  real beta0;                       // intercept 
  vector[p] theta;
  vector[N] Z;                      // location random effect
}
transformed parameters {
      vector[p] beta;
      beta = R_inv*theta;
}
model {  
  matrix[N, N] Sigma;
  vector[N] Z_mod;
      
      l ~ lognormal(-2,1);            // E[l] = 0.223
      sigma ~ lognormal(-1.5, 1.5);   // E[sigma] = 0.687
      beta0 ~ normal(0,5);
      
      // beta ~ normal(0,5);          // N(0,5) priors on all beta
      // beta = R^{-1} * theta ---> theta ~ N(0,5*R^{-1}*R^{-1}')
      // Correlated prior completely unnecessary
      
      theta ~ normal(0, 10);
      Sigma = cov_exp_quad(px_pz, sigma, l); 
      for (n in 1:N)
        Sigma[n, n] = Sigma[n, n] + 1e-6;
      Sigma = cholesky_decompose(Sigma); 
      
      Z ~ normal(0, 1);  // Each element is N(0,1)
      Z_mod = Sigma * Z; // (Cov matrix Cholesky)*MVN(0,1)
      
      hit ~ bernoulli_logit(beta0 + Q*theta + Z_mod);
}
