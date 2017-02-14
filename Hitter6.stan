data {                             
  int<lower=0> N;                 // N observations
  int<lower=0> p;                 // p predictors
  matrix[N,p] Q;                  // QR decomp - Q
  matrix[p,p] R;                  // QR decomp - R
  int<lower=0, upper=1> hit[N];   // 0/1 outcomes; array of integers
  vector[2] px_pz[N];             // N-dim array of 2-dim vectors
  vector[p] theta_SDs;            // theta prior SDs
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
  matrix[N, N] L;                     // Lwr triangular Cholesky decomp
  vector[N] Z_mod;
      
      l ~ lognormal(-2,1);            // E[l] = 0.223
      sigma ~ lognormal(-1.5, 1.5);   // E[sigma] = 0.687
      beta0 ~ normal(0,5);
      
      theta[1] ~ normal(0, theta_SDs[1]);
      theta[2] ~ normal(0, theta_SDs[2]);
      theta[3] ~ normal(0, theta_SDs[3]);
      theta[4] ~ normal(0, theta_SDs[4]);
      theta[5] ~ normal(0, theta_SDs[5]);
      theta[6] ~ normal(0, theta_SDs[6]);
      
      Sigma = cov_exp_quad(px_pz, sigma, l); 
      for (n in 1:N)
        Sigma[n, n] = Sigma[n, n] + 1e-6;
      L = cholesky_decompose(Sigma); // Sigma = LL' 
      
      Z ~ normal(0, 1);  // Each element is N(0,1)
      Z_mod = L * Z; // (Cov matrix Cholesky)*MVN(0,1)
      
      hit ~ bernoulli_logit(beta0 + Q*theta + Z_mod);
}
