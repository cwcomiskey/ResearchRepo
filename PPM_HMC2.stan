// Remove covariance "for loops"
data {                             
  int<lower=0> N;                 // N observations
  int<lower=0> p;                 // p predictors  
  int<lower=1, upper = N> m;        // m knots
  
  matrix[N,p] Q;                  // QR decomp - Q
  matrix[p,p] R;                  // QR decomp - R
  matrix[p,p] R_inv;
  vector[p] theta_SDs;              // theta prior SDs
  int<lower=0, upper=1> hit[N];     // binary, array of integers
  vector[2] knots[m];               // m-dim array of 2-dim vectors
  vector[2] obs[N];
}

parameters {                
  real<lower=0> phi;                // length-scale parameter
  real<lower=0> eta;                // scale parameter

  real beta0;                       // intercept 
  vector[p] theta;                  // QR decomp parameter
  
  vector[m] w_z;                    // Stndrd Nrml rndm effct 
  vector[N] e_z;                    
}

transformed parameters {
  vector[p] beta;                     // QR back-transform
  vector[N] w;                        // Location random effect
  vector[N] sigma_e_tilde;  
  vector[m] w_star;                   // Knot random effects 
  matrix[m,m] C_star;                 // Cov(knot, knot)
  matrix[m,m] inv_C_star;             // Cov(knot, knot)^-1
  matrix[N,m] C_site_star;            // Cov(obs, knots)
  matrix[N,m] C_site_star_inv_C_star; 
    // Cov(k, obs)*Cov(kn,kn)^{-1} 
  
  real eta_sq;
  eta_sq = pow(eta, 2);

  beta = R_inv*theta;
  
  // Gaussian process: Cov(knots, knots), m x m
  C_star = cov_exp_quad(knots, eta, phi); 
        
  inv_C_star = inverse(C_star);                     // This necessary?
  w_star = cholesky_decompose(C_star)*w_z;          
    // knot distribution, MV standard normal transform
  
  // Gaussian process at observed locations; little c; n x m

  C_site_star = cov_exp_quad(obs, knots, eta, phi);
  
  C_site_star_inv_C_star = C_site_star*inv_C_star;
  
  w = C_site_star*inv_C_star*w_star;                  // w tilde
  
  // Improved PPM, bias adjustment
  sigma_e_tilde = eta_sq - rows_dot_product(C_site_star_inv_C_star, C_site_star);
  
  for(i in 1:N){
    w[i] = w[i] + e_z[i]*sqrt(sigma_e_tilde[i]);
  }
}
  
model {  
      phi ~ lognormal(-2,1);            // E[l] = 0.223
      eta ~ lognormal(-1.5, 1.5);       // E[sigma] = 0.687
      w_z ~ normal(0,1);
      e_z ~ normal(0,1);
      beta0 ~ normal(0,5);
      
      # Transformed non-informative variances (due to QR)
      theta[1] ~ normal(0, theta_SDs[1]);
      theta[2] ~ normal(0, theta_SDs[2]);
      theta[3] ~ normal(0, theta_SDs[3]);
      theta[4] ~ normal(0, theta_SDs[4]);
      theta[5] ~ normal(0, theta_SDs[5]);
      theta[6] ~ normal(0, theta_SDs[6]);
      
      hit ~ bernoulli_logit(beta0 + Q*theta + w);
}
