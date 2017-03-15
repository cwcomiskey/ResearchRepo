// Fix covariates

data {                             
  int<lower=0> N;                 // N observations
  int<lower=1, upper = N> m;        // m knots
  int<lower=0, upper=1> hit[N];     // binary, array of integers
  vector[2] knots[m];               // m-dim array of 2-dim vectors
  vector[2] obs[N];
  vector[N] XB;
}

parameters {                
  real<lower=0> phi;                // length-scale parameter
  real<lower=0> eta;                // scale parameter
  vector[m] w_z;                    // Stndrd Nrml rndm effct 
  vector[N] e_z;                    
}

transformed parameters {
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

  // Gaussian process: Cov(knots, knots), m x m
  C_star = cov_exp_quad(knots, eta, phi); 
        
  inv_C_star = inverse(C_star);               // This necessary?
  w_star = cholesky_decompose(C_star)*w_z;          
    // Transform MVN(0,1) to w ~ MVN(0, C*)
  
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
      
      hit ~ bernoulli_logit(XB + w);
}
