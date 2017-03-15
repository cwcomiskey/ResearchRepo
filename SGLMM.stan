// We start by specifying the data, which consist of  
//    (1) an n-vector of outcomes (y) 
//    (2) an n by k matrix of predictors (x)
//    (3) an n-vector mapping pregnancies to women (g). 

data {
    int N; // number of obs (pregnancies)
    int M; // number of groups (women)
    int K; // number of predictors
    
    int y[N]; // outcome
    row_vector[K] x[N]; // predictors
    int g[N];    // map obs to groups (pregnancies to women)
}

// The parameters are the 
//    (1) constant (alpha)
//    (2) the woman-specific RANDOM EFFECT 'a' 
//    (3) the coefficients of the k predictors (beta)
//    (4) the standard deviation of random effects (sigma), in (0,10).
parameters {
    real alpha;
    real a[M]; 
    vector[K] beta;
    real<lower=0,upper=10> sigma;  
}

// The model provides non-informative normal priors for the fixed effects alpha and beta. Stan specifies normal distributions using the standard deviations, not the variance, nor the precision used by BUGS. The hyperprior for sigma is uniform(0,10) as is determined by the limits given. Finally the outcomes are Bernoulli with probability given by the inverse logit of the linear predictor.
model {
  alpha ~ normal(0,100);
  a ~ normal(0,sigma);
  beta ~ normal(0,100);
  for(n in 1:N) {
    y[n] ~ bernoulli(inv_logit( alpha + a[g[n]] + x[n]*beta));
  }

