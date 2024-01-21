//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int N;
  int N_new;
  int C;
  matrix[N,C] X;
  vector[N] y;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[C] coef;
  real intercept;
  real<lower=0> sigma;
}

transformed parameters{
  vector[N] mu;  // linear predictor
  for (i in 1:N){
    mu[i] = intercept + coef[1] * X[i,1]+coef[2] * X[i,2]+coef[3] * X[i,3];
  }
}
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  sigma ~ gamma(29.9, 1);
  intercept ~ normal(1,10);
    coef[1] ~ normal(7.2,4);
    coef[2] ~ normal(1.17, 2);
    coef[3] ~ normal(1.66, 2);
  y ~ normal(mu, sqrt(sigma));
}


generated quantities {

  vector[N] log_lik; 
  vector[N] ynew;
  
  for (i in 1:N) {
    log_lik[i] = normal_lpdf(y[i] | mu[i], sqrt(sigma));
  }

  for (n in 1:N){
    ynew[n] = normal_rng(mu[n], sqrt(sigma));
  }
  }

