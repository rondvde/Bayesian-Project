data {
  int<lower=0> N;// number of observations
  vector[N] bill_length;   // response variable
  int<lower=1> N_species;   // number of unique species
  int<lower=1, upper=N_species> species[N];  // species indicator
}

parameters {
  //real alpha;               // intercept
  //real beta_width;// slope for bill_width
  //real beta_sex;
  vector[N_species] gamma;// species-specific intercepts
  real<lower=0> sigma;      // standard deviation of the residuals
}
transformed parameters{
  vector[N] mu;  // linear predictor
  for (i in 1:N){
    mu[i]= gamma[species[i]];
  }
}
model {
  
  // Priors
  //alpha ~ normal(0, 10);
  //beta_width ~ normal(0, 10);
  //beta_sex ~ normal(0, 10)
  sigma ~ gamma(29.90633, 1);
  
  for (j in 1:N_species){
    gamma[j] ~ normal(20, 80);
  }
  
  bill_length ~ normal(mu, sqrt(sigma));
}

generated quantities {
  vector[N] log_lik;  // vector to store log likelihood values
  vector[N] ynew;
  
  for (i in 1:N) {
    log_lik[i] = normal_lpdf(bill_length[i] | mu[i], sqrt(sigma));
  }

  for (n in 1:N){
    ynew[n] = normal_rng(mu[n], sqrt(sigma));
  }
}

