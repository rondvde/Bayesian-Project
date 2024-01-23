data {
  int<lower=0> N;// number of observations
  int<lower=0> N_new[3];
  vector[N] bill_length;   // response variable
  vector[N] bill_width;// covariate
  vector[N] sex;
  int<lower=1> N_species;   // number of unique species
  int<lower=1, upper=N_species> species[N];  // species indicator
}

parameters {
  //real alpha;               // intercept
  //real beta_width;// slope for bill_width
  //real beta_sex;
  real tau_depth;
  real tau_sex;
  vector[N_species] gamma;// species-specific intercepts
  vector[N_species] beta_width_2; //spesicif intercept for bill width
  vector[N_species] beta_sex_2;
  real<lower=0> sigma;      // standard deviation of the residuals
}
transformed parameters{
  vector[N] mu;  // linear predictor
  for (i in 1:N){
    mu[i]= gamma[species[i]]+ beta_width_2[species[i]] * bill_width[i]+ beta_sex_2[species[i]] * sex[i];
  }
}
model {
  
  // Priors
  //alpha ~ normal(0, 10);
  //beta_width ~ normal(0, 10);
  //beta_sex ~ normal(0, 10)
  sigma ~ gamma(6, sqrt(0.1));
  tau_depth ~ normal(10, 2);
  tau_sex ~ normal(10, 2);
  
  for (j in 1:N_species){
    gamma[j] ~ normal(0, sqrt(12));
    beta_width_2[j] ~ normal(tau_depth, 2);
    beta_sex_2[j] ~ normal(tau_sex, 2);
  }
  
  bill_length ~ normal(mu, sqrt(sigma));
}

generated quantities {
  vector[N] log_lik;  // vector to store log likelihood values for each species
  vector[N] ynew;
  
  for (i in 1:N) {
    log_lik[i] = normal_lpdf(bill_length[i] | mu[i], sqrt(sigma));
  }
 
  for (n in 1:N){
    ynew[n] = normal_rng(mu[n], sqrt(sigma));
  }
}

