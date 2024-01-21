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
  int<lower=0> N;                   // number of observations
  int<lower=0> N_new[3];
  vector[N] bill_length;             // response variable
  vector[N] bill_width;              // covariate
  vector[N] sex;                     // sex indicator
  int<lower=1> N_species;            // number of unique species
  int<lower=1, upper=N_species> species[N];  // species indicator
  int<lower=1> N_sex;                // number of unique sexes
  int<lower=1, upper=N_sex> sex_id[N];      // sex indicator
}

parameters {
  vector[N_species] gamma_species;       // species-specific intercepts
  vector[N_species] beta_width_species;  // species-specific intercept for bill width
      

  vector[N_sex] gamma_sex;               // sex-specific intercepts
  vector[N_sex] beta_width_sex;          // sex-specific intercept for bill width
  real<lower=0> sigma;                   // standard deviation of the residuals
}

transformed parameters {
  vector[N] mu;  // linear predictor
  for (i in 1:N) {
    mu[i] = gamma_species[species[i]] + beta_width_species[species[i]] * bill_width[i] +
            gamma_sex[sex_id[i]] + beta_width_sex[sex_id[i]] * bill_width[i];
  }
}

model {
  // Priors
  sigma ~ gamma(29.90633, 1);

  // Species priors
  for (j in 1:N_species) {
    gamma_species[j] ~ normal(20, 5);
    beta_width_species[j] ~ normal(0.79, 2);
  }

  // Sex priors
  for (k in 1:N_sex) {
    gamma_sex[k] ~ normal(10, 5);
    beta_width_sex[k] ~ normal(0, 1);
  }

  // Likelihood
  bill_length ~ normal(mu, sqrt(sigma));
}

generated quantities {
  vector[N] log_lik;       // vector to store log likelihood values for each sex
  real ynew[N];
  for (i in 1:N) {
    log_lik[i] = normal_lpdf(bill_length[i] | mu[i], sqrt(sigma));
  }

  for (n in 1:N) {
    ynew[n] = normal_rng(mu[n], sqrt(sigma));
  }
}
