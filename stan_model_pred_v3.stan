 data {
 int<lower=3> K; // Number of alternatives (choices) in each scenario
 int<lower=0> N; // Number of obs
 int<lower=1> D; // Number of covariates or predictors
 int<lower=1, upper=K> y[N];
 matrix[N, D] x;
 int<lower=0> N_new;
 matrix[N_new, D] x_new;
 }

  transformed data{
  row_vector[D] zeros = rep_row_vector(0, D);
  print(zeros);
  }
  
  parameters { 
  matrix[K-1, D] beta_raw;
  }
  
  transformed parameters {
  matrix[K, D] beta;
  beta = append_row(beta_raw, zeros);
  }

 model {
 matrix[N, K] x_beta = x * beta';

 to_vector(beta_raw) ~ normal(0, 4);

 for (n in 1:N)
 y[n] ~ categorical_logit(x_beta[n]');
 }

generated quantities {
 int y_new [N_new];
 matrix[N_new, K] x_beta_new = x_new * beta';
 matrix[N, K] x_beta = x * beta';
 vector[N] log_lik;
 
 for (n in 1:N_new) {
    y_new[n] = categorical_logit_rng(x_beta_new[N_new]');
    }
    
  for (n in 1:N) {
    log_lik[n] = categorical_logit_lpmf(y[n] | x_beta[n]');
  }
 }

