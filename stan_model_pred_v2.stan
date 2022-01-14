 data {
 int<lower=2> K;
 int<lower=0> N;
 int<lower=1> D;
 int<lower=1, upper=K> y[N];
 matrix[N, D] x;
 int<lower=0> N_new;
 matrix[N_new, D] x_new;
 }
 
 parameters {
 matrix[D, K] beta;
 }
 
 model {
 matrix[N, K] x_beta = x * beta;

 to_vector(beta) ~ normal(0, 5);

 for (n in 1:N)
 y[n] ~ categorical_logit(x_beta[n]');
 }

generated quantities {
 int y_new [N_new];
 matrix[N_new, K] x_beta_new = x_new * beta;
 matrix[N, K] x_beta = x * beta;
 vector[N] log_lik;
 
 for (n in 1:N_new) {
    y_new[n] = categorical_logit_rng(x_beta_new[N_new]');
    }
    

  for (n in 1:N) {
    log_lik[n] = categorical_logit_lpmf(y[n] | x_beta[n]');
  }
 }

