data {
  int<lower = 1> N_obs;
  int<lower = 1, upper = 5> w_ans[N_obs];
  vector[N_obs] x;
}
parameters {
  simplex[5] theta;
  real alpha;
  real beta;
}
model {
  target += dirichlet_lpdf(theta | rep_vector(2, 5));
  for(n in 1:N_obs)
    target += categorical_lpmf(w_ans[n] | theta);
    theta = alpha + beta*x;
}
generated quantities{
  int pred_w_ans[N_obs];
  for(n in 1:N_obs)
    pred_w_ans[n] = categorical_rng(theta);
}
