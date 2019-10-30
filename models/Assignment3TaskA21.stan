data {
  int<lower = 0> N;
  int y[N];
}

parameters {
  real<lower = 0, upper = 1> theta;
}

model {
  y ~ bernoulli(theta);
}

generated quantities {
  int prob05;
  prob05 = theta > 0.5; 
}
