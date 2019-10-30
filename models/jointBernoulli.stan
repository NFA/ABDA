data {
  int<lower = 0> Ny;
  int<lower = 0> Nz;
  int y[Ny];
  int z[Nz];
}

parameters {
  real<lower = 0, upper = 1> theta[2];
}

model {
  theta[1] ~ beta(1,1);
  theta[2] ~ beta(1, 1 + theta[1]*theta[1]);
  y ~ bernoulli(theta[1]);
  z ~ bernoulli(theta[2]);
}
