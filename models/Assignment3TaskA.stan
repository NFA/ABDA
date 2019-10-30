data {
  int<lower = 0> Ny;
  int<lower = 0> Nz;
  int y[Ny];
  int z[Nz];
}

parameters {
  real<lower = 0, upper = 1> theta_y;
  real<lower = 0, upper = 1> theta_z;
}

model {
  y ~ bernoulli(theta_y);
  z ~ bernoulli(theta_z);
}

generated quantities {
  int prob05;
  int y_gr_z;
  real<lower = -1, upper = 1> d_theta;
  
  prob05 = theta_y > 0.5; 
  y_gr_z = theta_y > theta_z;
  d_theta = theta_y - theta_z;
  
}
