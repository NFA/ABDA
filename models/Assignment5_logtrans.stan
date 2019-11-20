
data {
   int<lower = 0> N_data;
   vector[N_data] reaction_time;
   
   int<lower = 0> N_subjects;
   int subjects[N_data];
}

transformed data {
   vector[N_data] log_reaction_time = log(reaction_time);
}

parameters {
   // Group level
   real<lower = 0> mu;
   real<lower = 0> tau;
   // Individual level
   vector<lower = 0>[N_subjects] theta;
   real<lower = 0> sigma;
} 

model {
   theta ~ normal(mu, tau);
   for (i in 1:N_data) {
      log_reaction_time[i] ~ normal(theta[subjects[i]], sigma);
   }
}

generated quantities { 
   real<lower = 0> mu_exp = exp(mu + tau^2/2 + sigma^2/2);
   //real<lower = 0> tau_exp = mu_exp * sqrt(exp(sigma^2) - 1);
   
   vector<lower = 0>[N_subjects] theta_exp;
   real<lower = 0> reaction_time_ppc;
   
   for (j in 1:N_subjects) {
      theta_exp[j] = exp(theta[j] + sigma^2/2);
   }
   
   reaction_time_ppc = exp(normal_rng(normal_rng(mu, tau), sigma));
}
