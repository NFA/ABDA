
data {
   int<lower = 0> N_data;
   vector[N_data] reaction_time;
   
   int<lower = 0> N_subjects;
   int subjects[N_data];
   int is_child[N_subjects];
   real<lower = 0, upper = 1> child_proportion;
}

transformed data {
   vector[N_data] log_reaction_time = log(reaction_time);
}

parameters {
   // Group level
   real<lower = 0> mu;
   real<lower = 0> tau;
   real phi;
   // Individual level
   vector<lower = 0>[N_subjects] theta;
   real<lower = 0> sigma;
} 

model {
   for (i in 1:N_data) {
      log_reaction_time[i] ~ normal(theta[subjects[i]], sigma);
   }
   for (i in 1:N_subjects) {
     theta[i] ~ normal(mu + phi * is_child[i], tau);
   }
}

generated quantities { 
   real<lower = 0> mu_exp = exp(mu + tau^2/2 + sigma^2/2);
   
   vector<lower = 0>[N_subjects] theta_exp;
  
   real<lower = 0> reaction_time_ppc_unknown;
   real<lower = 0> reaction_time_ppc_child;
   real<lower = 0> reaction_time_ppc_adult;
   
   for (j in 1:N_subjects) {
      theta_exp[j] = exp(theta[j] + sigma^2/2);
   }
   
   // PPC
   if (uniform_rng(0.0, 1.0) > child_proportion) {
     reaction_time_ppc_unknown = exp(normal_rng(normal_rng(mu, tau), sigma));
   } else {
     reaction_time_ppc_unknown = exp(normal_rng(normal_rng(mu + phi, tau), sigma));
   }
   reaction_time_ppc_adult = exp(normal_rng(normal_rng(mu, tau), sigma));
   reaction_time_ppc_child = exp(normal_rng(normal_rng(mu + phi, tau), sigma));
}
