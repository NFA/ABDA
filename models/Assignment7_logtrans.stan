
data {
   int<lower = 0> N_data;
   vector[N_data] reaction_time;
   
   int<lower = 0> N_subjects;
   int subjects[N_data];
   int attempts[N_data];
   int is_child[N_subjects];
   //row_vector[N_subjects] is_child;
   real<lower = 0, upper = 1> child_proportion;
}

transformed data {
   vector[N_data] log_reaction_time = log(reaction_time);
   //vector[N_data] zlog_reaction_time = (log_reaction_time - mean(log_reaction_time))/sd(log_reaction_time);
}

parameters {
   // Group level
   vector[2] mu;
   vector<lower = 0>[2] tau;
   vector[2] phi;
   // Individual level
   vector[N_subjects] theta[2];
   real<lower = 0> sigma;
} 

model {
   for (i in 1:N_data) {
      log_reaction_time[i] ~ normal(theta[1][subjects[i]] + theta[2][subjects[i]] * attempts[i] , sigma);
   }
   for (i in 1:N_subjects) {
      //theta ~ normal(mu + phi * is_child, tau);
      theta[1][i] ~ normal(mu[1] + phi[1]*is_child[i], tau[1]);
      theta[2][i] ~ normal(mu[2] + phi[2]*is_child[i], tau[2]);
   }
}

generated quantities { 
   //real<lower = 0> mu0_exp = exp(mu0 + tau0^2/2 + sigma^2/2);
   //real<lower = 0> mu1_exp = exp(mu1 + tau1^2/2 + sigma^2/2);
   
   //vector<lower = 0>[N_subjects] theta_exp;
  
   //real<lower = 0> reaction_time_ppc_unknown;
   //real<lower = 0> reaction_time_ppc_child;
   //real<lower = 0> reaction_time_ppc_adult;
   
   //for (j in 1:N_subjects) {
   //   theta_exp[j] = exp(theta[j] + sigma^2/2);
   //}
   
   //// PPC
   //if (uniform_rng(0.0, 1.0) > child_proportion) {
   //  reaction_time_ppc_unknown = exp(normal_rng(normal_rng(mu, tau), sigma));
   //} else {
   //  reaction_time_ppc_unknown = exp(normal_rng(normal_rng(mu + phi, tau), sigma));
   //}
   //reaction_time_ppc_adult = exp(normal_rng(normal_rng(mu, tau), sigma));
   //reaction_time_ppc_child = exp(normal_rng(normal_rng(mu + phi, tau), sigma));
}
