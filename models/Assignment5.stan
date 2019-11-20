
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
   real<lower = 0> mu;
   real<lower = 0> tau;
   
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
   vector<lower = 0>[N_subjects] theta_exp;
   
   for (j in 1:N_subjects) {
      theta_exp[j] = exp(theta[j] + sigma^2/2);
   }   
}
