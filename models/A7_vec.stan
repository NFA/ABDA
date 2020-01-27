
data {
   int<lower = 0> N_data;
   
   vector[N_data] reaction_time;
   
   int<lower = 0> N_subjects;
   int subjects[N_data];
   int attempts[N_data];
   int obs_per_subject[N_subjects];
   vector[N_subjects] is_child;
   real<lower = 0, upper = 1> child_proportion;
}

transformed data {
   vector[N_data] log_reaction_time = log(reaction_time);
}

parameters {
   // Group level
   vector[2] mu;
   vector<lower = 0>[2] tau;
   vector<lower = 0>[2] eta;
   vector[2] phi;
   // Individual level
   
   real<lower = 0> sigma;
} 

transformed parameters {
   matrix[2, N_subjects] theta;
   row_vector[N_subjects] rowN = rep_row_vector(1, N_subjects);
   theta = mu * rowN + phi * is_child' + (tau .* eta) * rowN;
}

model {
   // segmenting position tracker
   int pos = 1;
   
   // reparameterization parameter
   eta ~ normal(0, 1);
   
   for (i in 1:N_subjects) {
      // Number of observations for subject i
      int obs = obs_per_subject[i];
      // Vector of size N_subjects with all 0
      vector[N_subjects] subject = rep_vector(0, N_subjects);
      // Vector to store times for subject i
      vector[obs] times;
      
      // set the ith element to 1, mulitplying theta with subject will now
      // return [theta0, theta1]
      subject[i] = 1.0;
      // get the times for subject i
      times = segment(log_reaction_time, pos, obs);
      
      for (j in 1:obs) { 
         times[j] ~ normal(dot_product(theta * subject, [1, j]), sigma);
      }
      
      col(theta, i) ~ normal(mu + phi * is_child[i], tau);
      
      // advance position tracker
      pos = pos + obs;
   }
}

