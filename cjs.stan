// Stan model to estimate mortality rates of butterflies using
// Mark-recapture data.  Adapted from https://mc-stan.org/docs/2_26/stan-users-guide/mark-recapture-models.html
// Functions: first capture calculates the time of first capture for
// Each individual and last capture calculates the last capture
functions {
  int first_capture(int[] y_i) {
    for (k in 1:size(y_i))
      if (y_i[k])
        return k;
    return 0;
  }

  int last_capture(int[] y_i) {
    for (k_rev in 0:(size(y_i) - 1)) {
      int k;
      k = size(y_i) - k_rev;
      if (y_i[k])
        return k;
    }
    return 0;
  }

  row_vector prob_uncaptured(int T, vector p, row_vector phi) {
    row_vector[T] chi;
    chi[T] = 1.0;
    for (t in 1:(T - 1)) {
      int t_curr;
      int t_next;
      t_curr = T - t;
      t_next = t_curr + 1;
      chi[t_curr] = (1 - phi[t_curr])
                     + phi[t_curr]
                       * (1 - p[t_next])
                       * chi[t_next];
    }
    return chi;
  }
}


// The input data is a vector 'y' of length 'N'.
data {
  int<lower=2> T;
  int<lower=0> N;
  int<lower=0> Nfam;
  int<lower=0> Ntreat;
  int<lower=0,upper=1> y[N, T];
  int<lower=1, upper=Nfam> fam[N];
  int<lower=0, upper=Ntreat> photo_treat[N];
  real wing_length[N];
}

transformed data {
  int<lower=0,upper=T> first[N];
  int<lower=0,upper=T> last[N];
  for (i in 1:N)
    first[i] = first_capture(y[i]);
  for (i in 1:N)
    last[i] = last_capture(y[i]);
}

parameters {
  vector[Nfam] phi_f;
  real beta;
  real alpha;
  real beta_wing;
  real<lower=0> fam_tau;
  real<lower=0> phi_tau;
  real<lower=0> p_tau;
  vector[T-1] phi;
  vector[T] p;
}


transformed parameters {
  matrix<lower=0,upper=1>[N,T] chi;
  matrix<lower=0,upper=1>[N,T-1] phi_hat;

  for(i in 1:N){
    for(j in 1:(T-1)){
      phi_hat[i,j] = inv_logit(alpha + beta*photo_treat[i] + beta_wing * wing_length[i] + phi_f[fam[i]] + phi[j]);
    }
  chi[i] = prob_uncaptured(T, inv_logit(p), phi_hat[i]);
  }
}

model {
  phi ~ normal(0,phi_tau);
  phi_f ~ normal(0,fam_tau);
  p ~ normal(0, p_tau);
  
  for (i in 1:N) {
    if (first[i] > 0) {
      for (t in (first[i]+1):last[i]) {
        1 ~ bernoulli(phi_hat[i,t-1]);
        y[i, t] ~ bernoulli(inv_logit(p[t]));
      }
      1 ~ bernoulli(chi[i,last[i]]);
    }
  }
}


//generated quantities {
//  real beta;
//  beta = phi[T-1] * p[T];
//}

