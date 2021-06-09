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

  row_vector prob_uncaptured(int T, row_vector p, vector phi) {
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
  int<lower=1, upper=Ntreat> photo_treat[N];
}

transformed data {
  int<lower=0,upper=T> first[N];
  int<lower=0,upper=T> last[N];
  vector<lower=0,upper=N>[T] n_captured;
  for (i in 1:N)
    first[i] = first_capture(y[i]);
  for (i in 1:N)
    last[i] = last_capture(y[i]);
  n_captured = rep_vector(0, T);
  for (t in 1:T)
    for (i in 1:N)
      if (y[i, t])
        n_captured[t] = n_captured[t] + 1;
}

parameters {
  vector[Nfam] p_f;
  vector[Ntreat] beta;
  real<lower=0> fam_tau;
  real<lower=0> phi_tau;
  real<lower=0> p_tau;
  vector<lower=0,upper=1>[T-1] phi;
  vector<lower=0,upper=1>[T] p;
}


transformed parameters {
  matrix<lower=0,upper=1>[N,T] chi;
  matrix<lower=0,upper=1>[N,T] p_hat;

  for(i in 1:N){
    for(j in 1:T){
      p_hat[i,j] = inv_logit(beta[photo_treat[i]] + p_f[fam[i]] + p[j]);
    }
  chi[i] = prob_uncaptured(T, p_hat[i], phi);
  }
}

model {
  p ~ normal(0,p_tau);
  phi ~ normal(0,phi_tau);
  p_f ~ normal(0,fam_tau);

  for (i in 1:N) {
    if (first[i] > 0) {
      for (t in (first[i]+1):last[i]) {
        1 ~ bernoulli(phi[t-1]);
        y[i, t] ~ bernoulli(p_hat[i,t]);
      }
      1 ~ bernoulli(chi[i,last[i]]);
    }
  }
}


//generated quantities {
//  real beta;
//  beta = phi[T-1] * p[T];
//}

