library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
## look at magnitude of random effect of family
## random slope of family on photo period (not as important)
source("data_manipulation.R")
corfu1_wide_noNA <- filter(corfu1_wide, !is.na(FAM), !is.na(PHOTO))
x <- select(corfu1_wide_noNA, ..1991.07.16:..1991.07.26)
x <- matrix(as.numeric(x != 0), ncol = 10)
fam_re <- as.numeric(factor(corfu1_wide_noNA$FAM))
photo_treat <- as.numeric(factor(corfu1_wide_noNA$PHOTO))
corfu1_mixed <- stan("cjs.stan", data = list(T = 10, N = length(photo_treat), Nfam = length(unique(fam_re)),Ntreat = length(unique(photo_treat)), y = x, fam = fam_re, photo_treat = photo_treat), iter = 2000, chains = 3)

summary(corfu1_mixed, pars = c("beta","fam_tau","p_tau"), probs = c(.025,.5,.975))$summary