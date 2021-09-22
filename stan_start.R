library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
## look at magnitude of random effect of family
## random slope of family on photo period (not as important)
## effect of forewing length
## effect of sex & interaction
source("data_manipulation.R")
corfu_1991_2_wide <- mutate(corfu_1991_2_wide, FWL_cent = FWL - mean(FWL, na.rm = TRUE))
corfu_1991_2_wide_noNA <- filter(corfu_1991_2_wide,!is.na(FWL), !is.na(FAM), !is.na(PHOTO), !is.na(SEX))

x <- select(corfu_1991_2_wide_noNA, ..1991.07.16:..1991.07.26)
T = dim(x)[2]
x <- matrix(as.numeric(x != 0), ncol = T)
fam_re <- as.numeric(factor(corfu_1991_2_wide_noNA$FAM))
photo_treat <- as.numeric(factor(corfu_1991_2_wide_noNA$PHOTO))-1
sex <- as.numeric(factor(corfu_1991_2_wide_noNA$SEX))-1
wing_length <- corfu_1991_2_wide_noNA$FWL_cent
corfu_1991_2_mixed <- stan("cjs_full_logit.stan", data = list(T = T, N = length(photo_treat), Nfam = length(unique(fam_re)),Ntreat = length(unique(photo_treat)), y = x, fam = fam_re, photo_treat = photo_treat, sex = sex, wing_length = wing_length), iter = 8000, chains = 3)


corfu_1992_1_wide <- mutate(corfu_1992_1_wide, FWL_cent = FWL - mean(FWL, na.rm = TRUE))
corfu_1992_1_wide_noNA <- filter(corfu_1992_1_wide,!is.na(FWL), !is.na(FAM), !is.na(PHOTO))
x <- select(corfu_1992_1_wide_noNA, ..1992.05.04:..1992.05.17)
T = dim(x)[2]
x <- matrix(as.numeric(x != 0), ncol = dim(x)[2])
fam_re <- as.numeric(factor(corfu_1992_1_wide_noNA$FAM))
photo_treat <- as.numeric(factor(corfu_1992_1_wide_noNA$PHOTO))-1
wing_length <- corfu_1992_1_wide_noNA$FWL_cent
corfu_1992_1_mixed <- stan("cjs_full_logit.stan", data = list(T = T, N = length(photo_treat), Nfam = length(unique(fam_re)),Ntreat = length(unique(photo_treat)), y = x, fam = fam_re, photo_treat = photo_treat, wing_length = wing_length), iter = 8000, chains = 3)

corfu_1992_2_wide <- mutate(corfu_1992_2_wide, FWL_cent = FWL - mean(FWL, na.rm = TRUE))
corfu_1992_2_wide_noNA <- filter(corfu_1992_2_wide,!is.na(FWL), !is.na(FAM), !is.na(PHOTO))
x <- select(corfu_1992_2_wide_noNA, ..1992.07.27:..1992.08.04)
T = dim(x)[2]
x <- matrix(as.numeric(x != 0), ncol = dim(x)[2])
fam_re <- as.numeric(factor(corfu_1992_2_wide_noNA$FAM))
photo_treat <- as.numeric(factor(corfu_1992_2_wide_noNA$PHOTO))-1
wing_length <- corfu_1992_2_wide_noNA$FWL_cent
corfu_1992_2_mixed <- stan("cjs_full_logit.stan", data = list(T = T, N = length(photo_treat), Nfam = length(unique(fam_re)),Ntreat = length(unique(photo_treat)), y = x, fam = fam_re, photo_treat = photo_treat, wing_length = wing_length), iter = 2000, chains = 3)


summary(corfu_1991_2_mixed, pars = c("alpha","beta","wing_beta","fam_tau", "phi_tau", "p_tau"), probs = c(.025,.5,.975))$summary
summary(corfu_1992_1_mixed, pars = c("alpha","beta","wing_beta","fam_tau", "phi_tau", "p_tau"), probs = c(.025,.5,.975))$summary
summary(corfu_1992_2_mixed, pars = c("alpha","beta","wing_beta","fam_tau", "phi_tau", "p_tau"), probs = c(.025,.5,.975))$summary
