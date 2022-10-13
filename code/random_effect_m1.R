library(rethinking)
library(here)

# source file to use function to remove extra individual means
source("code/remove_individual_mean.R")
dat <- remove_individual_mean() 
dat_clean <- list(id = 1: nrow(dat), 
                  la = pull(dat, la), 
                  se = pull(dat, se_imp))

# my model
m1.1 <- ulam(
  alist(
    la ~ normal(lambda_bar[id], se),
    lambda_bar[id] ~ normal(lambda_0, sigma),
    lambda_0 ~ dlnorm(0, 1),
    sigma ~ dexp(1)
  ), data = dat_clean, chains = 4, cores = 4
)

dashboard(m1.1)

# model used in the original paper
m1.2 <- ulam(
  alist(
    la ~ normal(lambda_bar[id], se),
    lambda_bar[id] ~ normal(lambda_0, sigma),
    lambda_0 ~ dhalfnorm(1, 5),
    sigma ~ dhalfnorm(0, 5)
  ), data = dat_clean, chains = 4, cores = 4
)

dashboard(m1.2)

saveRDS(m1.1, "data/processed/random_effect_m1.1.RData")
saveRDS(m1.2, "data/processed/random_effect_m1.2.RData")
