library(tidyverse)
library(here)

# summary on the full dataset by estimation type
dat_la_se <- 
  here("data/raw/loss_aversion.csv") %>% 
  read_csv() %>%
  select(la, se_imp, la_type)

summary_by_type <- 
  dat_la_se %>% 
  group_by(la_type) %>% 
  summarize(
    n = n(),
    mean = mean(la),
    sd = sd(la),
    q1 = quantile(la, prob = 0.25),
    meadian = median(la),
    q3 = quantile(la, prob = 0.75),
    min = min(la),
    max = max(la)
  )

# source the script to remove extra cases reporting individual means
source("code/remove_individual_mean.R")

# overall stats after removing individual means
# combined with summary stats by type
overall_stats <- 
  remove_individual_mean() %>% 
  summarize(
    la_type = "Overall",
    n = n(),
    mean = mean(la),
    sd = sd(la),
    q1 = quantile(la, prob = 0.25),
    meadian = median(la),
    q3 = quantile(la, prob = 0.75),
    min = min(la),
    max = max(la)
  ) %>% 
  bind_rows(summary_by_type, .)


write_csv(overall_stats, "data/processed/overall_summary_stats.csv")
