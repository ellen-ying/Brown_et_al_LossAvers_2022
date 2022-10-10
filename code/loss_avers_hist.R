library(tidyverse)
library(here)
library(glue)

dat_la_se <- 
  here("data/raw/loss_aversion.csv") %>% 
  read_csv() %>%
  select(la, se_imp)

la_stats <- 
  dat_la_se %>% 
  filter(la <= 6) %>% 
  summarize(mean = mean(la), median = median(la)) %>% 
  pivot_longer(everything(), names_to = "stats", values_to = "value") %>% 
  mutate(
    label = glue("{stats} = {round(value, 2)}"),
    y = ifelse(stats == "median", 0.75, 0.7))

dat_la_se %>% 
  filter(la <= 6) %>% 
  ggplot(aes(x = la)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1,
                 color = "grey40", alpha = 0.5) +
  geom_density(size = 1.2) +
  geom_vline(xintercept = 1, linetype = 2) + 
  geom_vline(data = la_stats, aes(xintercept = value), color = "darkred") + 
  geom_label(data = la_stats, aes(x = value, y = y, label = label),
             label.padding = unit(0.2, "lines"), label.size = 0, nudge_x = 0.45) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 6, by  = 1), labels = seq(0, 6, by  = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0), add = c(0, 0.05)), 
                     limit = c(0, 0.8)) +
  labs(x = expression(paste("Loss aversion coefficient (", lambda, ")")), 
       y = "Density") +
  theme_classic()

ggsave("figures/loss_avers_hist.pdf", width = 8, height = 5)
