library(tidyverse)
library(ggtext)

dat_la_se <- 
  here("data/raw/loss_aversion.csv") %>% 
  read_csv() %>%
  select(la, se)

se_lab <- 
  tibble(x = 5.5, y = 0.004, n = filter(dat_la_se, la <= 6, !is.na(se)) %>% nrow())

dat_la_se %>% 
  filter(la <= 6, !is.na(se)) %>% 
  mutate(ul = 1 + 1.96*se, ll = 1 - 1.96*se) %>% 
  ggplot(aes(x = la, y = se)) +
  geom_point(color = "steelblue", alpha = 0.6, size = 2) +
  geom_vline(xintercept = 1) + 
  geom_line(aes(x = ll), linetype = 2) +
  geom_line(aes(x = ul), linetype = 2) +
  geom_richtext(data = se_lab, aes(x = x, y = y, label = glue("*n* = {n}")),
            hjust = 0, fill = NA, label.color = NA) +
  scale_y_log10(breaks = c(0.01, 0.1, 1, 10), 
                labels = c("0.01", "0.10", "1.00", "10.00"),
                expand = expansion(mult = c(0, 0), add = c(0.15, 0.3))) +
  scale_x_continuous(limits = c(0,6)) +
  labs(x = expression(paste("Loss aversion coefficient (", lambda, ")")), 
     y = "SE") +
  theme_classic() 

ggsave("figures/loss_avers_funnel.pdf", width = 8, height = 5)
