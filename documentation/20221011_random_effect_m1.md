20221011_random_effect_m1
================
Yurun (Ellen) Ying
2022-10-11

``` r
dat <- read.csv(here("data/raw/loss_aversion.csv"))
dat_clean <- data.frame(la = dat[,"la"], se = dat[, "se_imp"])
```

Now we have a dataset
![(\lambda_i, se_i)^m\_{i=1}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%28%5Clambda_i%2C%20se_i%29%5Em_%7Bi%3D1%7D "(\lambda_i, se_i)^m_{i=1}").
In the first benchmark model, we model each
![\lambda_i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Clambda_i "\lambda_i")
as a normal distribution with unknown mean
![\overline{\lambda}\_i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Coverline%7B%5Clambda%7D_i "\overline{\lambda}_i")
and known variance
![se_i^2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;se_i%5E2 "se_i^2").

![\lambda_i \mid \overline{\lambda}\_i, se_i \sim \text{Normal}(\overline{\lambda}\_i, se_i^2)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Clambda_i%20%5Cmid%20%5Coverline%7B%5Clambda%7D_i%2C%20se_i%20%5Csim%20%5Ctext%7BNormal%7D%28%5Coverline%7B%5Clambda%7D_i%2C%20se_i%5E2%29 "\lambda_i \mid \overline{\lambda}_i, se_i \sim \text{Normal}(\overline{\lambda}_i, se_i^2)")

The unknown means are normally distributed with unknown mean
![\lambda_0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Clambda_0 "\lambda_0")
and unknown variance
![\sigma^2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Csigma%5E2 "\sigma^2"):

![\overline{\lambda}\_i \mid \lambda_0, \sigma \sim \text{Normal}(\lambda_0, \sigma^2)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Coverline%7B%5Clambda%7D_i%20%5Cmid%20%5Clambda_0%2C%20%5Csigma%20%5Csim%20%5Ctext%7BNormal%7D%28%5Clambda_0%2C%20%5Csigma%5E2%29 "\overline{\lambda}_i \mid \lambda_0, \sigma \sim \text{Normal}(\lambda_0, \sigma^2)")

Now we would like to assign priors to the above model

``` r
set.seed(4238)
N <- 50
lambda_0 <- rlnorm(n = N)
sigma <- rexp(1, n = N)
lambda_bar <- rnorm(mean = lambda_0, sd = sigma, n = N)
lambda_i <- rnorm(mean = lambda_bar, sd = dat_clean[,"se"], n = N)

plot(lambda_i, ylim = c(-5,10), xlab = NULL, ylab = expression(lambda[i]))
abline(h = 0, lty = 2)
```

![](20221011_random_effect_m1_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Thus we have the model:

![\begin{aligned}
\lambda_i \mid \overline{\lambda}\_i, se_i &\sim \text{Normal}(\overline{\lambda}\_i, se_i^2) \\\\
\overline{\lambda}\_i \mid \lambda_0, \sigma &\sim \text{Normal}(\lambda_0, \sigma^2) \\\\
\lambda_0 &\sim \text{Log-Normal}(0, 1) \\\\
\sigma &\sim \text{Exp}(1)
\end{aligned}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cbegin%7Baligned%7D%0A%5Clambda_i%20%5Cmid%20%5Coverline%7B%5Clambda%7D_i%2C%20se_i%20%26%5Csim%20%5Ctext%7BNormal%7D%28%5Coverline%7B%5Clambda%7D_i%2C%20se_i%5E2%29%20%5C%5C%0A%5Coverline%7B%5Clambda%7D_i%20%5Cmid%20%5Clambda_0%2C%20%5Csigma%20%26%5Csim%20%5Ctext%7BNormal%7D%28%5Clambda_0%2C%20%5Csigma%5E2%29%20%5C%5C%0A%5Clambda_0%20%26%5Csim%20%5Ctext%7BLog-Normal%7D%280%2C%201%29%20%5C%5C%0A%5Csigma%20%26%5Csim%20%5Ctext%7BExp%7D%281%29%0A%5Cend%7Baligned%7D "\begin{aligned}
\lambda_i \mid \overline{\lambda}_i, se_i &\sim \text{Normal}(\overline{\lambda}_i, se_i^2) \\
\overline{\lambda}_i \mid \lambda_0, \sigma &\sim \text{Normal}(\lambda_0, \sigma^2) \\
\lambda_0 &\sim \text{Log-Normal}(0, 1) \\
\sigma &\sim \text{Exp}(1)
\end{aligned}")

Let’s inspect it

``` r
m1.1 <- readRDS(here("data/processed/random_effect_m1.1.RData"))
set.seed(4835)
post_1.1 <- extract.samples(m1.1)

# plot the posterior distribution of lambda_bar over the observed values of lambda
plot_1.1_post <- data.frame(post_lambda_bar = as.vector(post_1.1$lambda_bar)) %>% 
ggplot(aes(x = post_lambda_bar)) +
  # use geom_density with specific geom type to show lines in the legend
  # note the name of the color and linetype corresponds to the ones in the manual settings
  stat_density(size = 1.5, aes(color = "Estimated", linetype = "Estimated"),
               geom = "line", show.legend = TRUE) +
  stat_density(data = dat_clean, 
               aes(x = la, color = "Reported", linetype = "Reported"), 
               size = 1.5, geom = "line", 
               inherit.aes = FALSE, show.legend = TRUE) +
  geom_vline(xintercept = 1, linetype = 2) +
  scale_x_continuous(limits = c(0, 6), expand = c(0, 0),
                     breaks = seq(0, 6, by = 1), labels = seq(0, 6, by = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0), add = c(0, 0.05))) +
  scale_color_manual(name = NULL, 
                     breaks = c("Estimated","Reported"),  
                     values = c("Estimated" = "steelblue", 
                                "Reported" = "black"),
                     guide = guide_legend(keywidth = 3)) + 
  scale_linetype_manual(name = NULL, 
                     breaks = c("Estimated","Reported"),  
                     values = c("Estimated" = 1, "Reported" = 2),
                     guide = guide_legend(keywidth = 3)) +
  labs(x = expression(paste("Loss aversion coefficient (",lambda,")")),
       y = "Density") +
  theme_classic() +
  theme(legend.position = c(0.85, 0.9))

#ggsave("figures/m1_post_lambda_bar.pdf", width = 8, height = 5)
include_graphics(here("figures/m1_post_lambda_bar.pdf"))
```

![](../figures/m1_post_lambda_bar.pdf)<!-- -->
