#
#
#
#

library(here)
library(gamlss.dist)
library(ggplot2)
library(viridis)
library(dplyr)

load(file = here("models", "fg5kpezg.rda"))

# construct averages of predictor covariates
datavg = summarise_at(cl, c("pop", "elev", "evi", "lstnight", "lstday", "lstnight", "ndvi", "precip", "lon", "lat"), median, na.rm = TRUE)
datavg = bind_cols(datavg, climate = factor(x = c("Tropical, savannah", "Tropical, savannah"), levels = levels(cl$climate)))
datavg = bind_cols(datavg, urban = factor(x = c(1, 2), levels = c(1, 2), labels = c("rural", "urban")))


distparam = predict(mod, newdata = datavg, type = "response")

# average cluster size
N = 100L

df = data.frame(x = 1:N)
df$urban = dBB(x = df$x, mu = distparam$mu[1], sigma = distparam$sigma[1, 1], bd = N)
df$rural = dBB(x = df$x, mu = distparam$mu[2], sigma = distparam$sigma[2, 1], bd = N)
df = pivot_longer(df, cols = c("urban", "rural"), names_to = "urban")

ggplot() +
  geom_bar(data = df, mapping = aes(x = x, y = value, fill = urban, group = factor(urban)), position="dodge", stat="identity") +
  scale_fill_manual(values = viridis(n = 2, alpha = 0.8, begin = 0.3, end = 0.7)) +
  labs(x = expression(x), y = expression(p(x))) +
  theme(legend.position = "bottom")



df.BI = data.frame(x = x, BI = dBI(x = x, mu = MU, bd = N))
df.BB = data.frame(x = x, BB = dBB(x = x, mu = MU, sigma = 0.1, bd = N))

theme_set(theme_minimal())

plt = ggplot() +
  geom_bar(data = df.BB, mapping = aes(x, BB, fill = "betabinom"), stat="identity") +
  geom_bar(data = df.BI, mapping = aes(x, BI, fill = "binomial"), stat="identity") +
  scale_fill_manual(name = "Distribution",
                     values = c("betabinom" = viridis(n = 2, alpha = 0.8, begin = 0.3, end = 0.7)[1], "binomial" = viridis(n = 2, alpha = 0.8, begin = 0.3, end = 0.7)[2]),
                     labels = c("betabinom" = "Beta binomial BB(n = 50, mu = 0.2, sigma = 0.1)", "binomial" = "Binomia B(n = 50, mu = 0.2)")) +
  labs(x = expression(x), y = expression(p(x))) +
  theme(legend.position = "bottom")

ggsave(plot = plt, filename = "fig_distributions.png", path = here("results", "figures"), scale = 1.2, dpi = 600, width = 200, height = 100, units = "mm", device = png)

