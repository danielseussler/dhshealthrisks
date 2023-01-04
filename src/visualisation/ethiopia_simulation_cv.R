#
#
#
#

library(data.table)
library(Metrics)
library(ggplot2)
library(patchwork)

load(file = file.path("models", "avq78t6q.rda"))

res_moderately[, regression := "moderately or severely"]
res_severly[, regression := "severely"]

results = rbindlist(list(res_moderately, res_severly))

results[, risk_diff := true_risk - cv_risk]
results[, risk_diff_ratio := (true_risk - cv_risk) / true_risk]

results[, as.list(summary(risk_diff)), by = .(type)]
results[, as.list(sd(risk_diff)), by = .(type)]

results[, name := type]
results[type == "k-fold stratified", name := "k-fold\nsurvey stratified"]
results[type == "bootstrap stratified", name := "bootstrap\nsurvey stratified"]
results[type == "subsampling survey cluster", name := "subsampling\ncluster + survey strat."]


tmp = results[
  , .(mean = mean(cv_risk), sd = sd(cv_risk), true_mean = mean(true_risk), true_sd = sd(true_risk))
  , by = .(holdout, type, regression)
]

tmp
add1 = data.table(holdout = "A", type = "true", regression = "moderately or severely", true_mean = 0.6091619 , mean = 0.6091619 , true_sd = 0.007342777 , sd = 0.007342777 )
add2 = data.table(holdout = "B", type = "true", regression = "moderately or severely", true_mean = 0.6078629 , mean = 0.6078629 , true_sd = 0.007120522 , sd = 0.007120522 )
add3 = data.table(holdout = "A", type = "true", regression = "severely", true_mean = 0.3689129 , mean = 0.3689129 , true_sd = 0.011792346 , sd = 0.011792346 )
add4 = data.table(holdout = "B", type = "true", regression = "severely", true_mean = 0.3670628 , mean = 0.3670628, true_sd = 0.013586240 , sd = 0.013586240 )

tmp = rbindlist(list(tmp, add1, add2, add3, add4), fill=TRUE)
tmp[, lower := mean - qnorm(0.975) * sd]
tmp[, upper := mean + qnorm(0.975) * sd]

tmp[, bias := true_mean - mean]

ggplot(data = tmp, mapping = aes(x = type, y = mean)) +
  geom_pointrange(mapping = aes(ymin = lower, ymax = upper)) +
  facet_grid(regression ~ holdout, scales = "free") +
  labs(x = "Type", y = expression(italic(hat(R)))) +
  theme_light()

ggplot(data = tmp, mapping = aes(x = type, y = bias)) +
  geom_point() +
  geom_segment(mapping = aes(x = type, xend = type, y = 0, yend = bias)) +
  geom_abline(intercept = 0, slope = 0, color = "grey") +
  facet_grid(regression ~ holdout, scales = "free") +
  labs(x = "Type", y = "Bias") +
  theme_light()

ggplot(data = tmp, mapping = aes(x = type, y = sd)) +
  geom_point() +
  geom_segment(mapping = aes(x = type, xend = type, y = 0, yend = sd)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_grid(regression ~ holdout, scales = "free") +
  labs(x = "Type", y = "Standard Deviation") +
  theme_light()


ggsave(
  plot = plt.A + plt.B
  , filename = "ethiopia_simulation_cv.png"
  , path = file.path("results", "figures")
  , scale = 1.4
  , dpi = 600
  , width = 200
  , height = 80
  , units = "mm"
  , device = png
)
