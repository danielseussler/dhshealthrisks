#
#
#
#

pacman::p_load(ggplot2)

load(file = here::here("data", "processed", "ethiopia", "surveydata.rda"))

unique(sv$region)

ggplot(data = subset(sv, region == "Harari People"), aes(x = weight, group = strata)) +
  geom_histogram(alpha = 0.8)

ggplot(data = subset(sv, region == "Oromia"), aes(x = weight, group = strata)) +
  geom_histogram(alpha = 0.8)

df = aggregate(weight ~ cluster, data = sv, function(x) c(min(range(x)), max(range(x))))


table(deparse.level = )

isTRUE(all.equal(df$weight.1, df$weight.2))

