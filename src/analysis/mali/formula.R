# formulas
#
#
#

# smooth effects for continous and additional spatial smooth effects
frml.1 = list(
  mu = pos ~
    bols(intercept, intercept = FALSE) +
    bols(urban) + bols(climate) +

    bols(elev) + bbs(elev, center = TRUE, df = 1L, knots = 20L) +
    bols(lstday) + bbs(lstday, center = TRUE, df = 1L, knots = 20L) +

    bols(lstnight) + bbs(lstnight, center = TRUE, df = 1L, knots = 20L) +
    bols(ndvi) + bbs(ndvi, center = TRUE, df = 1L, knots = 20L) +

    bols(evi) + bbs(evi, center = TRUE, df = 1L, knots = 20L) +
    bols(precip) + bbs(precip, center = TRUE, df = 1L, knots = 20L) +

    bols(log_pop) + bbs(log_pop, center = TRUE, df = 1L, knots = 20L) +

    # brad(lon, lat, knots = 100, df = 1L, covFun = fields::stationary.cov,
    #      args = list(Covariance = "Matern", smoothness = 1.5, theta = NULL)),

    bols(lon) + bols(lat) + bols(lon, by = lat) +
    bspatial(lon, lat, df = 1L, center = TRUE),

  sigma = pos ~
    bols(intercept, intercept = FALSE) +
    bols(urban) +

    # brad(lon, lat, knots = 100, df = 1L, covFun = fields::stationary.cov,
    #      args = list(Covariance = "Matern", smoothness = 1.5, theta = NULL))

    bols(lon) + bols(lat) + bols(lon, by = lat) +
    bspatial(lon, lat, df = 1L, center = TRUE)
)


# space-varying coefficient models (with centered spatial effects)
frml.2 = list(
  mu = pos ~
    bols(intercept, intercept = FALSE) +

    bols(urban) + bspatial(lon, lat, by = urban, df = 1L, center = TRUE) +
    bols(climate) + bspatial(lon, lat, by = climate, df = 1L, center = TRUE) +
    bols(elev) + bspatial(lon, lat, by = elev, df = 1L, center = TRUE) +

    bols(lstday) + bspatial(lon, lat, by = lstday, df = 1L, center = TRUE) +
    bols(lstnight) + bspatial(lon, lat, by = lstnight, df = 1L, center = TRUE) +
    bols(ndvi) + bspatial(lon, lat, by = ndvi, df = 1L, center = TRUE) +

    bols(evi) + bspatial(lon, lat, by = evi, df = 1L, center = TRUE) +
    bols(precip) + bspatial(lon, lat, by = precip, df = 1L, center = TRUE) +
    bols(log_pop) + bspatial(lon, lat, by = log_pop, df = 1L, center = TRUE) +

    bols(lon) + bols(lat) + bols(lon, by = lat) +
    bspatial(lon, lat, df = 1L, center = TRUE),

  sigma = pos ~
    bols(intercept, intercept = FALSE) +
    bols(urban) + bspatial(lon, lat, by = urban, df = 1L, center = TRUE) +

    bols(lon) + bols(lat) + bols(lon, by = lat) +
    bspatial(lon, lat, df = 1L, center = TRUE)
)


frml.tree = list(
  mu = pos ~ intercept + urban + climate + elev + lstday + lstnight + ndvi + evi + precip + log_pop + lon + lat,
  sigma = pos ~ intercept + urban + lon + lat
)
