# model formulas
#
#
#

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


frml.tree = list(
  mu = pos ~ intercept + urban + climate + elev + lstday + lstnight + ndvi + evi + precip + log_pop + lon + lat,
  sigma = pos ~ intercept + urban + lon + lat
)
