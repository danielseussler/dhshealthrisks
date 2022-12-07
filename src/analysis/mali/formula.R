# model formulas
#
#
#

# dependent variable
# y = matrix(c(cl$npos, cl$nneg), ncol = 2)

frml.1 = list(
  mu = y ~
    bols(intercept, intercept = FALSE) +
    
    bols(urban, contrasts.arg = "contr.dummy") +
    bols(climate) +

    bols(elev) +
    bbs(elev, center = TRUE, df = 1L, knots = 20L) +

    bols(lstday) +
    bbs(lstday, center = TRUE, df = 1L, knots = 20L) +

    bols(lstnight) +
    bbs(lstnight, center = TRUE, df = 1L, knots = 20L) +

    bols(ndvi) +
    bbs(ndvi, center = TRUE, df = 1L, knots = 20L) +

    bols(evi) +
    bbs(evi, center = TRUE, df = 1L, knots = 20L) +

    bols(precip) +
    bbs(precip, center = TRUE, df = 1L, knots = 20L) +

    bols(pop) +
    bbs(pop, center = TRUE, df = 1L, knots = 20L) +

    # brad(lon, lat, knots = 100, df = 1L, covFun = fields::stationary.cov,
    #      args = list(Covariance = "Matern", smoothness = 1.5, theta = NULL)), 

    bols(lon) + bols(lat) + bols(lon, by = lat) +
    bspatial(lon, lat, df = 1L, center = TRUE),

  sigma = y ~
    bols(intercept, intercept = FALSE) +
    bols(urban, contrasts.arg = "contr.dummy") +
    
    # brad(lon, lat, knots = 100, df = 1L, covFun = fields::stationary.cov,
    #      args = list(Covariance = "Matern", smoothness = 1.5, theta = NULL))
  
    bols(lon) + bols(lat) + bols(lon, by = lat) +
    bspatial(lon, lat, df = 1L, center = TRUE)
)


frml.tree = list(
  mu = y ~ intercept + urban + climate + elev + lstday + lstnight + ndvi + evi + precip + pop + lon + lat,
  sigma = y ~ intercept + urban + lon + lat
)
