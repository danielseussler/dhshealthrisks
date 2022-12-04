# model formulas
#
#
#

# dependent variable
# y = matrix(c(cl$npos, cl$nneg), ncol = 2)

frml.1 = list(
  mu = y ~
    bols(intercept, intercept = FALSE) +
    
    bols(urban) +
    bols(climate) +

    bols(elev) +
    bols(elev, by = climate) +

    bols(lstday) +
    bols(lstday, by = climate) +

    bols(lstnight) +
    bols(lstnight, by = climate) +

    bols(ndvi) +
    bols(ndvi, by = climate) +

    bols(evi) +
    bols(evi, by = climate) +

    bols(precip) +
    bols(precip, by = climate) +

    bols(pop) +
    bols(pop, by = climate) +

    bols(lon) + bols(lat) + bols(lon, by = lat) +
    bspatial(lon, lat, df = 1L, center = TRUE),

  sigma = y ~
    bols(intercept, intercept = FALSE) +
    bols(urban) +
    
    bols(lon) + bols(lat) + bols(lon, by = lat) +
    bspatial(lon, lat, df = 1L, center = TRUE)
)


frml.tree = list(
  mu = y ~ intercept + urban + climate + elev + lstday + lstnight + ndvi + evi + precip + pop + lon + lat,
  sigma = y ~ intercept + urban + lon + lat
)
