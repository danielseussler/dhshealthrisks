# model formula
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
    bols(elev, by = urban) +

    bols(lstday) +
    bols(lstday, by = urban) +

    bols(lstnight) +
    bols(lstnight, by = urban) +

    bols(ndvi) +
    bols(ndvi, by = urban) +

    bols(evi) +
    bols(evi, by = urban) +

    bols(precip) +
    bols(precip, by = urban) +

    bols(pop) +
    bols(pop, by = urban) +

    bols(lon) + bols(lat) + bols(lon, by = lat) +
    bspatial(lon, lat, df = 1, center = TRUE),

  sigma = y ~
    bols(intercept, intercept = FALSE) +
    bols(urban) +
    bols(lon) + bols(lat) + bols(lon, by = lat) +
    bspatial(lon, lat, df = 1, center = TRUE)
)


frml.2 = list(
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
    bols(precip, by = urban) +

    bols(pop) +
    bols(pop, by = climate) +

    bols(lon) + bols(lat) + bols(lon, by = lat) +
    bspatial(lon, lat, df = 1, center = TRUE),

  sigma = y ~
    bols(intercept, intercept = FALSE) +
    bols(urban) +
    bols(lon) + bols(lat) + bols(lon, by = lat) +
    bspatial(lon, lat, df = 1, center = TRUE)
)


frml.3 = list(
  mu = y ~
    bols(intercept, intercept = FALSE) +
    bols(urban) +
    bols(climate) +

    bols(elev) +
    bols(elev, by = climate) +
    bols(elev, by = urban) +

    bols(lstday) +
    bols(lstday, by = climate) +
    bols(lstday, by = urban) +

    bols(lstnight) +
    bols(lstnight, by = climate) +
    bols(lstnight, by = urban) +

    bols(ndvi) +
    bols(ndvi, by = climate) +
    bols(ndvi, by = urban) +

    bols(evi) +
    bols(evi, by = climate) +
    bols(evi, by = urban) +

    bols(precip) +
    bols(precip, by = climate) +
    bols(precip, by = urban) +

    bols(pop) +
    bols(pop, by = climate) +
    bols(pop, by = urban) +

    bols(lon) + bols(lat) + bols(lon, by = lat) +
    bspatial(lon, lat, df = 1, center = TRUE),

  sigma = y ~
    bols(intercept, intercept = FALSE) +
    bols(urban) +
    bols(lon) + bols(lat) + bols(lon, by = lat) +
    bspatial(lon, lat, df = 1, center = TRUE)
)



frml.b = y ~
    bols(intercept, intercept = FALSE) +
    bols(urban) +
    bols(climate) +

    bols(elev) +
    bols(elev, by = climate) +
    bols(elev, by = urban) +

    bols(lstday) +
    bols(lstday, by = climate) +
    bols(lstday, by = urban) +

    bols(lstnight) +
    bols(lstnight, by = climate) +
    bols(lstnight, by = urban) +

    bols(ndvi) +
    bols(ndvi, by = climate) +
    bols(ndvi, by = urban) +

    bols(evi) +
    bols(evi, by = climate) +
    bols(evi, by = urban) +

    bols(precip) +
    bols(precip, by = climate) +
    bols(precip, by = urban) +

    bols(pop) +
    bols(pop, by = climate) +
    bols(pop, by = urban) +

    bols(lon) + bols(lat) + bols(lon, by = lat) +
    bspatial(lon, lat, df = 1, center = TRUE)


frml.bb = list(
  mu = y ~ intercept + urban + climate + elev + lstday + lstnight + ndvi + evi + precip + pop + lon + lat,
  sigma = y ~ intercept + urban + lon + lat
)
