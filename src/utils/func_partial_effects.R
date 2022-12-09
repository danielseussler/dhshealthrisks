# plot estimated partial effects, inspired by
# https://github.com/adamdsmith/NanSound_EcolEvol/blob/master/R/ggplot_effects.R
#
#

plot_numeric_partial_effects = function(.mod, .data, .parameter = NULL, .var, .ylim = NULL, .rugged = FALSE) {

  require(ggplot2)
  require(mboost)

  if (!is.null(.parameter)) .mod <- .mod[[.parameter]]

  bls = extract(.mod, what = "bnames", which = paste0("(", .var))

  x = as.data.frame(.data[, .var])
  names(x) = .var
  y = rowSums(predict(.mod, which = bls, newdata = x))

  tmp = data.frame(x = x[, .var], y = y - mean(y)) # but why mean centered?
  plt = ggplot(data = tmp, mapping = aes(x = x, y = y)) +
    geom_line(color = "black", linewidth = 1L) +
    labs(x = .var, y = expression(f[partial]))

  if (!is.null(.ylim)) plt = plt + ylim(.ylim)
  if (!is.null(.rugged)) plt = plt + geom_rug(sides = "b")

  return(plt)
}



plot_spatial_partial_effects = function(.mod, .data, .parameter = NULL, .var = c("lon", "lat"), .shp, .res = 5L) {

  require(ggplot2)
  require(h3)
  require(sf)
  require(mboost)

  if (!is.null(.parameter)) .mod <- .mod[[.parameter]]

  bls = unique(c(
    extract(.mod, what = "bnames", which = paste0(.var[1])),
    extract(.mod, what = "bnames", which = paste0(.var[2]))
  ))

  locs = polyfill(polygon = .shp, res = .res)

  coords = h3_to_geo(locs)
  coords = data.frame("lon" = coords[, 2], "lat" = coords[, 1])

  z = rowSums(predict(.mod, which = bls, newdata = coords))
  z = z - mean(z)

  dt_sf = h3_to_geo_boundary_sf(locs)
  dt_sf$z = z

  plt = ggplot(data = dt_sf) + geom_sf(aes(fill = z), color = NA)

  return(plt)
}


plot_factor_partial_effects = function(.mod, .data, .parameter = NULL, .var, .ylim = NULL) {

  return(invisible(NULL))
}
