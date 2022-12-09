#
#
#
#

create_plot_grid(.shp, .res = 5L) {
  
  require(h3)
  
  locs = h3::polyfill(polygon = .shp, res = .res)
  
  coords = h3::h3_to_geo(locs)
  coords = data.frame("lon" = coords[, 2], "lat" = coords[, 1])

  return(coords)
}