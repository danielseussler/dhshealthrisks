# create sampling frame as in Paige et al. 2022
#
#
#

library(here)
library(terra)
library(sf)
library(h3)
library(data.table)

shp = readRDS(file = here("data", "processed", "mali", "dhsboundaries.rds"))
shp = rmapshaper::ms_simplify(shp, keep = 0.3, keep_shapes = FALSE)

indices = polyfill(st_union(shp), res = 9) # average area of hexagon is 0.105332513
coords = h3_to_geo(indices)
grid = data.table(h3_index = indices, lon = coords[, 2] , lat = coords[, 1])
rm(indices, coords)

flist = list.files(path = here("data", "raw", "GHS_POP_E2020_GLOBE_R2022A_54009_100_V1_0"), pattern = ".tif", full.names = TRUE)

pop = flist |> lapply(rast) |> sprc() |> merge()
pop = crop(x = pop, y = st_transform(shp, crs = crs(pop)))
pop = project(x = pop, y = "epsg:4326", method = "bilinear", threads = 4)

plot(log(pop))
plot(st_geometry(shp$admin0), add = T, color = "transparent")


grid$pop = extract(pop, grid[, .(lon, lat)], method = "bilinear", ID = FALSE)

grid = grid[!is.na(pop)]

sum(grid$pop)







