# create zonal stats for h3 locations of regular grid and survey locations
#
#
#

library(here)
library(terra)
library(sf)
library(h3)
library(data.table)


# load data and create regular grid of locations
shp = readRDS(file = here("data", "processed", "mali", "dhsboundaries.rds"))
indices = polyfill(st_union(shp), res = 7L) # average area of hexagon is 5.161293360
coords = h3_to_geo(indices)
grid = data.table(h3_index = indices, lon = coords[, 2L] , lat = coords[, 1L])

cloc = readRDS(file = here("data", "raw", "rdhs", "MLGE81FL.rds"))
cloc = cloc |> st_as_sf() |> st_drop_geometry() |> as.data.table()
cloc = dplyr::select(cloc, cluster = DHSCLUST, urban = URBAN_RURA, lon = LONGNUM, lat = LATNUM)
cloc = dplyr::mutate(cloc, urban = factor(urban, levels = c("R", "U"), labels = c("rural", "urban")))
cloc = subset(cloc, lon != 0)


# elevation
elev = geodata::elevation_30s(country = "Mali", path = here("data", "raw", "geodata"))

plot(elev)

# population von GHSL
pop = rast(x = here("data", "raw", "GHS_POP_E2020_GLOBE_R2022A_54009_1000_V1_0", "GHS_POP_E2020_GLOBE_R2022A_54009_1000_V1_0.tif"))
pop = crop(x = pop, y = st_transform(shp, crs = "+proj=moll"))
pop = project(x = pop, y = "epsg:4326", method = "bilinear")

plot(log(mask(x = pop, mask = shp)))


# global climate zones
climzone = rast(x = here("data", "raw", "Beck_KG_V1", "Beck_KG_V1_present_0p083.tif"))
climzone = crop(x = climzone, y = shp)

# climzone = as.factor(climzone)
# val = data.frame(ID = c(3, 4, 6), label = c("Tropical, savannah", "Arid, desert, hot", "Arid, steppe, hot"))
# climzone = categories(climzone, value = val, index = 2)

plot(mask(x = climzone, mask = shp))

# urban
urban = rast(x = here("data/raw/GHS_SMOD_E2020_GLOBE_R2022A_54009_1000_V1_0/GHS_SMOD_E2020_GLOBE_R2022A_54009_1000_V1_0.tif"))
urban = crop(x = urban, y = st_transform(shp, crs = "+proj=moll"))
urban = project(x = urban, y = "epsg:4326", method = "near")
plot(urban)

# load google earth engine raster files
fpath = list.files(path = here("data", "raw", "earthengine"), full.names = TRUE, pattern = "Mali")
rsd = rast(x = fpath)

plot(mask(x = rsd, mask = shp))


# extract data for the grid
grid$elev = extract(elev, grid[, .(lon, lat)], method = "bilinear", ID = FALSE)
grid$pop = extract(pop, grid[, .(lon, lat)], method = "bilinear", ID = FALSE)

grid$urban = extract(urban, grid[, .(lon, lat)], method = "simple", ID = FALSE)
grid$urban = grid$urban > 20L
grid$urban = factor(grid$urban, levels = c(FALSE, TRUE), labels = c("rural", "urban"))

grid$climate = extract(climzone, grid[, .(lon, lat)], ID = FALSE)
grid$climate = factor(grid$climate, levels = c(3L, 4L, 6L), labels = c("Tropical, savannah", "Arid, desert, hot", "Arid, steppe, hot"))

tmp = extract(x = rsd, grid[, .(lon, lat)], method = "bilinear", ID = FALSE)
colnames(tmp) = c("evi", "lstday", "lstnight", "ndvi", "precip", "water_mask")
grid = cbind(grid, tmp)

grid = grid[complete.cases(grid)]


# extract data for the cluster locations
cloc$pop = extract(pop, cloc[, .(lon, lat)], method = "bilinear", ID = FALSE)
cloc$elev = extract(elev, cloc[, .(lon, lat)], method = "bilinear", ID = FALSE)

cloc$climate = extract(climzone, cloc[, .(lon, lat)], ID = FALSE)
cloc$climate = factor(cloc$climate, levels = c(3L, 4L, 6L), labels = c("Tropical, savannah", "Arid, desert, hot", "Arid, steppe, hot"))

tmp = extract(x = rsd, cloc[, .(lon, lat)], method = "bilinear", ID = FALSE)
colnames(tmp) = c("evi", "lstday", "lstnight", "ndvi", "precip", "water_mask")
cloc = cbind(cloc, tmp)


# alternative construct urban / rural indicator, I finally use the SMOD GHSL
# because of the (slighlty) higher share of regions classified as urban

# pop.sorted = sort(grid$pop)
# pop.cumsum = cumsum(pop.sorted) / sum(pop.sorted)
# pop.threshold = max(pop.sorted[pop.cumsum <= 1-0.226])
#
# grid$urban = grid$pop > pop.threshold
# grid$urban = factor(grid$urban, levels = c(FALSE, TRUE), labels = c("rural", "urban"))
# sum(grid$pop[grid$urban == "urban"]) / sum(grid$pop) # check if urban proportion was correct: Yes


save(cloc, grid, file = here("data", "processed", "mali", "geodata.rda"))
