install.packages(c("cartogram", "sf", "dplyr", "tmap"))
library(cartogram)
library(sf)
library(dplyr)
library(tmap)

data("World", package = "tmap")

download.file("https://github.com/elisamarchetto/Cartogram_chapter/raw/main/PresAbs.bias.rds", "PresAbs.bias.rds", mode = "wb")
PresAbs.bias <- readRDS("PresAbs.bias.rds")

head(PresAbs.bias)

PresAbs.bias <- st_as_sf(PresAbs.bias, coords = c("x", "y"), crs = "EPSG:4326")
World_ea <- st_transform(World, "EPSG:8857")
PresAbs.bias <- st_transform(PresAbs.bias, "EPSG:8857")

PresAbs.bias <- st_join(PresAbs.bias, World_ea["iso_a3"])

count.PresAbs.bias <- PresAbs.bias |>
  st_drop_geometry() |>
  group_by(iso_a3) |>
  summarize(countISO = sum(Observed))

bias_world <- World_ea |>
  left_join(count.PresAbs.bias, by = "iso_a3") |>
  mutate(countISO = ifelse(is.na(countISO), 0, countISO))

cart_cont <- cartogram_cont(bias_world, "countISO", itermax = 30, threshold = 0.16)

map_standard <- tm_shape(bias_world) +
  tm_polygons("countISO", style = "jenks", palette = "cividis") +
  tm_layout(frame = FALSE, legend.position = c("left", "bottom"),
            legend.width = 1.5, legend.outside = TRUE)
map_carto1 <- tm_shape(cart_cont) +
  tm_polygons("countISO", style = "jenks", palette = "cividis") +
  tm_layout(frame = FALSE, legend.position = c("left", "bottom"),
            legend.width = 1.5, legend.outside = TRUE)
tmap_arrange(map_standard, map_carto1)

cart_ncont <- cartogram_ncont(bias_world, "countISO")

tm_shape(World) +
  tm_borders() +
  tm_shape(cart_ncont) +
  tm_polygons("countISO", style = "jenks", palette = "cividis") +
  tm_layout(frame = FALSE, legend.position = c("left", "bottom"),
            legend.width = 1.5, legend.outside = TRUE)

cart_dorling <- cartogram_dorling(bias_world, "countISO")

tm_shape(World) + tm_borders() +
  tm_shape(cart_dorling) +
  tm_polygons("countISO", style = "jenks", palette = "cividis") +
  tm_layout(frame = FALSE, legend.position = c("left", "bottom"),
            legend.width = 1.5, legend.outside = TRUE)

