# install.packages(c("cartogram", "sf", "dplyr", "tmap"))
library(cartogram)
library(sf)
library(dplyr)
library(tmap)

data("World", package = "tmap")

download.file("https://github.com/elisamarchetto/Cartogram_chapter/raw/main/PresAbs.bias.rds", "PresAbs.bias.rds", mode = "wb")
PresAbs.bias <- readRDS("PresAbs.bias.rds")

head(PresAbs.bias)

PresAbs.bias <- st_as_sf(PresAbs.bias, coords = c("x", "y"), crs = "EPSG:4326")
World.ea <- st_transform(World, "EPSG:8857")
PresAbs.bias <- st_transform(PresAbs.bias, "EPSG:8857")

PresAbs.bias <- st_join(PresAbs.bias, World.ea["iso_a3"])

count.PresAbs.bias <- PresAbs.bias |>
    st_drop_geometry() |>
    group_by(iso_a3) |>
    summarize(countISO = sum(Observed))

World.bias <- World.ea |>
    left_join(count.PresAbs.bias, by = "iso_a3") |>
    mutate(countISO = ifelse(is.na(countISO), 0, countISO))

cart.cont <- cartogram_cont(World.bias, "countISO", itermax = 30, threshold = 0.16)

map.standard <- tm_shape(World.bias) +
    tm_polygons("countISO", style = "jenks", palette = "cividis") +
    tm_layout(frame = FALSE, legend.position = c("left", "bottom"),
              legend.width = 1.5, legend.outside = TRUE)
map.carto1 <- tm_shape(cart.cont) +
    tm_polygons("countISO", style = "jenks", palette = "cividis") +
    tm_layout(frame = FALSE, legend.position = c("left", "bottom"),
              legend.width = 1.5, legend.outside = TRUE)
tmap_arrange(map.standard, map.carto1)

cart.ncont <- cartogram_ncont(World.bias, "countISO")

tm_shape(World.ea) +
    tm_borders() +
    tm_shape(cart.ncont) +
    tm_polygons("countISO", style = "jenks", palette = "cividis") +
    tm_layout(frame = FALSE, legend.position = c("left", "bottom"),
              legend.width = 1.5, legend.outside = TRUE)

cart.dorling <- cartogram_dorling(World.bias, "countISO")

tm_shape(World.ea) +
    tm_borders() +
    tm_shape(cart.dorling) +
    tm_polygons("countISO", style = "jenks", palette = "cividis") +
    tm_layout(frame = FALSE, legend.position = c("left", "bottom"),
              legend.width = 1.5, legend.outside = TRUE)

