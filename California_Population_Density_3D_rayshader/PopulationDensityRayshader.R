# install.packages("tidyverse")
# install.packages("remotes")
# remotes::install_github("https://github.com/tylermorganwall/rayrender")
# remotes::install_github("https://github.com/tylermorganwall/rayshader")

library(sf)
library(tidyverse)
library(elevatr)
library(rayshader)
library(glue)
library(colorspace)
library(tigris)
library(stars)
library(NatParksPalettes)
library(rnaturalearth)

map <- "california"

# Original code and inspiration by https://github.com/Pecners

# Kontur data source: https://data.humdata.org/organization/kontur
# Download gpkg.zip, unzip and add it to data folder
d_layers <- st_layers("data/kontur_population_US_20220630.gpkg")

# Get the coordinate reference system (CRS) from the data
d_crs <- d_layers[["crs"]][[1]][[2]]

# Load US states data and transform its CRS to match our population data
s <- states() |> 
  st_transform(crs = d_crs)

# Filter out the state of California
st <- s |> 
  filter(NAME == str_to_title(str_replace_all(map, "_", " ")))

wkt_st <- st_as_text(st[[1,"geometry"]])

# Load population data for California using the boundary as a filter
data <- st_read("data/kontur_population_US_20220630.gpkg",
                wkt_filter = wkt_st)


# Load physical land data from Natural Earth and transform its CRS to match our population data

address <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_land.zip"
destdir <- tempdir()
utils::download.file(file.path(address), zip_file <- tempfile())
utils::unzip(zip_file, exdir = destdir)
land <- st_read(glue("{destdir}/ne_10m_land.shp")) |> 
  st_transform(crs = st_crs(data))

# Find the intersection between California's boundary and the land data

st_land <- st_intersection(st, land)

# Add a buffer with dummy population of 1 to boundary of state 
# so the boundary is visible in the final render
st_buff <- st_land |> 
  st_cast("MULTILINESTRING") |> 
  st_buffer(1609.34 * .25, endCapStyle = "FLAT", joinStyle = "MITRE") |> 
  mutate(population = 1) |> 
  rename(geom = geometry)

# Sanity check

st_buff |> 
  ggplot() +
  geom_sf()

# Merge population data and boundary data
st_dd <- st_join(data, st, left = FALSE)
st_d <- bind_rows(st_buff, st_dd)

# Get the bounding box of California
bb <- st_bbox(st_d)

# Calculate the aspect ratio of the bounding box to keep it correct in the rasterization process
# Rasterization is the process of converting vector data (like our boundary and population data) into a grid of cells (a raster)

yind <- st_distance(st_point(c(bb[["xmin"]], bb[["ymin"]])), 
                    st_point(c(bb[["xmin"]], bb[["ymax"]])))
xind <- st_distance(st_point(c(bb[["xmin"]], bb[["ymin"]])), 
                    st_point(c(bb[["xmax"]], bb[["ymin"]])))

if (yind > xind) {
  y_rat <- 1
  x_rat <- xind / yind
} else {
  x_rat <- 1
  y_rat <- yind / xind
}

# Keep small value initially to test and increase later to build a high resolution render
size <- 5000

# Convert the raster data into a matrix, a 2D representation of our data
rast <- st_rasterize(st_d |> 
                       select(population, geom),
                     nx = floor(size * x_rat), ny = floor(size * y_rat))


mat <- matrix(rast$population, nrow = floor(size * x_rat), ncol = floor(size * y_rat))


# Create color palette and texture for the 3D plot

pal <- "pats"
c2 <- mixcolor(alpha = seq(from = 0, to = .75, by = .25), color1 =  hex2RGB("#a50021"),
               color2 = hex2RGB("#ffffff")) |>
  hex() |> 
  rev()
colors <- c(c2)
swatchplot(colors)
texture <- grDevices::colorRampPalette(colors, .5)(256)
swatchplot(texture)

# Close any previous 3D plots
try(rgl::rgl.close())

# Create a 3D plot using the matrix data. 
# The matrix data is used to determine the heights (populations) of the points on the plot.

mat |> 
  height_shade(texture = texture) |> 
  plot_3d(heightmap = mat, 
          
          solid = FALSE,
          soliddepth = 0,
          # Change this depending on the data resolution
          # lower values exaggerate the height
          z = 150/5,
          # Set the location of the shadow, i.e. where the floor is.
          shadowdepth = 0,
          # Set the window size (initially small to render fast).
          windowsize = c(800,800), 
          # This is the azimuth, like the angle of the sun.
          # 90 degrees is directly above, 0 degrees is a profile view.
          phi = 90, 
          zoom = 1, 
          # `theta` is the rotations of the map. Keeping it at 0 will preserve
          # the standard (i.e. north is up) orientation of a plot
          theta = 0, 
          background = "white")

# Use this to adjust the view after building the window object
render_camera(phi = 35, zoom = .6, theta = 60)

outfile <- str_to_lower(glue("images/{map}_{pal}.png"))

{
  start_time <- Sys.time()
  cat(glue("Start Time: {start_time}"), "\n")
  render_highquality(
    filename = outfile,
    samples = 300,
    interactive = FALSE,
    lightcolor = c("#D28090", "white", "#c1f2fe", "white"),
    lightintensity = c(500, 75, 750, 75),
    lightaltitude = c(10, 80, 10, 80),
    lightdirection = rev(c(85, 85, 95, 95)),
    width = 6000, height = 6000,
    ground_material = rayrender::microfacet(roughness = .4,
                                            eta = c(1, .75, .1),
                                            kappa = c(.1, .75, 1))
  )
  end_time <- Sys.time()
  cat(glue("Total time: {end_time - start_time}"), "\n")
}