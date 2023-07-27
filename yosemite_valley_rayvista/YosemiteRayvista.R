
# install.packages("devtools")
# devtools::install_github("h-a-graham/rayvista", dependencies=TRUE)

library(elevatr)
library(sf)
library(rayvista)
library(rayshader)

# Lat and Long for Yosemite Valley
latitude <- 37.74570146754716
longitude <- -119.59348885948349

yosemite <- plot_3d_vista(latitude, longitude, elevation_detail=14,
                          radius=7000,
                            overlay_detail=15, show_vista = FALSE)

# You can plot using the above function itself by setting show_vista = TRUE 
# and commenting out the below line
yosemite$dem_matrix %>%
  height_shade()%>%
  add_shadow(ray_shade(yosemite$dem_matrix, zscale=4), 0.2) %>%
  add_overlay(., yosemite$texture,rescale_original=TRUE) %>%
  plot_3d(., yosemite$dem_matrix, zscale=3, solid=FALSE,
          windowsize = 1000, zoom=1, phi=35, theta=290, soliddepth = -900,
          background = "white")

# Adjust the scene
# phi is azimuth angle. 90 is directly above so doesn't look like 3d.
# theta is rotation of map
render_camera(phi = 30, zoom = 0.6, theta = 290)

render_highquality(clear=TRUE,
                   interactive = FALSE,
                   lightcolor = "#cad2c5",
                   lightintensity = 600,
                   lightaltitude = 45,
                   lightdirection = 220,
                   filename = "yosemite_valley_detailed.png",
                   samples=250,
                   width = 4000,
                   height=4000
)
