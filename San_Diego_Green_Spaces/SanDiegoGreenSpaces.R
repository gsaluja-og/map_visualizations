# Load libraries
library(sf)
library(ggplot2)
library(osmdata)
library(showtext)

# Read San Diego boundaries
sd_bound <- st_read('data/san_diego_boundary_datasd/san_diego_boundary_datasd.shp')

# Get bounding box of San Diego
bb <- getbb('san diego')

# Get green spaces data
leisure <- opq(bbox = bb) |>
  add_osm_feature(key = 'leisure', 
                  value = c(
                    'park',
                    'pitch', 
                    'garden',
                    'nature_reserve'),
                  value_exact = FALSE,
                  match_case = FALSE) |>
  osmdata_sf () 

# The coordinate reference system (CRS) of the green spaces data is transformed 
# to match the CRS of San Diego boundaries using st_transform.
leisure$osm_polygons <- leisure$osm_polygons |> 
  st_transform(st_crs(sd_bound))

# Similarly for other keys from open street map - landuse and natural
landuse <- opq(bbox = bb) |>
  add_osm_feature(key = 'landuse', 
                  value = c(
                    'forest',
                    'meadow',
                    'recreation_ground',
                    'cemetery')) |>
  osmdata_sf () 

landuse$osm_polygons <- landuse$osm_polygons |> 
  st_transform(st_crs(sd_bound))

natural <- opq(bbox = bb) |>
  add_osm_feature(key = 'natural', 
                  value = c('wood'),
                  value_exact = FALSE,
                  match_case = FALSE) |>
  osmdata_sf () 

natural$osm_polygons <- natural$osm_polygons |> 
  st_transform(st_crs(sd_bound))

# Intersect green spaces with San Diego boundaries
leisure_key <- st_intersection(sd_bound, leisure$osm_polygons)
landuse_key <- st_intersection(sd_bound, landuse$osm_polygons)
natural_key <- st_intersection(sd_bound, natural$osm_polygons)

# Create plot
ggplot()+
  geom_sf(data = sd_bound, fill = 'grey90', color = 'grey60', size = .2) +
  geom_sf(data = leisure_key, fill = '#2f9a62', color = NA) +
  geom_sf(data = landuse_key, fill = '#2f9a62', color = NA) +
  geom_sf(data = natural_key, fill = '#2f9a62', color = NA) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.background = element_rect(fill = "lightblue"))

# Save plot
ggsave('san_diego_green_spaces.png', width = 1200, height = 1200, units = "px")
