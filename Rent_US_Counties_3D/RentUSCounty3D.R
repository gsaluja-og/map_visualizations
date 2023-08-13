# install.packages(c("viridis"))

# Loading necessary libraries
# sf - for handling spatial data frames
# ggplot2 - for plotting and visualization
# tigris - for accessing US Census TIGER/Line shapefiles
# dplyr - for data manipulation
# viridis - for color-blind friendly color palettes
# rmapshaper - for simplifying and manipulating geographic shapes
# rayshader - for 2D and 3D plotting of ggplot objects
# colorspace - for color manipulation tools

library(sf)
library(ggplot2)
library(tigris)
library(dplyr)
library(viridis)
library(rmapshaper)
library(rayshader)
library(colorspace)

# Load the rent data from a CSV file
# Available here - https://www.zillow.com/research/data/
rent_data <- read.csv("County_zori_sm_month.csv")

# Use a cached version if available to speed up data retrieval.
options(tigris_use_cache = TRUE)

# Get US counties shape data using tigris library
counties <- tigris::counties(class = "sf")

# Simplify the geometry to speed up plotting and reduce file size
counties_simplified <- ms_simplify(counties)

# Define a bounding box to only include contiguous US counties (excluding Alaska, Hawaii, etc.)
bbox_sf <- st_as_sfc(st_bbox(c(xmin = -125, xmax = -66, ymin = 24, ymax = 50)))

# Crop the counties to the bounding box defined above
counties_contiguous <- st_crop(counties_simplified, bbox_sf)

# Sanity Check - Plot county map
# ggplot(data = counties_contiguous) + 
#   geom_sf() + 
#   theme_minimal() +
#   labs(title = "U.S. Counties (Contiguous)") +
#   coord_sf(xlim = c(-125, -66), ylim = c(24, 50))

# highest_rent_county <- rent_data %>%
#   arrange(desc(X2023.06.30)) %>%
#   slice_head(n = 1)
# 
# print(highest_rent_county)

# REMOVING PITKIN COUNTY - A BIG OUTLIER 18K RENT!!
rent_data_cleaned <- rent_data %>%
  filter(RegionName != "Pitkin County")

# Check histogram after outlier removal - looks good now
# ggplot(rent_data_cleaned, aes(x = X2023.06.30)) +
#   geom_histogram(binwidth = 50, fill = "blue", color = "black", alpha = 0.7) + 
#   labs(title = "Distribution of Rent Prices for 2023-06-30", 
#        x = "Rent Price", 
#        y = "Number of Counties") +
#   theme_minimal()


# Create unique identifiers for merging county shape data with rent data
counties_contiguous$unique_id <- paste0(counties_contiguous$NAMELSAD, "_", counties_contiguous$STATEFP)
rent_data_cleaned$unique_id <- paste0(rent_data_cleaned$RegionName, "_", sprintf("%02d", rent_data_cleaned$StateCodeFIPS))

# Merge rent data with counties data
counties_rent <- left_join(counties_contiguous, rent_data_cleaned, by = "unique_id")

# Fill NA with 0
counties_rent$X2023.06.30[is.na(counties_rent$X2023.06.30)] = 0

# Create a custom color palette using the colorspace library
# Mix primary colors with white to get gradient shades
c1 <- mixcolor(alpha = seq(from = 0, to = 1, by = .25), color1 =  hex2RGB("#02bcc9"), 
               color2 = hex2RGB("#ffffff")) |> 
  hex()
c2 <- mixcolor(alpha = seq(from = 0, to = 1, by = .25), color1 =  hex2RGB("#ff4992"), 
               color2 = hex2RGB("#ffffff")) |> 
  hex()

# c3 <- "#D3D3D3"
c3 <- "white"

# Combine the created colors to a single palette
colors <- c(c3, rev(c1[1:4]), rev(c2[1:4]))
swatchplot(colors)
my_palette <- colorRampPalette(colors, 2.5)(256)
swatchplot(my_palette)

# Create a 2D plot of the counties colored by rent data using ggplot2
gg = ggplot(data = counties_rent) + 
  geom_sf(aes(fill = X2023.06.30), color = "transparent") +
  scale_fill_gradientn(colors = my_palette) +
  theme(axis.line=element_blank(), 
        axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.text.y=element_blank(), axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks=element_blank(), 
        panel.background = element_rect(fill="#1F1F1F"),
        legend.position = "none",
        plot.background = element_rect(fill="#1F1F1F")) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50))

# Close any previous rgl windows to avoid conflicts
try(rgl::rgl.close())

# Convert the 2D ggplot object to a 3D plot using rayshader
plot_gg(gg, multicore=TRUE, width=5, height=5, scale=250, windowsize=c(1600,1000),
        zoom = 0.35, phi = 50, theta=0, solid=FALSE, background = "#1F1F1F", 
        shadow_intensity = 0.35)

# Adjust the camera settings to set the final view for the 3D plot
render_camera(zoom=0.32, phi = 55, theta=0)

# Save the final 3D plot to an image file
render_snapshot("RentUSCountyLevel3dFinal2.png", software_render = FALSE, width=1600, height = 1200)

# PLOT THE SAME MAP WITH LEGEND

# Plot 2D map with ggplot with legend
gg_with_legend = ggplot(data = counties_rent) + 
  geom_sf(aes(fill = X2023.06.30), color = "transparent") +
  scale_fill_gradientn(colors = my_palette, name = "") +
  theme(axis.line=element_blank(), 
        axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.text.y=element_blank(), axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks=element_blank(), 
        panel.background = element_rect(fill="#1F1F1F"),
        plot.background = element_rect(fill="#1F1F1F"),
        legend.background = element_rect(fill="#1F1F1F"),
        legend.text = element_text(color="white")) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50))

try(rgl::rgl.close())
# Now, use the rayshader package to convert the ggplot object to a 3D plot
plot_gg(gg_with_legend, multicore=TRUE, width=5, height=5, scale=250, windowsize=c(1600,1000),
        zoom = 0.35, phi = 50, theta=0, solid=FALSE, background = "#1F1F1F", 
        shadow_intensity = 0.35)

library(cowplot)

# Extract legend
legend_gg = get_legend(gg_with_legend)

# Save the legend as an image which we can overlay later
ggsave(filename = "legend.png", plot = legend_gg, width = 2, height = 4, dpi = 300)




# HOUSING DATA ---------------------------------------------------------------
# cols_to_read <- c("RegionName", "X2023.06.30")
# 
# all_cols <- read.csv("County_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_month.csv", header = TRUE, nrows = 1, stringsAsFactors = FALSE)
# all_cols <- names(all_cols)
# 
# col_classes <- ifelse(all_cols %in% cols_to_read, "character", "NULL")
# 
# single_family_home_data <- read.csv("County_zhvi_uc_sfr_tier_0.33_0.67_sm_sa_month.csv", header = TRUE, colClasses = col_classes, stringsAsFactors = FALSE)
# single_family_home_data$X2023.06.30 <- as.numeric(single_family_home_data$X2023.06.30)
# # What is up with this place?
# single_family_home_data<- single_family_home_data %>% filter(RegionName != "Pitkin County")
# 
# # Merge rent data with counties data
# counties_home <- left_join(counties_contiguous, single_family_home_data, by = c("NAMELSAD" = "RegionName"))
# 
# # Plot 2D map with ggplot
# gghome = ggplot(data = counties_home) + 
#   geom_sf(aes(fill = X2023.06.30)) +
#   scale_fill_viridis_c(na.value = "transparent") +
#   theme_minimal() +
#   labs(title = "U.S. County Single Family Home Prices ", fill = "Home Price") +
#   coord_sf(xlim = c(-125, -66), ylim = c(24, 50))
# 
# plot_gg(gghome, multicore=TRUE, width=5, height=5, scale=250)
# 
# 
# ggplot(single_family_home_data, aes(x = X2023.06.30)) +
#   geom_histogram(binwidth = 500, fill = "blue", color = "black", alpha = 0.7) +
#   labs(title = "Distribution of Home Prices for 2023-06-30",
#        x = "Home Price",
#        y = "Number of Counties") +
#   theme_minimal()
