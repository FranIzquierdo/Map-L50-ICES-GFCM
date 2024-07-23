#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Map ICES & GFSM areas with L50 and SST #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Modified 23/07/2024 #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Francisco Izquierdo        #
# francisco.izqtar@gmail.com #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# David Jose Nachon Garcia #
# david.nachon@ieo.csic.es #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clean workspace
rm(list=ls())

# Load necessary packages
library(sf)
library(ggplot2)
library(rnaturalearth)
library(viridis)
library(dplyr)
library(readr)
library(raster)
library(RColorBrewer)

# Download manually GFCM shapefile
# Link: https://www.fao.org/gfcm/data/maps/gsas/es/

# Load and prepare GFCM data
dir_dat_gfcm <- paste0(getwd(), "/Data/GFCM")
if (!file.exists(dir_dat_gfcm)) dir.create(dir_dat_gfcm)
areas_GFCM <- st_read(dsn = dir_dat_gfcm, layer = "GSAs_simplified")
areas_GFCM <- st_transform(areas_GFCM, crs = st_crs("+proj=longlat +datum=WGS84"))
areas_GFCM$source <- "GFCM"

# Download ICES ecoregions shapefile
# url <- "https://gis.ices.dk/shapefiles/ICES_areas.zip"
# file <- basename(url)
# download.file(url, destfile = file.path(dir_dat, file))

# Unzip
# unzip(file.path(dir_dat, file), exdir = dir_dat)

# Load and prepare ICES data
dir_dat_ices <- paste0(getwd(), "/Data/ICES")
if (!file.exists(dir_dat_ices)) dir.create(dir_dat_ices)
areas_ICES <- st_read(dsn = dir_dat_ices, layer = "ICES_Areas_20160601_cut_dense_3857")
areas_ICES <- st_transform(areas_ICES, crs = st_crs("+proj=longlat +datum=WGS84"))
areas_ICES$source <- "ICES"

# Combine the datasets
combined_areas <- bind_rows(areas_ICES, areas_GFCM)
combined_areas <- st_make_valid(combined_areas)  # Fix invalid geometries

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Define colors
fill_land <- "#a1b0ab"
fill_land2 <- "#73807C"
fill_seaI <- "#D5ECD4"
fill_seaG <- "#a5c4d4"

# Set map limits
map_limits <- list(x = c(-15, 40.1), y = c(30, 60)) # no bajar de 40

# Load L50 data
L50 <- read_csv("./Data/L50/L50.csv")

# Modify manually the location of ICES 8c9a points for better representation
L50 <- L50 %>% 
  mutate(Lon = if_else(Area == "ICES.9.a", -10.3, Lon),
         Lon = if_else(Area %in% c("ICES.8.c.9.a"), -10.35, Lon),
         Lat = if_else(Area %in% c("ICES.8.c.9.a"), 43, Lat)
  )

# Load SST data
sst_data <- raster(paste0(getwd(), "/Data/SST/mapa_SST.nc"))
sst_df <- as.data.frame(sst_data, xy = TRUE)
colnames(sst_df) <- c("lat", "long", "SST")
sst_df <- sst_df[complete.cases(sst_df), ]

# Define the color palette
num_colors <- 9
palette <- rev(brewer.pal(num_colors, "RdBu"))

# Calculate breaks for the size scale based on quantiles
breaks <- quantile(L50$Value, probs = seq(0, 1, length.out = 6))
breaks <- round(breaks, 1)

# seed
set.seed(1241)

# Create the final plot
ggplot() +
  geom_tile(data = sst_df, aes(x = long, y = lat, fill = SST), alpha = 0.9) +  # Increased alpha for SST layer
  scale_fill_gradientn(colors = palette) +
  geom_sf(data = world, fill = fill_land, color = fill_land2) +
  geom_sf(data = combined_areas[combined_areas$source == "ICES",], 
          fill = "#8ea8c3", color = fill_land2, size = 0.1, alpha = 0) +
  geom_sf(data = combined_areas[combined_areas$source == "GFCM",], 
          fill = "#8ea8c3", color = fill_land2, size = 0.1, alpha = 0) +
  geom_jitter(data = L50, aes(x = Lon, y = Lat, size = Value), 
              color = "#00A878", fill = "#00A878", alpha = 0.15, width = 0.8, height = 0.7) +  # Changed color to green
  scale_size_continuous(range = c(1, 5), breaks = breaks, 
                        labels = breaks) +
  geom_text(data = combined_areas, 
            aes(label = ifelse(source == "ICES", Area_27, SECT_COD), 
                x = st_coordinates(st_centroid(combined_areas))[, 1] + ifelse(source == "ICES", -0.5, 0.5), 
                y = st_coordinates(st_centroid(combined_areas))[, 2] + ifelse(source == "ICES", -0.5, 0.5)), 
            size = 1.9, color = "black", fontface = "bold") +
  coord_sf(xlim = map_limits$x, ylim = map_limits$y, expand = FALSE) +
  theme_light() +
  xlab(" ") + 
  ylab(" ") + 
  labs(fill = 'SST (ºC)', size = expression(L[50] ~ "(cm)")) +
  theme(legend.position = "bottom")

# Save the final plot
ggsave("./Figure/Figure MAP ICES GFCM L50 SST.jpeg", dpi = 300, width = 10, height = 6)
