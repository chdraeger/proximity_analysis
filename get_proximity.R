library(raster)
library(osmdata)
library(leaflet)
library(dplyr)
library(fasterize)
library(sf)
library(tmap)
library(tidyr)
library(ggplot2)
library(grafify)

source('utils.R')

# Configuration
#=====================================================
# feat_key <- "leisure"
# feat_value <- "park"
# feat_title <- "15 min city: Parks"

feat_key <- "amenity"
feat_value <- "library"
feat_title <- "15-Minute City: Library"

year <- 2020

# list of map features: https://wiki.openstreetmap.org/wiki/Map_features

# Read data
#=====================================================
r_canada <- crop_to_canada(raster(paste0("~/Data/population_projection/data_raw/population_projection_1km/SSP3/SSP3_", year, ".tif")), raster = TRUE)
r <- crop_to_vancouver(r_canada, raster = TRUE)

e <- extent(r)

# Build bbox matrix for osmdata
bb <- matrix(c(e@xmin, e@xmax,  # x (lon): min, max
                      e@ymin, e@ymax), # y (lat): min, max
                    nrow = 2, byrow = TRUE,
                    dimnames = list(c("x", "y"), c("min", "max")))
# bb <- getbb("Vancouver")

feature <- bb %>%
  opq() %>%
  add_osm_feature(key = feat_key, value = feat_value) %>%
  osmdata_sf()
feature_sf <- feature$osm_polygons


# 1) Get all raster cells that include feature centroids
#=====================================================
centroids <- st_centroid(feature_sf)
centroids_vec <- terra::vect(centroids)

r_terra <- terra::rast(r)
r_centroids <- raster(terra::rasterize(centroids_vec, r_terra, field = NULL, background = 0))

# 2) Get all raster cells that include polygons
#=====================================================
# shows only NA because polygons too small, if they don't intersect with cell centers
# --> has to be used with centroids
r_poly <- fasterize(feature_sf, r, field = NULL, fun = "max")
r_poly[is.na(r_poly)] <- 0

# 3) Combine centroids and polygons raster
#=====================================================
r_feat <- r_centroids + r_poly
r_feat[r_feat > 1] <- 1

# Add 15-min buffer (approximated through 3x3 matrix)
#=====================================================

# Use a 3x3 matrix to define the neighborhood (including center)
neighbors_matrix <- matrix(1, nrow = 3, ncol = 3)

# If any neighbor or center is 1: return 1, elseL 0
r_feat_all <- focal(r_feat, w = neighbors_matrix, fun = function(x) as.integer(any(x == 1)), pad = TRUE, padValue = 0)

# Set NA where no population
r_feat_all[is.na(r)] <- NA

# Plot
#=====================================================
r_feat_all_plot <- r_feat_all
r_feat_all_plot[r_feat_all_plot == 0] <- NA  # Set NA (to make those areas transparent in plot)


jpeg(paste0("plots/map_", year, ".jpg"), res = 150, width = 1000, height = 700)

tm_shape(r_feat_all_plot) +
  tm_basemap("OpenStreetMap") +
  tm_raster(alpha = 0.6, palette = c("darkred"), legend.show = TRUE, title = "Accessibility", labels = "Reachable within 15-minute walk") +
  tm_layout(main.title = feat_title, legend.outside = TRUE, legend.show = TRUE, legend.frame.lwd = 0)

dev.off()

# leaflet() %>%
#   addTiles() %>%
#   addProviderTiles("Esri.WorldGrayCanvas") %>%
#   addRasterImage(r_feat_all,
#                  colors = colorNumeric(palette = c("darkred"), domain = c(1)),
#                  opacity = 0.7)  # Adjust opacity as needed


# Analyze for different population groups
#=====================================================
coords_crop <- coordinates(r)  # cropped

mapping_crop <- data.frame(
  cell = cellFromXY(r_canada, coords_crop),  # Cell numbers in the original raster
  cell_crop = cellFromXY(r, coords_crop),  # Cell numbers in the cropped raster
  walkable = values(r_feat_all),
  tot_pop = values(r)
)
mapping_crop <- mapping_crop[(!is.na(mapping_crop$tot_pop)) & (mapping_crop$tot_pop > 0), ]


for(type in c("minority", "income", "education", "age_sex")){
  
  groups <- get_groups()[[type]]
  
  proj <- readRDS(paste0("~/Data/population_projection/train/", type, "/final/raster/raster_points_SSP3_", year, ".RDS"))
  
  res <- left_join(mapping_crop, proj, by = c("cell"))
  
  res <- res %>%
    pivot_longer(
      cols = all_of(groups),
      names_to = "group",
      values_to = "nr"
    )
  
  res <- res %>%
    group_by(group, walkable) %>%
    summarise(nr = sum(nr, na.rm = TRUE), .groups = "drop") %>%
    group_by(group) %>%
    mutate(percentage = nr / sum(nr) * 100)
  
  res <- res %>%
    mutate(walkable = ifelse(walkable == 1, "Yes", "No"))
  res$walkable <- factor(res$walkable, levels = c("Yes", "No"))
  
  res$group_title <- tools::toTitleCase(gsub("_", " ", res$group))
  res$nr <- res$nr / 1000
  
  # Rearrange
  res <- res %>%
    mutate(group = factor(group, levels = groups)) %>%
    arrange(group)
  res$group_title <- factor(res$group_title, levels = unique(res$group_title))
  
  # Plot
  #=====================================================
  
  # Plot (total population)
  jpeg(paste0("plots/demo_pop_", type, "_", year, ".jpg"), res = 150, width = 1000, height = 700)
  
  g <- ggplot(res, aes(x = group_title, y = nr, fill = factor(walkable))) +
    geom_bar(stat = "identity") +
    labs(title = paste0(feat_title, " (", year, ")"),
         x = "Demographic group",
         y = "Population (thds.)",
         fill = "Walkable") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      # plot.title = element_text(hjust = 0.5)  # center title
    ) +
    # scale_fill_brewer(palette = "Set2")
    scale_fill_grafify(palette = "fishy")
  print(g)
  
  dev.off()
  
  # Plot (percentage)
  jpeg(paste0("plots/demo_perc_", type, "_", year, ".jpg"), res = 150, width = 1000, height = 500)
  
  g <- ggplot(res, aes(x = group_title, y = percentage, fill = factor(walkable))) +
    geom_bar(stat = "identity") +
    labs(title = paste0(feat_title, " (", year, ")"),
         x = "Demographic group",
         y = "Population (%)",
         fill = "Walkable") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      # plot.title = element_text(hjust = 0.5)  # center title
    ) +
    scale_fill_grafify(palette = "fishy")
  print(g)
   
  dev.off()
  
}



