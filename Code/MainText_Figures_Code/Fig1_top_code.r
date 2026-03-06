## Figure 1 Top — Global Blue Capital Map

## Load Geospatial Data
    # Corals
    load(file="External_Data/output_modules/corals/geospatial_coral_polygons.rdat")

    # Mangroves — load from cache if available (first run builds cache: simplify + union)
    mangroves_cache <- "External_Data/input_modules/mangroves/gmw/gmw_simplified_cache.rds"
    if (file.exists(mangroves_cache)) {
      cat("  Loading cached simplified mangroves...\n")
      geospatial_mangroves_polygons <- readRDS(mangroves_cache)
    } else {
      cat("  Loading full mangroves shapefile (first run only)...\n")
      geospatial_mangroves_polygons <- st_read("External_Data/input_modules/mangroves/gmw/gmw_v3_2020_vec.shp")
      cat("  Simplifying + unioning mangroves (saving cache for future runs)...\n")
      geospatial_mangroves_polygons <- st_simplify(geospatial_mangroves_polygons,
                                                   dTolerance = 0.01,
                                                   preserveTopology = FALSE)
      geospatial_mangroves_polygons <- st_union(geospatial_mangroves_polygons)
      saveRDS(geospatial_mangroves_polygons, mangroves_cache)
      cat("  Mangroves cache saved.\n")
    }

    # Ports
    geospatial_ports_polygons <- st_read("External_Data/input_modules/ports/nodes_maritime.gpkg")

    # Fisheries raster
    geospatial_fish_raster <- readRDS("External_Data/input_modules/fish/species_richness_yr0.Rds")

    # Reproject corals to match mangroves CRS
    crs_map_mangroves <- st_crs(geospatial_mangroves_polygons)
    geospatial_coral_polygons <- st_transform(geospatial_coral_polygons, crs = crs_map_mangroves)

    # Convert fisheries raster to terra SpatRaster
    geospatial_fish_raster <- as.data.frame(geospatial_fish_raster)
    geospatial_fish_raster2 <- terra::rast(geospatial_fish_raster, type="xyz", crs="EPSG:4326")

    # Mask fisheries raster to EEZ boundaries and reproject to Robinson
    eez_boundaries <- st_read("External_Data/other/eez_v11.gpkg")
    eez_boundaries_terra <- vect(eez_boundaries)
    cropped_raster <- crop(geospatial_fish_raster2, eez_boundaries_terra)
    masked_raster  <- mask(cropped_raster, eez_boundaries_terra)
    masked_raster  <- project(masked_raster, "+proj=robin")
    masked_raster_df <- as.data.frame(masked_raster, xy = TRUE, na.rm = TRUE) %>%
      mutate(nspp_quantile = ntile(nspp, 4))

## Color setup
lighter_shades <- lighten(col_fish_low, amount = c(0.8, 0.7, 0.6, 0.5))
feature_colors <- c(
  "Corals"    = col_cor,
  "Mangroves" = "#4fe34f",
  "Ports"     = "#0aa3a3",
  "Fisheries" = col_fish_low,
  "1" = lighter_shades[1], "2" = lighter_shades[2],
  "3" = lighter_shades[3], "4" = lighter_shades[4]
)

world <- ne_countries(scale = "medium", returnclass = "sf")

## Figure 1A — Global map (Robinson projection)
ggplot() +
  geom_tile(data = masked_raster_df, aes(x = x, y = y, fill = factor(nspp_quantile)),
            show.legend = TRUE) +
  geom_sf(data = world, fill = "black", color = NA, show.legend = FALSE) +
  geom_sf(data = geospatial_coral_polygons,
          aes(color = "Corals", fill = "Corals"), show.legend = TRUE) +
  geom_sf(data = geospatial_mangroves_polygons,
          aes(color = "Mangroves", fill = "Mangroves"), show.legend = TRUE) +
  geom_sf(data = geospatial_ports_polygons %>% filter(infra == "port"),
          aes(color = "Ports", fill = "Ports"), size = 0.01, show.legend = TRUE) +
  scale_fill_gradient(low = col_fish_low, high = col_fish_high, name = "Fish Species") +
  scale_color_manual(name = "Features", values = feature_colors) +
  scale_fill_manual(name = "Features", values = feature_colors) +
  theme_minimal() +
  guides(fill = "none", color = "none") +
  coord_sf(crs = "+proj=robin")

ggsave("Figures/Main/Panels/Fig1_top.png")


## Figure 1A (Asia zoom) — lon 40–180, lat –20 to 60
ggplot() +
  geom_tile(data = masked_raster_df, aes(x = x, y = y, fill = factor(nspp_quantile)),
            show.legend = TRUE) +
  geom_sf(data = world, fill = "black", color = NA, show.legend = FALSE) +
  geom_sf(data = geospatial_coral_polygons,
          aes(color = "Corals", fill = "Corals"), show.legend = TRUE) +
  geom_sf(data = geospatial_mangroves_polygons,
          aes(color = "Mangroves", fill = "Mangroves"), show.legend = TRUE) +
  geom_sf(data = geospatial_ports_polygons %>% filter(infra == "port"),
          aes(color = "Ports", fill = "Ports"), size = 0.01, show.legend = TRUE) +
  scale_fill_gradient(low = col_fish_low, high = col_fish_high, name = "Fish Species") +
  scale_color_manual(name = "Features", values = feature_colors) +
  scale_fill_manual(name = "Features", values = feature_colors) +
  theme_minimal() +
  guides(fill = "none", color = "none") +
  coord_sf(xlim = c(40, 180), ylim = c(-20, 60), crs = "+proj=robin",
           default_crs = sf::st_crs(4326))

ggsave("Figures/Main/Fig1_top_asia.png", dpi = 300)
