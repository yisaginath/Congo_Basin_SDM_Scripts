
# resampling climate and topography data to extent of land cover

# Download ~1 km DEM (30 arc-seconds) for selected countries (Africa)
# Countries: Cameroon, Gabon, Congo, DR Congo, Central African Republic, Equatorial Guinea

# install.packages(c("geodata","terra"))
library(geodata)
library(terra)

out_dir <- "F:/global_DEM/Africa_DEM_1km"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ISO3 codes
countries <- c(
  CMR = "Cameroon",
  GAB = "Gabon",
  COG = "Congo",
  COD = "Democratic Republic of Congo",
  CAF = "Central African Republic",
  GNQ = "Equatorial Guinea"
)

# Download each DEM
dem_list <- list()

for (i in seq_along(countries)) {
  iso3 <- names(countries)[i]
  nm   <- unname(countries[i])
  
  cat("Downloading DEM for:", nm, "(", iso3, ")\n")
  
  dem_i <- geodata::elevation_30s(country = iso3, path = out_dir)
  
  # Save individual country DEM
  writeRaster(
    dem_i,
    filename = file.path(out_dir, paste0("DEM_", iso3, "_1km.tif")),
    overwrite = TRUE
  )
  
  dem_list[[iso3]] <- dem_i
}


# Keep only SpatRaster objects (safety)
dem_list <- dem_list[sapply(dem_list, inherits, what = "SpatRaster")]

# Merge safely (sequential merge)
dem_region <- dem_list[[1]]
if (length(dem_list) > 1) {
  for (i in 2:length(dem_list)) {
    dem_region <- terra::merge(dem_region, dem_list[[i]])
  }
}

# Save merged raster
writeRaster(
  dem_region,
  filename = file.path(out_dir, "DEM_CB_1km.tif"),
  overwrite = TRUE
)

dem_region
plot(dem_region)


# Quick check
dem_region
plot(dem_region)


# Resample topography to extent of forest

# species summary
forest <- rast("F:/WWF_data/SDMs_LC_climate/predictors/forest_2020.tif")
forest

elevation <- rast("F:/global_DEM/Africa_DEM_1km/DEM_CB_1km_project.tif")
elevation

slope <- rast("F:/global_DEM/Africa_DEM_1km/slope_CB_1km_project.tif")
slope

# resample elevation and slope
elevation_res <- resample(elevation, forest, method = "bilinear")
elevation_res
plot(elevation_res)

slope_res <- resample(slope, forest, method = "bilinear")
slope_res
plot(slope_res)

out_dir <- "F:/WWF_data/SDMs_LC_climate/predictors"
# Save merged raster
writeRaster(
  elevation_res,
  filename = file.path(out_dir, "elevation.tif"),
  overwrite = TRUE
)

writeRaster(
  slope_res,
  filename = file.path(out_dir, "slope.tif"),
  overwrite = TRUE
)


# clean species observation data and get summary

# species summary
forest <- rast("F:/WWF_data/SDMs_LC_climate/predictors/forest_2020.tif")
forest
ncell(forest)
species_observations <- read.csv("F:/WWF_data/climate_species_analysis/updated_cleaned/analysis/new_analysis/all_species_observation2_no_square_ID.csv")
head(species_observations)
species_observations

# lets do abit of cleaning

# 1. Delete rows where column "species" is NA
species_observations <- species_observations[!is.na(species_observations$species), ]

# 2. Replace values in column "species"
species_observations$species[species_observations$species == "gorilla spp"] <- "gorilla"
species_observations$species[species_observations$species == "troglodytes troglodytes"] <- "troglodytes"

# 3. Replace values in column "scientific_name"
species_observations$scientific_name[
  species_observations$scientific_name == "Gorilla gorilla spp"
] <- "Gorilla gorilla"

species_observations$scientific_name[
  species_observations$scientific_name == "Pan troglodytes troglodytes"
] <- "Pan troglodytes"

# Optional: trim spaces just in case (recommended)
species_observations$species <- trimws(species_observations$species)
species_observations$scientific_name <- trimws(species_observations$scientific_name)

# View result
head(species_observations)

# delete rows with species column "spp"
species_observations <- species_observations[species_observations$species != "spp", ]

# Ensure CRS consistency
crs_observations <- "EPSG:4326"  # Assuming the amphibian data is in WGS84
if (crs(forest) != crs_observations) {
  species_points <- vect(species_observations, geom = c("longitude", "latitude"), crs = crs_observations)
  species_points <- project(species_points, crs(forest))  # Reproject to match raster
} else {
  species_points <- vect(species_observations, geom = c("longitude", "latitude"), crs = crs_observations)
}

species_points
forest

# Extract raster cell index (square_id)
species_observations$square_id <- cells(forest, species_points)[, "cell"]
head(species_observations)


write.csv(species_observations, "F:/WWF_data/SDMs_LC_climate/mammal_observations_cleaned.csv", row.names = FALSE)


# Step 1: Group and summarize
species_summary <- species_observations %>%
  group_by(genus, species, scientific_name, group) %>%
  summarise(n_CB = n(), .groups = 'drop')

# Step 2: Reorder columns
species_summary <- species_summary %>%
  select(genus, species, group, scientific_name, n_CB)

View(species_summary)

# Step 3: Export to CSV
write.csv(species_summary, "F:/WWF_data/SDMs_LC_climate/mammal_summary.csv", row.names = FALSE)



