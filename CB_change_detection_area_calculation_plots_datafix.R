
library(terra)
library(dplyr)
library(sf)
library(raster)


# Main folder containing subfolders with raster files
base_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated"

# List all tif files in subfolders
raster_files <- list.files(base_dir,
                           pattern = "\\.tif$",
                           full.names = TRUE,
                           recursive = TRUE)

# Print file paths
print(raster_files)

# Read rasters
rasters <- lapply(raster_files, rast)

# Print raster summaries
for(i in seq_along(rasters)){
  cat("\n-----------------------------\n")
  cat("Raster:", raster_files[i], "\n")
  print(rasters[[i]])
}


MAT <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/MAT_current_99.tif")
MAT
MAP <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAP_filtered_5_95/MAP_current_99.tif")
MAP

MAT_2030_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2030_ssp245_filt5_99.tif")
MAT_2030_ssp245

MAT_2030_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2030_ssp585_filt5_99.tif")
MAT_2030_ssp585

MAT_2050_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2050_ssp126_filt5_99.tif")
MAT_2050_ssp126

MAT_2050_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2050_ssp245_filt5_99.tif")
MAT_2050_ssp245

MAT_2050_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2050_ssp585_filt5_99.tif")
MAT_2050_ssp585

MAT_2070_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2070_ssp126_filt5_99.tif")
MAT_2070_ssp126

MAT_2070_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2070_ssp245_filt5_99.tif")
MAT_2070_ssp245

MAT_2070_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2070_ssp585_filt5_99.tif")
MAT_2070_ssp585

MAT_2100_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2100_ssp126_filt5_99.tif")
MAT_2100_ssp126

MAT_2100_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2100_ssp245_filt5_99.tif")
MAT_2100_ssp245

MAT_2100_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2100_ssp585_filt5_99.tif")
MAT_2100_ssp585


# Crop current species suitable habitat to to future shifting temp patterns
# -----------------------------
base_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated"

species_files <- list.files(
  base_dir,
  pattern = "_mean_pixel_binary\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

species_files
length(species_files)

# -----------------------------
# 2) Future temperature rasters
# -----------------------------
temp_files <- c(
  "2050_ssp126" = "F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2050_ssp126_filt5_99.tif",
  "2050_ssp245" = "F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2050_ssp245_filt5_99.tif",
  "2050_ssp585" = "F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2050_ssp585_filt5_99.tif",
  "2070_ssp126" = "F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2070_ssp126_filt5_99.tif",
  "2070_ssp245" = "F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2070_ssp245_filt5_99.tif",
  "2070_ssp585" = "F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2070_ssp585_filt5_99.tif",
  "2100_ssp126" = "F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2100_ssp126_filt5_99.tif",
  "2100_ssp245" = "F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2100_ssp245_filt5_99.tif",
  "2100_ssp585" = "F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2100_ssp585_filt5_99.tif"
)

temp_rasters <- lapply(temp_files, rast)

# quick check
lapply(temp_rasters, function(x) {
  list(dim = dim(x), res = res(x), ext = ext(x), crs = crs(x))
})

# -----------------------------
# 3) Output folder
# -----------------------------
out_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated_overlap_with_future_temp"
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 4) Function to process one species x one scenario
# -----------------------------
process_species_temp_overlap <- function(sp_file, temp_rast, scen_name, out_root) {
  
  sp <- rast(sp_file)
  
  # species label from filename
  sp_name <- tools::file_path_sans_ext(basename(sp_file))
  sp_name <- sub("_mean_pixel_binary$", "", sp_name)
  
  # species folder name
  sp_folder <- basename(dirname(sp_file))
  out_dir <- file.path(out_root, sp_folder)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # make sure CRS matches
  if (!same.crs(sp, temp_rast)) {
    temp_rast <- project(temp_rast, sp, method = "bilinear")
  }
  
  # crop temperature raster to species extent
  temp_crop <- crop(temp_rast, sp)
  
  # align geometry if needed
  if (!compareGeom(sp, temp_crop, stopOnError = FALSE)) {
    temp_crop <- resample(temp_crop, sp, method = "bilinear")
  }
  
  # keep only species cells == 1 that overlap valid temp pixels
  # output = 1 where species == 1 and temp is not NA, otherwise NA
  out_rast <- ifel(sp == 1 & !is.na(temp_crop), 1, NA)
  names(out_rast) <- paste0(sp_name, "_", scen_name, "_binary_overlap")
  
  out_file <- file.path(out_dir, paste0(sp_name, "_binary_overlap_", scen_name, ".tif"))
  
  writeRaster(out_rast, out_file, overwrite = TRUE)
  
  return(out_file)
}

# -----------------------------
# 5) Run for all species and all future scenarios
# -----------------------------
all_outputs <- list()

for (sp_file in species_files) {
  cat("\nProcessing species:", sp_file, "\n")
  
  for (scen_name in names(temp_rasters)) {
    cat("   Scenario:", scen_name, "\n")
    
    out_file <- process_species_temp_overlap(
      sp_file   = sp_file,
      temp_rast = temp_rasters[[scen_name]],
      scen_name = scen_name,
      out_root  = out_root
    )
    
    all_outputs[[length(all_outputs) + 1]] <- data.frame(
      species_file = sp_file,
      scenario = scen_name,
      output_file = out_file,
      stringsAsFactors = FALSE
    )
  }
  
  gc()
}

all_outputs_df <- do.call(rbind, all_outputs)
print(all_outputs_df)

write.csv(
  all_outputs_df,
  file.path(out_root, "species_future_temp_overlap_files.csv"),
  row.names = FALSE
)

# -----------------------------
# 6) Plot one species across all 9 scenarios to verify
# -----------------------------
# Example: bonobo
example_species_file <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/bonobos/Pan paniscus_mean_pixel_binary.tif"
example_species_name <- sub("_mean_pixel_binary\\.tif$", "", basename(example_species_file))
example_species_folder <- basename(dirname(example_species_file))

example_out_files <- file.path(
  out_root,
  example_species_folder,
  paste0(example_species_name, "_binary_overlap_", names(temp_rasters), ".tif")
)

example_stack <- rast(example_out_files)

par(mfrow = c(3, 3), mar = c(2, 2, 3, 4))
for (i in 1:nlyr(example_stack)) {
  plot(example_stack[[i]], main = names(temp_rasters)[i])
}
par(mfrow = c(1, 1))

# -----------------------------
# 7) Optional: compare original vs one scenario for the example species
# -----------------------------
sp_ex <- rast(example_species_file)
ov_ex <- rast(file.path(
  out_root,
  example_species_folder,
  paste0(example_species_name, "_binary_overlap_2050_ssp126.tif")
))

par(mfrow = c(1, 2), mar = c(3, 3, 3, 4))
plot(sp_ex, main = "Original binary habitat")
plot(ov_ex, main = "Overlap with 2050_ssp126 temp")
par(mfrow = c(1, 1))



# rewrite code by merging to full study extent

# -----------------------------
# 1) Species binary rasters
# -----------------------------
base_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated"

species_files <- list.files(
  base_dir,
  pattern = "_mean_pixel_binary\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

# -----------------------------
# 2) Future MAT rasters
# -----------------------------
temp_files <- c(
  "2050_ssp126" = "F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2050_ssp126_filt5_99.tif",
  "2050_ssp245" = "F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2050_ssp245_filt5_99.tif",
  "2050_ssp585" = "F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2050_ssp585_filt5_99.tif",
  "2070_ssp126" = "F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2070_ssp126_filt5_99.tif",
  "2070_ssp245" = "F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2070_ssp245_filt5_99.tif",
  "2070_ssp585" = "F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2070_ssp585_filt5_99.tif",
  "2100_ssp126" = "F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2100_ssp126_filt5_99.tif",
  "2100_ssp245" = "F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2100_ssp245_filt5_99.tif",
  "2100_ssp585" = "F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2100_ssp585_filt5_99.tif"
)

temp_rasters <- lapply(temp_files, rast)

# -----------------------------
# 3) Empty raster template
# -----------------------------
empty_raster <- rast("F:/WWF_data/SDMs_LC_climate/maps_statistics/empty_full_raster/empty_raster.tif")

# -----------------------------
# 4) Output folder
# -----------------------------
out_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated_overlap_with_future_temp_full_extent"
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 5) Function: overlap + merge to empty raster
# -----------------------------
process_species_temp_full_extent <- function(sp_file, temp_rast, scen_name, empty_raster, out_root) {
  
  sp <- rast(sp_file)
  
  # species name from file
  sp_name <- tools::file_path_sans_ext(basename(sp_file))
  sp_name <- sub("_mean_pixel_binary$", "", sp_name)
  
  # species folder
  sp_folder <- basename(dirname(sp_file))
  out_dir <- file.path(out_root, sp_folder)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # project species raster if needed
  if (!same.crs(sp, empty_raster)) {
    sp <- project(sp, empty_raster, method = "near")
  }
  
  # align species raster exactly to empty raster
  if (!compareGeom(sp, empty_raster, stopOnError = FALSE)) {
    sp <- resample(sp, empty_raster, method = "near")
  }
  
  # make sure temp raster matches empty raster too
  if (!same.crs(temp_rast, empty_raster)) {
    temp_rast <- project(temp_rast, empty_raster, method = "bilinear")
  }
  
  if (!compareGeom(temp_rast, empty_raster, stopOnError = FALSE)) {
    temp_rast <- resample(temp_rast, empty_raster, method = "bilinear")
  }
  
  # keep only species cells == 1 where future temp has valid values
  overlap_rast <- ifel(sp == 1 & !is.na(temp_rast), 1, NA)
  
  # merge onto full empty raster
  # start with zeros everywhere, then replace overlap cells with 1
  full_rast <- classify(empty_raster, matrix(c(-Inf, Inf, 0), ncol = 3, byrow = TRUE))
  full_rast <- cover(overlap_rast, full_rast)
  
  names(full_rast) <- paste0(sp_name, "_", scen_name, "_full_extent_binary")
  
  out_file <- file.path(
    out_dir,
    paste0(sp_name, "_binary_overlap_", scen_name, "_full_extent.tif")
  )
  
  writeRaster(full_rast, out_file, overwrite = TRUE)
  
  return(out_file)
}

# -----------------------------
# 6) Run all species x all scenarios
# -----------------------------
all_outputs <- list()

for (sp_file in species_files) {
  cat("\nProcessing species:", basename(sp_file), "\n")
  
  for (scen_name in names(temp_rasters)) {
    cat("   Scenario:", scen_name, "\n")
    
    out_file <- process_species_temp_full_extent(
      sp_file      = sp_file,
      temp_rast    = temp_rasters[[scen_name]],
      scen_name    = scen_name,
      empty_raster = empty_raster,
      out_root     = out_root
    )
    
    all_outputs[[length(all_outputs) + 1]] <- data.frame(
      species_file = sp_file,
      scenario = scen_name,
      output_file = out_file,
      stringsAsFactors = FALSE
    )
  }
  
  gc()
}

all_outputs_df <- do.call(rbind, all_outputs)

write.csv(
  all_outputs_df,
  file.path(out_root, "species_future_temp_overlap_full_extent_files.csv"),
  row.names = FALSE
)

print(all_outputs_df)

# -----------------------------
# 7) Plot one species across all scenarios to verify
# -----------------------------
example_species_file <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/bonobos/Pan paniscus_mean_pixel_binary.tif"
example_species_name <- sub("_mean_pixel_binary\\.tif$", "", basename(example_species_file))
example_species_folder <- basename(dirname(example_species_file))

example_out_files <- file.path(
  out_root,
  example_species_folder,
  paste0(example_species_name, "_binary_overlap_", names(temp_rasters), "_full_extent.tif")
)

example_stack <- rast(example_out_files)

par(mfrow = c(3, 3), mar = c(2, 2, 3, 4))
for (i in 1:nlyr(example_stack)) {
  plot(example_stack[[i]], main = names(example_stack)[i])
}
par(mfrow = c(1, 1))

# -----------------------------
# 8) Compare original local raster vs full-extent result
# -----------------------------
sp_original <- rast(example_species_file)
sp_full <- rast(file.path(
  out_root,
  example_species_folder,
  paste0(example_species_name, "_binary_overlap_2050_ssp126_full_extent.tif")
))

par(mfrow = c(1, 2), mar = c(3, 3, 3, 4))
plot(sp_original, main = "Original species binary")
plot(sp_full, main = "Merged to full empty raster")
par(mfrow = c(1, 1))




#------- clip each species future suitability to corresponding range---------------

library(terra)
library(sf)

# =========================================================
# 1) Input folders
# =========================================================
base_dir_future <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated_overlap_with_future_temp_full_extent"
range_dir       <- "F:/WWF_data/SDMs_LC_climate/species_range"

future_files <- list.files(
  base_dir_future,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

shp_files <- list.files(
  range_dir,
  pattern = "\\.shp$",
  full.names = TRUE,
  recursive = FALSE
)

cat("Future rasters found:", length(future_files), "\n")
cat("Range shapefiles found:", length(shp_files), "\n")

# =========================================================
# 2) Build named list of range shapefiles
# =========================================================
range_paths <- setNames(
  shp_files,
  sub("_range$", "", tools::file_path_sans_ext(basename(shp_files)))
)

print(names(range_paths))

# =========================================================
# 3) Species-to-range lookup
#    Fill all NAs correctly before full run
# =========================================================
species_to_range <- c(
  "Atherurus africanus"         = "africanus",
  "Syncerus caffer"             = "caffer",
  "Lophocebus aterrimus"        = "aterrimus",
  "Tragelaphus eurycerus"       = "eurycerus",
  "Pan paniscus"                = "paniscus",
  "Pan troglodytes"             = "troglodytes",
  "Loxodanta cyclotis"          = "cyclotis",
  "Hylochoerus meinertzhageni"  = "meinertzhageni",
  "Gorilla gorilla"             = "gorilla",
  "Colobus guereza"             = "guereza",
  "Cercopithecus nictitans"     = "nictitans",
  "Cephalophus natalensis"      = "natalensis",
  "Potamochoerus porcus"        = "porcus",
  "Piliocolobus tholloni"       = "tholloni"
)

# quick checks
future_species <- unique(sub("_binary_overlap_.*$", "", basename(future_files)))
cat("\nSpecies in future rasters:\n")
print(future_species)

cat("\nSpecies with NA mapping:\n")
print(names(species_to_range)[is.na(species_to_range)])

# =========================================================
# 4) Output folder
# =========================================================
out_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/future_binary_clipped_to_species_range"
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

# =========================================================
# 5) Function: crop + mask
# =========================================================
crop_mask_future_to_range <- function(future_file, species_to_range, range_paths, out_root) {
  
  r <- rast(future_file)
  species_name <- sub("_binary_overlap_.*$", "", basename(future_file))
  
  if (!(species_name %in% names(species_to_range))) {
    message("Skipping: no lookup entry for ", species_name)
    return(NULL)
  }
  
  range_key <- species_to_range[[species_name]]
  
  if (is.na(range_key)) {
    message("Skipping: range mapping is NA for ", species_name)
    return(NULL)
  }
  
  if (!(range_key %in% names(range_paths))) {
    message("Skipping: shapefile not found for range key ", range_key)
    return(NULL)
  }
  
  # Read range shapefile
  rng_sf <- st_read(range_paths[[range_key]], quiet = TRUE)
  rng_vect <- vect(rng_sf)
  
  # Reproject range if needed
  if (!same.crs(r, rng_vect)) {
    rng_vect <- project(rng_vect, crs(r))
  }
  
  # Crop raster to polygon extent
  r_crop <- crop(r, rng_vect)
  
  # Mask cropped raster to polygon shape
  r_mask <- mask(r_crop, rng_vect)
  
  names(r_mask) <- paste0(species_name, "_range_crop_mask")
  
  # Save in same species folder structure
  sp_folder <- basename(dirname(future_file))
  out_dir <- file.path(out_root, sp_folder)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  out_file <- file.path(
    out_dir,
    sub("_full_extent\\.tif$", "_cropped_masked_to_range.tif", basename(future_file))
  )
  
  writeRaster(r_mask, out_file, overwrite = TRUE)
  
  return(data.frame(
    species = species_name,
    range_key = range_key,
    input_file = future_file,
    output_file = out_file,
    stringsAsFactors = FALSE
  ))
}



results_list <- list()

for (i in seq_along(future_files)) {
  cat("\nProcessing", i, "of", length(future_files), "\n")
  cat(basename(future_files[i]), "\n")
  
  res_i <- crop_mask_future_to_range(
    future_file = future_files[i],
    species_to_range = species_to_range,
    range_paths = range_paths,
    out_root = out_root
  )
  
  if (!is.null(res_i)) {
    results_list[[length(results_list) + 1]] <- res_i
  }
  
  gc()
}

results_df <- do.call(rbind, results_list)
print(results_df)

write.csv(
  results_df,
  file.path(out_root, "future_binary_cropped_masked_to_range_files.csv"),
  row.names = FALSE
)


example_species_folder <- "African_brush_tailed_porcupine"

example_files <- list.files(
  file.path(out_root, example_species_folder),
  pattern = "\\.tif$",
  full.names = TRUE
)

print(example_files)

example_stack <- rast(example_files)

par(mfrow = c(3, 3), mar = c(2, 2, 3, 4))
for (i in 1:nlyr(example_stack)) {
  plot(example_stack[[i]], main = names(example_stack)[i])
}
par(mfrow = c(1, 1))



# Mask outside range suitability

# -----------------------------
base_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/outside_range"

species_files <- list.files(
  path = base_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

species_files
length(species_files)


# =========================================================
# 1) Input folders
# =========================================================
base_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/outside_range"
range_dir <- "F:/WWF_data/SDMs_LC_climate/species_range"

species_files <- list.files(
  path = base_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

shp_files <- list.files(
  path = range_dir,
  pattern = "\\.shp$",
  full.names = TRUE,
  recursive = FALSE
)

cat("Raster files found:", length(species_files), "\n")
cat("Range shapefiles found:", length(shp_files), "\n")

# =========================================================
# 2) Build named shapefile list
# =========================================================
range_paths <- setNames(
  shp_files,
  sub("_range$", "", tools::file_path_sans_ext(basename(shp_files)))
)

print(names(range_paths))

# =========================================================
# 3) Species-to-range lookup
#    Fill any NA values with the correct range key if available
# =========================================================
species_to_range <- c(
  "Atherurus africanus"         = "africanus",
  "Syncerus caffer"             = "caffer",
  "Lophocebus aterrimus"        = "aterrimus",
  "Tragelaphus eurycerus"       = "eurycerus",
  "Pan paniscus"                = "paniscus",
  "Pan troglodytes"             = "troglodytes",
  "Loxodanta cyclotis"          = "cyclotis",
  "Hylochoerus meinertzhageni"  = "meinertzhageni",
  "Gorilla gorilla"             = "gorilla",
  "Colobus guereza"             = "guereza",
  "Cercopithecus nictitans"     = "nictitans",
  "Cephalophus natalensis"      = "natalensis",
  "Potamochoerus porcus"        = "porcus",
  "Piliocolobus tholloni"       = "tholloni"
)

# quick check
raster_species <- unique(sub("_mean_pixel_binary_.*$", "", basename(species_files)))
cat("\nSpecies detected in raster files:\n")
print(sort(raster_species))

cat("\nSpecies with missing range mapping:\n")
print(names(species_to_range)[is.na(species_to_range)])

# =========================================================
# 4) Output folder
# =========================================================
out_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/outside_range_masked"
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

# =========================================================
# 5) Function: keep only pixels outside species range
# =========================================================
mask_outside_range <- function(raster_file, species_to_range, range_paths, out_root) {
  
  r <- rast(raster_file)
  species_name <- sub("_mean_pixel_binary_.*$", "", basename(raster_file))
  
  if (!(species_name %in% names(species_to_range))) {
    message("Skipping: no lookup entry for ", species_name)
    return(NULL)
  }
  
  range_key <- species_to_range[[species_name]]
  
  if (is.na(range_key)) {
    message("Skipping: range mapping is NA for ", species_name)
    return(NULL)
  }
  
  if (!(range_key %in% names(range_paths))) {
    message("Skipping: shapefile not found for key ", range_key, " (", species_name, ")")
    return(NULL)
  }
  
  # Read species range
  rng_sf <- st_read(range_paths[[range_key]], quiet = TRUE)
  rng_vect <- vect(rng_sf)
  
  # Reproject range if needed
  if (!same.crs(r, rng_vect)) {
    rng_vect <- project(rng_vect, crs(r))
  }
  
  # Inverse mask:
  # keep values outside the polygon, set inside polygon to NA
  r_outside <- mask(r, rng_vect, inverse = TRUE)
  
  names(r_outside) <- paste0(species_name, "_outside_range")
  
  # Preserve nested folder structure under out_root
  rel_path <- dirname(sub(paste0("^", gsub("\\\\", "/", base_dir), "/?"), "", gsub("\\\\", "/", raster_file)))
  out_dir <- file.path(out_root, rel_path)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  out_file <- file.path(
    out_dir,
    sub("\\.tif$", "_outside_range_only.tif", basename(raster_file))
  )
  
  writeRaster(r_outside, out_file, overwrite = TRUE)
  
  return(data.frame(
    species = species_name,
    range_key = range_key,
    input_file = raster_file,
    output_file = out_file,
    stringsAsFactors = FALSE
  ))
}

# =========================================================
# 6) Run for all rasters
# =========================================================
results_list <- list()

for (i in seq_along(species_files)) {
  cat("\nProcessing", i, "of", length(species_files), "\n")
  cat(basename(species_files[i]), "\n")
  
  res_i <- mask_outside_range(
    raster_file = species_files[i],
    species_to_range = species_to_range,
    range_paths = range_paths,
    out_root = out_root
  )
  
  if (!is.null(res_i)) {
    results_list[[length(results_list) + 1]] <- res_i
  }
  
  gc()
}

results_df <- do.call(rbind, results_list)
print(results_df)

write.csv(
  results_df,
  file.path(out_root, "outside_range_masked_files.csv"),
  row.names = FALSE
)

#plot one species to verify
example_species_folder <- file.path(out_root, "2050/ssp126/Atherurus_africanus/binary")

example_files <- list.files(
  example_species_folder,
  pattern = "Atherurus africanus.*outside_range_only\\.tif$",
  full.names = TRUE
)

print(example_files)

if (length(example_files) > 0) {
  ex_stack <- rast(example_files)
  
  par(mfrow = c(1, length(example_files)), mar = c(2, 2, 3, 4))
  for (i in 1:nlyr(ex_stack)) {
    plot(ex_stack[[i]], main = names(ex_stack)[i])
  }
  par(mfrow = c(1, 1))
}









#---------------------------------------------------------------------------

#---------------Area and change detection calculations-----------------------

# read all binary raster for area calculation and change detection analysis

current_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/current"

species_files <- list.files(
  path = current_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

species_files
length(species_files)



future_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/future_binary_clipped_to_species_range"

species_files <- list.files(
  path = future_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

species_files
length(species_files)


library(terra)

# ==================perform calculations=====================
# Inputs
# ==========================================
current_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/current"
future_dir  <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/future_binary_clipped_to_species_range"
out_csv     <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/stats_results/species_current_future_area_with_percent_range.csv"

# ==========================================
# List files
# ==========================================
cur_files <- list.files(
  path = current_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

fut_files <- list.files(
  path = future_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(cur_files) == 0) stop("No current rasters found in: ", current_dir)
if (length(fut_files) == 0) stop("No future rasters found in: ", future_dir)

cat("Current rasters found:", length(cur_files), "\n")
cat("Future rasters found:", length(fut_files), "\n")

# ==========================================
# Helpers
# ==========================================

# Current filename example:
# "Atherurus africanus_mean_pixel_binary.tif"
get_species_current <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub("_mean_pixel_binary$", "", nm)
}

# Future filename example:
# "Atherurus africanus_binary_overlap_2050_ssp126_cropped_masked_to_range.tif"
get_species_future <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub("_binary_overlap_\\d{4}_ssp\\d+_cropped_masked_to_range$", "", nm)
}

get_year_future <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  as.integer(sub(".*_binary_overlap_(\\d{4})_ssp\\d+_cropped_masked_to_range$", "\\1", nm))
}

get_scenario_future <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub(".*_binary_overlap_\\d{4}_(ssp\\d+)_cropped_masked_to_range$", "\\1", nm)
}

cell_area_km2 <- function(r) {
  resm <- terra::res(r)
  abs(resm[1] * resm[2]) / 1e6
}

# Range area from all non-NA cells
range_km2_from_raster <- function(r) {
  a_cell <- cell_area_km2(r)
  n_total <- as.numeric(global(!is.na(r), "sum", na.rm = TRUE)[1, 1])
  n_total * a_cell
}

# Suitable area from cells == 1
suitable_km2_from_raster <- function(r) {
  a_cell <- cell_area_km2(r)
  n_suit <- as.numeric(global(r == 1, "sum", na.rm = TRUE)[1, 1])
  n_suit * a_cell
}

# ==========================================
# 1) CURRENT: build species lookup
# ==========================================
current_range_lookup <- list()
current_cell_lookup  <- list()

for (f in cur_files) {
  r <- rast(f)
  sp <- get_species_current(f)
  
  current_range_lookup[[sp]] <- range_km2_from_raster(r)
  current_cell_lookup[[sp]]  <- cell_area_km2(r)
}

current_range_lookup <- unlist(current_range_lookup)
current_cell_lookup  <- unlist(current_cell_lookup)

cat("\nCurrent range lookup:\n")
print(current_range_lookup)

# ==========================================
# 2) Compile CURRENT + FUTURE results
# ==========================================
res <- list()

# --------------------------
# CURRENT rows
# --------------------------
for (f in cur_files) {
  r <- rast(f)
  sp <- get_species_current(f)
  
  suit_km2 <- suitable_km2_from_raster(r)
  range_km2 <- current_range_lookup[[sp]]
  pct_range <- ifelse(range_km2 > 0, (suit_km2 / range_km2) * 100, NA_real_)
  
  res[[length(res) + 1]] <- data.frame(
    species = sp,
    year = 2020,
    scenario = "current",
    cell_km2 = current_cell_lookup[[sp]],
    species_range_km2 = range_km2,
    suitable_km2 = suit_km2,
    pct_of_species_range = pct_range,
    file = f,
    stringsAsFactors = FALSE
  )
}

# --------------------------
# FUTURE rows
# --------------------------
for (f in fut_files) {
  r <- rast(f)
  sp <- get_species_future(f)
  yr <- get_year_future(f)
  ssp <- get_scenario_future(f)
  
  if (!sp %in% names(current_range_lookup)) {
    warning("Species not found in current lookup, skipping: ", sp, " | file: ", f)
    next
  }
  
  suit_km2 <- suitable_km2_from_raster(r)
  range_km2 <- current_range_lookup[[sp]]
  pct_range <- ifelse(range_km2 > 0, (suit_km2 / range_km2) * 100, NA_real_)
  
  res[[length(res) + 1]] <- data.frame(
    species = sp,
    year = yr,
    scenario = ssp,
    cell_km2 = cell_area_km2(r),
    species_range_km2 = range_km2,
    suitable_km2 = suit_km2,
    pct_of_species_range = pct_range,
    file = f,
    stringsAsFactors = FALSE
  )
}

# ==========================================
# 3) Combine and save
# ==========================================
df <- do.call(rbind, res)
df <- df[order(df$species, df$year, df$scenario), ]

write.csv(df, out_csv, row.names = FALSE)

cat("\nSaved CSV to:\n", out_csv, "\n")








library(terra)

# ==========================================
# Inputs
# ==========================================
current_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/current"
future_dir  <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/future_binary_clipped_to_species_range"
out_csv     <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/stats_results/species_current_future_area_with_percent_range.csv"
# ==========================================
# List files
# ==========================================
cur_files <- list.files(
  path = current_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

fut_files <- list.files(
  path = future_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(cur_files) == 0) stop("No current rasters found in: ", current_dir)
if (length(fut_files) == 0) stop("No future rasters found in: ", future_dir)

cat("Current rasters found:", length(cur_files), "\n")
cat("Future rasters found:", length(fut_files), "\n")

# ==========================================
# Helpers
# ==========================================
get_species_current <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub("_mean_pixel_binary$", "", nm)
}

get_species_future <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub("_binary_overlap_\\d{4}_ssp\\d+_cropped_masked_to_range$", "", nm)
}

get_year_future <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  as.integer(sub(".*_binary_overlap_(\\d{4})_ssp\\d+_cropped_masked_to_range$", "\\1", nm))
}

get_scenario_future <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub(".*_binary_overlap_\\d{4}_(ssp\\d+)_cropped_masked_to_range$", "\\1", nm)
}

cell_area_km2 <- function(r) {
  resm <- terra::res(r)
  abs(resm[1] * resm[2]) / 1e6
}

range_km2_from_raster <- function(r) {
  a_cell <- cell_area_km2(r)
  n_total <- as.numeric(global(!is.na(r), "sum", na.rm = TRUE)[1, 1])
  n_total * a_cell
}

suitable_km2_from_raster <- function(r) {
  a_cell <- cell_area_km2(r)
  n_suit <- as.numeric(global(r == 1, "sum", na.rm = TRUE)[1, 1])
  n_suit * a_cell
}

# ==========================================
# 1) CURRENT: build species lookup
# ==========================================
current_range_lookup <- list()
current_cell_lookup  <- list()

for (f in cur_files) {
  r <- rast(f)
  sp <- get_species_current(f)
  
  current_range_lookup[[sp]] <- range_km2_from_raster(r)
  current_cell_lookup[[sp]]  <- cell_area_km2(r)
}

current_range_lookup <- unlist(current_range_lookup)
current_cell_lookup  <- unlist(current_cell_lookup)

# ==========================================
# 2) Compile CURRENT + FUTURE results
# ==========================================
res <- list()

# --------------------------
# CURRENT rows
# --------------------------
for (f in cur_files) {
  r <- rast(f)
  sp <- get_species_current(f)
  
  suit_km2 <- suitable_km2_from_raster(r)
  range_km2 <- current_range_lookup[[sp]]
  pct_range <- ifelse(range_km2 > 0, (suit_km2 / range_km2) * 100, NA_real_)
  
  res[[length(res) + 1]] <- data.frame(
    species = sp,
    year = 2020,
    scenario = "current",
    cell_km2 = current_cell_lookup[[sp]],
    species_range_km2 = range_km2,
    suitable_km2 = suit_km2,
    pct_of_species_range = pct_range,
    stringsAsFactors = FALSE
  )
}

# --------------------------
# FUTURE rows
# --------------------------
for (f in fut_files) {
  r <- rast(f)
  sp <- get_species_future(f)
  yr <- get_year_future(f)
  ssp <- get_scenario_future(f)
  
  if (!sp %in% names(current_range_lookup)) {
    warning("Species not found in current lookup, skipping: ", sp)
    next
  }
  
  suit_km2 <- suitable_km2_from_raster(r)
  range_km2 <- current_range_lookup[[sp]]
  pct_range <- ifelse(range_km2 > 0, (suit_km2 / range_km2) * 100, NA_real_)
  
  res[[length(res) + 1]] <- data.frame(
    species = sp,
    year = yr,
    scenario = ssp,
    cell_km2 = cell_area_km2(r),
    species_range_km2 = range_km2,
    suitable_km2 = suit_km2,
    pct_of_species_range = pct_range,
    stringsAsFactors = FALSE
  )
}

# ==========================================
# 3) Combine and save
# ==========================================
df <- do.call(rbind, res)
df <- df[order(df$species, df$year, df$scenario), ]

print(df)

write.csv(df, out_csv, row.names = FALSE)

cat("\nSaved CSV to:\n", out_csv, "\n")



# Plot results

library(dplyr)
library(ggplot2)

# -----------------------------------
# 1) Prepare data for plotting
# -----------------------------------
df_plot <- df %>%
  mutate(
    scenario_plot = ifelse(year == 2020, "current", scenario),
    year = as.integer(year)
  )

# repeat the 2020 current value for each future scenario
current_rows <- df_plot %>%
  filter(year == 2020) %>%
 dplyr::select(species, year, suitable_km2, pct_of_species_range)

current_expanded <- bind_rows(
  current_rows %>% mutate(scenario_plot = "ssp126"),
  current_rows %>% mutate(scenario_plot = "ssp245"),
  current_rows %>% mutate(scenario_plot = "ssp585")
)

future_rows <- df_plot %>%
  filter(year != 2020) %>%
  dplyr::select(species, year, scenario_plot, suitable_km2, pct_of_species_range)

plot_df <- bind_rows(current_expanded, future_rows) %>%
  arrange(species, scenario_plot, year)

print(plot_df)

# -----------------------------------
# 2) Plot percent of species range
# -----------------------------------
ggplot(plot_df,
       aes(x = year, y = pct_of_species_range,
           color = scenario_plot, group = scenario_plot)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.7) +
  facet_wrap(~ species, scales = "free_y") +
  scale_x_continuous(breaks = c(2020, 2050, 2070, 2100)) +
  scale_color_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    x = "Year",
    y = "Suitable habitat (% of species range)",
    color = "Scenario",
    title = "Current and future suitable habitat within species ranges"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9)
  )





library(ggplot2)
library(dplyr)

df_plot <- df %>%
  mutate(year = factor(year))

ggplot(df_plot %>% filter(year != 2020),
       aes(x = year, y = suitable_km2, fill = scenario)) +
  
  geom_bar(stat = "identity", position = "dodge") +
  
  facet_wrap(~ species, scales = "free_y") +
  
  scale_fill_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  
  labs(
    x = "Year",
    y = "Suitable habitat area (km²)",
    fill = "Scenario",
    title = "Projected suitable habitat area under climate scenarios"
  ) +
  
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9)
  )



# Now, do change detection. Loss, gain, stability

# ==========================================
# Inputs
# ==========================================
current_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/current"
future_dir  <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/future_binary_clipped_to_species_range"

out_change_dir  <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/stats_results/change_detection/change_rasters"
out_refugia_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/stats_results/change_detection/refugia_rasters"
out_loss_dir    <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/stats_results/change_detection/loss_rasters"
out_gain_dir    <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/stats_results/change_detection/gain_rasters"
out_csv         <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/stats_results/change_detection/change_detection_stats.csv"

dir.create(out_change_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(out_refugia_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_loss_dir,    recursive = TRUE, showWarnings = FALSE)
dir.create(out_gain_dir,    recursive = TRUE, showWarnings = FALSE)

# ==========================================
# List files
# ==========================================
cur_files <- list.files(
  path = current_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

fut_files <- list.files(
  path = future_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(cur_files) == 0) stop("No current rasters found in: ", current_dir)
if (length(fut_files) == 0) stop("No future rasters found in: ", future_dir)

cat("Current rasters found:", length(cur_files), "\n")
cat("Future rasters found:", length(fut_files), "\n")

# ==========================================
# Helpers
# ==========================================
get_species_current <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub("_mean_pixel_binary$", "", nm)
}

get_species_future <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub("_binary_overlap_\\d{4}_ssp\\d+_cropped_masked_to_range$", "", nm)
}

get_year_future <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  as.integer(sub(".*_binary_overlap_(\\d{4})_ssp\\d+_cropped_masked_to_range$", "\\1", nm))
}

get_scenario_future <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub(".*_binary_overlap_\\d{4}_(ssp\\d+)_cropped_masked_to_range$", "\\1", nm)
}

cell_area_km2 <- function(r) {
  rr <- res(r)
  abs(rr[1] * rr[2]) / 1e6
}

range_km2_from_raster <- function(r) {
  a <- cell_area_km2(r)
  n <- as.numeric(global(!is.na(r), "sum", na.rm = TRUE)[1, 1])
  n * a
}

count_value_cells <- function(r, val) {
  as.numeric(global(r == val, "sum", na.rm = TRUE)[1, 1])
}

# ==========================================
# Current raster lookup
# ==========================================
cur_species <- vapply(cur_files, get_species_current, character(1))
cur_map <- setNames(cur_files, cur_species)

cur_range_lookup <- sapply(names(cur_map), function(sp) {
  r <- rast(cur_map[[sp]])
  range_km2_from_raster(r)
}, simplify = TRUE)

# ==========================================
# Future file table
# ==========================================
future_tbl <- data.frame(
  file = fut_files,
  species = vapply(fut_files, get_species_future, character(1)),
  future_year = vapply(fut_files, get_year_future, integer(1)),
  scenario = vapply(fut_files, get_scenario_future, character(1)),
  stringsAsFactors = FALSE
)

future_tbl <- future_tbl[order(future_tbl$species, future_tbl$future_year, future_tbl$scenario), ]
print(future_tbl)

# ==========================================
# Change coding
#  1 = stable (1->1)
#  2 = gain   (0->1)
# -1 = loss   (1->0)
#  0 = unchanged (0->0)
# ==========================================
results <- list()

# choose one species to plot for verification
species_to_plot <- "Atherurus africanus"
plotted_once <- FALSE

for (sp in names(cur_map)) {
  
  cat("\nProcessing species:", sp, "\n")
  
  r_cur <- rast(cur_map[[sp]])
  
  fut_sub <- future_tbl[future_tbl$species == sp, ]
  
  if (nrow(fut_sub) == 0) {
    warning("No future rasters found for species: ", sp)
    next
  }
  
  for (i in seq_len(nrow(fut_sub))) {
    
    yr  <- fut_sub$future_year[i]
    ssp <- fut_sub$scenario[i]
    f_fut <- fut_sub$file[i]
    
    r_fut <- rast(f_fut)
    
    # align future raster to current raster if needed
    if (!isTRUE(compareGeom(r_cur, r_fut, stopOnError = FALSE))) {
      r_fut <- resample(r_fut, r_cur, method = "near")
    }
    
    # only evaluate where both rasters are not NA
    ok <- !is.na(r_cur) & !is.na(r_fut)
    
    change <- rast(r_cur)
    values(change) <- NA
    
    change[ ok & r_cur == 1 & r_fut == 1 ] <-  1
    change[ ok & r_cur == 0 & r_fut == 1 ] <-  2
    change[ ok & r_cur == 1 & r_fut == 0 ] <- -1
    change[ ok & r_cur == 0 & r_fut == 0 ] <-  0
    
    names(change) <- paste0(sp, "_change_2020_to_", yr, "_", ssp)
    
    # derived rasters
    refugia <- ifel(change == 1, 1, NA)
    gain    <- ifel(change == 2, 1, NA)
    loss    <- ifel(change == -1, 1, NA)
    
    names(refugia) <- paste0(sp, "_refugia_2020_to_", yr, "_", ssp)
    names(gain)    <- paste0(sp, "_gain_2020_to_", yr, "_", ssp)
    names(loss)    <- paste0(sp, "_loss_2020_to_", yr, "_", ssp)
    
    # save rasters
    safe_sp <- gsub(" ", "_", sp)
    
    out_change <- file.path(out_change_dir,  paste0(safe_sp, "_change_2020_to_", yr, "_", ssp, ".tif"))
    out_ref    <- file.path(out_refugia_dir, paste0(safe_sp, "_refugia_2020_to_", yr, "_", ssp, ".tif"))
    out_gain   <- file.path(out_gain_dir,    paste0(safe_sp, "_gain_2020_to_", yr, "_", ssp, ".tif"))
    out_loss   <- file.path(out_loss_dir,    paste0(safe_sp, "_loss_2020_to_", yr, "_", ssp, ".tif"))
    
    writeRaster(change,  out_change, overwrite = TRUE)
    writeRaster(refugia, out_ref,    overwrite = TRUE)
    writeRaster(gain,    out_gain,   overwrite = TRUE)
    writeRaster(loss,    out_loss,   overwrite = TRUE)
    
    # verification plot once
    if (!plotted_once && sp == species_to_plot) {
      plot(change,
           main = paste0(sp, " change 2020 -> ", yr, " ", ssp,
                         "\n1=stable, 2=gain, -1=loss, 0=unchanged"))
      plotted_once <- TRUE
    }
    
    # stats
    a_cell <- cell_area_km2(r_cur)
    
    stable_n    <- count_value_cells(change,  1)
    gain_n      <- count_value_cells(change,  2)
    loss_n      <- count_value_cells(change, -1)
    unchanged_n <- count_value_cells(change,  0)
    
    stable_km2    <- stable_n * a_cell
    gain_km2      <- gain_n * a_cell
    loss_km2      <- loss_n * a_cell
    unchanged_km2 <- unchanged_n * a_cell
    
    range_km2_2020 <- cur_range_lookup[[sp]]
    
    stable_pct    <- ifelse(range_km2_2020 > 0, stable_km2    / range_km2_2020 * 100, NA_real_)
    gain_pct      <- ifelse(range_km2_2020 > 0, gain_km2      / range_km2_2020 * 100, NA_real_)
    loss_pct      <- ifelse(range_km2_2020 > 0, loss_km2      / range_km2_2020 * 100, NA_real_)
    unchanged_pct <- ifelse(range_km2_2020 > 0, unchanged_km2 / range_km2_2020 * 100, NA_real_)
    
    results[[length(results) + 1]] <- data.frame(
      species = sp,
      baseline_year = 2020,
      future_year = yr,
      scenario = ssp,
      range_km2_2020 = range_km2_2020,
      stable_km2 = stable_km2,
      stable_pct_of_2020_range = stable_pct,
      gain_km2 = gain_km2,
      gain_pct_of_2020_range = gain_pct,
      loss_km2 = loss_km2,
      loss_pct_of_2020_range = loss_pct,
      unchanged_km2 = unchanged_km2,
      unchanged_pct_of_2020_range = unchanged_pct,
      stringsAsFactors = FALSE
    )
    
    gc()
  }
}

# ==========================================
# Save results
# ==========================================
df_change_stats <- do.call(rbind, results)
df_change_stats <- df_change_stats[order(df_change_stats$species,
                                         df_change_stats$future_year,
                                         df_change_stats$scenario), ]

print(df_change_stats)

write.csv(df_change_stats, out_csv, row.names = FALSE)

cat("\nSaved CSV:\n", out_csv, "\n")
cat("\nSaved change rasters in:\n", out_change_dir, "\n")
cat("Saved refugia rasters in:\n", out_refugia_dir, "\n")
cat("Saved gain rasters in:\n", out_gain_dir, "\n")
cat("Saved loss rasters in:\n", out_loss_dir, "\n")




# plot refugia

df_refugia <- df_change_stats %>%
  mutate(
    future_year = as.integer(future_year),
    scenario = factor(scenario, levels = c("ssp126", "ssp245", "ssp585"))
  ) %>%
  arrange(species, scenario, future_year)

p_refugia <- ggplot(
  df_refugia,
  aes(x = future_year, y = stable_pct_of_2020_range,
      color = scenario, group = scenario)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ species, scales = "free_y") +
  scale_x_continuous(breaks = c(2050, 2070, 2100)) +
  scale_color_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    x = "Year",
    y = " % In situ Refugia",
    color = "Scenario",
    title = "Percentage In situ Climate refugia trajectories for Congo Basin Mammals"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9)
  )

print(p_refugia)

#ggsave(
# filename = "F:/WWF_data/SDMs_LC_climate/maps_statistics/change_detection/refugia_line_plot.png",
#plot = p_refugia,
#width = 14,
#height = 10,
#dpi = 300
#)


# vulnerability plot column
df_vuln_2100 <- df_change_stats %>%
  filter(future_year == 2100) %>%
  mutate(
    scenario = factor(scenario, levels = c("ssp126", "ssp245", "ssp585"))
  )

# rank by worst-case vulnerability under SSP585
rank_order <- df_vuln_2100 %>%
  filter(scenario == "ssp585") %>%
  arrange(desc(loss_pct_of_2020_range)) %>%
  pull(species)

df_vuln_2100 <- df_vuln_2100 %>%
  mutate(species = factor(species, levels = rank_order))

p_vuln <- ggplot(
  df_vuln_2100,
  aes(x = species, y = loss_pct_of_2020_range, fill = scenario)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  coord_flip() +
  scale_fill_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    x = NULL,
    y = "Habitat loss (% of 2020 range)",
    fill = "Scenario",
    title = "Species vulnerability ranking by 2100"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 9)
  )

print(p_vuln)

ggsave(
  filename = "F:/WWF_data/SDMs_LC_climate/maps_statistics/change_detection/vulnerability_ranking_2100.png",
  plot = p_vuln,
  width = 11,
  height = 8.5,
  dpi = 300
)


# vulnerability plot facet

df_vuln_2100_facet <- df_change_stats %>%
  filter(future_year == 2100) %>%
  mutate(
    scenario = factor(scenario, levels = c("ssp126", "ssp245", "ssp585"))
  ) %>%
  group_by(scenario) %>%
  arrange(desc(loss_pct_of_2020_range), .by_group = TRUE) %>%
  ungroup()

p_vuln_facet <- ggplot(
  df_vuln_2100_facet,
  aes(x = reorder(species, loss_pct_of_2020_range), y = loss_pct_of_2020_range, fill = scenario)
) +
  geom_col(width = 0.75) +
  coord_flip() +
  facet_wrap(~ scenario, scales = "free_y") +
  scale_fill_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    x = NULL,
    y = "% Climate Risk Area",
    fill = "Scenario",
    title = "Species vulnerability ranking by 2100"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10),
    axis.text.y = element_text(size = 8)
  )

print(p_vuln_facet)

#ggsave(
# filename = "F:/WWF_data/SDMs_LC_climate/maps_statistics/change_detection/vulnerability_ranking_2100_by_scenario.png",
#plot = p_vuln_facet,
#width = 13,
#height = 9,
#dpi = 300
#)



df_vuln_2070_facet <- df_change_stats %>%
  filter(future_year == 2070) %>%
  mutate(
    scenario = factor(scenario, levels = c("ssp126", "ssp245", "ssp585"))
  ) %>%
  group_by(scenario) %>%
  arrange(desc(loss_pct_of_2020_range), .by_group = TRUE) %>%
  ungroup()

p_vuln_facet <- ggplot(
  df_vuln_2100_facet,
  aes(x = reorder(species, loss_pct_of_2020_range), y = loss_pct_of_2020_range, fill = scenario)
) +
  geom_col(width = 0.75) +
  coord_flip() +
  facet_wrap(~ scenario, scales = "free_y") +
  scale_fill_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    x = NULL,
    y = "% Climate Risk Area",
    fill = "Scenario",
    title = "Species vulnerability ranking by 2070"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10),
    axis.text.y = element_text(size = 8)
  )

print(p_vuln_facet)




# calculate species richness

# Inputs
# ==========================================
current_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/current"
future_dir  <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/future_binary_clipped_to_species_range"
out_rich_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/stats_results/richness_outputs"

dir.create(out_rich_dir, recursive = TRUE, showWarnings = FALSE)

out_csv <- file.path(out_rich_dir, "species_richness_value_area_summary_pct_vs_currentRange.csv")

# ==========================================
# Helpers
# ==========================================
cell_area_km2 <- function(r) {
  rr <- terra::res(r)
  abs(rr[1] * rr[2]) / 1e6
}

make_template <- function(r_list) {
  tpl <- r_list[[1]]
  if (length(r_list) > 1) {
    for (i in 2:length(r_list)) {
      tpl <- extend(tpl, ext(r_list[[i]]))
    }
  }
  tpl
}

to_template <- function(r, tpl) {
  if (!isTRUE(compareGeom(r, tpl, stopOnError = FALSE))) {
    r <- resample(r, tpl, method = "near")
  }
  r
}

# Current richness range area = cells non-NA in at least one species raster
union_range_area_km2 <- function(r_list_aligned, tpl) {
  a_cell <- cell_area_km2(tpl)
  rng_sum <- Reduce(`+`, lapply(r_list_aligned, function(r) !is.na(r)))
  rng <- rng_sum > 0
  n_rng <- as.numeric(global(rng, "sum", na.rm = TRUE)[1, 1])
  n_rng * a_cell
}

# Filename helpers
get_species_current <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub("_mean_pixel_binary$", "", nm)
}

get_species_future <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub("_binary_overlap_\\d{4}_ssp\\d+_cropped_masked_to_range$", "", nm)
}

get_year_future <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  as.integer(sub(".*_binary_overlap_(\\d{4})_ssp\\d+_cropped_masked_to_range$", "\\1", nm))
}

get_scenario_future <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub(".*_binary_overlap_\\d{4}_(ssp\\d+)_cropped_masked_to_range$", "\\1", nm)
}

# Area table by richness value
area_by_value_vs_currentRange <- function(rich_pos, a_cell, current_rich_range_km2) {
  vals_all <- values(rich_pos, mat = FALSE)
  vals_all <- vals_all[!is.na(vals_all)]
  
  if (length(vals_all) == 0) {
    return(data.frame(
      richness_value = integer(0),
      n_cells = integer(0),
      area_km2 = numeric(0),
      pct_of_CURRENT_richness_range = numeric(0)
    ))
  }
  
  max_r <- max(vals_all, na.rm = TRUE)
  vals <- 1:max_r
  
  out <- lapply(vals, function(v) {
    n_v <- as.numeric(global(rich_pos == v, "sum", na.rm = TRUE)[1, 1])
    area_km2 <- n_v * a_cell
    pct <- ifelse(current_rich_range_km2 > 0, area_km2 / current_rich_range_km2 * 100, NA_real_)
    
    data.frame(
      richness_value = v,
      n_cells = n_v,
      area_km2 = area_km2,
      pct_of_CURRENT_richness_range = pct
    )
  })
  
  do.call(rbind, out)
}

# Compute richness raster + area table
calc_richness_vs_currentRange <- function(files, label, current_rich_range_km2,
                                          plot_it = FALSE, out_dir = out_rich_dir) {
  if (length(files) == 0) stop("No rasters for: ", label)
  
  r_list <- lapply(files, rast)
  tpl <- make_template(r_list)
  r_list <- lapply(r_list, to_template, tpl = tpl)
  
  # Replace NA with 0, then sum binary rasters
  r01 <- lapply(r_list, function(r) ifel(is.na(r), 0, r))
  rich <- Reduce(`+`, r01)
  names(rich) <- paste0("richness_", label)
  
  # Convert richness 0 to NA
  rich_pos <- ifel(rich == 0, NA, rich)
  names(rich_pos) <- paste0("richness_pos_", label)
  
  if (plot_it) {
    plot(rich_pos, main = paste0("Richness (>0 only): ", label))
  }
  
  out_rich <- file.path(out_dir, paste0("richness_pos_", label, ".tif"))
  writeRaster(rich_pos, out_rich, overwrite = TRUE)
  
  a_cell <- cell_area_km2(tpl)
  tab <- area_by_value_vs_currentRange(rich_pos, a_cell, current_rich_range_km2)
  
  tab$label <- label
  tab$n_species <- length(files)
  tab$cell_km2 <- a_cell
  tab$current_richness_range_area_km2 <- current_rich_range_km2
  
  tab[, c("label", "n_species", "cell_km2", "current_richness_range_area_km2",
          "richness_value", "n_cells", "area_km2", "pct_of_CURRENT_richness_range")]
}

# ==========================================
# 1) CURRENT richness
# ==========================================
cur_files <- list.files(
  path = current_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(cur_files) == 0) stop("No current rasters found in: ", current_dir)

cat("Current rasters found:", length(cur_files), "\n")

cur_list <- lapply(cur_files, rast)
cur_tpl  <- make_template(cur_list)
cur_list <- lapply(cur_list, to_template, tpl = cur_tpl)

current_rich_range_km2 <- union_range_area_km2(cur_list, cur_tpl)
cat("CURRENT richness range area (km2): ", current_rich_range_km2, "\n")

df_all <- list()

df_all[[1]] <- calc_richness_vs_currentRange(
  files = cur_files,
  label = "2020_current",
  current_rich_range_km2 = current_rich_range_km2,
  plot_it = TRUE
)

# ==========================================
# 2) FUTURE richness by year and scenario
# ==========================================
fut_files <- list.files(
  path = future_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(fut_files) == 0) stop("No future rasters found in: ", future_dir)

cat("Future rasters found:", length(fut_files), "\n")

future_tbl <- data.frame(
  file = fut_files,
  species = vapply(fut_files, get_species_future, character(1)),
  year = vapply(fut_files, get_year_future, integer(1)),
  scenario = vapply(fut_files, get_scenario_future, character(1)),
  stringsAsFactors = FALSE
)

future_tbl <- future_tbl[order(future_tbl$year, future_tbl$scenario, future_tbl$species), ]
print(future_tbl)

years <- sort(unique(future_tbl$year))
ssps  <- sort(unique(future_tbl$scenario))

k <- 2

for (yr in years) {
  for (ssp in ssps) {
    
    files_i <- future_tbl$file[future_tbl$year == yr & future_tbl$scenario == ssp]
    
    if (length(files_i) == 0) {
      warning("No future rasters found for: ", yr, " ", ssp)
      next
    }
    
    label_i <- paste0(yr, "_", ssp)
    
    df_all[[k]] <- calc_richness_vs_currentRange(
      files = files_i,
      label = label_i,
      current_rich_range_km2 = current_rich_range_km2,
      plot_it = FALSE
    )
    
    k <- k + 1
  }
}

# ==========================================
# 3) SAVE CSV
# ==========================================
df_rich_val_area <- do.call(rbind, df_all)
df_rich_val_area <- df_rich_val_area[order(df_rich_val_area$label, df_rich_val_area$richness_value), ]

print(df_rich_val_area)

write.csv(df_rich_val_area, out_csv, row.names = FALSE)

cat("\nSaved CSV:\n", out_csv, "\n")
cat("Saved richness rasters in:\n", out_rich_dir, "\n")


#------------plot richness--------------

library(ggplot2)
library(dplyr)

# -----------------------------------
# 1) Prepare richness data
# -----------------------------------
d <- df_rich_val_area %>%
  mutate(
    area_km2 = n_cells * cell_km2,
    pct_rich = (area_km2 / current_richness_range_area_km2) * 100,
    year = ifelse(label == "2020_current",
                  2020,
                  as.integer(sub("_.*$", "", label))),
    scenario = ifelse(label == "2020_current",
                      "current",
                      sub("^\\d{4}_", "", label))
  )

# -----------------------------------
# 2) Duplicate 2020 rows for each scenario
# -----------------------------------
d_current <- d %>%
  filter(year == 2020)

d_current_expanded <- bind_rows(
  d_current %>% mutate(scenario = "ssp126"),
  d_current %>% mutate(scenario = "ssp245"),
  d_current %>% mutate(scenario = "ssp585")
)

d_future <- d %>%
  filter(year != 2020)

d_plot <- bind_rows(d_current_expanded, d_future) %>%
  mutate(
    scenario = factor(scenario, levels = c("ssp126", "ssp245", "ssp585")),
    richness_value = factor(richness_value, levels = sort(unique(richness_value)))
  ) %>%
  arrange(richness_value, scenario, year)

# -----------------------------------
# 3) Plot richness trajectories
# -----------------------------------
p_rich <- ggplot(
  d_plot,
  aes(x = year, y = pct_rich, color = scenario, group = scenario)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ richness_value, ncol = 3, scales = "free_y") +
  scale_x_continuous(breaks = c(2020, 2050, 2070, 2100)) +
  scale_color_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    title = "Variation in Percentage In situ Species Richness Area Across Time",
    x = "Year",
    y = "% In situ Richness",
    color = "Scenario"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9)
  )

print(p_rich)

# -----------------------------------
# 4) Save figure
# -----------------------------------
# ggsave(
# filename = "F:/WWF_data/SDMs_LC_climate/maps_statistics/richness_outputs/pct_richness_facets_by_value_connected.png",
# plot = p_rich,
# width = 12,
# height = 8,
# dpi = 300
# )




# -------estimate and plot top 10% richness sites-------------

# ==========================================
# Inputs
# ==========================================
rich_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/stats_results/richness_outputs"
out_top10_dir <- file.path(rich_dir, "top10_richness_sites")
dir.create(out_top10_dir, recursive = TRUE, showWarnings = FALSE)

out_csv_top10 <- file.path(out_top10_dir, "top10_richness_sites_summary.csv")

# If already in memory, this value is fine:
# current_rich_range_km2 <- 465555

# Or recover it safely from the richness summary table:
current_rich_range_km2 <- unique(df_rich_val_area$current_richness_range_area_km2)[1]

# ==========================================
# List richness rasters
# ==========================================
rich_files <- list.files(
  path = rich_dir,
  pattern = "^richness_pos_.*\\.tif$",
  full.names = TRUE,
  recursive = FALSE
)

if (length(rich_files) == 0) stop("No richness rasters found in: ", rich_dir)

print(rich_files)

# ==========================================
# Helpers
# ==========================================
cell_area_km2 <- function(r) {
  rr <- terra::res(r)
  abs(rr[1] * rr[2]) / 1e6
}

get_label_from_richfile <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub("^richness_pos_", "", nm)
}

get_year_from_label <- function(label) {
  if (label == "2020_current") return(2020L)
  as.integer(sub("_.*$", "", label))
}

get_scenario_from_label <- function(label) {
  if (label == "2020_current") return("current")
  sub("^\\d{4}_", "", label)
}

# ==========================================
# Extract top 10% richness hotspots
# ==========================================
res_top10 <- list()

for (i in seq_along(rich_files)) {
  
  f <- rich_files[i]
  r <- rast(f)
  label <- get_label_from_richfile(f)
  
  cat("\nProcessing:", label, "\n")
  
  vals <- values(r, mat = FALSE)
  vals <- vals[!is.na(vals)]
  
  if (length(vals) == 0) {
    warning("No non-NA richness values in: ", f)
    next
  }
  
  # threshold for top 10% of richness values
  thr <- as.numeric(quantile(vals, probs = 0.90, na.rm = TRUE, type = 7))
  
  # keep cells >= threshold
  top10 <- ifel(r >= thr, 1, NA)
  names(top10) <- paste0("top10_richness_", label)
  
  out_rast <- file.path(out_top10_dir, paste0("top10_richness_", label, ".tif"))
  writeRaster(top10, out_rast, overwrite = TRUE)
  
  a_cell <- cell_area_km2(r)
  n_top10 <- as.numeric(global(top10 == 1, "sum", na.rm = TRUE)[1, 1])
  area_top10_km2 <- n_top10 * a_cell
  pct_current_rich_range <- ifelse(
    current_rich_range_km2 > 0,
    area_top10_km2 / current_rich_range_km2 * 100,
    NA_real_
  )
  
  res_top10[[length(res_top10) + 1]] <- data.frame(
    label = label,
    year = get_year_from_label(label),
    scenario = get_scenario_from_label(label),
    threshold_top10 = thr,
    hotspot_cells = n_top10,
    hotspot_area_km2 = area_top10_km2,
    pct_of_current_richness_range = pct_current_rich_range,
    stringsAsFactors = FALSE
  )
}

df_top10 <- do.call(rbind, res_top10)
df_top10 <- df_top10[order(df_top10$year, df_top10$scenario), ]

print(df_top10)

write.csv(df_top10, out_csv_top10, row.names = FALSE)

cat("\nSaved CSV:\n", out_csv_top10, "\n")
cat("Saved top10 rasters in:\n", out_top10_dir, "\n")


# plot top10% richness
# ==========================================
# Prepare plotting data
# ==========================================
df_top10_current <- df_top10 %>%
  filter(year == 2020)

df_top10_current_expanded <- bind_rows(
  df_top10_current %>% mutate(scenario = "ssp126"),
  df_top10_current %>% mutate(scenario = "ssp245"),
  df_top10_current %>% mutate(scenario = "ssp585")
)

df_top10_future <- df_top10 %>%
  filter(year != 2020)

df_top10_plot <- bind_rows(df_top10_current_expanded, df_top10_future) %>%
  mutate(
    scenario = factor(scenario, levels = c("ssp126", "ssp245", "ssp585"))
  ) %>%
  arrange(scenario, year)

print(df_top10_plot)

# ==========================================
# Plot hotspot area in km2
# ==========================================
p_top10_km2 <- ggplot(
  df_top10_plot,
  aes(x = year, y = hotspot_area_km2, color = scenario, group = scenario)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = c(2020, 2050, 2070, 2100)) +
  scale_color_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    x = "Year",
    y = "Top 10% richness hotspot area (km²)",
    color = "Scenario",
    title = "Top 10% Species Richness Area"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

print(p_top10_km2)

ggsave(
  filename = file.path(out_top10_dir, "top10_richness_hotspot_area_lineplot.png"),
  plot = p_top10_km2,
  width = 8,
  height = 5.5,
  dpi = 300
)



# calculate exsitu refugia area
library(tidyr)

# ==========================================
# Inputs
# ==========================================
future_outside_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/outside_range_masked"

out_exsitu_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/exsitu_refugia_outputs"
dir.create(out_exsitu_dir, recursive = TRUE, showWarnings = FALSE)

out_csv <- file.path(out_exsitu_dir, "exsitu_refugia_area_summary.csv")

# ==========================================
# List files
# ==========================================
fut_outside_files <- list.files(
  path = future_outside_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(fut_outside_files) == 0) {
  stop("No future rasters found in: ", future_outside_dir)
}

cat("Future rasters found:", length(fut_outside_files), "\n")

# ==========================================
# Helpers
# ==========================================
get_species_exsitu <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub("_mean_pixel_binary_\\d{4}s_ssp\\d+_outside_range_only$", "", nm)
}

get_year_exsitu <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  as.integer(sub(".*_mean_pixel_binary_(\\d{4})s_ssp\\d+_outside_range_only$", "\\1", nm))
}

get_scenario_exsitu <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub(".*_mean_pixel_binary_\\d{4}s_(ssp\\d+)_outside_range_only$", "\\1", nm)
}

cell_area_km2 <- function(r) {
  rr <- terra::res(r)
  abs(rr[1] * rr[2]) / 1e6
}

count_non_na_cells <- function(r) {
  as.numeric(global(!is.na(r), "sum", na.rm = TRUE)[1, 1])
}

count_value1_cells <- function(r) {
  as.numeric(global(r == 1, "sum", na.rm = TRUE)[1, 1])
}

# ==========================================
# Calculate ex situ refugia stats
# ==========================================
res <- list()

for (f in fut_outside_files) {
  r <- rast(f)
  
  sp  <- get_species_exsitu(f)
  yr  <- get_year_exsitu(f)
  ssp <- get_scenario_exsitu(f)
  
  a_cell <- cell_area_km2(r)
  
  n_outside <- count_non_na_cells(r)   # all valid outside-range cells
  n_exsitu  <- count_value1_cells(r)   # only ex situ refugia cells
  
  outside_area_km2 <- n_outside * a_cell
  exsitu_area_km2  <- n_exsitu  * a_cell
  
  exsitu_pct <- ifelse(
    outside_area_km2 > 0,
    exsitu_area_km2 / outside_area_km2 * 100,
    NA_real_
  )
  
  res[[length(res) + 1]] <- data.frame(
    species = sp,
    year = yr,
    scenario = ssp,
    cell_km2 = a_cell,
    outside_range_area_km2 = outside_area_km2,
    exsitu_refugia_km2 = exsitu_area_km2,
    exsitu_refugia_pct = exsitu_pct,
    stringsAsFactors = FALSE
  )
}

df_exsitu <- do.call(rbind, res)
df_exsitu <- df_exsitu[order(df_exsitu$species, df_exsitu$year, df_exsitu$scenario), ]

print(df_exsitu)

write.csv(df_exsitu, out_csv, row.names = FALSE)

cat("\nSaved CSV to:\n", out_csv, "\n")


# plot line graphs
df_exsitu$scenario <- factor(df_exsitu$scenario, levels = c("ssp126", "ssp245", "ssp585"))

p_exsitu_pct <- ggplot(
  df_exsitu,
  aes(x = year, y = exsitu_refugia_km2, color = scenario, group = scenario)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ species, scales = "free_y") +
  scale_x_continuous(breaks = c(2050, 2070, 2100)) +
  scale_color_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    x = "Year",
    y = "Ex situ refugia Area (km2)",
    color = "Scenario",
    title = "Ex situ refugia trajectories for Congo Basin Mammals"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9)
  )

print(p_exsitu_pct)

ggsave(
  filename = file.path(out_exsitu_dir, "exsitu_refugia_pct_lineplot.png"),
  plot = p_exsitu_pct,
  width = 14,
  height = 10,
  dpi = 300
)




# Calculate exsitu refugia richness

# Inputs
# ==========================================
future_outside_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/outside_range_masked"
out_exsitu_rich_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/exsitu_refugia_richness"

dir.create(out_exsitu_rich_dir, recursive = TRUE, showWarnings = FALSE)

out_csv_summary <- file.path(out_exsitu_rich_dir, "exsitu_refugia_richness_summary.csv")
out_csv_by_value <- file.path(out_exsitu_rich_dir, "exsitu_refugia_richness_value_area_summary.csv")

# ==========================================
# List files
# ==========================================
fut_outside_files <- list.files(
  path = future_outside_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(fut_outside_files) == 0) {
  stop("No future rasters found in: ", future_outside_dir)
}

cat("Future outside-range rasters found:", length(fut_outside_files), "\n")

# ==========================================
# Helpers
# ==========================================
get_species_exsitu <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub("_mean_pixel_binary_\\d{4}s_ssp\\d+_outside_range_only$", "", nm)
}

get_year_exsitu <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  as.integer(sub(".*_mean_pixel_binary_(\\d{4})s_ssp\\d+_outside_range_only$", "\\1", nm))
}

get_scenario_exsitu <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub(".*_mean_pixel_binary_\\d{4}s_(ssp\\d+)_outside_range_only$", "\\1", nm)
}

cell_area_km2 <- function(r) {
  rr <- terra::res(r)
  abs(rr[1] * rr[2]) / 1e6
}

make_template <- function(r_list) {
  tpl <- r_list[[1]]
  if (length(r_list) > 1) {
    for (i in 2:length(r_list)) {
      tpl <- extend(tpl, ext(r_list[[i]]))
    }
  }
  tpl
}

to_template <- function(r, tpl) {
  if (!isTRUE(compareGeom(r, tpl, stopOnError = FALSE))) {
    r <- resample(r, tpl, method = "near")
  }
  r
}

area_by_value <- function(rich_pos, a_cell) {
  vals_all <- values(rich_pos, mat = FALSE)
  vals_all <- vals_all[!is.na(vals_all)]
  
  if (length(vals_all) == 0) {
    return(data.frame(
      richness_value = integer(0),
      n_cells = integer(0),
      area_km2 = numeric(0)
    ))
  }
  
  max_r <- max(vals_all, na.rm = TRUE)
  vals <- 1:max_r
  
  out <- lapply(vals, function(v) {
    n_v <- as.numeric(global(rich_pos == v, "sum", na.rm = TRUE)[1, 1])
    area_km2 <- n_v * a_cell
    data.frame(
      richness_value = v,
      n_cells = n_v,
      area_km2 = area_km2
    )
  })
  
  do.call(rbind, out)
}

# ==========================================
# Build future table
# ==========================================
future_tbl <- data.frame(
  file = fut_outside_files,
  species = vapply(fut_outside_files, get_species_exsitu, character(1)),
  year = vapply(fut_outside_files, get_year_exsitu, integer(1)),
  scenario = vapply(fut_outside_files, get_scenario_exsitu, character(1)),
  stringsAsFactors = FALSE
)

future_tbl <- future_tbl[order(future_tbl$year, future_tbl$scenario, future_tbl$species), ]

print(head(future_tbl))

years <- sort(unique(future_tbl$year))
scenarios <- sort(unique(future_tbl$scenario))

# ==========================================
# Calculate ex situ richness
# ==========================================
summary_list <- list()
value_list <- list()

k1 <- 1
k2 <- 1

for (yr in years) {
  for (ssp in scenarios) {
    
    files_i <- future_tbl$file[future_tbl$year == yr & future_tbl$scenario == ssp]
    
    if (length(files_i) == 0) {
      warning("No files found for ", yr, " ", ssp)
      next
    }
    
    cat("\nProcessing ex situ richness for:", yr, ssp, "\n")
    
    r_list <- lapply(files_i, rast)
    tpl <- make_template(r_list)
    r_list <- lapply(r_list, to_template, tpl = tpl)
    
    # convert NA to 0, then sum species layers
    r01 <- lapply(r_list, function(r) ifel(is.na(r), 0, r))
    rich <- Reduce(`+`, r01)
    names(rich) <- paste0("exsitu_richness_", yr, "_", ssp)
    
    # convert 0 back to NA
    rich_pos <- ifel(rich == 0, NA, rich)
    names(rich_pos) <- paste0("exsitu_richness_pos_", yr, "_", ssp)
    
    # save raster
    out_rast <- file.path(out_exsitu_rich_dir, paste0("exsitu_richness_pos_", yr, "_", ssp, ".tif"))
    writeRaster(rich_pos, out_rast, overwrite = TRUE)
    
    # summary stats
    a_cell <- cell_area_km2(tpl)
    vals_all <- values(rich_pos, mat = FALSE)
    vals_all <- vals_all[!is.na(vals_all)]
    
    n_species <- length(files_i)
    occupied_cells <- if (length(vals_all) == 0) 0 else length(vals_all)
    occupied_area_km2 <- occupied_cells * a_cell
    max_richness <- if (length(vals_all) == 0) NA else max(vals_all, na.rm = TRUE)
    mean_richness <- if (length(vals_all) == 0) NA else mean(vals_all, na.rm = TRUE)
    
    summary_list[[k1]] <- data.frame(
      year = yr,
      scenario = ssp,
      n_species = n_species,
      cell_km2 = a_cell,
      occupied_cells = occupied_cells,
      occupied_area_km2 = occupied_area_km2,
      max_richness = max_richness,
      mean_richness = mean_richness,
      stringsAsFactors = FALSE
    )
    k1 <- k1 + 1
    
    # area by richness value
    tab_val <- area_by_value(rich_pos, a_cell)
    tab_val$year <- yr
    tab_val$scenario <- ssp
    tab_val$n_species <- n_species
    
    value_list[[k2]] <- tab_val[, c("year", "scenario", "n_species", "richness_value", "n_cells", "area_km2")]
    k2 <- k2 + 1
  }
}

df_exsitu_rich_summary <- do.call(rbind, summary_list)
df_exsitu_rich_summary <- df_exsitu_rich_summary[order(df_exsitu_rich_summary$year, df_exsitu_rich_summary$scenario), ]

df_exsitu_rich_value <- do.call(rbind, value_list)
df_exsitu_rich_value <- df_exsitu_rich_value[order(df_exsitu_rich_value$year, df_exsitu_rich_value$scenario, df_exsitu_rich_value$richness_value), ]

print(df_exsitu_rich_summary)
print(df_exsitu_rich_value)

write.csv(df_exsitu_rich_summary, out_csv_summary, row.names = FALSE)
write.csv(df_exsitu_rich_value, out_csv_by_value, row.names = FALSE)

cat("\nSaved summary CSV:\n", out_csv_summary, "\n")
cat("Saved value-area CSV:\n", out_csv_by_value, "\n")
cat("Saved richness rasters in:\n", out_exsitu_rich_dir, "\n")


# create plots

df_exsitu_rich_summary$scenario <- factor(
  df_exsitu_rich_summary$scenario,
  levels = c("ssp126", "ssp245", "ssp585")
)

p_mean <- ggplot(
  df_exsitu_rich_summary,
  aes(x = year, y = occupied_area_km2, color = scenario, group = scenario)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = c(2050, 2070, 2100)) +
  scale_color_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    x = "Year",
    y = "Sum of all ex situ richness Area",
    color = "Scenario",
    title = "Sum of all ex situ refugia richness through time"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

print(p_mean)

ggsave(
  filename = file.path(out_exsitu_rich_dir, "mean_exsitu_richness_lineplot.png"),
  plot = p_mean,
  width = 8,
  height = 5.5,
  dpi = 300
)



# plot by richness value

df_exsitu_rich_value$scenario <- factor(
  df_exsitu_rich_value$scenario,
  levels = c("ssp126", "ssp245", "ssp585")
)

df_exsitu_rich_value$richness_value <- factor(
  df_exsitu_rich_value$richness_value,
  levels = sort(unique(df_exsitu_rich_value$richness_value))
)

p_value <- ggplot(
  df_exsitu_rich_value,
  aes(x = year, y = area_km2, color = scenario, group = scenario)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ richness_value, scales = "free_y") +
  scale_x_continuous(breaks = c(2050, 2070, 2100)) +
  scale_color_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    x = "Year",
    y = "Area (km²)",
    color = "Scenario",
    title = "Ex situ refugia richness-area trajectories by richness value"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9)
  )

print(p_value)

ggsave(
  filename = file.path(out_exsitu_rich_dir, "exsitu_richness_value_area_lineplot.png"),
  plot = p_value,
  width = 12,
  height = 8,
  dpi = 300
)




# combined plot (In situ + Exsitu Refugia)
library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

# ==========================================
# Inputs
# ==========================================
future_outside_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/outside_range_masked"
future_inside_dir  <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/stats_results/change_detection/refugia_rasters"

study_area <- st_read("F:/WWF_data/climate_species_analysis/defined_study_area/study_area_fit.shp", quiet = TRUE)

out_combined_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/combined_refugia_outputs"
dir.create(out_combined_dir, recursive = TRUE, showWarnings = FALSE)

out_csv <- file.path(out_combined_dir, "combined_insitu_exsitu_refugia_summary.csv")

# ==========================================
# List files
# ==========================================
fut_outside_files <- list.files(
  path = future_outside_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

fut_inside_files <- list.files(
  path = future_inside_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = FALSE
)

if (length(fut_outside_files) == 0) stop("No outside-range rasters found.")
if (length(fut_inside_files) == 0) stop("No in situ refugia rasters found.")

cat("Outside rasters:", length(fut_outside_files), "\n")
cat("Inside rasters:", length(fut_inside_files), "\n")

# ==========================================
# Helpers
# ==========================================
cell_area_km2 <- function(r) {
  rr <- terra::res(r)
  abs(rr[1] * rr[2]) / 1e6
}

count_value1_cells <- function(r) {
  as.numeric(global(r == 1, "sum", na.rm = TRUE)[1, 1])
}

# outside example:
# Atherurus africanus_mean_pixel_binary_2050s_ssp126_outside_range_only.tif
get_species_outside <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub("_mean_pixel_binary_\\d{4}s_ssp\\d+_outside_range_only$", "", nm)
}
get_year_outside <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  as.integer(sub(".*_mean_pixel_binary_(\\d{4})s_ssp\\d+_outside_range_only$", "\\1", nm))
}
get_scenario_outside <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub(".*_mean_pixel_binary_\\d{4}s_(ssp\\d+)_outside_range_only$", "\\1", nm)
}

# inside example:
# Atherurus_africanus_refugia_2020_to_2050_ssp126.tif
get_species_inside <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  gsub("_", " ", sub("_refugia_2020_to_\\d{4}_ssp\\d+$", "", nm))
}
get_year_inside <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  as.integer(sub(".*_refugia_2020_to_(\\d{4})_ssp\\d+$", "\\1", nm))
}
get_scenario_inside <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub(".*_refugia_2020_to_\\d{4}_(ssp\\d+)$", "\\1", nm)
}

# ==========================================
# Build lookup tables
# ==========================================
outside_tbl <- data.frame(
  file = fut_outside_files,
  species = vapply(fut_outside_files, get_species_outside, character(1)),
  year = vapply(fut_outside_files, get_year_outside, integer(1)),
  scenario = vapply(fut_outside_files, get_scenario_outside, character(1)),
  stringsAsFactors = FALSE
)

inside_tbl <- data.frame(
  file = fut_inside_files,
  species = vapply(fut_inside_files, get_species_inside, character(1)),
  year = vapply(fut_inside_files, get_year_inside, integer(1)),
  scenario = vapply(fut_inside_files, get_scenario_inside, character(1)),
  stringsAsFactors = FALSE
)

match_tbl <- merge(
  inside_tbl,
  outside_tbl,
  by = c("species", "year", "scenario"),
  suffixes = c("_inside", "_outside")
)

if (nrow(match_tbl) == 0) stop("No matching inside/outside raster pairs found.")

match_tbl <- match_tbl[order(match_tbl$species, match_tbl$year, match_tbl$scenario), ]
print(match_tbl)

# ==========================================
# Total study area (km2)
# project study area to raster CRS first
# ==========================================
r0 <- rast(match_tbl$file_inside[1])
study_area_vect <- vect(study_area)

if (!same.crs(r0, study_area_vect)) {
  study_area_vect <- project(study_area_vect, crs(r0))
}

total_study_area_km2 <- expanse(study_area_vect, unit = "km")[1]
cat("Total study area (km2):", total_study_area_km2, "\n")

# ==========================================
# Combine in situ + ex situ refugia
# ==========================================
res <- list()

for (i in seq_len(nrow(match_tbl))) {
  sp  <- match_tbl$species[i]
  yr  <- match_tbl$year[i]
  ssp <- match_tbl$scenario[i]
  
  cat("\nProcessing:", sp, yr, ssp, "\n")
  
  r_in  <- rast(match_tbl$file_inside[i])
  r_out <- rast(match_tbl$file_outside[i])
  
  # align outside raster to inside raster if needed
  if (!isTRUE(compareGeom(r_in, r_out, stopOnError = FALSE))) {
    r_out <- resample(r_out, r_in, method = "near")
  }
  
  # keep only ex situ refugia cells == 1
  r_out1 <- ifel(r_out == 1, 1, NA)
  
  # combine: 1 where either in situ or ex situ refugia exists
  combined <- ifel((r_in == 1) | (r_out1 == 1), 1, NA)
  names(combined) <- paste0(gsub(" ", "_", sp), "_combined_refugia_", yr, "_", ssp)
  
  out_rast <- file.path(
    out_combined_dir,
    paste0(gsub(" ", "_", sp), "_combined_refugia_", yr, "_", ssp, ".tif")
  )
  writeRaster(combined, out_rast, overwrite = TRUE)
  
  a_cell <- cell_area_km2(r_in)
  n_comb <- count_value1_cells(combined)
  combined_area_km2 <- n_comb * a_cell
  combined_pct_study_area <- ifelse(
    total_study_area_km2 > 0,
    combined_area_km2 / total_study_area_km2 * 100,
    NA_real_
  )
  
  res[[length(res) + 1]] <- data.frame(
    species = sp,
    year = yr,
    scenario = ssp,
    cell_km2 = a_cell,
    total_study_area_km2 = total_study_area_km2,
    combined_refugia_km2 = combined_area_km2,
    combined_refugia_pct_of_study_area = combined_pct_study_area,
    stringsAsFactors = FALSE
  )
}

df_combined_refugia <- do.call(rbind, res)
df_combined_refugia <- df_combined_refugia[order(df_combined_refugia$species, df_combined_refugia$year, df_combined_refugia$scenario), ]

print(df_combined_refugia)
write.csv(df_combined_refugia, out_csv, row.names = FALSE)

cat("\nSaved CSV:\n", out_csv, "\n")
cat("Saved combined rasters in:\n", out_combined_dir, "\n")


# plot results
df_combined_refugia$scenario <- factor(
  df_combined_refugia$scenario,
  levels = c("ssp126", "ssp245", "ssp585")
)

p_combined_pct <- ggplot(
  df_combined_refugia,
  aes(x = year, y = combined_refugia_pct_of_study_area,
      color = scenario, group = scenario)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ species, scales = "free_y") +
  scale_x_continuous(breaks = c(2050, 2070, 2100)) +
  scale_color_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    x = "Year",
    y = "Combined refugia (% of total study area)",
    color = "Scenario",
    title = "Combined in situ + ex situ refugia trajectories"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9)
  )

print(p_combined_pct)

ggsave(
  filename = file.path(out_combined_dir, "combined_refugia_pct_lineplot.png"),
  plot = p_combined_pct,
  width = 14,
  height = 10,
  dpi = 300
)





# combine insitu + exsitu refugia richness

# ==========================================
# Inputs
# ==========================================
future_outside_richness_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/exsitu_refugia_richness"
future_inside_richness_dir  <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/stats_results/richness_outputs"

study_area <- st_read(
  "F:/WWF_data/climate_species_analysis/defined_study_area/study_area_fit.shp",
  quiet = TRUE
)

out_combined_rich_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/combined_insitu_exsitu_refugia_richness"
dir.create(out_combined_rich_dir, recursive = TRUE, showWarnings = FALSE)

out_csv <- file.path(
  out_combined_rich_dir,
  "combined_insitu_exsitu_refugia_richness_value_area_summary.csv"
)

# ==========================================
# List files
# ==========================================
fut_outside_richness_files <- list.files(
  path = future_outside_richness_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = FALSE
)

fut_inside_richness_files <- list.files(
  path = future_inside_richness_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = FALSE
)

if (length(fut_outside_richness_files) == 0) stop("No ex situ richness rasters found.")
if (length(fut_inside_richness_files) == 0) stop("No in situ richness rasters found.")

# exclude current richness file from in situ data
fut_inside_richness_files <- fut_inside_richness_files[
  !grepl("richness_pos_2020_current\\.tif$", fut_inside_richness_files)
]

cat("Outside richness rasters:", length(fut_outside_richness_files), "\n")
cat("Inside richness rasters:", length(fut_inside_richness_files), "\n")

# ==========================================
# Helpers
# ==========================================
cell_area_km2 <- function(r) {
  rr <- terra::res(r)
  abs(rr[1] * rr[2]) / 1e6
}

get_label_outside <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub("^exsitu_richness_pos_", "", nm)
}

get_label_inside <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub("^richness_pos_", "", nm)
}

get_year_from_label <- function(label) {
  as.integer(sub("_.*$", "", label))
}

get_scenario_from_label <- function(label) {
  sub("^\\d{4}_", "", label)
}

area_by_value_vs_studyarea <- function(rich_pos, a_cell, total_study_area_km2) {
  vals_all <- values(rich_pos, mat = FALSE)
  vals_all <- vals_all[!is.na(vals_all)]
  
  if (length(vals_all) == 0) {
    return(data.frame(
      richness_value = integer(0),
      n_cells = integer(0),
      area_km2 = numeric(0),
      pct_of_study_area = numeric(0)
    ))
  }
  
  max_r <- max(vals_all, na.rm = TRUE)
  vals <- 1:max_r
  
  out <- lapply(vals, function(v) {
    n_v <- as.numeric(global(rich_pos == v, "sum", na.rm = TRUE)[1, 1])
    area_km2 <- n_v * a_cell
    pct <- ifelse(total_study_area_km2 > 0, area_km2 / total_study_area_km2 * 100, NA_real_)
    
    data.frame(
      richness_value = v,
      n_cells = n_v,
      area_km2 = area_km2,
      pct_of_study_area = pct
    )
  })
  
  do.call(rbind, out)
}

# ==========================================
# Match in situ and ex situ richness rasters
# ==========================================
outside_tbl <- data.frame(
  file = fut_outside_richness_files,
  label = vapply(fut_outside_richness_files, get_label_outside, character(1)),
  stringsAsFactors = FALSE
)

inside_tbl <- data.frame(
  file = fut_inside_richness_files,
  label = vapply(fut_inside_richness_files, get_label_inside, character(1)),
  stringsAsFactors = FALSE
)

match_tbl <- merge(
  inside_tbl,
  outside_tbl,
  by = "label",
  suffixes = c("_inside", "_outside")
)

if (nrow(match_tbl) == 0) stop("No matching in situ and ex situ richness raster pairs found.")

match_tbl$year <- get_year_from_label(match_tbl$label)
match_tbl$scenario <- get_scenario_from_label(match_tbl$label)
match_tbl <- match_tbl[order(match_tbl$year, match_tbl$scenario), ]

print(match_tbl)

# ==========================================
# Study area in raster CRS
# ==========================================
r0 <- rast(match_tbl$file_inside[1])
study_area_vect <- vect(study_area)

if (!same.crs(r0, study_area_vect)) {
  study_area_vect <- project(study_area_vect, crs(r0))
}

total_study_area_km2 <- expanse(study_area_vect, unit = "km")[1]
cat("Total study area (km2):", total_study_area_km2, "\n")

# ==========================================
# Combine in situ + ex situ richness
# ==========================================
all_tabs <- list()

for (i in seq_len(nrow(match_tbl))) {
  
  label_i <- match_tbl$label[i]
  yr_i <- match_tbl$year[i]
  ssp_i <- match_tbl$scenario[i]
  
  cat("\nProcessing:", label_i, "\n")
  
  r_in <- rast(match_tbl$file_inside[i])
  r_out <- rast(match_tbl$file_outside[i])
  
  # align ex situ raster to in situ raster if needed
  if (!isTRUE(compareGeom(r_in, r_out, stopOnError = FALSE))) {
    r_out <- resample(r_out, r_in, method = "near")
  }
  
  # replace NA with 0 before combining
  r_in0 <- ifel(is.na(r_in), 0, r_in)
  r_out0 <- ifel(is.na(r_out), 0, r_out)
  
  combined <- r_in0 + r_out0
  combined_pos <- ifel(combined == 0, NA, combined)
  names(combined_pos) <- paste0("combined_refugia_richness_", label_i)
  
  out_rast <- file.path(
    out_combined_rich_dir,
    paste0("combined_refugia_richness_", label_i, ".tif")
  )
  writeRaster(combined_pos, out_rast, overwrite = TRUE)
  
  a_cell <- cell_area_km2(r_in)
  tab_i <- area_by_value_vs_studyarea(combined_pos, a_cell, total_study_area_km2)
  
  tab_i$label <- label_i
  tab_i$year <- yr_i
  tab_i$scenario <- ssp_i
  tab_i$cell_km2 <- a_cell
  tab_i$total_study_area_km2 <- total_study_area_km2
  
  all_tabs[[length(all_tabs) + 1]] <- tab_i[, c(
    "label", "year", "scenario", "cell_km2", "total_study_area_km2",
    "richness_value", "n_cells", "area_km2", "pct_of_study_area"
  )]
}

df_combined_refugia_rich <- do.call(rbind, all_tabs)
df_combined_refugia_rich <- df_combined_refugia_rich[
  order(df_combined_refugia_rich$year,
        df_combined_refugia_rich$scenario,
        df_combined_refugia_rich$richness_value),
]

print(df_combined_refugia_rich)

write.csv(df_combined_refugia_rich, out_csv, row.names = FALSE)

cat("\nSaved CSV:\n", out_csv, "\n")
cat("Saved combined rasters in:\n", out_combined_rich_dir, "\n")


# plot results

d <- df_combined_refugia_rich %>%
  mutate(
    scenario = factor(scenario, levels = c("ssp126", "ssp245", "ssp585")),
    richness_value = factor(richness_value, levels = sort(unique(richness_value)))
  ) %>%
  arrange(richness_value, scenario, year)

p_combined_rich_pct <- ggplot(
  d,
  aes(x = year, y = pct_of_study_area, color = scenario, group = scenario)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ richness_value, ncol = 3, scales = "free_y") +
  scale_x_continuous(breaks = c(2050, 2070, 2100)) +
  scale_color_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    title = "Combined in situ + ex situ refugia richness trajectories",
    subtitle = "Area expressed as % of total study area",
    x = "Year",
    y = "% of total study area",
    color = "Scenario"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9)
  )

print(p_combined_rich_pct)

ggsave(
  filename = file.path(
    out_combined_rich_dir,
    "combined_refugia_richness_pct_of_study_area_lineplot.png"
  ),
  plot = p_combined_rich_pct,
  width = 12,
  height = 8,
  dpi = 300
)


#calculate and plot top 10% richness sites

# Inputs
# ==========================================
combined_rich_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/combined_insitu_exsitu_refugia_richness"
out_top10_dir <- file.path(combined_rich_dir, "top10_combined_refugia_hotspots")
dir.create(out_top10_dir, recursive = TRUE, showWarnings = FALSE)

out_csv <- file.path(out_top10_dir, "top10_combined_refugia_hotspots_summary.csv")

# if already known from earlier workflow, keep this
# otherwise set manually if needed
total_study_area_km2 <- unique(df_combined_refugia_rich$total_study_area_km2)[1]

# ==========================================
# List combined richness rasters
# ==========================================
combined_files <- list.files(
  path = combined_rich_dir,
  pattern = "^combined_refugia_richness_\\d{4}_ssp\\d+\\.tif$",
  full.names = TRUE,
  recursive = FALSE
)

if (length(combined_files) == 0) {
  stop("No combined refugia richness rasters found in: ", combined_rich_dir)
}

print(combined_files)

# ==========================================
# Helpers
# ==========================================
cell_area_km2 <- function(r) {
  rr <- terra::res(r)
  abs(rr[1] * rr[2]) / 1e6
}

get_label_from_file <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub("^combined_refugia_richness_", "", nm)
}

get_year_from_label <- function(label) {
  as.integer(sub("_.*$", "", label))
}

get_scenario_from_label <- function(label) {
  sub("^\\d{4}_", "", label)
}

# ==========================================
# Extract top 10% hotspots
# ==========================================
res <- list()

for (f in combined_files) {
  r <- rast(f)
  label <- get_label_from_file(f)
  yr <- get_year_from_label(label)
  ssp <- get_scenario_from_label(label)
  
  cat("\nProcessing:", label, "\n")
  
  vals <- values(r, mat = FALSE)
  vals <- vals[!is.na(vals)]
  
  if (length(vals) == 0) {
    warning("No non-NA values in: ", f)
    next
  }
  
  # threshold for top 10% richness values
  thr <- as.numeric(quantile(vals, probs = 0.90, na.rm = TRUE, type = 7))
  
  # keep cells >= threshold
  top10 <- ifel(r >= thr, 1, NA)
  names(top10) <- paste0("top10_combined_refugia_", label)
  
  out_rast <- file.path(
    out_top10_dir,
    paste0("top10_combined_refugia_", label, ".tif")
  )
  writeRaster(top10, out_rast, overwrite = TRUE)
  
  a_cell <- cell_area_km2(r)
  n_top10 <- as.numeric(global(top10 == 1, "sum", na.rm = TRUE)[1, 1])
  area_top10_km2 <- n_top10 * a_cell
  pct_study_area <- ifelse(
    total_study_area_km2 > 0,
    area_top10_km2 / total_study_area_km2 * 100,
    NA_real_
  )
  
  res[[length(res) + 1]] <- data.frame(
    label = label,
    year = yr,
    scenario = ssp,
    threshold_top10 = thr,
    hotspot_cells = n_top10,
    hotspot_area_km2 = area_top10_km2,
    pct_of_study_area = pct_study_area,
    stringsAsFactors = FALSE
  )
}

df_top10_combined <- do.call(rbind, res)
df_top10_combined <- df_top10_combined[order(df_top10_combined$year, df_top10_combined$scenario), ]

print(df_top10_combined)

write.csv(df_top10_combined, out_csv, row.names = FALSE)

cat("\nSaved CSV:\n", out_csv, "\n")
cat("Saved top 10% hotspot rasters in:\n", out_top10_dir, "\n")


# plot results  
df_top10_combined$scenario <- factor(
  df_top10_combined$scenario,
  levels = c("ssp126", "ssp245", "ssp585")
)

p_top10_km2 <- ggplot(
  df_top10_combined,
  aes(x = year, y = hotspot_area_km2, color = scenario, group = scenario)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = c(2050, 2070, 2100)) +
  scale_color_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    x = "Year",
    y = "Top 10% combined refugia area (km²)",
    color = "Scenario",
    title = "Trajectories of top 10% combined refugia hotspots"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

print(p_top10_km2)

ggsave(
  filename = file.path(out_top10_dir, "top10_combined_refugia_area_lineplot.png"),
  plot = p_top10_km2,
  width = 8,
  height = 5.5,
  dpi = 300
)



p_top10_pct <- ggplot(
  df_top10_combined,
  aes(x = year, y = pct_of_study_area, color = scenario, group = scenario)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = c(2050, 2070, 2100)) +
  scale_color_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    x = "Year",
    y = "Top 10% combined refugia area (% of study area)",
    color = "Scenario",
    title = "Relative trajectories of top 10% combined refugia hotspots"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

print(p_top10_pct)

ggsave(
  filename = file.path(out_top10_dir, "top10_combined_refugia_pct_lineplot.png"),
  plot = p_top10_pct,
  width = 8,
  height = 5.5,
  dpi = 300
)





#--------------------------------------------------------------------------------------

# Calculate suitable, richness, and refugia area within landscapes

library(terra)
library(sf)
library(dplyr)

# -----------------------------
# INPUTS
# -----------------------------
landscapes <- st_read("F:/WWF_data/climate_species_analysis/defined_study_area/landscapes.shp", quiet = TRUE)

root_current <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/current"
root_future  <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/future_binary_clipped_to_species_range"

out_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/landscape_results"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_csv <- file.path(out_dir, "suitable_area_by_landscape.csv")

# -----------------------------
# List raster files
# -----------------------------
root_current_files <- list.files(
  path = root_current,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

root_future_files <- list.files(
  path = root_future,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(root_current_files) == 0) stop("No current rasters found.")
if (length(root_future_files) == 0) stop("No future rasters found.")

cat("Current rasters:", length(root_current_files), "\n")
cat("Future rasters:", length(root_future_files), "\n")

# -----------------------------
# Helpers
# -----------------------------
get_species_current <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub("_mean_pixel_binary$", "", nm)
}

get_species_future <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub("_binary_overlap_\\d{4}_ssp\\d+_cropped_masked_to_range$", "", nm)
}

get_year_future <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  as.integer(sub(".*_binary_overlap_(\\d{4})_ssp\\d+_cropped_masked_to_range$", "\\1", nm))
}

get_scenario_future <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub(".*_binary_overlap_\\d{4}_(ssp\\d+)_cropped_masked_to_range$", "\\1", nm)
}

cell_area_km2 <- function(r) {
  rr <- terra::res(r)
  abs(rr[1] * rr[2]) / 1e6
}

total_suitable_km2 <- function(r) {
  a_cell <- cell_area_km2(r)
  n_suitable <- as.numeric(global(r == 1, "sum", na.rm = TRUE)[1, 1])
  n_suitable * a_cell
}

# -----------------------------
# Main helper: suitable area by landscape
# -----------------------------
calc_landscape_area <- function(r, species, year, scenario, landscapes_sf) {
  
  land_v <- vect(landscapes_sf)
  
  # project landscapes to raster CRS if needed
  if (!same.crs(r, land_v)) {
    land_v <- project(land_v, crs(r))
  }
  
  # landscape areas in km2
  landscape_area_km2 <- as.numeric(expanse(land_v, unit = "km"))
  
  # total suitable area for this species raster
  species_total_suitable_km2 <- as.numeric(total_suitable_km2(r))
  
  # raster cell area
  a_cell <- cell_area_km2(r)
  
  # extract raster values by landscape
  e <- terra::extract(r, land_v)
  names(e)[2] <- "value"
  
  # count suitable cells (value == 1) per landscape
  suitable_counts <- tapply(e$value == 1, e$ID, sum, na.rm = TRUE)
  
  # keep all landscapes, even if zero
  all_ids <- seq_len(nrow(land_v))
  suitable_counts_full <- rep(0, length(all_ids))
  names(suitable_counts_full) <- all_ids
  
  if (length(suitable_counts) > 0) {
    suitable_counts_full[names(suitable_counts)] <- suitable_counts
  }
  
  suitable_km2 <- as.numeric(suitable_counts_full) * a_cell
  
  # percent of each landscape that is suitable
  pct_landscape_suitable <- rep(NA_real_, length(suitable_km2))
  ok_land <- landscape_area_km2 > 0
  pct_landscape_suitable[ok_land] <- (suitable_km2[ok_land] / landscape_area_km2[ok_land]) * 100
  
  # percent of total species suitable habitat covered by each landscape
  if (species_total_suitable_km2 > 0) {
    pct_species_suitable_covered_by_landscape <- (suitable_km2 / species_total_suitable_km2) * 100
  } else {
    pct_species_suitable_covered_by_landscape <- rep(NA_real_, length(suitable_km2))
  }
  
  df <- data.frame(
    species = species,
    landscape = land_v$NAME,
    year = year,
    scenario = scenario,
    landscape_area_km2 = landscape_area_km2,
    species_total_suitable_km2 = species_total_suitable_km2,
    suitable_km2_in_landscape = suitable_km2,
    pct_landscape_suitable = pct_landscape_suitable,
    pct_species_suitable_covered_by_landscape = pct_species_suitable_covered_by_landscape,
    stringsAsFactors = FALSE
  )
  
  return(df)
}

# -----------------------------
# 1) CURRENT
# -----------------------------
current_results <- list()

for (i in seq_along(root_current_files)) {
  f <- root_current_files[i]
  cat("Processing current:", i, "of", length(root_current_files), "\n")
  
  r <- rast(f)
  species <- get_species_current(f)
  
  current_results[[i]] <- calc_landscape_area(
    r = r,
    species = species,
    year = 2020,
    scenario = "current",
    landscapes_sf = landscapes
  )
  
  gc()
}

df_current <- do.call(rbind, current_results)
df_current

# -----------------------------
# 2) FUTURE
# -----------------------------
future_results <- list()

for (i in seq_along(root_future_files)) {
  f <- root_future_files[i]
  cat("Processing future:", i, "of", length(root_future_files), "\n")
  
  r <- rast(f)
  species <- get_species_future(f)
  year <- get_year_future(f)
  scenario <- get_scenario_future(f)
  
  future_results[[i]] <- calc_landscape_area(
    r = r,
    species = species,
    year = year,
    scenario = scenario,
    landscapes_sf = landscapes
  )
  
  gc()
}

df_future <- do.call(rbind, future_results)

# -----------------------------
# Combine + Save
# -----------------------------
df_all <- rbind(df_current, df_future)
df_all <- df_all[order(df_all$species, df_all$year, df_all$scenario, df_all$landscape), ]

print(df_all)

write.csv(df_all, out_csv, row.names = FALSE)

cat("\nSaved:", out_csv, "\n")



# plot results

# -----------------------------------
# Prepare plotting data
# -----------------------------------
df_plot <- df_all %>%
  mutate(
    year = as.integer(year),
    scenario = as.character(scenario)
  )

# duplicate 2020 rows for each future scenario so lines connect
df_2020 <- df_plot %>%
  filter(year == 2020) %>%
dplyr::select(species, landscape, year,
         pct_species_suitable_covered_by_landscape,
         pct_landscape_suitable,
         suitable_km2_in_landscape)

df_2020_expanded <- bind_rows(
  df_2020 %>% mutate(scenario = "ssp126"),
  df_2020 %>% mutate(scenario = "ssp245"),
  df_2020 %>% mutate(scenario = "ssp585")
)

df_future <- df_plot %>%
  filter(year != 2020) %>%
  dplyr::select(species, landscape, year, scenario,
         pct_species_suitable_covered_by_landscape,
         pct_landscape_suitable,
         suitable_km2_in_landscape)

plot_df <- bind_rows(df_2020_expanded, df_future) %>%
  mutate(
    scenario = factor(scenario, levels = c("ssp126", "ssp245", "ssp585"))
  ) %>%
  arrange(species, landscape, scenario, year)

# -----------------------------------
# Plot per landscape area
# -----------------------------------
sp_pick <- "Pan troglodytes"

plot_df <- plot_df %>%
  dplyr::filter(landscape != "Mount Cameroon-Korup-Bakossi")

plot_df$landscape_short <- dplyr::recode(
  plot_df$landscape,
  "Campo-Ma'an" = "Campo",
  "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
  "Bas-Oogué" = "Bas-Ogoue",
  "Gamba-Mayumba-Conkouati" = "Gamba",
  "Lac Télé-Lac Tumba" = "LacTele",
  "Shanga Trii-National" = "TNS"
)

plot_df_sp <- plot_df %>%
  filter(species == sp_pick)

p_one_species <- ggplot(
  plot_df_sp,
  aes(x = year,
      y = pct_landscape_suitable,
      color = scenario,
      group = scenario)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  facet_wrap(~ landscape_short, scales = "free_y") +
  scale_x_continuous(breaks = c(2020, 2050, 2070, 2100)) +
  scale_color_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    x = "Year",
    y = "% Suitable habitat per landscape area",
    color = "Scenario",
    title = paste("Percentage Suitability Habitat Per Landscape Area:", sp_pick)
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9)
  )

print(p_one_species)


# plot suitable area covered
plot_df_sp <- plot_df %>%
  filter(species == sp_pick)

p_one_species <- ggplot(
  plot_df_sp,
  aes(x = year,
      y = pct_species_suitable_covered_by_landscape,
      color = scenario,
      group = scenario)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  facet_wrap(~ landscape_short, scales = "free_y") +
  scale_x_continuous(breaks = c(2020, 2050, 2070, 2100)) +
  scale_color_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    x = "Year",
    y = "% Suitable habitat covered",
    color = "Scenario",
    title = paste("Percentage Suitability Habitat Covered by Landscape Category:", sp_pick)
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9)
  )

print(p_one_species)




# Rank mean and total retained suitable habitats for all species within landscapes

# -----------------------------------
# Split current and future
# -----------------------------------
df_current_land <- df_all %>%
  filter(year == 2020, scenario == "current") %>%
 dplyr::select(
    species,
    landscape,
    current_suitable_km2 = suitable_km2_in_landscape
  )

df_future_land <- df_all %>%
  filter(year != 2020, scenario %in% c("ssp126", "ssp245", "ssp585")) %>%
  dplyr::select(
    species,
    landscape,
    year,
    scenario,
    future_suitable_km2 = suitable_km2_in_landscape
  )

# -----------------------------------
# Join current to future
# -----------------------------------

df_retained <- df_future_land %>%
  left_join(df_current_land, by = c("species", "landscape")) %>%
  mutate(
    retained_pct = ifelse(
      current_suitable_km2 > 0,
      (future_suitable_km2 / current_suitable_km2) * 100,
      NA_real_
    ),
    retained_change_km2 = future_suitable_km2 - current_suitable_km2
  )

print(df_retained)

df_retained <- df_retained %>%
  filter(landscape != "Mount Cameroon-Korup-Bakossi") # excludes mt cameroon bakossi
unique(df_retained$landscape)

# mean
rank_landscapes_mean <- df_retained %>%
  group_by(year, scenario, landscape) %>%
  summarise(
    n_species = sum(!is.na(retained_pct)),
    mean_retained_pct = mean(retained_pct, na.rm = TRUE),
    median_retained_pct = median(retained_pct, na.rm = TRUE),
    mean_change_km2 = mean(retained_change_km2, na.rm = TRUE),
    total_future_km2 = sum(future_suitable_km2, na.rm = TRUE),
    total_current_km2 = sum(current_suitable_km2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(year, scenario) %>%
  arrange(year, scenario, desc(mean_retained_pct), .by_group = TRUE) %>%
  mutate(rank_mean_retention = row_number()) %>%
  ungroup()

print(rank_landscapes_mean)


# total
rank_landscapes_total <- df_retained %>%
  group_by(year, scenario, landscape) %>%
  summarise(
    total_future_km2 = sum(future_suitable_km2, na.rm = TRUE),
    total_current_km2 = sum(current_suitable_km2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    total_retained_pct = ifelse(
      total_current_km2 > 0,
      (total_future_km2 / total_current_km2) * 100,
      NA_real_
    ),
    total_change_km2 = total_future_km2 - total_current_km2
  ) %>%
  group_by(year, scenario) %>%
  arrange(year, scenario, desc(total_retained_pct), .by_group = TRUE) %>%
  mutate(rank_total_retention = row_number()) %>%
  ungroup()

print(rank_landscapes_total)



# rank mean

df_retained <- df_retained %>%
  filter(landscape != "Mount Cameroon-Korup-Bakossi")

rank_landscapes_mean$landscape_short <- dplyr::recode(
  rank_landscapes_mean$landscape,
  "Campo-Ma'an" = "Campo",
  "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
  "Bas-Oogué" = "Bas-Ogoue",
  "Gamba-Mayumba-Conkouati" = "Gamba",
  "Lac Télé-Lac Tumba" = "LacTele",
  "Shanga Trii-National" = "TNS"
)

rank_landscapes_mean$label <- paste(rank_landscapes_mean$year, rank_landscapes_mean$scenario, sep = "_")

p_rank_heat <- ggplot(
  rank_landscapes_mean,
  aes(x = label, y = reorder(landscape_short, mean_retained_pct), fill = mean_retained_pct)
) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(mean_retained_pct, 1)), size = 3) +
  labs(
    x = "Scenario-Year",
    y = "Landscape",
    fill = "Mean retained %",
    title = "Ranking of landscapes Retaining Average Combined In Situ Habitat Refugia"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_rank_heat)

ggsave(
  file.path(out_dir, "landscape_ranking_heatmap_mean_retained.png"),
  p_rank_heat,
  width = 10,
  height = 6,
  dpi = 300
)



# best plot

# -----------------------------------
# Prepare data
# -----------------------------------
df_retained2 <- df_retained %>%
  filter(landscape != "Mount Cameroon-Korup-Bakossi") %>%
  mutate(
    landscape_short = dplyr::recode(
      landscape,
      "Campo-Ma'an" = "Campo",
      "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
      "Bas-Oogué" = "Bas-Ogoue",
      "Gamba-Mayumba-Conkouati" = "Gamba",
      "Lac Télé-Lac Tumba" = "LacTele",
      "Shanga Trii-National" = "TNS"
    ),
    scenario_year = paste(year, scenario, sep = "_")
  )

print(df_retained2)

# -----------------------------------
# Mean retained biodiversity by landscape
# -----------------------------------
rank_landscapes_mean <- df_retained2 %>%
  group_by(landscape, landscape_short, year, scenario, scenario_year) %>%
  summarise(
    mean_retained_pct = mean(retained_pct, na.rm = TRUE),
    .groups = "drop"
  )

print(rank_landscapes_mean)

# -----------------------------------
# Create one fixed landscape order
# -----------------------------------
landscape_order <- rank_landscapes_mean %>%
  group_by(landscape_short) %>%
  summarise(
    mean_rank_value = mean(mean_retained_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_rank_value) %>%
  pull(landscape_short)

rank_landscapes_mean2 <- rank_landscapes_mean %>%
  mutate(
    landscape_short = factor(landscape_short, levels = rev(landscape_order))
  )

# -----------------------------------
# Plot heatmap with one shared y-axis
# -----------------------------------
p_rank_heat <- ggplot(
  rank_landscapes_mean2,
  aes(
    x = scenario_year,
    y = landscape_short,
    fill = mean_retained_pct
  )
) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(mean_retained_pct, 1)), size = 3) +
  scale_fill_gradientn(
    colours = c(
      "#7f0000",
      "#d7301f",
      "#fc8d59",
      "#fee08b",
      "#d9ef8b",
      "#91cf60",
      "#00cc44"
      
    ),
    values = scales::rescale(c(0, 10, 25, 40, 60, 80, 100)),
    name = "Mean Refugia %"
  ) +
  labs(
    x = "Scenario and year",
    y = "Landscape",
    title = "Mean In Situ Mammal Refugia Ranked by Landscape Category"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_rank_heat)

ggsave(
  file.path(out_dir, "landscape_ranking_heatmap_mean_retained.png"),
  p_rank_heat,
  width = 10,
  height = 6,
  dpi = 300
)


# rank total
df_retained <- df_retained %>%
  filter(landscape != "Mount Cameroon-Korup-Bakossi")

rank_landscapes_total$landscape_short <- dplyr::recode(
  rank_landscapes_mean$landscape,
  "Campo-Ma'an" = "Campo",
  "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
  "Bas-Oogué" = "Bas-Ogoue",
  "Gamba-Mayumba-Conkouati" = "Gamba",
  "Lac Télé-Lac Tumba" = "LacTele",
  "Shanga Trii-National" = "TNS",
  "Mount Cameroon-Korup-Bakossi" = "MtCam-Korup"
)

rank_landscapes_total$label <- paste(rank_landscapes_total$year, rank_landscapes_total$scenario, sep = "_")

p_rank_heat <- ggplot(
  rank_landscapes_total,
  aes(x = label, y = reorder(landscape_short, total_retained_pct), fill = total_retained_pct)
) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(total_retained_pct, 1)), size = 3) +
  labs(
    x = "Scenario-Year",
    y = "Landscape",
    fill = "Total retained %",
    title = "Ranking of landscapes by retained suitable habitat"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_rank_heat)

ggsave(
  file.path(out_dir, "landscape_ranking_heatmap_mean_retained.png"),
  p_rank_heat,
  width = 10,
  height = 6,
  dpi = 300
)


# save results
out_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/landscape_results"

write.csv(
  df_retained,
  file.path(out_dir, "species_landscape_retained_habitat.csv"),
  row.names = FALSE
)

write.csv(
  rank_landscapes_mean,
  file.path(out_dir, "landscape_ranking_mean_retained_habitat.csv"),
  row.names = FALSE
)

write.csv(
  rank_landscapes_total,
  file.path(out_dir, "landscape_ranking_total_retained_habitat.csv"),
  row.names = FALSE
)



# -------Rank Insitu refugia by individual species----------

# -----------------------------------
# Remove Mount Cameroon
# -----------------------------------
df_retained <- df_all %>%
  filter(landscape != "Mount Cameroon-Korup-Bakossi")

# -----------------------------------
# Separate current and future
# -----------------------------------
df_current_land <- df_retained %>%
  filter(year == 2020) %>%
dplyr::select(
    species,
    landscape,
    current_suitable_km2 = suitable_km2_in_landscape
  )

df_future_land <- df_retained %>%
  filter(year != 2020) %>%
  dplyr::select(
    species,
    landscape,
    year,
    scenario,
    future_suitable_km2 = suitable_km2_in_landscape
  )

# -----------------------------------
# Join current and future
# -----------------------------------
df_species_retention <- df_future_land %>%
  left_join(df_current_land, by = c("species","landscape")) %>%
  mutate(
    retained_pct = ifelse(
      current_suitable_km2 > 0,
      (future_suitable_km2 / current_suitable_km2) * 100,
      NA_real_
    )
  )


# rank
rank_species_landscape <- df_species_retention %>%
  group_by(species, year, scenario) %>%
  arrange(desc(retained_pct), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  ungroup()

print(rank_species_landscape)

# save
write.csv(
  rank_species_landscape,
  "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/landscape_results/species_landscape_ranking.csv",
  row.names = FALSE
)

# select top ranked landscape
top_landscape_species <- rank_species_landscape %>%
  filter(rank == 1)

print(top_landscape_species)

# save top ranked landscapes
write.csv(
  top_landscape_species,
  "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/landscape_results/top_landscape_per_species.csv",
  row.names = FALSE
)



# plot heat map ranked results for each species

rank_species_landscape$landscape_short <- dplyr::recode(
  rank_species_landscape$landscape,
  "Campo-Ma'an" = "Campo",
  "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
  "Bas-Oogué" = "Bas-Ogoue",
  "Gamba-Mayumba-Conkouati" = "Gamba",
  "Lac Télé-Lac Tumba" = "LacTele",
  "Shanga Trii-National" = "TNS"
)

rank_species_landscape$scenario_year <- paste(
  rank_species_landscape$year,
  rank_species_landscape$scenario,
  sep="_"
)

p_species_heat <- ggplot(
  rank_species_landscape,
  aes(
    x = landscape_short,
    y = species,
    fill = retained_pct
  )
) +
  geom_tile(color="white") +
  facet_wrap(~scenario_year) +
  labs(
    x="Landscape",
    y="Species",
    fill="% In Situ Refugia",
    title="Percentage Insitu Habitat Refugia for Species per Conservation Landscape"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle=45,hjust=1)
  )

print(p_species_heat)



# best ranking heat map

# Prepare data
# -----------------------------------
rank_species_landscape2 <- rank_species_landscape %>%
  filter(landscape != "Mount Cameroon-Korup-Bakossi") %>%
  mutate(
    landscape_short = dplyr::recode(
      landscape,
      "Campo-Ma'an" = "Campo",
      "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
      "Bas-Oogué" = "Bas-Ogoue",
      "Gamba-Mayumba-Conkouati" = "Gamba",
      "Lac Télé-Lac Tumba" = "LacTele",
      "Shanga Trii-National" = "TNS"
    ),
    scenario_year = paste(year, scenario, sep = "_")
  )

print(rank_species_landscape2)

# -----------------------------------
# Create one fixed landscape order
# -----------------------------------
landscape_order <- rank_species_landscape2 %>%
  group_by(landscape_short) %>%
  summarise(
    mean_retained = mean(retained_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_retained) %>%
  pull(landscape_short)

rank_species_landscape3 <- rank_species_landscape2 %>%
  mutate(
    landscape_short = factor(landscape_short, levels = rev(landscape_order))
  )

# -----------------------------------
# Optional: species order for panels
# -----------------------------------
species_order <- rank_species_landscape3 %>%
  group_by(species) %>%
  summarise(
    mean_retained = mean(retained_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_retained)) %>%
  pull(species)

rank_species_landscape3 <- rank_species_landscape3 %>%
  mutate(
    species = factor(species, levels = species_order)
  )

# -----------------------------------
# Plot: landscapes on y-axis, species as panels
# -----------------------------------
p_species_heat <- ggplot(
  rank_species_landscape3,
  aes(
    x = scenario_year,
    y = landscape_short,
    fill = retained_pct
  )
) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(retained_pct, 1)), size = 3) +
  scale_fill_gradientn(
    colours = c(
      "#7f0000",
      "#d7301f",
      "#fc8d59",
      "#fee08b",
      "#d9ef8b",
      "#91cf60",
      "#00cc44"
      
    ),
    values = scales::rescale(c(0, 10, 25, 40, 60, 80, 100)),
    name = "% In Situ\nRefugia"
  ) +
  facet_wrap(~ species) +
  labs(
    x = "Scenario and year",
    y = "Landscape",
    title = "Percentage In Situ Habitat Refugia for Species per Conservation Landscape"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 9)
  )

print(p_species_heat)




#bipartite network plot

# -----------------------------------
# Choose threshold and scenario-year
# -----------------------------------
retain_threshold <- 50
year_pick <- 2100
scenario_pick <- "ssp245"

# -----------------------------------
# Short landscape labels
# -----------------------------------
net_df <- rank_species_landscape %>%
  filter(
    year == year_pick,
    scenario == scenario_pick,
    !is.na(retained_pct)
  ) %>%
  mutate(
    landscape_short = dplyr::recode(
      landscape,
      "Campo-Ma'an" = "Campo",
      "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
      "Bas-Oogué" = "Bas-Ogoue",
      "Gamba-Mayumba-Conkouati" = "Gamba",
      "Lac Télé-Lac Tumba" = "LacTele",
      "Shanga Trii-National" = "TNS"
    )
  )

# -----------------------------------
# Edge list: keep only retained links
# -----------------------------------
edges_species_landscape <- net_df %>%
  filter(retained_pct >= retain_threshold) %>%
  transmute(
    from = species,
    to = landscape_short,
    weight = retained_pct,
    future_suitable_km2 = future_suitable_km2,
    current_suitable_km2 = current_suitable_km2
  )

print(edges_species_landscape)

# save
write.csv(
  edges_species_landscape,
  file = "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/landscape_results/network_edges_species_landscape.csv",
  row.names = FALSE
)

library(dplyr)
library(igraph)
library(ggraph)
library(ggplot2)

# -----------------------------------
# Rebuild node table
# -----------------------------------
species_nodes <- edges_species_landscape %>%
  count(from, name = "degree") %>%
  transmute(
    name = from,
    node_class = "species",
    type = FALSE,   # bipartite requirement
    degree = degree
  )

landscape_nodes <- edges_species_landscape %>%
  count(to, name = "degree") %>%
  transmute(
    name = to,
    node_class = "landscape",
    type = TRUE,    # bipartite requirement
    degree = degree
  )

nodes_species_landscape <- bind_rows(species_nodes, landscape_nodes)

print(nodes_species_landscape)


g <- graph_from_data_frame(
  d = edges_species_landscape,
  vertices = nodes_species_landscape,
  directed = FALSE
)


p_net <- ggraph(g, layout = "bipartite") +
  geom_edge_link(aes(width = weight), alpha = 0.5) +
  geom_node_point(aes(size = degree, shape = node_class)) +
  geom_node_text(
    aes(label = name, filter = (node_class == "landscape")),
    repel = TRUE, size = 4
  ) +
  geom_node_text(
    aes(label = name, filter = (node_class == "species")),
    repel = TRUE, size = 3
  ) +
  scale_edge_width(range = c(0.4, 2.5)) +
  labs(
    title = paste0("Species–landscape refugia network: ", year_pick, " ", scenario_pick),
    subtitle = paste0("Links show retained habitat ≥ ", retain_threshold, "%"),
    edge_width = "Retained habitat (%)",
    size = "Node degree",
    shape = "Node type"
  ) +
  theme_void()

print(p_net)




# ---------Reproduce workflow for species richness----------

# INPUTS
# -----------------------------
landscapes <- st_read(
  "F:/WWF_data/climate_species_analysis/defined_study_area/landscapes.shp",
  quiet = TRUE
)

root_richness <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/stats_results/richness_outputs"

out_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/landscape_results_richness"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_csv <- file.path(out_dir, "richness_area_by_landscape.csv")

# -----------------------------
# List raster files
# -----------------------------
root_richness_files <- list.files(
  path = root_richness,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = FALSE
)

if (length(root_richness_files) == 0) stop("No richness rasters found.")

print(root_richness_files)

# -----------------------------
# Helpers
# -----------------------------
cell_area_km2 <- function(r) {
  rr <- terra::res(r)
  abs(rr[1] * rr[2]) / 1e6
}

get_year_richness <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  if (grepl("2020_current", nm)) return(2020L)
  as.integer(sub("^richness_pos_(\\d{4})_.*$", "\\1", nm))
}

get_scenario_richness <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  if (grepl("2020_current", nm)) return("current")
  sub("^richness_pos_\\d{4}_(ssp\\d+)$", "\\1", nm)
}

total_richness_area_km2 <- function(r) {
  a_cell <- cell_area_km2(r)
  n_cells <- as.numeric(global(!is.na(r), "sum", na.rm = TRUE)[1, 1])
  n_cells * a_cell
}

calc_richness_landscape_area <- function(r, year, scenario, landscapes_sf) {
  
  land_v <- vect(landscapes_sf)
  
  # project landscapes to raster CRS if needed
  if (!same.crs(r, land_v)) {
    land_v <- project(land_v, crs(r))
  }
  
  landscape_area_km2 <- as.numeric(expanse(land_v, unit = "km"))
  a_cell <- cell_area_km2(r)
  
  # full-raster richness area (>0 cells only, since zeros are NA already)
  total_rich_area_km2 <- total_richness_area_km2(r)
  
  # extract values by landscape
  e <- terra::extract(r, land_v)
  names(e)[2] <- "value"
  
  # count cells with richness > 0 per landscape
  rich_counts <- tapply(!is.na(e$value), e$ID, sum, na.rm = TRUE)
  
  # mean richness in occupied cells per landscape
  mean_rich_occ <- tapply(e$value, e$ID, mean, na.rm = TRUE)
  
  # max richness per landscape
  max_rich <- tapply(e$value, e$ID, max, na.rm = TRUE)
  
  all_ids <- seq_len(nrow(land_v))
  
  rich_counts_full <- rep(0, length(all_ids))
  names(rich_counts_full) <- all_ids
  
  mean_rich_full <- rep(NA_real_, length(all_ids))
  names(mean_rich_full) <- all_ids
  
  max_rich_full <- rep(NA_real_, length(all_ids))
  names(max_rich_full) <- all_ids
  
  if (length(rich_counts) > 0) {
    rich_counts_full[names(rich_counts)] <- rich_counts
  }
  if (length(mean_rich_occ) > 0) {
    mean_rich_full[names(mean_rich_occ)] <- mean_rich_occ
  }
  if (length(max_rich) > 0) {
    max_rich_full[names(max_rich)] <- max_rich
  }
  
  richness_area_km2 <- as.numeric(rich_counts_full) * a_cell
  
  pct_landscape_with_richness <- rep(NA_real_, length(richness_area_km2))
  ok_land <- landscape_area_km2 > 0
  pct_landscape_with_richness[ok_land] <- 
    (richness_area_km2[ok_land] / landscape_area_km2[ok_land]) * 100
  
  if (total_rich_area_km2 > 0) {
    pct_total_richness_area_covered_by_landscape <- 
      (richness_area_km2 / total_rich_area_km2) * 100
  } else {
    pct_total_richness_area_covered_by_landscape <- rep(NA_real_, length(richness_area_km2))
  }
  
  data.frame(
    landscape = land_v$NAME,
    year = year,
    scenario = scenario,
    landscape_area_km2 = landscape_area_km2,
    total_richness_area_km2 = total_rich_area_km2,
    richness_area_km2_in_landscape = richness_area_km2,
    pct_landscape_with_richness = pct_landscape_with_richness,
    pct_total_richness_area_covered_by_landscape = pct_total_richness_area_covered_by_landscape,
    mean_richness_in_landscape = as.numeric(mean_rich_full),
    max_richness_in_landscape = as.numeric(max_rich_full),
    stringsAsFactors = FALSE
  )
}

# -----------------------------
# Run all rasters
# -----------------------------
richness_results <- vector("list", length(root_richness_files))

for (i in seq_along(root_richness_files)) {
  f <- root_richness_files[i]
  cat("Processing:", i, "of", length(root_richness_files), "\n")
  
  r <- rast(f)
  year <- get_year_richness(f)
  scenario <- get_scenario_richness(f)
  
  richness_results[[i]] <- calc_richness_landscape_area(
    r = r,
    year = year,
    scenario = scenario,
    landscapes_sf = landscapes
  )
  
  gc()
}

df_rich_land <- do.call(rbind, richness_results)
df_rich_land <- df_rich_land[order(df_rich_land$year, df_rich_land$scenario, df_rich_land$landscape), ]

print(df_rich_land)

write.csv(df_rich_land, out_csv, row.names = FALSE)

cat("\nSaved:", out_csv, "\n")


# plot

df_rich_land <- df_rich_land %>%
  filter(landscape != "Mount Cameroon-Korup-Bakossi")

plot_df <- df_rich_land %>%
  filter(landscape != "Mount Cameroon-Korup-Bakossi") %>%
  mutate(
    landscape_short = dplyr::recode(
      landscape,
      "Campo-Ma'an" = "Campo",
      "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
      "Bas-Oogué" = "Bas-Ogoue",
      "Gamba-Mayumba-Conkouati" = "Gamba",
      "Lac Télé-Lac Tumba" = "LacTele",
      "Shanga Trii-National" = "TNS"
    )
  )

# duplicate 2020 for each scenario so lines connect
df_2020 <- plot_df %>%
  filter(year == 2020) %>%
 dplyr::select(landscape, landscape_short, year,
         pct_total_richness_area_covered_by_landscape,
         richness_area_km2_in_landscape,
         pct_landscape_with_richness,
         mean_richness_in_landscape,
         max_richness_in_landscape)

df_2020_expanded <- bind_rows(
  df_2020 %>% mutate(scenario = "ssp126"),
  df_2020 %>% mutate(scenario = "ssp245"),
  df_2020 %>% mutate(scenario = "ssp585")
)

df_future <- plot_df %>%
  filter(year != 2020)

plot_df2 <- bind_rows(df_2020_expanded, df_future) %>%
  mutate(
    scenario = factor(scenario, levels = c("ssp126", "ssp245", "ssp585")),
    landscape_short = factor(landscape_short,
                             levels = c("Campo", "TRIDOM", "Bas-Ogoue", "Gamba", "LacTele", "TNS"))
  ) %>%
  arrange(landscape_short, scenario, year)

p_rich_pct <- ggplot(
  plot_df2,
  aes(x = year, y = pct_total_richness_area_covered_by_landscape,
      color = scenario, group = scenario)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ landscape_short, scales = "free_y") +
  scale_x_continuous(breaks = c(2020, 2050, 2070, 2100)) +
  scale_color_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    x = "Year",
    y = "Richness area covered by landscape (%)",
    color = "Scenario",
    title = "Richness trajectories by landscape"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9)
  )

print(p_rich_pct)

ggsave(
  filename = file.path(out_dir, "richness_trajectories_by_landscape_pct.png"),
  plot = p_rich_pct,
  width = 11,
  height = 7,
  dpi = 300
)



p_mean_rich <- ggplot(
  plot_df2,
  aes(x = year, y = mean_richness_in_landscape,
      color = scenario, group = scenario)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ landscape_short, scales = "free_y") +
  scale_x_continuous(breaks = c(2020, 2050, 2070, 2100)) +
  scale_color_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    x = "Year",
    y = "Mean richness in landscape",
    color = "Scenario",
    title = "Mean richness trajectories by landscape"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9)
  )

print(p_mean_rich)

ggsave(
  filename = file.path(out_dir, "mean_richness_trajectories_by_landscape.png"),
  plot = p_mean_rich,
  width = 11,
  height = 7,
  dpi = 300
)



# Ranking refugia richness

# -----------------------------------
# Remove Mount Cameroon
# -----------------------------------
df_rich_land2 <- df_rich_land %>%
  filter(landscape != "Mount Cameroon-Korup-Bakossi")

# -----------------------------------
# Separate current and future
# -----------------------------------
rich_current <- df_rich_land2 %>%
  filter(year == 2020) %>%
 dplyr::select(
    landscape,
    current_richness_area_km2 = richness_area_km2_in_landscape
  )

rich_future <- df_rich_land2 %>%
  filter(year != 2020) %>%
 dplyr::select(
    landscape,
    year,
    scenario,
    future_richness_area_km2 = richness_area_km2_in_landscape
  )

# -----------------------------------
# Join and calculate retention
# -----------------------------------
rich_retention <- rich_future %>%
  left_join(rich_current, by = "landscape") %>%
  mutate(
    retained_richness_pct =
      (future_richness_area_km2 / current_richness_area_km2) * 100,
    
    richness_change_km2 =
      future_richness_area_km2 - current_richness_area_km2
  )

print(rich_retention)


# rank

rank_richness_landscapes <- rich_retention %>%
  group_by(year, scenario) %>%
  arrange(desc(retained_richness_pct), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  ungroup()

print(rank_richness_landscapes)

# select top ranked areas
top_richness_landscapes <- rank_richness_landscapes %>%
  filter(rank <= 3)

print(top_richness_landscapes)


# plot ranked richness (line plot)
rank_richness_landscapes$landscape_short <- dplyr::recode(
  rank_richness_landscapes$landscape,
  "Campo-Ma'an" = "Campo",
  "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
  "Bas-Oogué" = "Bas-Ogoue",
  "Gamba-Mayumba-Conkouati" = "Gamba",
  "Lac Télé-Lac Tumba" = "LacTele",
  "Shanga Trii-National" = "TNS"
)

p_rich_retention <- ggplot(
  rank_richness_landscapes,
  aes(
    x = year,
    y = retained_richness_pct,
    color = landscape_short,
    group = landscape_short
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(~scenario) +
  scale_x_continuous(breaks = c(2050, 2070, 2100)) +
  labs(
    x = "Year",
    y = "Retained richness area (%)",
    color = "Landscape",
    title = "Landscape retention of species richness under climate change"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )

print(p_rich_retention)


# plot heatmap

rank_richness_landscapes$scenario_year <-
  paste(rank_richness_landscapes$year,
        rank_richness_landscapes$scenario)

p_heat_rank <- ggplot(
  rank_richness_landscapes,
  aes(
    x = scenario_year,
    y = landscape_short,
    fill = retained_richness_pct
  )
) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(retained_richness_pct,1)), size = 3) +
  labs(
    x = "Scenario-Year",
    y = "Landscape",
    fill = "Retained richness (%)",
    title = "Landscape ranking by In Situ Refugia richness"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_heat_rank)



# Best ranking

library(dplyr)
library(ggplot2)

# -----------------------------------
# Prepare data
# -----------------------------------
rank_richness_landscapes2 <- rank_richness_landscapes %>%
  filter(landscape != "Mount Cameroon-Korup-Bakossi") %>%
  mutate(
    landscape_short = dplyr::recode(
      landscape,
      "Campo-Ma'an" = "Campo",
      "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
      "Bas-Oogué" = "Bas-Ogoue",
      "Gamba-Mayumba-Conkouati" = "Gamba",
      "Lac Télé-Lac Tumba" = "LacTele",
      "Shanga Trii-National" = "TNS"
    ),
    scenario_year = paste(year, scenario, sep = "_")
  )

print(rank_richness_landscapes2)

# -----------------------------------
# Create fixed landscape order
# -----------------------------------
landscape_order <- rank_richness_landscapes2 %>%
  group_by(landscape_short) %>%
  summarise(
    mean_retained = mean(retained_richness_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_retained) %>%
  pull(landscape_short)

rank_richness_landscapes3 <- rank_richness_landscapes2 %>%
  mutate(
    landscape_short = factor(landscape_short, levels = rev(landscape_order))
  )

# -----------------------------------
# Plot heatmap
# -----------------------------------
p_heat_rank <- ggplot(
  rank_richness_landscapes3,
  aes(
    x = scenario_year,
    y = landscape_short,
    fill = retained_richness_pct
  )
) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(retained_richness_pct,1)), size = 3) +
  scale_fill_gradientn(
    colours = c(
      "#7f0000",
      "#d7301f",
      "#fc8d59",
      "#fee08b",
      "#d9ef8b",
      "#91cf60",
      "#00cc44"
      
    ),
    values = scales::rescale(c(0,10,25,40,60,80,100)),
    name = "Refugia\nrichness (%)"
  ) +
  labs(
    x = "Scenario and year",
    y = "Landscape",
    title = "Landscape Ranking by Combined In Situ Refugia Richness"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_heat_rank)



# plot by richness value

# Landscapes
# -----------------------------
land_v <- vect(landscapes)

# -----------------------------
# Helper
# -----------------------------
cell_area_km2 <- function(r) {
  rr <- res(r)
  abs(rr[1] * rr[2]) / 1e6
}

get_year_richness <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  if (grepl("2020_current", nm)) return(2020L)
  as.integer(sub("^richness_pos_(\\d{4})_.*$", "\\1", nm))
}

get_scenario_richness <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  if (grepl("2020_current", nm)) return("current")
  sub("^richness_pos_\\d{4}_(ssp\\d+)$", "\\1", nm)
}

# -----------------------------
# Main calculation
# -----------------------------
rich_val_results <- list()

for (i in seq_along(root_richness_files)) {
  
  f <- root_richness_files[i]
  cat("Processing:", basename(f), "\n")
  
  r <- rast(f)
  
  year <- get_year_richness(f)
  scenario <- get_scenario_richness(f)
  
  # project landscapes if needed
  if (!same.crs(r, land_v)) {
    land_proj <- project(land_v, crs(r))
  } else {
    land_proj <- land_v
  }
  
  a_cell <- cell_area_km2(r)
  
  # extract richness values
  e <- terra::extract(r, land_proj)
  names(e)[2] <- "rich"
  
  e <- e[!is.na(e$rich), ]
  
  tab <- e %>%
    group_by(ID, rich) %>%
    summarise(
      n_cells = n(),
      .groups = "drop"
    )
  
  tab$area_km2 <- tab$n_cells * a_cell
  
  tab$landscape <- land_proj$NAME[tab$ID]
  tab$year <- year
  tab$scenario <- scenario
  
  rich_val_results[[i]] <- tab
}

df_rich_values <- bind_rows(rich_val_results)

print(df_rich_values)



df_rich_values <- df_rich_values %>%
  filter(landscape != "Mount Cameroon-Korup-Bakossi")

df_heat <- df_rich_values %>%
  mutate(
    landscape_short = recode(
      landscape,
      "Campo-Ma'an" = "Campo",
      "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
      "Bas-Oogué" = "Bas-Ogoue",
      "Gamba-Mayumba-Conkouati" = "Gamba",
      "Lac Télé-Lac Tumba" = "LacTele",
      "Shanga Trii-National" = "TNS"
    ),
    scenario_year = paste(year, scenario)
  )

p_heat_richness <- ggplot(
  df_heat,
  aes(
    x = landscape_short,
    y = factor(rich),
    fill = area_km2
  )
) +
  geom_tile(color = "white") +
  facet_wrap(~scenario_year) +
  labs(
    x = "Landscape",
    y = "Richness value",
    fill = "Area (km²)",
    title = "Distribution of richness values across landscapes"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

print(p_heat_richness)



# best plotting approach
library(dplyr)
library(ggplot2)

# -----------------------------------
# Prepare data
# -----------------------------------
df_heat <- df_rich_values %>%
  filter(landscape != "Mount Cameroon-Korup-Bakossi") %>%
  mutate(
    landscape_short = dplyr::recode(
      landscape,
      "Campo-Ma'an" = "Campo",
      "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
      "Bas-Oogué" = "Bas-Ogoue",
      "Gamba-Mayumba-Conkouati" = "Gamba",
      "Lac Télé-Lac Tumba" = "LacTele",
      "Shanga Trii-National" = "TNS"
    ),
    scenario_year = paste(year, scenario, sep = "_")
  )

print(df_heat, n = Inf)

# -----------------------------------
# Create one fixed landscape order
# -----------------------------------
landscape_order <- df_heat %>%
  group_by(landscape_short) %>%
  summarise(
    mean_area = mean(area_km2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_area) %>%
  pull(landscape_short)

df_heat2 <- df_heat %>%
  mutate(
    landscape_short = factor(landscape_short, levels = rev(landscape_order))
  )

# -----------------------------------
# Optional: create fixed richness order
# -----------------------------------
rich_order <- sort(unique(df_heat2$rich))

df_heat2 <- df_heat2 %>%
  mutate(
    rich = factor(rich, levels = rev(rich_order))
  )

# -----------------------------------
# Plot with recent shared-y-axis style
# -----------------------------------
p_heat_richness <- ggplot(
  df_heat2,
  aes(
    x = scenario_year,
    y = landscape_short,
    fill = area_km2
  )
) +
  geom_tile(color = "white") +
  facet_wrap(~ rich) +
  scale_fill_gradientn(
    colours = c(
      "#7f0000",
      "#d7301f",
      "#fc8d59",
      "#fee08b",
      "#d9ef8b",
      "#91cf60",
      "#00cc44"
      
    ),
    name = "Richness Area (km²)"
  ) +
  labs(
    x = "Scenario and year",
    y = "Landscape",
    title = "Distribution of Richness Values Across Landscapes"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    strip.text = element_text(size = 9)
  )

print(p_heat_richness)



# calculate richness change relative to current situation

# df_heat <- rich_retention %>%
# mutate(
# scenario_year = paste(year, scenario, sep = "_"),
# landscape_short = dplyr::recode(
# landscape,
# "Campo-Ma'an" = "Campo",
# "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
# "Bas-Oogué" = "Bas-Ogoue",
#  "Gamba-Mayumba-Conkouati" = "Gamba",
# "Lac Télé-Lac Tumba" = "LacTele",
# "Shanga Trii-National" = "TNS"
# )
# )

# p_heat_retention <- ggplot(df_heat,
#  aes(x = scenario_year,
#   y = landscape_short,
#  fill = retained_richness_pct)) +
#  geom_tile(color = "white") +
# scale_fill_gradientn(
# colors = c("darkred", "orange", "yellow", "lightgreen", "darkgreen"),
# limits = c(0,100),
# name = "% richness retained"
# ) +
# labs(
#  x = "Scenario and year",
#  y = "Landscape",
# title = "Retention of species richness across landscapes"
# ) +
# theme_bw() +
# theme(
#  axis.text.x = element_text(angle = 45, hjust = 1)
# )

# print(p_heat_retention)


# richnes change
p_heat_change <- ggplot(df_heat,
                        aes(x = scenario_year,
                            y = landscape_short,
                            fill = richness_change_km2)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "darkred",
    mid = "white",
    high = "darkblue",
    midpoint = 0,
    name = "Richness change (km²)"
  ) +
  labs(
    x = "Scenario and year",
    y = "Landscape",
    title = "Change in Combined In Situ Richness Area across landscapes"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_heat_change)



# plot richness area change by richness values across landscapes

# baseline from 2020
baseline <- df_rich_values %>%
  filter(year == 2020, scenario == "current") %>%
 dplyr::select(landscape, rich, baseline_area = area_km2)

print(baseline, n = Inf)

future_tbl <- df_rich_values %>%
  filter(year != 2020) %>%
 dplyr::select(landscape, year, scenario, rich, future_area = area_km2)

# make sure every baseline richness class exists for every future scenario
future_complete <- expand_grid(
  landscape = unique(baseline$landscape),
  rich = unique(baseline$rich),
  year = sort(unique(future_tbl$year)),
  scenario = sort(unique(future_tbl$scenario))
) %>%
  left_join(future_tbl, by = c("landscape", "rich", "year", "scenario")) %>%
  mutate(future_area = ifelse(is.na(future_area), 0, future_area))

print(future_complete, n = 100)


df_rich_change <- future_complete %>%
  left_join(baseline, by = c("landscape", "rich")) %>%
  mutate(
    baseline_area = ifelse(is.na(baseline_area), 0, baseline_area),
    richness_change_km2 = future_area - baseline_area,
    pct_change = ifelse(baseline_area > 0,
                        (richness_change_km2 / baseline_area) * 100,
                        NA_real_),
    scenario_year = paste(year, scenario, sep = "_")
  )

print(df_rich_change, n = Inf)


df_rich_change$landscape_short <- dplyr::recode(
  df_rich_change$landscape,
  "Campo-Ma'an" = "Campo",
  "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
  "Bas-Oogué" = "Bas-Ogoue",
  "Gamba-Mayumba-Conkouati" = "Gamba",
  "Lac Télé-Lac Tumba" = "LacTele",
  "Shanga Trii-National" = "TNS"
)


p_heat_rich_change <- ggplot(
  df_rich_change,
  aes(x = scenario_year, y = factor(rich), fill = richness_change_km2)
) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "darkred",
    mid = "white",
    high = "darkblue",
    midpoint = 0,
    name = "Change (km²)"
  ) +
  facet_wrap(~ landscape_short) +
  labs(
    x = "Scenario and year",
    y = "Richness value",
    title = "Change in Landscape Richness Area by richness value"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_heat_rich_change)


# identify risk zones in the combined richness refugia line plot

df_traj <- rich_retention %>%
  mutate(
    landscape_short = dplyr::recode(
      landscape,
      "Campo-Ma'an" = "Campo",
      "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
      "Bas-Oogué" = "Bas-Ogoue",
      "Gamba-Mayumba-Conkouati" = "Gamba",
      "Lac Télé-Lac Tumba" = "LacTele",
      "Shanga Trii-National" = "TNS"
    ),
    scenario = factor(scenario, levels = c("ssp126", "ssp245", "ssp585"))
  )


p_landscape_risk <- ggplot(
  df_traj,
  aes(
    x = year,
    y = retained_richness_pct,
    color = scenario,
    group = scenario
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  facet_wrap(~ landscape_short, scales = "free_y") +
  scale_x_continuous(breaks = c(2050, 2070, 2100)) +
  scale_y_continuous(limits = c(0, 105)) +
  scale_color_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    x = "Year",
    y = "% In Situ Combined Refugia Richness",
    color = "Scenario",
    title = "Combined Habitat refugia Richness Trajectories Across Landscapes"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9)
  )

print(p_landscape_risk)

ggsave(
  "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/landscape_results/landscape_climate_risk_trajectories.png",
  plot = p_landscape_risk,
  width = 11,
  height = 7,
  dpi = 300
)




p_landscape_risk_zones <- ggplot(
  df_traj,
  aes(
    x = year,
    y = retained_richness_pct,
    color = scenario,
    group = scenario
  )
) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 75, ymax = 100,
           alpha = 0.08, fill = "green3") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 40, ymax = 75,
           alpha = 0.08, fill = "gold") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 40,
           alpha = 0.08, fill = "red") +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  facet_wrap(~ landscape_short, scales = "fixed") +
  scale_x_continuous(breaks = c(2050, 2070, 2100)) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_color_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    x = "Year",
    y = "Refugia richness area (%)",
    color = "Scenario",
    title = "Landscape climate-risk trajectories",
    subtitle = "Green = low risk, yellow = moderate risk, red = high risk"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9)
  )

print(p_landscape_risk_zones)

ggsave(
  "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/landscape_results/landscape_climate_risk_trajectories_zones.png",
  plot = p_landscape_risk_zones,
  width = 11,
  height = 7,
  dpi = 300
)






#-------------Repeat plotting workflow for exsitu refugia----------------


# ==========================================

exsitu_refugia_dir  <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/future_binary_clipped_to_species_range"

exsitu_refugia_files <- list.files(
  path = exsitu_refugia_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

exsitu_refugia_files



library(terra)
library(sf)
library(dplyr)

# -----------------------------
# INPUTS
# -----------------------------
landscapes <- st_read("F:/WWF_data/climate_species_analysis/defined_study_area/landscapes.shp")

exsitu_refugia_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/future_binary_clipped_to_species_range"

exsitu_refugia_files <- list.files(
  path = exsitu_refugia_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

out_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/landscape_results_exsitu"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_csv <- file.path(out_dir, "exsitu_suitable_area_by_landscape.csv")

if (length(exsitu_refugia_files) == 0) {
  stop("No raster files found in: ", exsitu_refugia_dir)
}

# -----------------------------
# Convert landscapes to terra
# -----------------------------
land_v <- vect(landscapes)

# remove Mount Cameroon-Korup-Bakossi if needed
land_v <- land_v[land_v$NAME != "Mount Cameroon-Korup-Bakossi", ]

land_names <- land_v$NAME

# -----------------------------
# Helpers
# -----------------------------
cell_area_km2 <- function(r) {
  rr <- terra::res(r)
  abs(rr[1] * rr[2]) / 1e6
}

# total suitable area in raster
species_total_suitable_km2 <- function(r) {
  a_cell <- cell_area_km2(r)
  n_suit <- as.numeric(terra::global(r == 1, "sum", na.rm = TRUE)[1, 1])
  n_suit * a_cell
}

# landscape polygon area in km2 after projection to raster CRS
get_landscape_areas_km2 <- function(land_vect, r) {
  if (!terra::same.crs(land_vect, r)) {
    land_vect <- terra::project(land_vect, terra::crs(r))
  }
  a <- expanse(land_vect, unit = "km")
  data.frame(
    landscape = land_vect$NAME,
    landscape_area_km2 = as.numeric(a)
  )
}

# parse future raster name
parse_future_info <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  
  # example:
  # Atherurus africanus_binary_overlap_2050_ssp126_cropped_masked_to_range
  m <- regexec("^(.*)_binary_overlap_(\\d{4})_(ssp\\d{3})_cropped_masked_to_range$", nm)
  hit <- regmatches(nm, m)[[1]]
  
  if (length(hit) == 0) {
    return(NULL)
  }
  
  list(
    species = hit[2],
    year = as.integer(hit[3]),
    scenario = hit[4]
  )
}

# compute suitable area by landscape
calc_landscape_area <- function(r, species, year, scenario) {
  
  # project landscapes to raster CRS if needed
  land_proj <- land_v
  if (!terra::same.crs(r, land_proj)) {
    land_proj <- terra::project(land_proj, terra::crs(r))
  }
  
  a_cell <- cell_area_km2(r)
  
  # total suitable area for this species/scenario raster
  total_suitable_km2 <- species_total_suitable_km2(r)
  
  # extract raster values by landscape
  e <- terra::extract(r, land_proj)
  
  # e has columns: ID, raster_value
  # count cells == 1 in each landscape
  suitable_cells <- tapply(e[[2]] == 1, e[[1]], sum, na.rm = TRUE)
  
  # ensure all landscapes are present
  all_ids <- seq_len(nrow(land_proj))
  suitable_cells_full <- rep(0, length(all_ids))
  names(suitable_cells_full) <- all_ids
  
  if (length(suitable_cells) > 0) {
    suitable_cells_full[names(suitable_cells)] <- suitable_cells
  }
  
  suitable_km2 <- suitable_cells_full * a_cell
  
  # polygon areas
  land_area_df <- get_landscape_areas_km2(land_proj, r)
  
  df <- data.frame(
    species = species,
    landscape = land_proj$NAME,
    year = year,
    scenario = scenario,
    landscape_area_km2 = land_area_df$landscape_area_km2,
    species_total_suitable_km2 = total_suitable_km2,
    suitable_km2_in_landscape = suitable_km2,
    pct_landscape_suitable = ifelse(
      land_area_df$landscape_area_km2 > 0,
      (suitable_km2 / land_area_df$landscape_area_km2) * 100,
      NA_real_
    ),
    pct_species_suitable_covered_by_landscape = ifelse(
      total_suitable_km2 > 0,
      (suitable_km2 / total_suitable_km2) * 100,
      NA_real_
    ),
    stringsAsFactors = FALSE
  )
  
  df
}

# -----------------------------
# Run all rasters
# -----------------------------
results <- list()

for (i in seq_along(exsitu_refugia_files)) {
  f <- exsitu_refugia_files[i]
  info <- parse_future_info(f)
  
  if (is.null(info)) {
    warning("Could not parse file name: ", f)
    next
  }
  
  r <- rast(f)
  
  results[[length(results) + 1]] <- calc_landscape_area(
    r = r,
    species = info$species,
    year = info$year,
    scenario = info$scenario
  )
  
  cat("Processed:", i, "of", length(exsitu_refugia_files), "-", basename(f), "\n")
}

df_exsitu_landscape <- do.call(rbind, results)
df_exsitu_landscape <- df_exsitu_landscape %>%
  arrange(species, year, scenario, landscape)

print(df_exsitu_landscape)

write.csv(df_exsitu_landscape, out_csv, row.names = FALSE)
cat("\nSaved:", out_csv, "\n")



# plot landscape coverage

plot_df <- df_exsitu_landscape %>%
  mutate(
    landscape_short = dplyr::recode(
      landscape,
      "Campo-Ma'an" = "Campo",
      "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
      "Bas-Oogué" = "Bas-Ogoue",
      "Gamba-Mayumba-Conkouati" = "Gamba",
      "Lac Télé-Lac Tumba" = "LacTele",
      "Shanga Trii-National" = "TNS"
    )
  )

ggplot(plot_df,
       aes(x = year, y = pct_species_suitable_covered_by_landscape,
           color = scenario, group = scenario)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ landscape_short, scales = "free_y") +
  scale_x_continuous(breaks = c(2050, 2070, 2100)) +
  labs(
    x = "Year",
    y = "% of species suitable area covered by landscape",
    color = "Scenario",
    title = "Ex situ suitable habitat coverage by landscape"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")


# rank species suitable per landscape

library(dplyr)
library(ggplot2)
library(forcats)


# use your ex situ landscape results table
df_plot <- df_exsitu_landscape %>%
  filter(landscape != "Mount Cameroon-Korup-Bakossi") %>%
  mutate(
    landscape_short = dplyr::recode(
      landscape,
      "Campo-Ma'an" = "Campo",
      "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
      "Bas-Oogué" = "Bas-Ogoue",
      "Gamba-Mayumba-Conkouati" = "Gamba",
      "Lac Télé-Lac Tumba" = "LacTele",
      "Shanga Trii-National" = "TNS"
    ),
    scenario_year = paste(year, scenario, sep = "_")
  )

print(df_plot)


df_rank_species <- df_plot %>%
  group_by(species, scenario_year) %>%
  arrange(desc(pct_landscape_suitable), .by_group = TRUE) %>%
  mutate(
    landscape_rank = row_number()
  ) %>%
  ungroup()

print(df_rank_species)


# Create one fixed landscape order
# -----------------------------------
landscape_order <- df_rank_species %>%
  group_by(landscape_short) %>%
  summarise(mean_rank = mean(landscape_rank, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_rank) %>%
  pull(landscape_short)

df_rank_species2 <- df_rank_species %>%
  mutate(
    landscape_short = factor(landscape_short, levels = rev(landscape_order))
  )


# -----------------------------------
# Plot with one shared y-axis
# -----------------------------------
p_rank_species <- ggplot(
  df_rank_species2,
  aes(
    x = scenario_year,
    y = landscape_short,
    fill = pct_landscape_suitable
  )
) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colours = c(
      "#7f0000",
      "#d7301f",
      "#fc8d59",
      "#fee08b",
      "#d9ef8b",
      "#91cf60",
      "#00cc44"
    ),
    values = scales::rescale(c(0, 1, 5, 10, 20, 40, 70)),
    name = "% landscape\nrefugia"
  ) +
  facet_wrap(~ species) +
  labs(
    x = "Scenario and year",
    y = "Landscape",
    title = "Ex Situ Climate refugia for species ranked by Landscape category"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 9)
  )

print(p_rank_species)




# -----Rank mean biodiversity---

# -----------------------------------
# Mean biodiversity ranking by landscape

# 1. Calculate mean biodiversity across species
df_mean_biodiv <- df_plot %>%
  group_by(landscape_short, scenario_year) %>%
  summarise(
    mean_pct_landscape_suitable = mean(pct_landscape_suitable, na.rm = TRUE),
    .groups = "drop"
  )

print(df_mean_biodiv)

# 2. Rank landscapes within each scenario-year
df_rank_mean <- df_mean_biodiv %>%
  group_by(scenario_year) %>%
  arrange(desc(mean_pct_landscape_suitable), .by_group = TRUE) %>%
  mutate(
    landscape_rank = row_number()
  ) %>%
  ungroup()

print(df_rank_mean)

# 3. Create one fixed landscape order from mean rank
landscape_order_mean <- df_rank_mean %>%
  group_by(landscape_short) %>%
  summarise(mean_rank = mean(landscape_rank, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_rank) %>%
  pull(landscape_short)

df_rank_mean2 <- df_rank_mean %>%
  mutate(
    landscape_short = factor(landscape_short, levels = rev(landscape_order_mean))
  )

# 4. Plot ranked mean biodiversity heatmap
p_rank_mean <- ggplot(
  df_rank_mean2,
  aes(
    x = scenario_year,
    y = landscape_short,
    fill = mean_pct_landscape_suitable
  )
) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colours = c(
      "#7f0000",
      "#d7301f",
      "#fc8d59",
      "#fee08b",
      "#d9ef8b",
      "#91cf60",
      "#00cc44"
    ),
    values = scales::rescale(c(0, 1, 5, 10, 20, 40, 70)),
    name = "Mean % landscape\nrefugia"
  ) +
  labs(
    x = "Scenario and year",
    y = "Landscape",
    title = "Mean biodiversity ex situ climate refugia ranked by landscape category"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_rank_mean)




# Do ranking by exsitu refugia richness

exsitu_refugia_richness_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/exsitu_refugia_richness"

exsitu_refugia_richness_files <- list.files(
  path = exsitu_refugia_richness_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = FALSE
)

exsitu_refugia_richness_files


# load landscapes
landscapes <- st_read("F:/WWF_data/climate_species_analysis/defined_study_area/landscapes.shp")
land_v <- vect(landscapes)
land_v <- land_v[land_v$NAME != "Mount Cameroon-Korup-Bakossi", ]

# helper function
parse_richness_name <- function(f) {
  
  nm <- tools::file_path_sans_ext(basename(f))
  
  # exsitu_richness_pos_2050_ssp126
  parts <- strsplit(nm, "_")[[1]]
  
  year <- as.numeric(parts[4])
  scenario <- parts[5]
  
  list(
    year = year,
    scenario = scenario
  )
}

# extract richness values within landscapes
results <- list()

for (i in seq_along(exsitu_refugia_richness_files)) {
  
  f <- exsitu_refugia_richness_files[i]
  
  info <- parse_richness_name(f)
  
  r <- rast(f)
  
  if (!same.crs(r, land_v)) {
    land_proj <- project(land_v, crs(r))
  } else {
    land_proj <- land_v
  }
  
  a_cell <- cell_area_km2(r)
  
  e <- terra::extract(r, land_proj)
  
  names(e)[2] <- "rich"
  
  df <- e %>%
    filter(!is.na(rich)) %>%
    group_by(ID, rich) %>%
    summarise(
      n_cells = n(),
      .groups = "drop"
    ) %>%
    mutate(
      area_km2 = n_cells * a_cell,
      landscape = land_proj$NAME[ID],
      year = info$year,
      scenario = info$scenario
    )
  
  results[[i]] <- df
  
  cat("Processed:", basename(f), "\n")
}

df_exsitu_rich_values <- bind_rows(results)
print(df_exsitu_rich_values)

# calculate percent area per landscape
land_area <- data.frame(
  landscape = land_v$NAME,
  landscape_area_km2 = expanse(land_v, unit = "km")
)

# merge results
df_exsitu_rich_values <- df_exsitu_rich_values %>%
  left_join(land_area, by = "landscape") %>%
  mutate(
    pct_landscape_area = (area_km2 / landscape_area_km2) * 100,
    scenario_year = paste(year, scenario, sep = "_")
  )


# prepare plot data
df_exsitu_rich_values <- df_exsitu_rich_values %>%
  mutate(
    landscape_short = dplyr::recode(
      landscape,
      "Campo-Ma'an" = "Campo",
      "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
      "Bas-Oogué" = "Bas-Ogoue",
      "Gamba-Mayumba-Conkouati" = "Gamba",
      "Lac Télé-Lac Tumba" = "LacTele",
      "Shanga Trii-National" = "TNS"
    )
  )


# rank percent richness per landscape area
df_rank_rich <- df_exsitu_rich_values %>%
  group_by(rich, scenario_year) %>%
  arrange(desc(pct_landscape_area), .by_group = TRUE) %>%
  mutate(
    landscape_rank = row_number()
  ) %>%
  ungroup()

landscape_order <- df_rank_rich %>%
  group_by(landscape_short) %>%
  summarise(mean_rank = mean(landscape_rank), .groups = "drop") %>%
  arrange(mean_rank) %>%
  pull(landscape_short)

df_rank_rich2 <- df_rank_rich %>%
  mutate(
    landscape_short = factor(landscape_short, levels = rev(landscape_order))
  )

# plot
p_rank_rich <- ggplot(
  df_rank_rich2,
  aes(
    x = scenario_year,
    y = landscape_short,
    fill = pct_landscape_area
  )
) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colours = c(
      "#7f0000",
      "#d7301f",
      "#fc8d59",
      "#fee08b",
      "#d9ef8b",
      "#91cf60",
      "#00cc44"
    ),
    values = scales::rescale(c(0, 1, 5, 10, 20, 40, 70)),
    name = "% landscape\nrefugia richness"
  ) +
  facet_wrap(~ rich) +
  labs(
    x = "Scenario and year",
    y = "Landscape",
    title = "Ex situ refugia richness ranked by landscape"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 9)
  )

print(p_rank_rich)


write.csv(
  df_exsitu_rich_values,
  "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/landscape_results/exsitu_refugia_richness_by_landscape.csv",
  row.names = FALSE
)

# ggsave(
# "exsitu_refugia_richness_heatmap.png",
# p_rank_rich,
#  width = 14,
# height = 10,
# dpi = 300
# )



# Do ranking by combining insitu + exsitu refugia 

insitu_refugia_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/stats_results/change_detection/refugia_rasters"

insitu_refugia_files <- list.files(
  path = insitu_refugia_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = FALSE
)

insitu_refugia_files


# landscapes already read with st_read(...)
land_v <- vect(landscapes)

# remove Mount Cameroon-Korup-Bakossi
land_v <- land_v[land_v$NAME != "Mount Cameroon-Korup-Bakossi", ]

land_area <- data.frame(
  landscape = land_v$NAME,
  landscape_area_km2 = as.numeric(expanse(land_v, unit = "km"))
)

print(land_area)

#  prepare a helper function
cell_area_km2 <- function(r) {
  rr <- terra::res(r)
  abs(rr[1] * rr[2]) / 1e6
}

parse_insitu_name <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  # example:
  # Atherurus_africanus_refugia_2020_to_2050_ssp126
  
  m <- regexec("^(.*)_refugia_2020_to_(\\d{4})_(ssp\\d{3})$", nm)
  hit <- regmatches(nm, m)[[1]]
  
  if (length(hit) == 0) {
    return(NULL)
  }
  
  list(
    species_us = hit[2],
    year = as.integer(hit[3]),
    scenario = hit[4]
  )
}

parse_exsitu_name <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  
  # try format like:
  # Atherurus africanus_mean_pixel_binary_2050s_ssp126_outside_range_only
  m1 <- regexec("^(.*)_mean_pixel_binary_(\\d{4})s_(ssp\\d{3})_outside_range_only$", nm)
  hit1 <- regmatches(nm, m1)[[1]]
  
  if (length(hit1) > 0) {
    sp_space <- hit1[2]
    return(list(
      species_us = gsub(" ", "_", sp_space),
      year = as.integer(hit1[3]),
      scenario = hit1[4]
    ))
  }
  
  # try format like:
  # Atherurus africanus_binary_overlap_2050_ssp126_cropped_masked_to_range
  m2 <- regexec("^(.*)_binary_overlap_(\\d{4})_(ssp\\d{3}).*$", nm)
  hit2 <- regmatches(nm, m2)[[1]]
  
  if (length(hit2) > 0) {
    sp_space <- hit2[2]
    return(list(
      species_us = gsub(" ", "_", sp_space),
      year = as.integer(hit2[3]),
      scenario = hit2[4]
    ))
  }
  
  return(NULL)
}


# build look up tales for insitu and exsitu refugia
insitu_meta <- lapply(insitu_refugia_files, parse_insitu_name)
insitu_meta <- insitu_meta[!sapply(insitu_meta, is.null)]

insitu_lookup <- data.frame(
  file = insitu_refugia_files[!sapply(lapply(insitu_refugia_files, parse_insitu_name), is.null)],
  species_us = sapply(insitu_meta, `[[`, "species_us"),
  year = sapply(insitu_meta, `[[`, "year"),
  scenario = sapply(insitu_meta, `[[`, "scenario"),
  stringsAsFactors = FALSE
)

exsitu_meta <- lapply(exsitu_refugia_files, parse_exsitu_name)
exsitu_meta <- exsitu_meta[!sapply(exsitu_meta, is.null)]

exsitu_lookup <- data.frame(
  file = exsitu_refugia_files[!sapply(lapply(exsitu_refugia_files, parse_exsitu_name), is.null)],
  species_us = sapply(exsitu_meta, `[[`, "species_us"),
  year = sapply(exsitu_meta, `[[`, "year"),
  scenario = sapply(exsitu_meta, `[[`, "scenario"),
  stringsAsFactors = FALSE
)

print(insitu_lookup)
print(exsitu_lookup)

# Function to calculate combined refugia by landscape
calc_combined_refugia_landscape <- function(r_in, r_ex, species_us, year, scenario, land_v, land_area) {
  
  # reproject landscapes if needed
  if (!same.crs(r_in, land_v)) {
    land_proj <- project(land_v, crs(r_in))
  } else {
    land_proj <- land_v
  }
  
  # align rasters if needed
  if (!isTRUE(compareGeom(r_in, r_ex, stopOnError = FALSE))) {
    r_ex <- resample(r_ex, r_in, method = "near")
  }
  
  a_cell <- cell_area_km2(r_in)
  
  # convert to 0/1 safe layers
  rin01 <- ifel(is.na(r_in), 0, ifel(r_in > 0, 1, 0))
  rex01 <- ifel(is.na(r_ex), 0, ifel(r_ex > 0, 1, 0))
  
  # union of in situ + ex situ refugia
  r_comb <- ifel((rin01 + rex01) > 0, 1, 0)
  names(r_comb) <- paste0(species_us, "_combined_refugia_", year, "_", scenario)
  
  # total combined refugia for this species
  species_total_cells <- as.numeric(global(r_comb == 1, "sum", na.rm = TRUE)[1, 1])
  species_total_km2 <- species_total_cells * a_cell
  
  # extract by landscape
  e <- terra::extract(r_comb, land_proj)
  names(e)[2] <- "value"
  
  landscape_cells <- tapply(e$value == 1, e$ID, sum, na.rm = TRUE)
  landscape_cells[is.na(landscape_cells)] <- 0
  
  df <- data.frame(
    ID = as.integer(names(landscape_cells)),
    combined_refugia_km2_in_landscape = as.numeric(landscape_cells) * a_cell
  )
  
  df$landscape <- land_proj$NAME[df$ID]
  
  df <- df %>%
    left_join(land_area, by = "landscape") %>%
    mutate(
      species = gsub("_", " ", species_us),
      year = year,
      scenario = scenario,
      species_total_combined_refugia_km2 = species_total_km2,
      pct_landscape_combined_refugia = ifelse(
        landscape_area_km2 > 0,
        combined_refugia_km2_in_landscape / landscape_area_km2 * 100,
        NA_real_
      ),
      pct_species_combined_refugia_covered_by_landscape = ifelse(
        species_total_combined_refugia_km2 > 0,
        combined_refugia_km2_in_landscape / species_total_combined_refugia_km2 * 100,
        NA_real_
      )
    ) %>%
   dplyr::select(
      species, landscape, year, scenario,
      landscape_area_km2,
      species_total_combined_refugia_km2,
      combined_refugia_km2_in_landscape,
      pct_landscape_combined_refugia,
      pct_species_combined_refugia_covered_by_landscape
    )
  
  list(df = df, raster = r_comb)
}

# loop through matching species by year and scenario
out_combined_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/combined_refugia_landscape"
dir.create(out_combined_dir, recursive = TRUE, showWarnings = FALSE)

out_combined_raster_dir <- file.path(out_combined_dir, "combined_refugia_rasters")
dir.create(out_combined_raster_dir, recursive = TRUE, showWarnings = FALSE)

results <- list()

keys <- inner_join(
  insitu_lookup,
  exsitu_lookup,
  by = c("species_us", "year", "scenario"),
  suffix = c("_insitu", "_exsitu")
)

print(keys)

for (i in seq_len(nrow(keys))) {
  
  cat("Processing:", i, "of", nrow(keys), "\n")
  
  r_in <- rast(keys$file_insitu[i])
  r_ex <- rast(keys$file_exsitu[i])
  
  tmp <- calc_combined_refugia_landscape(
    r_in = r_in,
    r_ex = r_ex,
    species_us = keys$species_us[i],
    year = keys$year[i],
    scenario = keys$scenario[i],
    land_v = land_v,
    land_area = land_area
  )
  
  # save combined raster
  out_r <- file.path(
    out_combined_raster_dir,
    paste0(keys$species_us[i], "_combined_refugia_", keys$year[i], "_", keys$scenario[i], ".tif")
  )
  writeRaster(tmp$raster, out_r, overwrite = TRUE)
  
  results[[i]] <- tmp$df
}

df_combined_landscape <- bind_rows(results)

print(df_combined_landscape)

# save csv
out_csv <- file.path(out_combined_dir, "combined_refugia_by_landscape.csv")
write.csv(df_combined_landscape, out_csv, row.names = FALSE)
cat("Saved:", out_csv, "\n")

# prepare plot data
df_plot <- df_combined_landscape %>%
  mutate(
    landscape_short = dplyr::recode(
      landscape,
      "Campo-Ma'an" = "Campo",
      "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
      "Bas-Oogué" = "Bas-Ogoue",
      "Gamba-Mayumba-Conkouati" = "Gamba",
      "Lac Télé-Lac Tumba" = "LacTele",
      "Shanga Trii-National" = "TNS"
    ),
    scenario_year = paste(year, scenario, sep = "_")
  )

print(df_plot)

# rank
df_rank_species <- df_plot %>%
  group_by(species, scenario_year) %>%
  arrange(desc(pct_landscape_combined_refugia), .by_group = TRUE) %>%
  mutate(
    landscape_rank = row_number()
  ) %>%
  ungroup()

print(df_rank_species)


landscape_order <- df_rank_species %>%
  group_by(landscape_short) %>%
  summarise(mean_rank = mean(landscape_rank, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_rank) %>%
  pull(landscape_short)

df_rank_species2 <- df_rank_species %>%
  mutate(
    landscape_short = factor(landscape_short, levels = rev(landscape_order))
  )


p_rank_species <- ggplot(
  df_rank_species2,
  aes(
    x = scenario_year,
    y = landscape_short,
    fill = pct_landscape_combined_refugia
  )
) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colours = c(
      "#7f0000",
      "#d7301f",
      "#fc8d59",
      "#fee08b",
      "#d9ef8b",
      "#91cf60",
      "#00cc44"
    ),
    values = scales::rescale(c(0, 1, 5, 10, 20, 40, 70)),
    name = "% landscape\ncombined refugia"
  ) +
  facet_wrap(~ species) +
  labs(
    x = "Scenario and year",
    y = "Landscape",
    title = "Combined in situ + ex situ refugia ranked by landscape category"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 9)
  )

print(p_rank_species)



# rank mean biodiversity
df_mean_biodiv <- df_plot %>%
  group_by(landscape_short, scenario_year) %>%
  summarise(
    mean_pct_landscape_combined_refugia = mean(pct_landscape_combined_refugia, na.rm = TRUE),
    .groups = "drop"
  )

landscape_order_mean <- df_mean_biodiv %>%
  group_by(landscape_short) %>%
  summarise(mean_value = mean(mean_pct_landscape_combined_refugia, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_value) %>%
  pull(landscape_short)

df_mean_biodiv2 <- df_mean_biodiv %>%
  mutate(
    landscape_short = factor(landscape_short, levels = rev(landscape_order_mean))
  )

p_rank_mean <- ggplot(
  df_mean_biodiv2,
  aes(
    x = scenario_year,
    y = landscape_short,
    fill = mean_pct_landscape_combined_refugia
  )
) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colours = c(
      "#7f0000",
      "#d7301f",
      "#fc8d59",
      "#fee08b",
      "#d9ef8b",
      "#91cf60",
      "#00cc44"
    ),
    values = scales::rescale(c(0, 1, 5, 10, 20, 40, 70)),
    name = "Mean % landscape\ncombined refugia"
  ) +
  labs(
    x = "Scenario and year",
    y = "Landscape",
    title = "Mean biodiversity of combined in situ + ex situ refugia by landscape"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_rank_mean)




# compute ranking for insitu + exsitu refugia richness combined

insitu_refugia_richness_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/stats_results/richness_outputs"

insitu_refugia_richness_files <- list.files(
  path = insitu_refugia_richness_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = FALSE
)

insitu_refugia_richness_files

# keep only future in insitu richness
insitu_refugia_richness_files2 <- insitu_refugia_richness_files[
  !grepl("2020_current", basename(insitu_refugia_richness_files))
]

print(insitu_refugia_richness_files2)

# define a helper function for calculating area
cell_area_km2 <- function(r) {
  rr <- res(r)
  abs(rr[1] * rr[2]) / 1e6
}

parse_insitu_rich_name <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  # richness_pos_2050_ssp126
  
  m <- regexec("^richness_pos_(\\d{4})_(ssp\\d{3})$", nm)
  hit <- regmatches(nm, m)[[1]]
  
  if (length(hit) == 0) return(NULL)
  
  list(
    year = as.integer(hit[2]),
    scenario = hit[3]
  )
}

parse_exsitu_rich_name <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  # exsitu_richness_pos_2050_ssp126
  
  m <- regexec("^exsitu_richness_pos_(\\d{4})_(ssp\\d{3})$", nm)
  hit <- regmatches(nm, m)[[1]]
  
  if (length(hit) == 0) return(NULL)
  
  list(
    year = as.integer(hit[2]),
    scenario = hit[3]
  )
}


# build look up tables
insitu_meta <- lapply(insitu_refugia_richness_files2, parse_insitu_rich_name)
insitu_keep <- !sapply(insitu_meta, is.null)

insitu_lookup <- data.frame(
  file = insitu_refugia_richness_files2[insitu_keep],
  year = sapply(insitu_meta[insitu_keep], `[[`, "year"),
  scenario = sapply(insitu_meta[insitu_keep], `[[`, "scenario"),
  stringsAsFactors = FALSE
)

exsitu_meta <- lapply(exsitu_refugia_richness_files, parse_exsitu_rich_name)
exsitu_keep <- !sapply(exsitu_meta, is.null)

exsitu_lookup <- data.frame(
  file = exsitu_refugia_richness_files[exsitu_keep],
  year = sapply(exsitu_meta[exsitu_keep], `[[`, "year"),
  scenario = sapply(exsitu_meta[exsitu_keep], `[[`, "scenario"),
  stringsAsFactors = FALSE
)

print(insitu_lookup)
print(exsitu_lookup)

# match insitu and exsitu rasters
rich_keys <- inner_join(
  insitu_lookup,
  exsitu_lookup,
  by = c("year", "scenario"),
  suffix = c("_insitu", "_exsitu")
)

print(rich_keys)


# Function to calculate combined richness by landscape
calc_combined_refugia_richness_landscape <- function(r_in, r_ex, year, scenario, land_v, land_area) {
  
  if (!same.crs(r_in, land_v)) {
    land_proj <- project(land_v, crs(r_in))
  } else {
    land_proj <- land_v
  }
  
  if (!isTRUE(compareGeom(r_in, r_ex, stopOnError = FALSE))) {
    r_ex <- resample(r_ex, r_in, method = "near")
  }
  
  # replace NA with 0 before combining
  rin0 <- ifel(is.na(r_in), 0, r_in)
  rex0 <- ifel(is.na(r_ex), 0, r_ex)
  
  # combined richness
  r_comb <- rin0 + rex0
  
  # remove zeros
  r_comb_pos <- ifel(r_comb == 0, NA, r_comb)
  names(r_comb_pos) <- paste0("combined_refugia_richness_", year, "_", scenario)
  
  a_cell <- cell_area_km2(r_comb_pos)
  
  e <- terra::extract(r_comb_pos, land_proj)
  names(e)[2] <- "rich"
  
  df <- e %>%
    filter(!is.na(rich)) %>%
    group_by(ID, rich) %>%
    summarise(
      n_cells = n(),
      .groups = "drop"
    ) %>%
    mutate(
      area_km2 = n_cells * a_cell,
      landscape = land_proj$NAME[ID],
      year = year,
      scenario = scenario
    ) %>%
    left_join(land_area, by = "landscape") %>%
    mutate(
      pct_landscape_area = ifelse(
        landscape_area_km2 > 0,
        area_km2 / landscape_area_km2 * 100,
        NA_real_
      )
    ) %>%
   dplyr::select(
      ID, rich, n_cells, area_km2,
      landscape, landscape_area_km2,
      pct_landscape_area,
      year, scenario
    )
  
  list(df = df, raster = r_comb_pos)
}

# run the workflow
out_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/combined_refugia_richness_landscape"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_raster_dir <- file.path(out_dir, "combined_richness_rasters")
dir.create(out_raster_dir, recursive = TRUE, showWarnings = FALSE)

results <- list()

for (i in seq_len(nrow(rich_keys))) {
  
  cat("Processing", i, "of", nrow(rich_keys), "\n")
  
  r_in <- rast(rich_keys$file_insitu[i])
  r_ex <- rast(rich_keys$file_exsitu[i])
  
  tmp <- calc_combined_refugia_richness_landscape(
    r_in = r_in,
    r_ex = r_ex,
    year = rich_keys$year[i],
    scenario = rich_keys$scenario[i],
    land_v = land_v,
    land_area = land_area
  )
  
  out_r <- file.path(
    out_raster_dir,
    paste0("combined_refugia_richness_pos_", rich_keys$year[i], "_", rich_keys$scenario[i], ".tif")
  )
  
  writeRaster(tmp$raster, out_r, overwrite = TRUE)
  
  results[[i]] <- tmp$df
}

df_combined_rich_landscape <- bind_rows(results)

print(df_combined_rich_landscape)

# save results
out_csv <- file.path(out_dir, "combined_refugia_richness_by_landscape.csv")
write.csv(df_combined_rich_landscape, out_csv, row.names = FALSE)
cat("Saved:", out_csv, "\n")

# prepare plot data
df_plot <- df_combined_rich_landscape %>%
  mutate(
    landscape_short = dplyr::recode(
      landscape,
      "Campo-Ma'an" = "Campo",
      "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
      "Bas-Oogué" = "Bas-Ogoue",
      "Gamba-Mayumba-Conkouati" = "Gamba",
      "Lac Télé-Lac Tumba" = "LacTele",
      "Shanga Trii-National" = "TNS"
    ),
    scenario_year = paste(year, scenario, sep = "_")
  )

print(df_plot, n = Inf)

# rank
df_rank_rich <- df_plot %>%
  group_by(rich, scenario_year) %>%
  arrange(desc(pct_landscape_area), .by_group = TRUE) %>%
  mutate(
    landscape_rank = row_number()
  ) %>%
  ungroup()

print(df_rank_rich, n = Inf)

landscape_order <- df_rank_rich %>%
  group_by(landscape_short) %>%
  summarise(mean_rank = mean(landscape_rank, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_rank) %>%
  pull(landscape_short)

df_rank_rich2 <- df_rank_rich %>%
  mutate(
    landscape_short = factor(landscape_short, levels = rev(landscape_order))
  )


# plot ranked heat map by richness value
p_rank_rich <- ggplot(
  df_rank_rich2,
  aes(
    x = scenario_year,
    y = landscape_short,
    fill = pct_landscape_area
  )
) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colours = c(
      "#7f0000",
      "#d7301f",
      "#fc8d59",
      "#fee08b",
      "#d9ef8b",
      "#91cf60",
      "#00cc44"
    ),
    values = scales::rescale(c(0, 0.5, 1, 2, 5, 10, 20)),
    name = "% landscape\ncombined refugia richness"
  ) +
  facet_wrap(~ rich) +
  labs(
    x = "Scenario and year",
    y = "Landscape",
    title = "Combined in situ + ex situ refugia richness ranked by landscape"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 9)
  )

print(p_rank_rich)



# calculate and rank top 10% sites

top10_results <- list()

for (i in seq_len(nrow(rich_keys))) {
  
  cat("Processing:", i, "of", nrow(rich_keys), "\n")
  
  r_in <- rast(rich_keys$file_insitu[i])
  r_ex <- rast(rich_keys$file_exsitu[i])
  
  # make CRS match
  if (!same.crs(r_in, r_ex)) {
    r_ex <- project(r_ex, r_in, method = "near")
  }
  
  # make geometry match
  if (!isTRUE(compareGeom(r_in, r_ex, stopOnError = FALSE))) {
    r_ex <- resample(r_ex, r_in, method = "near")
  }
  
  # replace NA with 0
  rin0 <- ifel(is.na(r_in), 0, r_in)
  rex0 <- ifel(is.na(r_ex), 0, r_ex)
  
  # combined richness
  r_comb <- rin0 + rex0
  
  values_vec <- values(r_comb, mat = FALSE)
  values_vec <- values_vec[!is.na(values_vec) & values_vec > 0]
  
  if (length(values_vec) == 0) {
    warning("No positive richness values for: ", rich_keys$year[i], "_", rich_keys$scenario[i])
    next
  }
  
  threshold <- as.numeric(quantile(values_vec, 0.90, na.rm = TRUE))
  
  cat("Top10% threshold:", threshold, "\n")
  
  r_top10 <- ifel(r_comb >= threshold, 1, NA)
  names(r_top10) <- paste0("top10_richness_", rich_keys$year[i], "_", rich_keys$scenario[i])
  
  a_cell <- prod(res(r_top10)) / 1e6
  
  if (!same.crs(r_top10, land_v)) {
    land_proj <- project(land_v, crs(r_top10))
  } else {
    land_proj <- land_v
  }
  
  e <- terra::extract(r_top10, land_proj)
  names(e)[2] <- "value"
  
  df <- e %>%
    filter(!is.na(value)) %>%
    group_by(ID) %>%
    summarise(
      n_cells = n(),
      .groups = "drop"
    ) %>%
    mutate(
      area_km2 = n_cells * a_cell,
      landscape = land_proj$NAME[ID],
      year = rich_keys$year[i],
      scenario = rich_keys$scenario[i]
    )
  
  top10_results[[i]] <- df
}


df_top10 <- bind_rows(top10_results)

df_top10 <- df_top10 %>%
  left_join(land_area, by = "landscape") %>%
  mutate(
    pct_landscape = (area_km2 / landscape_area_km2) * 100,
    scenario_year = paste(year, scenario, sep = "_")
  )

print(df_top10)


df_top10_rank <- df_top10 %>%
  group_by(scenario_year) %>%
  arrange(desc(pct_landscape), .by_group = TRUE) %>%
  mutate(
    landscape_rank = row_number()
  ) %>%
  ungroup()

print(df_top10_rank)


df_top10_rank <- df_top10_rank %>%
  mutate(
    landscape_short = recode(
      landscape,
      "Campo-Ma'an" = "Campo",
      "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
      "Bas-Oogué" = "Bas-Ogoue",
      "Gamba-Mayumba-Conkouati" = "Gamba",
      "Lac Télé-Lac Tumba" = "LacTele",
      "Shanga Trii-National" = "TNS"
    )
  )

landscape_order <- df_top10_rank %>%
  group_by(landscape_short) %>%
  summarise(mean_rank = mean(landscape_rank), .groups = "drop") %>%
  arrange(mean_rank) %>%
  pull(landscape_short)

df_top10_rank$landscape_short <- factor(
  df_top10_rank$landscape_short,
  levels = rev(landscape_order)
)


p_top10 <- ggplot(
  df_top10_rank,
  aes(
    x = scenario_year,
    y = landscape_short,
    fill = pct_landscape
  )
) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colours = c(
      "#7f0000",
      "#d7301f",
      "#fc8d59",
      "#fee08b",
      "#d9ef8b",
      "#91cf60",
      "#00cc44"
    ),
    values = scales::rescale(c(0, 1, 3, 6, 10)),
    name = "% landscape\nTop10 richness"
  ) +
  labs(
    x = "Scenario and year",
    y = "Landscape",
    title = "Top 10% Combined Refugia Richness Hotspots by Landscape"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_top10)





#--------------------------------------------------------------------------------------

# Calculate suitable, richness, and refugia area within protected areas

library(terra)
library(sf)
library(dplyr)

# INPUTS
# -----------------------------
PA <- st_read("F:/WWF_data/SDMs_LC_climate/PA/PA_clipped_to_study_area.shp", quiet = TRUE)

root_current <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/current"
root_future  <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/future_binary_clipped_to_species_range"

out_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/PA_results"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_csv <- file.path(out_dir, "suitable_area_by_PA.csv")

# -----------------------------
# List raster files
# -----------------------------
root_current_files <- list.files(
  path = root_current,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

root_future_files <- list.files(
  path = root_future,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(root_current_files) == 0) stop("No current rasters found.")
if (length(root_future_files) == 0) stop("No future rasters found.")

cat("Current rasters:", length(root_current_files), "\n")
cat("Future rasters:", length(root_future_files), "\n")

# -----------------------------
# Helpers
# -----------------------------
get_species_current <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub("_mean_pixel_binary$", "", nm)
}

get_species_future <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub("_binary_overlap_\\d{4}_ssp\\d+_cropped_masked_to_range$", "", nm)
}

get_year_future <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  as.integer(sub(".*_binary_overlap_(\\d{4})_ssp\\d+_cropped_masked_to_range$", "\\1", nm))
}

get_scenario_future <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub(".*_binary_overlap_\\d{4}_(ssp\\d+)_cropped_masked_to_range$", "\\1", nm)
}

cell_area_km2 <- function(r) {
  rr <- terra::res(r)
  abs(rr[1] * rr[2]) / 1e6
}

total_suitable_km2 <- function(r) {
  a_cell <- cell_area_km2(r)
  n_suitable <- as.numeric(terra::global(r == 1, "sum", na.rm = TRUE)[1, 1])
  n_suitable * a_cell
}

# -----------------------------
# Main helper: suitable area by PA
# -----------------------------
calc_pa_area <- function(r, species, year, scenario, pa_sf) {
  
  pa_v <- terra::vect(pa_sf)
  
  # Project PA polygons to raster CRS if needed
  if (!terra::same.crs(r, pa_v)) {
    pa_v <- terra::project(pa_v, terra::crs(r))
  }
  
  # PA areas in km2
  pa_area_km2 <- as.numeric(terra::expanse(pa_v, unit = "km"))
  
  # Total suitable area in the full raster
  species_total_suitable_km2 <- as.numeric(total_suitable_km2(r))
  
  # Raster cell area
  a_cell <- cell_area_km2(r)
  
  # Extract raster values by PA
  e <- terra::extract(r, pa_v)
  names(e)[2] <- "value"
  
  # Count suitable cells (value == 1) per PA
  suitable_counts <- tapply(e$value == 1, e$ID, sum, na.rm = TRUE)
  
  # Keep all PAs, even if zero
  all_ids <- seq_len(nrow(pa_v))
  suitable_counts_full <- rep(0, length(all_ids))
  names(suitable_counts_full) <- all_ids
  
  if (length(suitable_counts) > 0) {
    suitable_counts_full[names(suitable_counts)] <- suitable_counts
  }
  
  suitable_km2 <- as.numeric(suitable_counts_full) * a_cell
  
  # Percent of each PA that is suitable
  pct_PA_suitable <- rep(NA_real_, length(suitable_km2))
  ok_pa <- pa_area_km2 > 0
  pct_PA_suitable[ok_pa] <- (suitable_km2[ok_pa] / pa_area_km2[ok_pa]) * 100
  
  # Percent of total species suitable habitat covered by each PA
  if (species_total_suitable_km2 > 0) {
    pct_species_suitable_covered_by_PA <- (suitable_km2 / species_total_suitable_km2) * 100
  } else {
    pct_species_suitable_covered_by_PA <- rep(NA_real_, length(suitable_km2))
  }
  
  df <- data.frame(
    species = species,
    PA = pa_v$PA,
    year = year,
    scenario = scenario,
    PA_area_km2 = pa_area_km2,
    species_total_suitable_km2 = species_total_suitable_km2,
    suitable_km2_in_PA = suitable_km2,
    pct_PA_suitable = pct_PA_suitable,
    pct_species_suitable_covered_by_PA = pct_species_suitable_covered_by_PA,
    stringsAsFactors = FALSE
  )
  
  return(df)
}

# -----------------------------
# 1) CURRENT
# -----------------------------
current_results <- vector("list", length(root_current_files))

for (i in seq_along(root_current_files)) {
  f <- root_current_files[i]
  cat("Processing current:", i, "of", length(root_current_files), "\n")
  
  r <- terra::rast(f)
  species <- get_species_current(f)
  
  current_results[[i]] <- calc_pa_area(
    r = r,
    species = species,
    year = 2020,
    scenario = "current",
    pa_sf = PA
  )
  
  rm(r)
  gc()
}

df_current <- do.call(rbind, current_results)

# -----------------------------
# 2) FUTURE
# -----------------------------
future_results <- vector("list", length(root_future_files))

for (i in seq_along(root_future_files)) {
  f <- root_future_files[i]
  cat("Processing future:", i, "of", length(root_future_files), "\n")
  
  r <- terra::rast(f)
  species <- get_species_future(f)
  year <- get_year_future(f)
  scenario <- get_scenario_future(f)
  
  future_results[[i]] <- calc_pa_area(
    r = r,
    species = species,
    year = year,
    scenario = scenario,
    pa_sf = PA
  )
  
  rm(r)
  gc()
}

df_future <- do.call(rbind, future_results)

# -----------------------------
# Combine + Save
# -----------------------------
df_all <- rbind(df_current, df_future)
df_all <- df_all[order(df_all$species, df_all$year, df_all$scenario, df_all$PA), ]

print(df_all)

write.csv(df_all, out_csv, row.names = FALSE)

cat("\nSaved:", out_csv, "\n")


# plot results
library(dplyr)
library(tidyr)
library(ggplot2)

# -----------------------------------
# Pick one species
# -----------------------------------
sp_pick <- "Atherurus africanus"

# -----------------------------------
# Summarize by species, PA, year, scenario
# -----------------------------------
plot_df <- df_all %>%
  filter(species == sp_pick) %>%
  group_by(species, PA, year, scenario) %>%
  summarise(
    PA_area_km2 = sum(PA_area_km2, na.rm = TRUE),
    species_total_suitable_km2 = first(species_total_suitable_km2),
    suitable_km2_in_PA = sum(suitable_km2_in_PA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_PA_suitable = ifelse(
      PA_area_km2 > 0,
      (suitable_km2_in_PA / PA_area_km2) * 100,
      NA_real_
    ),
    pct_species_suitable_covered_by_PA = ifelse(
      species_total_suitable_km2 > 0,
      (suitable_km2_in_PA / species_total_suitable_km2) * 100,
      NA_real_
    ),
    PA_short = recode(
      PA,
      "Community Hunting Zone" = "CHZ",
      "Forest Management Unit" = "FMU",
      "National Park" = "NP",
      "Wildlife Sanctuary" = "WS"
    )
  )

# Order PA panels
plot_df$PA_short <- factor(plot_df$PA_short, levels = c("NP", "WS", "FMU", "CHZ"))

# -----------------------------------
# Duplicate current rows so each future scenario starts from 2020
# -----------------------------------
current_dup <- plot_df %>%
  filter(scenario == "current") %>%
 dplyr::select(-scenario) %>%
  tidyr::crossing(scenario = c("ssp126", "ssp245", "ssp585"))

future_only <- plot_df %>%
  filter(scenario != "current")

plot_df2 <- bind_rows(current_dup, future_only) %>%
  mutate(
    year = as.integer(year),
    scenario = factor(scenario, levels = c("ssp126", "ssp245", "ssp585"))
  ) %>%
  arrange(PA_short, scenario, year)

# -----------------------------------
# Plot 1: % suitable habitat within each PA category
# -----------------------------------
p1 <- ggplot(
  plot_df2,
  aes(
    x = year,
    y = pct_PA_suitable,
    color = scenario,
    group = scenario
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  facet_wrap(~ PA_short, scales = "free_y") +
  scale_x_continuous(breaks = c(2020, 2050, 2070, 2100)) +
  scale_color_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    x = "Year",
    y = "% Suitable habitat per PA area",
    color = "Scenario",
    title = paste("Percentage Suitable Habitat per Protected Area Category:", sp_pick)
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10)
  )

print(p1)

# -----------------------------------
# Plot 2: % of total species suitable habitat covered by each PA category
# -----------------------------------
p2 <- ggplot(
  plot_df2,
  aes(
    x = year,
    y = pct_species_suitable_covered_by_PA,
    color = scenario,
    group = scenario
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  facet_wrap(~ PA_short, scales = "free_y") +
  scale_x_continuous(breaks = c(2020, 2050, 2070, 2100)) +
  scale_color_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    x = "Year",
    y = "% Species suitable habitat covered",
    color = "Scenario",
    title = paste("Percentage of Species Suitable Habitat Covered by PA Category:", sp_pick)
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10)
  )

print(p2)


# Rank mean and total retained suitable habitats for all species within landscapes

# -----------------------------------
# Split current and future
# -----------------------------------
df_current_PA <- df_all %>%
  filter(year == 2020, scenario == "current") %>%
  group_by(species, PA) %>%
  summarise(
    current_suitable_km2 = sum(suitable_km2_in_PA, na.rm = TRUE),
    .groups = "drop"
  )

df_future_PA <- df_all %>%
  filter(year != 2020, scenario %in% c("ssp126", "ssp245", "ssp585")) %>%
  group_by(species, PA, year, scenario) %>%
  summarise(
    future_suitable_km2 = sum(suitable_km2_in_PA, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------------
# Join current to future
# -----------------------------------
df_retained <- df_future_PA %>%
  left_join(df_current_PA, by = c("species", "PA")) %>%
  mutate(
    retained_pct = ifelse(
      current_suitable_km2 > 0,
      (future_suitable_km2 / current_suitable_km2) * 100,
      NA_real_
    ),
    retained_change_km2 = future_suitable_km2 - current_suitable_km2
  )

print(df_retained)

unique(df_retained$PA)

# -----------------------------------
# Rank by mean retention across species
# -----------------------------------
rank_PA_mean <- df_retained %>%
  group_by(year, scenario, PA) %>%
  summarise(
    n_species = sum(!is.na(retained_pct)),
    mean_retained_pct = mean(retained_pct, na.rm = TRUE),
    median_retained_pct = median(retained_pct, na.rm = TRUE),
    mean_change_km2 = mean(retained_change_km2, na.rm = TRUE),
    total_future_km2 = sum(future_suitable_km2, na.rm = TRUE),
    total_current_km2 = sum(current_suitable_km2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(year, scenario) %>%
  arrange(desc(mean_retained_pct), .by_group = TRUE) %>%
  mutate(rank_mean_retention = row_number()) %>%
  ungroup()

print(rank_PA_mean)

# -----------------------------------
# Rank by total retention
# -----------------------------------
rank_PA_total <- df_retained %>%
  group_by(year, scenario, PA) %>%
  summarise(
    total_future_km2 = sum(future_suitable_km2, na.rm = TRUE),
    total_current_km2 = sum(current_suitable_km2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    total_retained_pct = ifelse(
      total_current_km2 > 0,
      (total_future_km2 / total_current_km2) * 100,
      NA_real_
    ),
    total_change_km2 = total_future_km2 - total_current_km2
  ) %>%
  group_by(year, scenario) %>%
  arrange(desc(total_retained_pct), .by_group = TRUE) %>%
  mutate(rank_total_retention = row_number()) %>%
  ungroup()

print(rank_PA_total)

# -----------------------------------
# Prepare labels for plotting
# -----------------------------------
rank_PA_mean$PA_short <- dplyr::recode(
  rank_PA_mean$PA,
  "Community Hunting Zone" = "CHZ",
  "Forest Management Unit" = "FMU",
  "National Park" = "NP",
  "Wildlife Sanctuary" = "WS"
)

rank_PA_mean$label <- paste(rank_PA_mean$year, rank_PA_mean$scenario, sep = "_")

# optional ordering
rank_PA_mean$PA_short <- factor(rank_PA_mean$PA_short, levels = c("NP", "WS", "FMU", "CHZ"))

# -----------------------------------
# Heatmap: ranking by mean retained %
# -----------------------------------
p_rank_heat <- ggplot(
  rank_PA_mean,
  aes(x = label, y = reorder(PA_short, mean_retained_pct), fill = mean_retained_pct)
) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(mean_retained_pct, 1)), size = 3) +
  labs(
    x = "Scenario-Year",
    y = "Protected Area category",
    fill = "Mean retained %",
    title = "Ranking of Protected Area Categories Retaining Average Suitable Habitat"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_rank_heat)

ggsave(
  file.path(out_dir, "PA_ranking_heatmap_mean_retained.png"),
  p_rank_heat,
  width = 10,
  height = 6,
  dpi = 300
)


# best plot

# -----------------------------------
# Prepare data

library(scales)

# -----------------------------------
# Prepare retained data for PA ranking
# -----------------------------------
df_retained2 <- df_retained %>%
  mutate(
    PA_short = dplyr::recode(
      PA,
      "Community Hunting Zone" = "CHZ",
      "Forest Management Unit" = "FMU",
      "National Park" = "NP",
      "Wildlife Sanctuary" = "WS"
    ),
    scenario_year = paste(year, scenario, sep = "_")
  )

print(df_retained2)

# -----------------------------------
# Mean retained biodiversity by PA category
# -----------------------------------
rank_PA_mean <- df_retained2 %>%
  group_by(PA, PA_short, year, scenario, scenario_year) %>%
  summarise(
    mean_retained_pct = mean(retained_pct, na.rm = TRUE),
    .groups = "drop"
  )

print(rank_PA_mean)

# -----------------------------------
# Create one fixed PA order
# -----------------------------------
PA_order <- rank_PA_mean %>%
  group_by(PA_short) %>%
  summarise(
    mean_rank_value = mean(mean_retained_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_rank_value) %>%
  pull(PA_short)

rank_PA_mean2 <- rank_PA_mean %>%
  mutate(
    PA_short = factor(PA_short, levels = rev(PA_order))
  )

# -----------------------------------
# Plot heatmap with one shared y-axis
# -----------------------------------
p_rank_heat <- ggplot(
  rank_PA_mean2,
  aes(
    x = scenario_year,
    y = PA_short,
    fill = mean_retained_pct
  )
) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(mean_retained_pct, 1)), size = 3) +
  scale_fill_gradientn(
    colours = c(
      "#7f0000",
      "#d7301f",
      "#fc8d59",
      "#fee08b",
      "#d9ef8b",
      "#91cf60",
      "#00cc44"
    ),
    values = scales::rescale(c(0, 10, 25, 40, 60, 80, 100)),
    limits = c(0, 100),
    name = "Mean Refugia %"
  ) +
  labs(
    x = "Scenario and year",
    y = "Protected area category",
    title = "Mean In Situ Mammal Habitat Refugia Ranked by Protected Area Category"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_rank_heat)

ggsave(
  file.path(out_dir, "PA_ranking_heatmap_mean_retained.png"),
  p_rank_heat,
  width = 10,
  height = 6,
  dpi = 300
)


# save results
out_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/PA_results"

write.csv(
  rank_PA_mean2,
  file.path(out_dir, "PA_ranking_mean_retained_habitat.csv"),
  row.names = FALSE
)



# ------- Rank retained suitable habitat by individual species and PA ----------

library(dplyr)
library(ggplot2)

# ------- Rank retained suitable habitat by individual species and PA ----------

# -----------------------------------
# Current suitable habitat from df_all
# -----------------------------------
df_current_PA <- df_all %>%
  filter(year == 2020, scenario == "current") %>%
  group_by(species, PA) %>%
  summarise(
    current_suitable_km2 = sum(suitable_km2_in_PA, na.rm = TRUE),
    .groups = "drop"
  )

print(df_current_PA)

# -----------------------------------
# Future suitable habitat from df_all
# -----------------------------------
df_future_PA <- df_all %>%
  filter(year != 2020, scenario %in% c("ssp126", "ssp245", "ssp585")) %>%
  group_by(species, PA, year, scenario) %>%
  summarise(
    future_suitable_km2 = sum(suitable_km2_in_PA, na.rm = TRUE),
    .groups = "drop"
  )

print(df_future_PA)

# -----------------------------------
# Join current and future
# -----------------------------------
df_species_retention <- df_future_PA %>%
  left_join(df_current_PA, by = c("species", "PA")) %>%
  mutate(
    retained_pct = dplyr::case_when(
      !is.na(current_suitable_km2) & current_suitable_km2 > 0 ~
        (future_suitable_km2 / current_suitable_km2) * 100,
      TRUE ~ NA_real_
    )
  )

print(df_species_retention)

# -----------------------------------
# Rank PA category for each species
# -----------------------------------
rank_species_PA <- df_species_retention %>%
  group_by(species, year, scenario) %>%
  arrange(desc(retained_pct), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  ungroup()

print(rank_species_PA)

# -----------------------------------
# Save full ranking
# -----------------------------------
write.csv(
  rank_species_PA,
  "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/PA_results/species_PA_ranking.csv",
  row.names = FALSE
)

# -----------------------------------
# Select top-ranked PA per species
# -----------------------------------
top_PA_species <- rank_species_PA %>%
  filter(rank == 1)

print(top_PA_species)

# -----------------------------------
# Save top-ranked PA per species
# -----------------------------------
write.csv(
  top_PA_species,
  "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/PA_results/top_PA_per_species.csv",
  row.names = FALSE
)

# -----------------------------------
# Prepare labels for plotting
# -----------------------------------
plot_species_PA <- rank_species_PA %>%
  mutate(
    PA_short = dplyr::recode(
      PA,
      "Community Hunting Zone" = "CHZ",
      "Forest Management Unit" = "FMU",
      "National Park" = "NP",
      "Wildlife Sanctuary" = "WS"
    ),
    scenario_year = paste(year, scenario, sep = "_")
  )

# fixed PA order (ecological hierarchy)
plot_species_PA$PA_short <- factor(
  plot_species_PA$PA_short,
  levels = c("NP", "WS", "FMU", "CHZ")
)


# -----------------------------------
# Plot heat map
# -----------------------------------
p_species_heat <- ggplot(
  plot_species_PA,
  aes(
    x = scenario_year,
    y = PA_short,
    fill = retained_pct
  )
) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(retained_pct,1)), size = 2.7) +
  facet_wrap(~ species) +
  scale_fill_gradientn(
    colours = c(
      "#7f0000",
      "#d7301f",
      "#fc8d59",
      "#fee08b",
      "#d9ef8b",
      "#91cf60",
      "#00cc44"
    ),
    values = scales::rescale(c(0,10,25,40,60,80,100)),
    limits = c(0,100),
    na.value = "grey90",
    name = "% Refugia"
  ) +
  labs(
    x = "Future scenario",
    y = "Protected area category",
    title = "In Situ Mammal Habitat Refugia per Protected Area Category"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 9)
  )

print(p_species_heat)



# ---------Reproduce workflow for species richness----------

# INPUTS
# -----------------------------
PA <- st_read("F:/WWF_data/SDMs_LC_climate/PA/PA_clipped_to_study_area.shp", quiet = TRUE)

root_richness <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/stats_results/richness_outputs"

out_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/PA_results_richness"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_csv <- file.path(out_dir, "richness_area_by_PA.csv")

# -----------------------------
# List raster files
# -----------------------------
root_richness_files <- list.files(
  path = root_richness,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = FALSE
)

if (length(root_richness_files) == 0) stop("No richness rasters found.")

print(root_richness_files)

# -----------------------------
# Helpers
# -----------------------------
cell_area_km2 <- function(r) {
  rr <- terra::res(r)
  abs(rr[1] * rr[2]) / 1e6
}

get_year_richness <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  if (grepl("2020_current", nm)) return(2020L)
  as.integer(sub("^richness_pos_(\\d{4})_.*$", "\\1", nm))
}

get_scenario_richness <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  if (grepl("2020_current", nm)) return("current")
  sub("^richness_pos_\\d{4}_(ssp\\d+)$", "\\1", nm)
}

total_richness_area_km2 <- function(r) {
  a_cell <- cell_area_km2(r)
  n_cells <- as.numeric(terra::global(!is.na(r), "sum", na.rm = TRUE)[1, 1])
  n_cells * a_cell
}

# -----------------------------
# Main helper: richness by PA
# -----------------------------
calc_richness_PA_area <- function(r, year, scenario, pa_sf) {
  
  pa_v <- terra::vect(pa_sf)
  
  # project PAs to raster CRS if needed
  if (!terra::same.crs(r, pa_v)) {
    pa_v <- terra::project(pa_v, terra::crs(r))
  }
  
  PA_area_km2 <- as.numeric(terra::expanse(pa_v, unit = "km"))
  a_cell <- cell_area_km2(r)
  
  # full-raster richness area (>0 cells only, assuming zeros are NA already)
  total_rich_area_km2 <- total_richness_area_km2(r)
  
  # extract raster values by PA
  e <- terra::extract(r, pa_v)
  names(e)[2] <- "value"
  
  # count cells with richness > 0 / non-NA per PA
  rich_counts <- tapply(!is.na(e$value), e$ID, sum, na.rm = TRUE)
  
  # mean richness in occupied cells per PA
  mean_rich_occ <- tapply(e$value, e$ID, mean, na.rm = TRUE)
  
  # max richness per PA
  max_rich <- tapply(e$value, e$ID, max, na.rm = TRUE)
  
  all_ids <- seq_len(nrow(pa_v))
  
  rich_counts_full <- rep(0, length(all_ids))
  names(rich_counts_full) <- all_ids
  
  mean_rich_full <- rep(NA_real_, length(all_ids))
  names(mean_rich_full) <- all_ids
  
  max_rich_full <- rep(NA_real_, length(all_ids))
  names(max_rich_full) <- all_ids
  
  if (length(rich_counts) > 0) {
    rich_counts_full[names(rich_counts)] <- rich_counts
  }
  if (length(mean_rich_occ) > 0) {
    mean_rich_full[names(mean_rich_occ)] <- mean_rich_occ
  }
  if (length(max_rich) > 0) {
    max_rich_full[names(max_rich)] <- max_rich
  }
  
  richness_area_km2 <- as.numeric(rich_counts_full) * a_cell
  
  pct_PA_with_richness <- rep(NA_real_, length(richness_area_km2))
  ok_pa <- PA_area_km2 > 0
  pct_PA_with_richness[ok_pa] <- 
    (richness_area_km2[ok_pa] / PA_area_km2[ok_pa]) * 100
  
  if (total_rich_area_km2 > 0) {
    pct_total_richness_area_covered_by_PA <- 
      (richness_area_km2 / total_rich_area_km2) * 100
  } else {
    pct_total_richness_area_covered_by_PA <- rep(NA_real_, length(richness_area_km2))
  }
  
  data.frame(
    PA = pa_v$PA,
    year = year,
    scenario = scenario,
    PA_area_km2 = PA_area_km2,
    total_richness_area_km2 = total_rich_area_km2,
    richness_area_km2_in_PA = richness_area_km2,
    pct_PA_with_richness = pct_PA_with_richness,
    pct_total_richness_area_covered_by_PA = pct_total_richness_area_covered_by_PA,
    mean_richness_in_PA = as.numeric(mean_rich_full),
    max_richness_in_PA = as.numeric(max_rich_full),
    stringsAsFactors = FALSE
  )
}

# -----------------------------
# Run all rasters
# -----------------------------
richness_results <- vector("list", length(root_richness_files))

for (i in seq_along(root_richness_files)) {
  f <- root_richness_files[i]
  cat("Processing:", i, "of", length(root_richness_files), "\n")
  
  r <- terra::rast(f)
  year <- get_year_richness(f)
  scenario <- get_scenario_richness(f)
  
  richness_results[[i]] <- calc_richness_PA_area(
    r = r,
    year = year,
    scenario = scenario,
    pa_sf = PA
  )
  
  rm(r)
  gc()
}

df_rich_PA <- do.call(rbind, richness_results)
df_rich_PA <- df_rich_PA[order(df_rich_PA$year, df_rich_PA$scenario, df_rich_PA$PA), ]

print(df_rich_PA)

write.csv(df_rich_PA, out_csv, row.names = FALSE)

cat("\nSaved:", out_csv, "\n")



# plot
library(dplyr)
library(ggplot2)
library(scales)

# -----------------------------------
# Clean and summarise by PA category
# -----------------------------------
plot_rich_PA <- df_rich_PA %>%
  mutate(
    mean_richness_in_PA = ifelse(is.nan(mean_richness_in_PA), NA_real_, mean_richness_in_PA),
    max_richness_in_PA  = ifelse(is.infinite(max_richness_in_PA), NA_real_, max_richness_in_PA)
  ) %>%
  group_by(PA, year, scenario) %>%
  summarise(
    PA_area_km2 = sum(PA_area_km2, na.rm = TRUE),
    total_richness_area_km2 = first(total_richness_area_km2),
    richness_area_sum_km2 = sum(richness_area_km2_in_PA, na.rm = TRUE),
    mean_richness_in_PA = weighted.mean(
      x = mean_richness_in_PA,
      w = richness_area_km2_in_PA,
      na.rm = TRUE
    ),
    max_richness_in_PA = if (all(is.na(max_richness_in_PA))) NA_real_ else max(max_richness_in_PA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    richness_area_km2_in_PA = richness_area_sum_km2
  ) %>%
  mutate(
    PA_short = dplyr::recode(
      PA,
      "Community Hunting Zone" = "CHZ",
      "Forest Management Unit" = "FMU",
      "National Park" = "NP",
      "Wildlife Sanctuary" = "WS"
    ),
    scenario_year = paste(year, scenario, sep = "_")
  )

# -----------------------------------
# Set axis order
# -----------------------------------
plot_rich_PA$PA_short <- factor(
  plot_rich_PA$PA_short,
  levels = c("NP", "WS", "FMU", "CHZ")
)

plot_rich_PA$scenario_year <- factor(
  plot_rich_PA$scenario_year,
  levels = c(
    "2020_current",
    "2050_ssp126", "2050_ssp245", "2050_ssp585",
    "2070_ssp126", "2070_ssp245", "2070_ssp585",
    "2100_ssp126", "2100_ssp245", "2100_ssp585"
  )
)

print(plot_rich_PA, n = Inf)

# -----------------------------------
# Plot heat map of mean richness in PA
# -----------------------------------
p_rich_heat <- ggplot(
  plot_rich_PA,
  aes(
    x = scenario_year,
    y = PA_short,
    fill = richness_area_km2_in_PA
  )
) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(richness_area_km2_in_PA, 1)), size = 3) +
  scale_fill_gradientn(
    colours = c(
      "#f7fcf5",
      "#c7e9c0",
      "#74c476",
      "#238b45",
      "#00441b"
    ),
    na.value = "grey90",
    name = "Mean richness (km2)"
  ) +
  labs(
    x = "Scenario and year",
    y = "Protected area category",
    title = "Mean In Situ Species Richness in Protected Area Categories"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_rich_heat)


ggsave(
  file.path(out_dir, "mean_richness_heatmap_by_PA.png"),
  p_rich_heat,
  width = 10,
  height = 6,
  dpi = 300
)


# Ranking refugia richness

library(dplyr)
library(ggplot2)
library(scales)

# -----------------------------------
# Keep only future years and keep scenarios
# -----------------------------------
rank_rich_future <- plot_rich_PA %>%
  filter(year %in% c(2050, 2070, 2100)) %>%
  mutate(
    scenario_year = paste(year, scenario, sep = "_")
  ) %>%
  group_by(PA, PA_short, year, scenario, scenario_year) %>%
  summarise(
    mean_richness = mean(richness_area_km2_in_PA, na.rm = TRUE),
    .groups = "drop"
  )

print(rank_rich_future, n = Inf)

# -----------------------------------
# Create one fixed PA order across all future scenario-years
# -----------------------------------
PA_order <- rank_rich_future %>%
  group_by(PA_short) %>%
  summarise(
    mean_rank_value = mean(mean_richness, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_rank_value) %>%
  pull(PA_short)

rank_rich_future <- rank_rich_future %>%
  mutate(
    PA_short = factor(PA_short, levels = rev(PA_order)),
    scenario_year = factor(
      scenario_year,
      levels = c(
        "2050_ssp126", "2050_ssp245", "2050_ssp585",
        "2070_ssp126", "2070_ssp245", "2070_ssp585",
        "2100_ssp126", "2100_ssp245", "2100_ssp585"
      )
    )
  )

# -----------------------------------
# Plot heatmap
# -----------------------------------
p_rich_rank <- ggplot(
  rank_rich_future,
  aes(
    x = scenario_year,
    y = PA_short,
    fill = mean_richness
  )
) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(mean_richness, 1)), size = 3) +
  scale_fill_gradientn(
    colours = c(
      "#f7fcf5",
      "#c7e9c0",
      "#74c476",
      "#238b45",
      "#00441b"
    ),
    na.value = "grey90",
    name = "Mean richness Area (km2)"
  ) +
  labs(
    x = "Scenario and year",
    y = "Protected area category",
    title = "In Situ Average Refugia Richness Ranked by Protected Area Category"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_rich_rank)

ggsave(
  file.path(out_dir, "future_mean_richness_ranking_PA.png"),
  p_rich_rank,
  width = 10,
  height = 6,
  dpi = 300
)



# plot by richness value

library(sf)
library(terra)
library(dplyr)
library(ggplot2)

# -----------------------------
# INPUT PA
# -----------------------------
PA <- st_read("F:/WWF_data/SDMs_LC_climate/PA/PA_clipped_to_study_area.shp", quiet = TRUE)
pa_v <- terra::vect(PA)

# -----------------------------
# Helpers
# -----------------------------
cell_area_km2 <- function(r) {
  rr <- terra::res(r)
  abs(rr[1] * rr[2]) / 1e6
}

get_year_richness <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  if (grepl("2020_current", nm)) return(2020L)
  as.integer(sub("^richness_pos_(\\d{4})_.*$", "\\1", nm))
}

get_scenario_richness <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  if (grepl("2020_current", nm)) return("current")
  sub("^richness_pos_\\d{4}_(ssp\\d+)$", "\\1", nm)
}

# -----------------------------
# Main calculation: richness values by PA
# -----------------------------
rich_val_results <- list()

for (i in seq_along(root_richness_files)) {
  
  f <- root_richness_files[i]
  cat("Processing:", basename(f), "\n")
  
  r <- terra::rast(f)
  year <- get_year_richness(f)
  scenario <- get_scenario_richness(f)
  
  # project PA to raster CRS if needed
  if (!terra::same.crs(r, pa_v)) {
    pa_proj <- terra::project(pa_v, terra::crs(r))
  } else {
    pa_proj <- pa_v
  }
  
  a_cell <- cell_area_km2(r)
  
  # extract richness values by PA polygon
  e <- terra::extract(r, pa_proj)
  names(e)[2] <- "rich"
  
  # keep only cells with richness values
  e <- e[!is.na(e$rich), ]
  
  # summarise number of cells per PA polygon and richness value
  tab <- e %>%
    group_by(ID, rich) %>%
    summarise(
      n_cells = n(),
      .groups = "drop"
    )
  
  tab$area_km2 <- tab$n_cells * a_cell
  tab$PA <- pa_proj$PA[tab$ID]
  tab$year <- year
  tab$scenario <- scenario
  
  rich_val_results[[i]] <- tab
  
  rm(r, e, tab, pa_proj)
  gc()
}

df_rich_values <- bind_rows(rich_val_results)

print(df_rich_values, n = 100)

# -----------------------------
# Summarise by PA category
# -----------------------------
df_heat <- df_rich_values %>%
  group_by(PA, rich, year, scenario) %>%
  summarise(
    area_km2 = sum(area_km2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    PA_short = dplyr::recode(
      PA,
      "Community Hunting Zone" = "CHZ",
      "Forest Management Unit" = "FMU",
      "National Park" = "NP",
      "Wildlife Sanctuary" = "WS"
    ),
    scenario_year = paste(year, scenario, sep = "_")
  )

# Set PA order
df_heat$PA_short <- factor(
  df_heat$PA_short,
  levels = c("NP", "WS", "FMU", "CHZ")
)

# Set scenario-year order
df_heat$scenario_year <- factor(
  df_heat$scenario_year,
  levels = c(
    "2020_current",
    "2050_ssp126", "2050_ssp245", "2050_ssp585",
    "2070_ssp126", "2070_ssp245", "2070_ssp585",
    "2100_ssp126", "2100_ssp245", "2100_ssp585"
  )
)

print(df_heat, n = Inf)

# -----------------------------
# Plot heatmap of richness values across PA categories
# -----------------------------
p_heat_richness <- ggplot(
  df_heat,
  aes(
    x = PA_short,
    y = factor(rich),
    fill = area_km2
  )
) +
  geom_tile(color = "white") +
  facet_wrap(~scenario_year) +
  scale_fill_gradientn(
    colours = c(
      "#7f0000",
      "#d7301f",
      "#fc8d59",
      "#fee08b",
      "#d9ef8b",
      "#91cf60",
      "#00cc44"
    ),
    name = "Area (km²)"
  ) +
  labs(
    x = "Protected Area Category",
    y = "Richness value",
    title = "Distribution of Richness Values Across Protected Area Categories"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

print(p_heat_richness)

ggsave(
  file.path(out_dir, "richness_value_heatmap_by_PA.png"),
  p_heat_richness,
  width = 12,
  height = 7,
  dpi = 300
)


# Best plotting
# Prepare data
# -----------------------------------
df_heat <- df_rich_values %>%
  group_by(PA, rich, year, scenario) %>%
  summarise(
    area_km2 = sum(area_km2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    PA_short = dplyr::recode(
      PA,
      "Community Hunting Zone" = "CHZ",
      "Forest Management Unit" = "FMU",
      "National Park" = "NP",
      "Wildlife Sanctuary" = "WS"
    ),
    scenario_year = paste(year, scenario, sep = "_")
  )

print(df_heat, n = Inf)

# -----------------------------------
# Create one fixed PA order
# -----------------------------------
PA_order <- df_heat %>%
  group_by(PA_short) %>%
  summarise(
    mean_area = mean(area_km2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_area) %>%
  pull(PA_short)

df_heat2 <- df_heat %>%
  mutate(
    PA_short = factor(PA_short, levels = rev(PA_order))
  )

# -----------------------------------
# Optional: create fixed richness order
# -----------------------------------
rich_order <- sort(unique(df_heat2$rich))

df_heat2 <- df_heat2 %>%
  mutate(
    rich = factor(rich, levels = rev(rich_order)),
    scenario_year = factor(
      scenario_year,
      levels = c(
        "2020_current",
        "2050_ssp126", "2050_ssp245", "2050_ssp585",
        "2070_ssp126", "2070_ssp245", "2070_ssp585",
        "2100_ssp126", "2100_ssp245", "2100_ssp585"
      )
    )
  )

# -----------------------------------
# Plot with shared-y-axis style
# -----------------------------------
p_heat_richness <- ggplot(
  df_heat2,
  aes(
    x = scenario_year,
    y = PA_short,
    fill = area_km2
  )
) +
  geom_tile(color = "white") +
  facet_wrap(~ rich) +
  scale_fill_gradientn(
    colours = c(
      "#7f0000",
      "#d7301f",
      "#fc8d59",
      "#fee08b",
      "#d9ef8b",
      "#91cf60",
      "#00cc44"
    ),
    name = "Richness Area (km²)"
  ) +
  labs(
    x = "Scenario and year",
    y = "Protected Area",
    title = "Distribution of Richness Values Across Protected Area Categories"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    strip.text = element_text(size = 9)
  )

print(p_heat_richness)

ggsave(
  file.path(out_dir, "richness_value_heatmap_by_PA_shared_y.png"),
  p_heat_richness,
  width = 12,
  height = 7,
  dpi = 300
)


# richnes change

# Baseline from 2020 current
# -----------------------------------
baseline <- df_rich_values %>%
  filter(year == 2020, scenario == "current") %>%
  group_by(PA, rich) %>%
  summarise(
    baseline_area = sum(area_km2, na.rm = TRUE),
    .groups = "drop"
  )

print(baseline, n = Inf)

# -----------------------------------
# Future table
# -----------------------------------
future_tbl <- df_rich_values %>%
  filter(year %in% c(2050, 2070, 2100),
         scenario %in% c("ssp126", "ssp245", "ssp585")) %>%
  group_by(PA, year, scenario, rich) %>%
  summarise(
    future_area = sum(area_km2, na.rm = TRUE),
    .groups = "drop"
  )

print(future_tbl, n = Inf)

# -----------------------------------
# Make sure every baseline richness class exists
# for every future scenario-year
# -----------------------------------
future_complete <- tidyr::expand_grid(
  PA = unique(baseline$PA),
  rich = sort(unique(baseline$rich)),
  year = c(2050, 2070, 2100),
  scenario = c("ssp126", "ssp245", "ssp585")
) %>%
  left_join(future_tbl, by = c("PA", "rich", "year", "scenario")) %>%
  mutate(
    future_area = ifelse(is.na(future_area), 0, future_area)
  )

print(future_complete, n = Inf)

# -----------------------------------
# Join baseline and calculate change
# -----------------------------------
df_rich_change <- future_complete %>%
  left_join(baseline, by = c("PA", "rich")) %>%
  mutate(
    baseline_area = ifelse(is.na(baseline_area), 0, baseline_area),
    richness_change_km2 = future_area - baseline_area,
    pct_change = ifelse(
      baseline_area > 0,
      (richness_change_km2 / baseline_area) * 100,
      NA_real_
    ),
    scenario_year = paste(year, scenario, sep = "_"),
    PA_short = dplyr::recode(
      PA,
      "Community Hunting Zone" = "CHZ",
      "Forest Management Unit" = "FMU",
      "National Park" = "NP",
      "Wildlife Sanctuary" = "WS"
    )
  )

print(df_rich_change, n = Inf)

# -----------------------------------
# Create one fixed PA order
# -----------------------------------
PA_order <- df_rich_change %>%
  group_by(PA_short) %>%
  summarise(
    mean_change = mean(richness_change_km2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_change) %>%
  pull(PA_short)

df_rich_change2 <- df_rich_change %>%
  mutate(
    PA_short = factor(PA_short, levels = rev(PA_order)),
    scenario_year = factor(
      scenario_year,
      levels = c(
        "2050_ssp126", "2050_ssp245", "2050_ssp585",
        "2070_ssp126", "2070_ssp245", "2070_ssp585",
        "2100_ssp126", "2100_ssp245", "2100_ssp585"
      )
    )
  )

# -----------------------------------
# Optional: create fixed richness order
# -----------------------------------
rich_order <- sort(unique(df_rich_change2$rich))

df_rich_change2 <- df_rich_change2 %>%
  mutate(
    rich = factor(rich, levels = rev(rich_order))
  )

# -----------------------------------
# Plot richness area change by richness value
# shared y-axis style
# -----------------------------------
p_heat_rich_change <- ggplot(
  df_rich_change2,
  aes(
    x = scenario_year,
    y = PA_short,
    fill = richness_change_km2
  )
) +
  geom_tile(color = "white") +
  facet_wrap(~ rich) +
  scale_fill_gradient2(
    low = "darkred",
    mid = "white",
    high = "darkblue",
    midpoint = 0,
    name = "Richness Change (km²)"
  ) +
  labs(
    x = "Scenario and year",
    y = "Protected Area",
    title = "Change in In Situ Protected Area Richness by Richness Value"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    strip.text = element_text(size = 9)
  )

print(p_heat_rich_change)

ggsave(
  file.path(out_dir, "richness_area_change_by_richness_value_PA.png"),
  p_heat_rich_change,
  width = 12,
  height = 7,
  dpi = 300
)





#-------------Repeat plotting workflow for exsitu refugia----------------

# INPUT
# -----------------------------
PA <- st_read("F:/WWF_data/SDMs_LC_climate/PA/PA_clipped_to_study_area.shp", quiet = TRUE)

exsitu_refugia_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/future_binary_clipped_to_species_range"

exsitu_refugia_files <- list.files(
  path = exsitu_refugia_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

out_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/PA_results_exsitu"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_csv <- file.path(out_dir, "exsitu_suitable_area_by_PA.csv")

if (length(exsitu_refugia_files) == 0) {
  stop("No raster files found in: ", exsitu_refugia_dir)
}

# -----------------------------
# Convert PA to terra
# -----------------------------
pa_v <- terra::vect(PA)

# -----------------------------
# Helpers
# -----------------------------
cell_area_km2 <- function(r) {
  rr <- terra::res(r)
  abs(rr[1] * rr[2]) / 1e6
}

# total suitable area in raster
species_total_suitable_km2 <- function(r) {
  a_cell <- cell_area_km2(r)
  n_suit <- as.numeric(terra::global(r == 1, "sum", na.rm = TRUE)[1, 1])
  n_suit * a_cell
}

# PA polygon area in km2 after projection to raster CRS
get_PA_areas_km2 <- function(pa_vect, r) {
  if (!terra::same.crs(pa_vect, r)) {
    pa_vect <- terra::project(pa_vect, terra::crs(r))
  }
  a <- terra::expanse(pa_vect, unit = "km")
  data.frame(
    PA = pa_vect$PA,
    PA_area_km2 = as.numeric(a)
  )
}

# parse future raster name
parse_future_info <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  
  # example:
  # Atherurus africanus_binary_overlap_2050_ssp126_cropped_masked_to_range
  m <- regexec("^(.*)_binary_overlap_(\\d{4})_(ssp\\d{3})_cropped_masked_to_range$", nm)
  hit <- regmatches(nm, m)[[1]]
  
  if (length(hit) == 0) {
    return(NULL)
  }
  
  list(
    species = hit[2],
    year = as.integer(hit[3]),
    scenario = hit[4]
  )
}

# compute suitable area by PA
calc_PA_area <- function(r, species, year, scenario) {
  
  # project PAs to raster CRS if needed
  pa_proj <- pa_v
  if (!terra::same.crs(r, pa_proj)) {
    pa_proj <- terra::project(pa_proj, terra::crs(r))
  }
  
  a_cell <- cell_area_km2(r)
  
  # total suitable area for this species/scenario raster
  total_suitable_km2 <- species_total_suitable_km2(r)
  
  # extract raster values by PA
  e <- terra::extract(r, pa_proj)
  
  # count cells == 1 in each PA polygon
  suitable_cells <- tapply(e[[2]] == 1, e[[1]], sum, na.rm = TRUE)
  
  # ensure all PA polygons are present
  all_ids <- seq_len(nrow(pa_proj))
  suitable_cells_full <- rep(0, length(all_ids))
  names(suitable_cells_full) <- all_ids
  
  if (length(suitable_cells) > 0) {
    suitable_cells_full[names(suitable_cells)] <- suitable_cells
  }
  
  suitable_km2 <- suitable_cells_full * a_cell
  
  # polygon areas
  PA_area_df <- get_PA_areas_km2(pa_proj, r)
  
  df <- data.frame(
    species = species,
    PA = pa_proj$PA,
    year = year,
    scenario = scenario,
    PA_area_km2 = PA_area_df$PA_area_km2,
    species_total_suitable_km2 = total_suitable_km2,
    suitable_km2_in_PA = suitable_km2,
    pct_PA_suitable = ifelse(
      PA_area_df$PA_area_km2 > 0,
      (suitable_km2 / PA_area_df$PA_area_km2) * 100,
      NA_real_
    ),
    pct_species_suitable_covered_by_PA = ifelse(
      total_suitable_km2 > 0,
      (suitable_km2 / total_suitable_km2) * 100,
      NA_real_
    ),
    stringsAsFactors = FALSE
  )
  
  df
}

# -----------------------------
# Run all rasters
# -----------------------------
results <- list()

for (i in seq_along(exsitu_refugia_files)) {
  f <- exsitu_refugia_files[i]
  info <- parse_future_info(f)
  
  if (is.null(info)) {
    warning("Could not parse file name: ", f)
    next
  }
  
  r <- terra::rast(f)
  
  results[[length(results) + 1]] <- calc_PA_area(
    r = r,
    species = info$species,
    year = info$year,
    scenario = info$scenario
  )
  
  rm(r)
  gc()
  
  cat("Processed:", i, "of", length(exsitu_refugia_files), "-", basename(f), "\n")
}

df_exsitu_PA <- do.call(rbind, results)
df_exsitu_PA <- df_exsitu_PA %>%
  arrange(species, year, scenario, PA)

print(df_exsitu_PA)

write.csv(df_exsitu_PA, out_csv, row.names = FALSE)
cat("\nSaved:", out_csv, "\n")



# plot landscape coverage

library(dplyr)
library(ggplot2)
library(scales)

# -----------------------------------
# Prepare data
# -----------------------------------
df_plot <- df_exsitu_PA %>%
  group_by(species, PA, year, scenario) %>%
  summarise(
    PA_area_km2 = sum(PA_area_km2, na.rm = TRUE),
    species_total_suitable_km2 = first(species_total_suitable_km2),
    suitable_km2_in_PA = sum(suitable_km2_in_PA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_PA_suitable = ifelse(
      PA_area_km2 > 0,
      (suitable_km2_in_PA / PA_area_km2) * 100,
      NA_real_
    ),
    pct_species_suitable_covered_by_PA = ifelse(
      species_total_suitable_km2 > 0,
      (suitable_km2_in_PA / species_total_suitable_km2) * 100,
      NA_real_
    ),
    PA_short = dplyr::recode(
      PA,
      "Community Hunting Zone" = "CHZ",
      "Forest Management Unit" = "FMU",
      "National Park" = "NP",
      "Wildlife Sanctuary" = "WS"
    ),
    scenario_year = paste(year, scenario, sep = "_")
  )

print(df_plot, n = Inf)

# -----------------------------------
# Rank PA for each species and scenario-year
# -----------------------------------
df_rank_species <- df_plot %>%
  group_by(species, scenario_year) %>%
  arrange(desc(pct_PA_suitable), .by_group = TRUE) %>%
  mutate(
    PA_rank = row_number()
  ) %>%
  ungroup()

print(df_rank_species, n = Inf)

# -----------------------------------
# Create one fixed PA order
# -----------------------------------
PA_order <- df_rank_species %>%
  group_by(PA_short) %>%
  summarise(mean_rank = mean(PA_rank, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_rank) %>%
  pull(PA_short)

df_rank_species2 <- df_rank_species %>%
  mutate(
    PA_short = factor(PA_short, levels = rev(PA_order)),
    scenario_year = factor(
      scenario_year,
      levels = c(
        "2050_ssp126", "2050_ssp245", "2050_ssp585",
        "2070_ssp126", "2070_ssp245", "2070_ssp585",
        "2100_ssp126", "2100_ssp245", "2100_ssp585"
      )
    )
  )

# -----------------------------------
# Plot with one shared y-axis
# -----------------------------------
p_rank_species <- ggplot(
  df_rank_species2,
  aes(
    x = scenario_year,
    y = PA_short,
    fill = pct_PA_suitable
  )
) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(pct_PA_suitable, 1)), size = 3) +
  scale_fill_gradientn(
    colours = c(
      "#7f0000",
      "#d7301f",
      "#fc8d59",
      "#fee08b",
      "#d9ef8b",
      "#91cf60",
      "#00cc44"
    ),
    values = scales::rescale(c(0, 1, 5, 10, 20, 40, 70)),
    name = "% PA\nrefugia"
  ) +
  facet_wrap(~ species) +
  labs(
    x = "Scenario and year",
    y = "Protected Area",
    title = "Ex Situ Climate Refugia for Species Ranked by Protected Area Category"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 9)
  )

print(p_rank_species)



# -----Rank mean biodiversity---

# -----------------------------------
# Mean biodiversity ranking by landscape

# 1. Calculate mean biodiversity across species
df_mean_biodiv <- df_plot %>%
  group_by(PA_short, scenario_year) %>%
  summarise(
    mean_pct_PA_suitable = mean(pct_PA_suitable, na.rm = TRUE),
    .groups = "drop"
  )

print(df_mean_biodiv, n = Inf)

# 2. Rank PA categories within each scenario-year
df_rank_mean <- df_mean_biodiv %>%
  group_by(scenario_year) %>%
  arrange(desc(mean_pct_PA_suitable), .by_group = TRUE) %>%
  mutate(
    PA_rank = row_number()
  ) %>%
  ungroup()

print(df_rank_mean, n = Inf)

# 3. Create one fixed PA order from mean rank
PA_order_mean <- df_rank_mean %>%
  group_by(PA_short) %>%
  summarise(mean_rank = mean(PA_rank, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_rank) %>%
  pull(PA_short)

df_rank_mean2 <- df_rank_mean %>%
  mutate(
    PA_short = factor(PA_short, levels = rev(PA_order_mean)),
    scenario_year = factor(
      scenario_year,
      levels = c(
        "2050_ssp126", "2050_ssp245", "2050_ssp585",
        "2070_ssp126", "2070_ssp245", "2070_ssp585",
        "2100_ssp126", "2100_ssp245", "2100_ssp585"
      )
    )
  )

# 4. Plot ranked mean biodiversity heatmap
p_rank_mean <- ggplot(
  df_rank_mean2,
  aes(
    x = scenario_year,
    y = PA_short,
    fill = mean_pct_PA_suitable
  )
) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(mean_pct_PA_suitable, 1)), size = 3) +
  scale_fill_gradientn(
    colours = c(
      "#7f0000",
      "#d7301f",
      "#fc8d59",
      "#fee08b",
      "#d9ef8b",
      "#91cf60",
      "#00cc44"
    ),
    values = scales::rescale(c(0, 1, 5, 10, 20, 40, 70)),
    name = "Mean % PA\nrefugia"
  ) +
  labs(
    x = "Scenario and year",
    y = "Protected Area",
    title = "Mean Mammal Ex Situ Climate Refugia Ranked by Protected Area Category"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_rank_mean)



# Do ranking by exsitu refugia richness

library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(scales)

# -----------------------------
# INPUTS
# -----------------------------
PA <- st_read("F:/WWF_data/SDMs_LC_climate/PA/PA_clipped_to_study_area.shp", quiet = TRUE)
pa_v <- terra::vect(PA)

exsitu_refugia_richness_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/exsitu_refugia_richness"

exsitu_refugia_richness_files <- list.files(
  path = exsitu_refugia_richness_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = FALSE
)

print(exsitu_refugia_richness_files)

# -----------------------------
# Helper functions
# -----------------------------
cell_area_km2 <- function(r) {
  rr <- terra::res(r)
  abs(rr[1] * rr[2]) / 1e6
}

parse_richness_name <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  # exsitu_richness_pos_2050_ssp126
  parts <- strsplit(nm, "_")[[1]]
  
  list(
    year = as.numeric(parts[4]),
    scenario = parts[5]
  )
}

# -----------------------------
# Extract richness values within PA
# -----------------------------
results <- list()

for (i in seq_along(exsitu_refugia_richness_files)) {
  
  f <- exsitu_refugia_richness_files[i]
  info <- parse_richness_name(f)
  r <- terra::rast(f)
  
  if (!terra::same.crs(r, pa_v)) {
    pa_proj <- terra::project(pa_v, terra::crs(r))
  } else {
    pa_proj <- pa_v
  }
  
  a_cell <- cell_area_km2(r)
  
  e <- terra::extract(r, pa_proj)
  names(e)[2] <- "rich"
  
  df <- e %>%
    filter(!is.na(rich)) %>%
    group_by(ID, rich) %>%
    summarise(
      n_cells = n(),
      .groups = "drop"
    ) %>%
    mutate(
      area_km2 = n_cells * a_cell,
      PA = pa_proj$PA[ID],
      year = info$year,
      scenario = info$scenario
    )
  
  results[[i]] <- df
  
  rm(r, e, df, pa_proj)
  gc()
  
  cat("Processed:", basename(f), "\n")
}

df_exsitu_rich_values <- bind_rows(results)
print(df_exsitu_rich_values, n = Inf)

# -----------------------------
# Calculate percent area per PA category
# -----------------------------
PA_area <- data.frame(
  PA = pa_v$PA,
  PA_area_km2 = terra::expanse(pa_v, unit = "km")
)

# aggregate PA polygon areas to PA category
PA_area_sum <- PA_area %>%
  group_by(PA) %>%
  summarise(
    PA_area_km2 = sum(PA_area_km2, na.rm = TRUE),
    .groups = "drop"
  )

# aggregate richness area to PA category
df_exsitu_rich_values <- df_exsitu_rich_values %>%
  group_by(PA, year, scenario, rich) %>%
  summarise(
    area_km2 = sum(area_km2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(PA_area_sum, by = "PA") %>%
  mutate(
    pct_PA_area = ifelse(
      PA_area_km2 > 0,
      (area_km2 / PA_area_km2) * 100,
      NA_real_
    ),
    scenario_year = paste(year, scenario, sep = "_"),
    PA_short = dplyr::recode(
      PA,
      "Community Hunting Zone" = "CHZ",
      "Forest Management Unit" = "FMU",
      "National Park" = "NP",
      "Wildlife Sanctuary" = "WS"
    )
  )

print(df_exsitu_rich_values, n = Inf)

# -----------------------------
# Rank percent richness per PA area
# -----------------------------
df_rank_rich <- df_exsitu_rich_values %>%
  group_by(rich, scenario_year) %>%
  arrange(desc(pct_PA_area), .by_group = TRUE) %>%
  mutate(
    PA_rank = row_number()
  ) %>%
  ungroup()

PA_order <- df_rank_rich %>%
  group_by(PA_short) %>%
  summarise(mean_rank = mean(PA_rank, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_rank) %>%
  pull(PA_short)

df_rank_rich2 <- df_rank_rich %>%
  mutate(
    PA_short = factor(PA_short, levels = rev(PA_order)),
    scenario_year = factor(
      scenario_year,
      levels = c(
        "2050_ssp126", "2050_ssp245", "2050_ssp585",
        "2070_ssp126", "2070_ssp245", "2070_ssp585",
        "2100_ssp126", "2100_ssp245", "2100_ssp585"
      )
    )
  )

# -----------------------------
# Plot
# -----------------------------
p_rank_rich <- ggplot(
  df_rank_rich2,
  aes(
    x = scenario_year,
    y = PA_short,
    fill = pct_PA_area
  )
) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(pct_PA_area, 1)), size = 3) +
  scale_fill_gradientn(
    colours = c(
      "#7f0000",
      "#d7301f",
      "#fc8d59",
      "#fee08b",
      "#d9ef8b",
      "#91cf60",
      "#00cc44"
    ),
    values = scales::rescale(c(0, 1, 5, 10, 20, 40, 70)),
    name = "% PA\nrefugia richness"
  ) +
  facet_wrap(~ rich) +
  labs(
    x = "Scenario and year",
    y = "Protected Area",
    title = "Ex Situ Refugia Richness Ranked by Protected Area Category"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 9)
  )

print(p_rank_rich)

# -----------------------------
# Save table
# -----------------------------
write.csv(
  df_exsitu_rich_values,
  "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/PA_results/exsitu_refugia_richness_by_PA.csv",
  row.names = FALSE
)


# ggsave(
# "exsitu_refugia_richness_heatmap.png",
# p_rank_rich,
#  width = 14,
# height = 10,
# dpi = 300
# )

# combine insitu + exsitu refugia

library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(scales)

# -----------------------------------
# INPUTS
# -----------------------------------
PA <- st_read("F:/WWF_data/SDMs_LC_climate/PA/PA_clipped_to_study_area.shp", quiet = TRUE)
pa_v <- terra::vect(PA)

insitu_refugia_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/stats_results/change_detection/refugia_rasters"
exsitu_refugia_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/future_binary_clipped_to_species_range"

insitu_refugia_files <- list.files(
  path = insitu_refugia_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = FALSE
)

exsitu_refugia_files <- list.files(
  path = exsitu_refugia_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

print(insitu_refugia_files)
print(exsitu_refugia_files)

# -----------------------------------
# PA areas
# -----------------------------------
PA_area <- data.frame(
  PA = pa_v$PA,
  PA_area_km2 = as.numeric(terra::expanse(pa_v, unit = "km"))
)

print(PA_area)

# -----------------------------------
# Helpers
# -----------------------------------
cell_area_km2 <- function(r) {
  rr <- terra::res(r)
  abs(rr[1] * rr[2]) / 1e6
}

parse_insitu_name <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  # example: Atherurus_africanus_refugia_2020_to_2050_ssp126
  
  m <- regexec("^(.*)_refugia_2020_to_(\\d{4})_(ssp\\d{3})$", nm)
  hit <- regmatches(nm, m)[[1]]
  
  if (length(hit) == 0) return(NULL)
  
  list(
    species_us = hit[2],
    year = as.integer(hit[3]),
    scenario = hit[4]
  )
}

parse_exsitu_name <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  
  # format 1: Atherurus africanus_mean_pixel_binary_2050s_ssp126_outside_range_only
  m1 <- regexec("^(.*)_mean_pixel_binary_(\\d{4})s_(ssp\\d{3})_outside_range_only$", nm)
  hit1 <- regmatches(nm, m1)[[1]]
  
  if (length(hit1) > 0) {
    return(list(
      species_us = gsub(" ", "_", hit1[2]),
      year = as.integer(hit1[3]),
      scenario = hit1[4]
    ))
  }
  
  # format 2: Atherurus africanus_binary_overlap_2050_ssp126_cropped_masked_to_range
  m2 <- regexec("^(.*)_binary_overlap_(\\d{4})_(ssp\\d{3}).*$", nm)
  hit2 <- regmatches(nm, m2)[[1]]
  
  if (length(hit2) > 0) {
    return(list(
      species_us = gsub(" ", "_", hit2[2]),
      year = as.integer(hit2[3]),
      scenario = hit2[4]
    ))
  }
  
  return(NULL)
}

# -----------------------------------
# Build lookup tables
# -----------------------------------
insitu_meta_all <- lapply(insitu_refugia_files, parse_insitu_name)
insitu_keep <- !sapply(insitu_meta_all, is.null)
insitu_meta <- insitu_meta_all[insitu_keep]

insitu_lookup <- data.frame(
  file = insitu_refugia_files[insitu_keep],
  species_us = sapply(insitu_meta, `[[`, "species_us"),
  year = sapply(insitu_meta, `[[`, "year"),
  scenario = sapply(insitu_meta, `[[`, "scenario"),
  stringsAsFactors = FALSE
)

exsitu_meta_all <- lapply(exsitu_refugia_files, parse_exsitu_name)
exsitu_keep <- !sapply(exsitu_meta_all, is.null)
exsitu_meta <- exsitu_meta_all[exsitu_keep]

exsitu_lookup <- data.frame(
  file = exsitu_refugia_files[exsitu_keep],
  species_us = sapply(exsitu_meta, `[[`, "species_us"),
  year = sapply(exsitu_meta, `[[`, "year"),
  scenario = sapply(exsitu_meta, `[[`, "scenario"),
  stringsAsFactors = FALSE
)

print(insitu_lookup)
print(exsitu_lookup)

# -----------------------------------
# Function to calculate combined refugia by PA
# -----------------------------------
calc_combined_refugia_PA <- function(r_in, r_ex, species_us, year, scenario, pa_v, PA_area) {
  
  # reproject PA if needed
  if (!terra::same.crs(r_in, pa_v)) {
    pa_proj <- terra::project(pa_v, terra::crs(r_in))
  } else {
    pa_proj <- pa_v
  }
  
  # align rasters if needed
  if (!isTRUE(terra::compareGeom(r_in, r_ex, stopOnError = FALSE))) {
    r_ex <- terra::resample(r_ex, r_in, method = "near")
  }
  
  a_cell <- cell_area_km2(r_in)
  
  # safe binary layers
  rin01 <- terra::ifel(is.na(r_in), 0, terra::ifel(r_in > 0, 1, 0))
  rex01 <- terra::ifel(is.na(r_ex), 0, terra::ifel(r_ex > 0, 1, 0))
  
  # union of in situ + ex situ refugia
  r_comb <- terra::ifel((rin01 + rex01) > 0, 1, 0)
  names(r_comb) <- paste0(species_us, "_combined_refugia_", year, "_", scenario)
  
  # total combined refugia area
  species_total_cells <- as.numeric(terra::global(r_comb == 1, "sum", na.rm = TRUE)[1, 1])
  species_total_km2 <- species_total_cells * a_cell
  
  # extract by PA polygon
  e <- terra::extract(r_comb, pa_proj)
  names(e)[2] <- "value"
  
  PA_cells <- tapply(e$value == 1, e$ID, sum, na.rm = TRUE)
  
  all_ids <- seq_len(nrow(pa_proj))
  PA_cells_full <- rep(0, length(all_ids))
  names(PA_cells_full) <- all_ids
  
  if (length(PA_cells) > 0) {
    PA_cells_full[names(PA_cells)] <- PA_cells
  }
  
  df <- data.frame(
    ID = all_ids,
    combined_refugia_km2_in_PA = as.numeric(PA_cells_full) * a_cell
  )
  
  df$PA <- pa_proj$PA[df$ID]
  
  df <- df %>%
    left_join(PA_area, by = "PA") %>%
    mutate(
      species = gsub("_", " ", species_us),
      year = year,
      scenario = scenario,
      species_total_combined_refugia_km2 = species_total_km2,
      pct_PA_combined_refugia = ifelse(
        PA_area_km2 > 0,
        (combined_refugia_km2_in_PA / PA_area_km2) * 100,
        NA_real_
      ),
      pct_species_combined_refugia_covered_by_PA = ifelse(
        species_total_combined_refugia_km2 > 0,
        (combined_refugia_km2_in_PA / species_total_combined_refugia_km2) * 100,
        NA_real_
      )
    ) %>%
    dplyr::select(
      species, PA, year, scenario,
      PA_area_km2,
      species_total_combined_refugia_km2,
      combined_refugia_km2_in_PA,
      pct_PA_combined_refugia,
      pct_species_combined_refugia_covered_by_PA
    )
  
  list(df = df, raster = r_comb)
}

# -----------------------------------
# Loop through matching species/year/scenario
# -----------------------------------
out_combined_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/combined_refugia_PA"
dir.create(out_combined_dir, recursive = TRUE, showWarnings = FALSE)

out_combined_raster_dir <- file.path(out_combined_dir, "combined_refugia_rasters")
dir.create(out_combined_raster_dir, recursive = TRUE, showWarnings = FALSE)

results <- list()

keys <- inner_join(
  insitu_lookup,
  exsitu_lookup,
  by = c("species_us", "year", "scenario"),
  suffix = c("_insitu", "_exsitu")
)

print(keys)

for (i in seq_len(nrow(keys))) {
  
  cat("Processing:", i, "of", nrow(keys), "\n")
  
  r_in <- terra::rast(keys$file_insitu[i])
  r_ex <- terra::rast(keys$file_exsitu[i])
  
  tmp <- calc_combined_refugia_PA(
    r_in = r_in,
    r_ex = r_ex,
    species_us = keys$species_us[i],
    year = keys$year[i],
    scenario = keys$scenario[i],
    pa_v = pa_v,
    PA_area = PA_area
  )
  
  out_r <- file.path(
    out_combined_raster_dir,
    paste0(keys$species_us[i], "_combined_refugia_", keys$year[i], "_", keys$scenario[i], ".tif")
  )
  terra::writeRaster(tmp$raster, out_r, overwrite = TRUE)
  
  results[[i]] <- tmp$df
  
  rm(r_in, r_ex, tmp)
  gc()
}

df_combined_PA <- bind_rows(results)

print(df_combined_PA)

# -----------------------------------
# Save csv
# -----------------------------------
out_csv <- file.path(out_combined_dir, "combined_refugia_by_PA.csv")
write.csv(df_combined_PA, out_csv, row.names = FALSE)
cat("Saved:", out_csv, "\n")

# -----------------------------------
# Prepare plot data: aggregate by PA category
# -----------------------------------
df_plot <- df_combined_PA %>%
  group_by(species, PA, year, scenario) %>%
  summarise(
    PA_area_km2 = sum(PA_area_km2, na.rm = TRUE),
    species_total_combined_refugia_km2 = first(species_total_combined_refugia_km2),
    combined_refugia_km2_in_PA = sum(combined_refugia_km2_in_PA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_PA_combined_refugia = ifelse(
      PA_area_km2 > 0,
      (combined_refugia_km2_in_PA / PA_area_km2) * 100,
      NA_real_
    ),
    pct_species_combined_refugia_covered_by_PA = ifelse(
      species_total_combined_refugia_km2 > 0,
      (combined_refugia_km2_in_PA / species_total_combined_refugia_km2) * 100,
      NA_real_
    ),
    PA_short = dplyr::recode(
      PA,
      "Community Hunting Zone" = "CHZ",
      "Forest Management Unit" = "FMU",
      "National Park" = "NP",
      "Wildlife Sanctuary" = "WS"
    ),
    scenario_year = paste(year, scenario, sep = "_")
  )

print(df_plot, n = Inf)

# -----------------------------------
# Rank species by PA
# -----------------------------------
df_rank_species <- df_plot %>%
  group_by(species, scenario_year) %>%
  arrange(desc(pct_PA_combined_refugia), .by_group = TRUE) %>%
  mutate(
    PA_rank = row_number()
  ) %>%
  ungroup()

print(df_rank_species, n = Inf)

PA_order <- df_rank_species %>%
  group_by(PA_short) %>%
  summarise(mean_rank = mean(PA_rank, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_rank) %>%
  pull(PA_short)

df_rank_species2 <- df_rank_species %>%
  mutate(
    PA_short = factor(PA_short, levels = rev(PA_order)),
    scenario_year = factor(
      scenario_year,
      levels = c(
        "2050_ssp126", "2050_ssp245", "2050_ssp585",
        "2070_ssp126", "2070_ssp245", "2070_ssp585",
        "2100_ssp126", "2100_ssp245", "2100_ssp585"
      )
    )
  )

p_rank_species <- ggplot(
  df_rank_species2,
  aes(
    x = scenario_year,
    y = PA_short,
    fill = pct_PA_combined_refugia
  )
) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(pct_PA_combined_refugia, 1)), size = 3) +
  scale_fill_gradientn(
    colours = c(
      "#7f0000",
      "#d7301f",
      "#fc8d59",
      "#fee08b",
      "#d9ef8b",
      "#91cf60",
      "#00cc44"
    ),
    values = scales::rescale(c(0, 1, 5, 10, 20, 40, 70)),
    name = "% PA\ncombined refugia"
  ) +
  facet_wrap(~ species) +
  labs(
    x = "Scenario and year",
    y = "Protected Area",
    title = "Combined In Situ + Ex Situ Refugia Ranked by Protected Area Category"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 9)
  )

print(p_rank_species)


# -----------------------------------
# Rank mean biodiversity
# -----------------------------------
df_mean_biodiv <- df_plot %>%
  group_by(PA_short, scenario_year) %>%
  summarise(
    mean_pct_PA_combined_refugia = mean(pct_PA_combined_refugia, na.rm = TRUE),
    .groups = "drop"
  )

PA_order_mean <- df_mean_biodiv %>%
  group_by(PA_short) %>%
  summarise(mean_value = mean(mean_pct_PA_combined_refugia, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_value) %>%
  pull(PA_short)

df_mean_biodiv2 <- df_mean_biodiv %>%
  mutate(
    PA_short = factor(PA_short, levels = rev(PA_order_mean)),
    scenario_year = factor(
      scenario_year,
      levels = c(
        "2050_ssp126", "2050_ssp245", "2050_ssp585",
        "2070_ssp126", "2070_ssp245", "2070_ssp585",
        "2100_ssp126", "2100_ssp245", "2100_ssp585"
      )
    )
  )

p_rank_mean <- ggplot(
  df_mean_biodiv2,
  aes(
    x = scenario_year,
    y = PA_short,
    fill = mean_pct_PA_combined_refugia
  )
) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(mean_pct_PA_combined_refugia, 1)), size = 3) +
  scale_fill_gradientn(
    colours = c(
      "#7f0000",
      "#d7301f",
      "#fc8d59",
      "#fee08b",
      "#d9ef8b",
      "#91cf60",
      "#00cc44"
    ),
    values = scales::rescale(c(0, 1, 5, 10, 20, 40, 70)),
    name = "Mean % PA\ncombined refugia"
  ) +
  labs(
    x = "Scenario and year",
    y = "Protected Area",
    title = "Mean Combined In Situ + Ex Situ Refugia by Protected Area Category"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_rank_mean)



# Rank insitu + exsitu refgia richness
# INPUTS
# -----------------------------------
PA <- st_read("F:/WWF_data/SDMs_LC_climate/PA/PA_clipped_to_study_area.shp", quiet = TRUE)
pa_v <- terra::vect(PA)

insitu_refugia_richness_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/stats_results/richness_outputs"
exsitu_refugia_richness_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/exsitu_refugia_richness"

insitu_refugia_richness_files <- list.files(
  path = insitu_refugia_richness_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = FALSE
)

exsitu_refugia_richness_files <- list.files(
  path = exsitu_refugia_richness_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = FALSE
)

print(insitu_refugia_richness_files)
print(exsitu_refugia_richness_files)

# keep only future insitu richness rasters
insitu_refugia_richness_files2 <- insitu_refugia_richness_files[
  !grepl("2020_current", basename(insitu_refugia_richness_files))
]

print(insitu_refugia_richness_files2)

# -----------------------------------
# Helpers
# -----------------------------------
cell_area_km2 <- function(r) {
  rr <- terra::res(r)
  abs(rr[1] * rr[2]) / 1e6
}

parse_insitu_rich_name <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  # richness_pos_2050_ssp126
  
  m <- regexec("^richness_pos_(\\d{4})_(ssp\\d{3})$", nm)
  hit <- regmatches(nm, m)[[1]]
  
  if (length(hit) == 0) return(NULL)
  
  list(
    year = as.integer(hit[2]),
    scenario = hit[3]
  )
}

parse_exsitu_rich_name <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  # exsitu_richness_pos_2050_ssp126
  
  m <- regexec("^exsitu_richness_pos_(\\d{4})_(ssp\\d{3})$", nm)
  hit <- regmatches(nm, m)[[1]]
  
  if (length(hit) == 0) return(NULL)
  
  list(
    year = as.integer(hit[2]),
    scenario = hit[3]
  )
}

# -----------------------------------
# Build lookup tables
# -----------------------------------
insitu_meta <- lapply(insitu_refugia_richness_files2, parse_insitu_rich_name)
insitu_keep <- !sapply(insitu_meta, is.null)

insitu_lookup <- data.frame(
  file = insitu_refugia_richness_files2[insitu_keep],
  year = sapply(insitu_meta[insitu_keep], `[[`, "year"),
  scenario = sapply(insitu_meta[insitu_keep], `[[`, "scenario"),
  stringsAsFactors = FALSE
)

exsitu_meta <- lapply(exsitu_refugia_richness_files, parse_exsitu_rich_name)
exsitu_keep <- !sapply(exsitu_meta, is.null)

exsitu_lookup <- data.frame(
  file = exsitu_refugia_richness_files[exsitu_keep],
  year = sapply(exsitu_meta[exsitu_keep], `[[`, "year"),
  scenario = sapply(exsitu_meta[exsitu_keep], `[[`, "scenario"),
  stringsAsFactors = FALSE
)

print(insitu_lookup)
print(exsitu_lookup)

# match insitu and exsitu rasters
rich_keys <- inner_join(
  insitu_lookup,
  exsitu_lookup,
  by = c("year", "scenario"),
  suffix = c("_insitu", "_exsitu")
)

print(rich_keys)

# -----------------------------------
# PA areas
# -----------------------------------
PA_area <- data.frame(
  PA = pa_v$PA,
  PA_area_km2 = as.numeric(terra::expanse(pa_v, unit = "km"))
)

PA_area_sum <- PA_area %>%
  group_by(PA) %>%
  summarise(
    PA_area_km2 = sum(PA_area_km2, na.rm = TRUE),
    .groups = "drop"
  )

print(PA_area_sum)

# -----------------------------------
# Function to calculate combined richness by PA
# -----------------------------------
calc_combined_refugia_richness_PA <- function(r_in, r_ex, year, scenario, pa_v, PA_area_sum) {
  
  if (!terra::same.crs(r_in, pa_v)) {
    pa_proj <- terra::project(pa_v, terra::crs(r_in))
  } else {
    pa_proj <- pa_v
  }
  
  if (!isTRUE(terra::compareGeom(r_in, r_ex, stopOnError = FALSE))) {
    r_ex <- terra::resample(r_ex, r_in, method = "near")
  }
  
  # replace NA with 0 before combining
  rin0 <- terra::ifel(is.na(r_in), 0, r_in)
  rex0 <- terra::ifel(is.na(r_ex), 0, r_ex)
  
  # combined richness
  r_comb <- rin0 + rex0
  
  # remove zeros
  r_comb_pos <- terra::ifel(r_comb == 0, NA, r_comb)
  names(r_comb_pos) <- paste0("combined_refugia_richness_", year, "_", scenario)
  
  a_cell <- cell_area_km2(r_comb_pos)
  
  e <- terra::extract(r_comb_pos, pa_proj)
  names(e)[2] <- "rich"
  
  df <- e %>%
    filter(!is.na(rich)) %>%
    group_by(ID, rich) %>%
    summarise(
      n_cells = n(),
      .groups = "drop"
    ) %>%
    mutate(
      area_km2 = n_cells * a_cell,
      PA = pa_proj$PA[ID],
      year = year,
      scenario = scenario
    ) %>%
    group_by(PA, rich, year, scenario) %>%
    summarise(
      n_cells = sum(n_cells, na.rm = TRUE),
      area_km2 = sum(area_km2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(PA_area_sum, by = "PA") %>%
    mutate(
      pct_PA_area = ifelse(
        PA_area_km2 > 0,
        (area_km2 / PA_area_km2) * 100,
        NA_real_
      )
    ) %>%
    dplyr::select(
      PA, rich, n_cells, area_km2,
      PA_area_km2, pct_PA_area,
      year, scenario
    )
  
  list(df = df, raster = r_comb_pos)
}

# -----------------------------------
# Run the workflow
# -----------------------------------
out_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/updated/combined_refugia_richness_PA"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_raster_dir <- file.path(out_dir, "combined_richness_rasters")
dir.create(out_raster_dir, recursive = TRUE, showWarnings = FALSE)

results <- list()

for (i in seq_len(nrow(rich_keys))) {
  
  cat("Processing", i, "of", nrow(rich_keys), "\n")
  
  r_in <- terra::rast(rich_keys$file_insitu[i])
  r_ex <- terra::rast(rich_keys$file_exsitu[i])
  
  tmp <- calc_combined_refugia_richness_PA(
    r_in = r_in,
    r_ex = r_ex,
    year = rich_keys$year[i],
    scenario = rich_keys$scenario[i],
    pa_v = pa_v,
    PA_area_sum = PA_area_sum
  )
  
  out_r <- file.path(
    out_raster_dir,
    paste0("combined_refugia_richness_pos_", rich_keys$year[i], "_", rich_keys$scenario[i], ".tif")
  )
  
  terra::writeRaster(tmp$raster, out_r, overwrite = TRUE)
  
  results[[i]] <- tmp$df
  
  rm(r_in, r_ex, tmp)
  gc()
}

df_combined_rich_PA <- bind_rows(results)

print(df_combined_rich_PA, n = Inf)

# save results
out_csv <- file.path(out_dir, "combined_refugia_richness_by_PA.csv")
write.csv(df_combined_rich_PA, out_csv, row.names = FALSE)
cat("Saved:", out_csv, "\n")

# -----------------------------------
# Prepare plot data
# -----------------------------------
df_plot <- df_combined_rich_PA %>%
  mutate(
    PA_short = dplyr::recode(
      PA,
      "Community Hunting Zone" = "CHZ",
      "Forest Management Unit" = "FMU",
      "National Park" = "NP",
      "Wildlife Sanctuary" = "WS"
    ),
    scenario_year = paste(year, scenario, sep = "_")
  )

print(df_plot, n = Inf)

# -----------------------------------
# Rank
# -----------------------------------
df_rank_rich <- df_plot %>%
  group_by(rich, scenario_year) %>%
  arrange(desc(pct_PA_area), .by_group = TRUE) %>%
  mutate(
    PA_rank = row_number()
  ) %>%
  ungroup()

print(df_rank_rich, n = Inf)

PA_order <- df_rank_rich %>%
  group_by(PA_short) %>%
  summarise(mean_rank = mean(PA_rank, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_rank) %>%
  pull(PA_short)

df_rank_rich2 <- df_rank_rich %>%
  mutate(
    PA_short = factor(PA_short, levels = rev(PA_order)),
    scenario_year = factor(
      scenario_year,
      levels = c(
        "2050_ssp126", "2050_ssp245", "2050_ssp585",
        "2070_ssp126", "2070_ssp245", "2070_ssp585",
        "2100_ssp126", "2100_ssp245", "2100_ssp585"
      )
    )
  )

# -----------------------------------
# Plot ranked heat map by richness value
# -----------------------------------
p_rank_rich <- ggplot(
  df_rank_rich2,
  aes(
    x = scenario_year,
    y = PA_short,
    fill = pct_PA_area
  )
) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(pct_PA_area, 1)), size = 3) +
  scale_fill_gradientn(
    colours = c(
      "#7f0000",
      "#d7301f",
      "#fc8d59",
      "#fee08b",
      "#d9ef8b",
      "#91cf60",
      "#00cc44"
    ),
    values = scales::rescale(c(0, 0.5, 1, 2, 5, 10, 20)),
    name = "% PA\ncombined refugia richness"
  ) +
  facet_wrap(~ rich) +
  labs(
    x = "Scenario and year",
    y = "Protected Area",
    title = "Combined In Situ + Ex Situ Refugia Richness Ranked by Protected Area Category"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 9)
  )

print(p_rank_rich)



# calculate and rank top 10% sites

# INPUT PA
# -----------------------------------
PA <- st_read("F:/WWF_data/SDMs_LC_climate/PA/PA_clipped_to_study_area.shp", quiet = TRUE)
pa_v <- terra::vect(PA)

# -----------------------------------
# PA areas
# -----------------------------------
PA_area <- data.frame(
  PA = pa_v$PA,
  PA_area_km2 = as.numeric(terra::expanse(pa_v, unit = "km"))
)

PA_area_sum <- PA_area %>%
  group_by(PA) %>%
  summarise(
    PA_area_km2 = sum(PA_area_km2, na.rm = TRUE),
    .groups = "drop"
  )

print(PA_area_sum)

# -----------------------------------
# Run top 10% richness workflow
# -----------------------------------
top10_results <- list()

for (i in seq_len(nrow(rich_keys))) {
  
  cat("Processing:", i, "of", nrow(rich_keys), "\n")
  
  r_in <- terra::rast(rich_keys$file_insitu[i])
  r_ex <- terra::rast(rich_keys$file_exsitu[i])
  
  # make CRS match
  if (!terra::same.crs(r_in, r_ex)) {
    r_ex <- terra::project(r_ex, r_in, method = "near")
  }
  
  # make geometry match
  if (!isTRUE(terra::compareGeom(r_in, r_ex, stopOnError = FALSE))) {
    r_ex <- terra::resample(r_ex, r_in, method = "near")
  }
  
  # replace NA with 0
  rin0 <- terra::ifel(is.na(r_in), 0, r_in)
  rex0 <- terra::ifel(is.na(r_ex), 0, r_ex)
  
  # combined richness
  r_comb <- rin0 + rex0
  
  values_vec <- terra::values(r_comb, mat = FALSE)
  values_vec <- values_vec[!is.na(values_vec) & values_vec > 0]
  
  if (length(values_vec) == 0) {
    warning("No positive richness values for: ", rich_keys$year[i], "_", rich_keys$scenario[i])
    next
  }
  
  threshold <- as.numeric(stats::quantile(values_vec, 0.90, na.rm = TRUE))
  
  cat("Top10% threshold:", threshold, "\n")
  
  r_top10 <- terra::ifel(r_comb >= threshold, 1, NA)
  names(r_top10) <- paste0("top10_richness_", rich_keys$year[i], "_", rich_keys$scenario[i])
  
  a_cell <- prod(terra::res(r_top10)) / 1e6
  
  if (!terra::same.crs(r_top10, pa_v)) {
    pa_proj <- terra::project(pa_v, terra::crs(r_top10))
  } else {
    pa_proj <- pa_v
  }
  
  e <- terra::extract(r_top10, pa_proj)
  names(e)[2] <- "value"
  
  df <- e %>%
    filter(!is.na(value)) %>%
    group_by(ID) %>%
    summarise(
      n_cells = n(),
      .groups = "drop"
    ) %>%
    mutate(
      area_km2 = n_cells * a_cell,
      PA = pa_proj$PA[ID],
      year = rich_keys$year[i],
      scenario = rich_keys$scenario[i]
    )
  
  top10_results[[i]] <- df
  
  rm(r_in, r_ex, rin0, rex0, r_comb, r_top10, e, df, pa_proj, values_vec)
  gc()
}

df_top10 <- bind_rows(top10_results)

# -----------------------------------
# Aggregate to PA category
# -----------------------------------
df_top10 <- df_top10 %>%
  group_by(PA, year, scenario) %>%
  summarise(
    area_km2 = sum(area_km2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(PA_area_sum, by = "PA") %>%
  mutate(
    pct_PA = ifelse(
      PA_area_km2 > 0,
      (area_km2 / PA_area_km2) * 100,
      NA_real_
    ),
    scenario_year = paste(year, scenario, sep = "_")
  )

print(df_top10, n = Inf)

# -----------------------------------
# Rank PA categories
# -----------------------------------
df_top10_rank <- df_top10 %>%
  group_by(scenario_year) %>%
  arrange(desc(pct_PA), .by_group = TRUE) %>%
  mutate(
    PA_rank = row_number()
  ) %>%
  ungroup()

print(df_top10_rank, n = Inf)

# -----------------------------------
# Short labels
# -----------------------------------
df_top10_rank <- df_top10_rank %>%
  mutate(
    PA_short = dplyr::recode(
      PA,
      "Community Hunting Zone" = "CHZ",
      "Forest Management Unit" = "FMU",
      "National Park" = "NP",
      "Wildlife Sanctuary" = "WS"
    )
  )

PA_order <- df_top10_rank %>%
  group_by(PA_short) %>%
  summarise(mean_rank = mean(PA_rank, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_rank) %>%
  pull(PA_short)

df_top10_rank <- df_top10_rank %>%
  mutate(
    PA_short = factor(PA_short, levels = rev(PA_order)),
    scenario_year = factor(
      scenario_year,
      levels = c(
        "2050_ssp126", "2050_ssp245", "2050_ssp585",
        "2070_ssp126", "2070_ssp245", "2070_ssp585",
        "2100_ssp126", "2100_ssp245", "2100_ssp585"
      )
    )
  )

# -----------------------------------
# Plot
# -----------------------------------
p_top10 <- ggplot(
  df_top10_rank,
  aes(
    x = scenario_year,
    y = PA_short,
    fill = pct_PA
  )
) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(pct_PA, 1)), size = 3) +
  scale_fill_gradientn(
    colours = c(
      "#7f0000",
      "#d7301f",
      "#fc8d59",
      "#fee08b",
      "#d9ef8b",
      "#91cf60",
      "#00cc44"
    ),
    values = scales::rescale(c(0, 1, 3, 6, 10)),
    name = "% PA\nTop10 richness"
  ) +
  labs(
    x = "Scenario and year",
    y = "Protected Area",
    title = "Top 10% Combined Refugia Richness Hotspots by Protected Area Category"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_top10)


















