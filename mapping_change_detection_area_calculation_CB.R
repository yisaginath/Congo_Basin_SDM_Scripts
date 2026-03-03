# ape_elephant climate change analysis

# Read all .tif rasters inside a folder + all its subfolders, then print + plot first one
# Works with terra SpatRaster

library(terra)
library(sf)

# Root folder (your path)
root_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/current"

# 1) List all .tif files recursively (in all subfolders)
tif_files <- list.files(
  path = root_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

cat("Found", length(tif_files), ".tif files\n")

if (length(tif_files) == 0) {
  stop("No .tif files found. Double-check the folder path and file extensions.")
}

# 2) Read all rasters
# (This creates a list of SpatRaster objects; safer than stacking if extents differ across species)
rasters_list <- lapply(tif_files, function(f) {
  r <- rast(f)
  names(r) <- tools::file_path_sans_ext(basename(f))
  r
})

# 3) Print all rasters to verify (shows class, dims, resolution, extent, CRS, min/max, etc.)
for (i in seq_along(rasters_list)) {
  cat("\n--- Raster", i, "of", length(rasters_list), "---\n")
  cat("File:", tif_files[i], "\n")
  print(rasters_list[[i]])
}

# 4) Plot the first raster to visually verify (binary suitability)
plot(rasters_list[[1]], main = paste("First binary raster:", names(rasters_list[[1]])))


# read species range data
# Folder containing species range shapefiles
range_dir <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

# 1) List all shapefiles (set recursive=TRUE if they are inside subfolders)
shp_files <- list.files(
  path = range_dir,
  pattern = "\\.shp$",
  full.names = TRUE,
  recursive = FALSE   # change to TRUE if shapefiles are inside subfolders
)

cat("Found", length(shp_files), "shapefiles\n")
stopifnot(length(shp_files) > 0)

# 2) Read all shapefiles into a list
ranges_list <- lapply(shp_files, st_read, quiet = TRUE)
names(ranges_list) <- tools::file_path_sans_ext(basename(shp_files))

# 3) Print verification summary
for (i in seq_along(ranges_list)) {
  cat("\n--- Range", i, "of", length(ranges_list), "---\n")
  cat("File:", shp_files[i], "\n")
  print(ranges_list[[i]])
}

# 4) Optional: quick summary table
summary_table <- data.frame(
  file = basename(shp_files),
  geom_type = sapply(ranges_list, function(x) as.character(st_geometry_type(x)[1])),
  features = sapply(ranges_list, nrow),
  crs = sapply(ranges_list, function(x) st_crs(x)$input),
  xmin = sapply(ranges_list, function(x) st_bbox(x)["xmin"]),
  xmax = sapply(ranges_list, function(x) st_bbox(x)["xmax"]),
  ymin = sapply(ranges_list, function(x) st_bbox(x)["ymin"]),
  ymax = sapply(ranges_list, function(x) st_bbox(x)["ymax"]),
  stringsAsFactors = FALSE
)

print(summary_table)

# 5) Plot the first species range (verification)
plot(st_geometry(ranges_list[[1]]), main = paste("First species range:", names(ranges_list)[1]))



# rewrite code to clip and save each raster file to its corresponding range

library(terra)
library(sf)
library(stringr)

# ---------------------------
# 1) Paths
# ---------------------------

# Binary rasters (your earlier folder)
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/current"

# Species ranges (your current folder)
range_dir <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

# Output folder
out_dir <- file.path(raster_root, "clipped_to_range")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# 2) List files
# ---------------------------

tif_files <- list.files(raster_root, pattern="\\.tif$", full.names=TRUE, recursive=TRUE)
shp_files <- list.files(range_dir,  pattern="\\.shp$", full.names=TRUE, recursive=FALSE)

cat("Found", length(tif_files), "raster tif files\n")
cat("Found", length(shp_files), "range shapefiles\n")
stopifnot(length(tif_files) > 0, length(shp_files) > 0)

# ---------------------------
# 3) Build matching keys
#    (assumes ranges are like: blanchardi_range_clipped.shp
#     rasters contain species name like: "Acris blanchardi_mean_pixel_binary.tif"
# ---------------------------

# Key from shapefile names: "blanchardi_range_clipped" -> "blanchardi"
range_key <- tolower(basename(shp_files))
range_key <- str_replace(range_key, "_range\\.shp$", "")
names(shp_files) <- range_key

# Key from raster names: grab the second word in "Genus species_....tif"
# Example: "Acris blanchardi_mean_pixel_binary.tif" -> "blanchardi"
raster_base <- basename(tif_files)
raster_key <- tolower(raster_base)
raster_key <- str_replace(raster_key, "_mean_pixel_binary\\.tif$", "")
raster_key <- str_split_fixed(raster_key, " ", 3)[,2]  # second token = species epithet
names(tif_files) <- raster_key

# Report matching
cat("\nKeys in rasters:\n"); print(sort(unique(names(tif_files))))
cat("\nKeys in ranges:\n");  print(sort(unique(names(shp_files))))

common_keys <- intersect(unique(names(tif_files)), unique(names(shp_files)))
cat("\nCommon keys matched:", length(common_keys), "\n")
print(sort(common_keys))

if (length(common_keys) == 0) stop("No matches found. Check naming conventions.")

# ---------------------------
# 4) Clip each raster to its matching range and save
# ---------------------------

clipped_files <- character(0)

for (k in common_keys) {
  # Read raster
  r <- rast(tif_files[[k]])
  
  # Read range (sf -> terra vect)
  rng_sf <- st_read(shp_files[[k]], quiet = TRUE)
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Crop to bbox, then mask to polygon (strictly inside range)
  r_crop <- crop(r, rng_v)
  r_mask <- mask(r_crop, rng_v)
  
  # Write output
  out_file <- file.path(out_dir, paste0(k, "_binary_clipped.tif"))
  writeRaster(r_mask, out_file, overwrite = TRUE)
  
  clipped_files <- c(clipped_files, out_file)
  
  cat("Saved:", out_file, "\n")
}

cat("\nTotal clipped rasters saved:", length(clipped_files), "\n")

# ---------------------------
# 5) Plot the first clipped raster to verify
# ---------------------------
if (length(clipped_files) > 0) {
  r1 <- rast(clipped_files[1])
  plot(r1, main = paste("Clipped binary raster:", basename(clipped_files[1])))
}


# cover clipped range results to empty raster

# Paths
# -----------------------------
root_dir_bin   <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/current/clipped_to_range"
root_dir_empty <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/current/clipped_to_range/empty_rasters"

out_dir <- file.path(root_dir_bin, "binary_merged_to_empty")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Helper to extract species name
#   "cyclotis_binary_clipped.tif" -> "cyclotis"
#   "cyclotis_empty_raster.tif"   -> "cyclotis"
# -----------------------------
get_species_id <- function(x) {
  x <- tools::file_path_sans_ext(basename(x))
  x <- gsub("_binary_clipped$", "", x)
  x <- gsub("_empty_raster$",  "", x)
  x
}

# -----------------------------
# List files
# -----------------------------
bin_files   <- list.files(root_dir_bin,   pattern = "_binary_clipped\\.tif$", full.names = TRUE, recursive = TRUE)
empty_files <- list.files(root_dir_empty, pattern = "_empty_raster\\.tif$",  full.names = TRUE, recursive = FALSE)

cat("Found", length(bin_files), "binary rasters\n")
cat("Found", length(empty_files), "empty rasters\n")

if (length(bin_files) == 0)   stop("No *_binary_clipped.tif found in: ", root_dir_bin)
if (length(empty_files) == 0) stop("No *_empty_raster.tif found in: ", root_dir_empty)

# Map species -> file
bin_map   <- setNames(bin_files,   vapply(bin_files,   get_species_id, character(1)))
empty_map <- setNames(empty_files, vapply(empty_files, get_species_id, character(1)))

# Work only on shared species
species_common <- intersect(names(bin_map), names(empty_map))
cat("Matched", length(species_common), "species:\n", paste(species_common, collapse = ", "), "\n")

if (length(species_common) == 0) stop("No matching species between binaries and empty rasters.")

# -----------------------------
# Merge each binary to its empty raster
# (ensures same grid; fills outside binary extent with 0 from empty raster)
# -----------------------------
for (sp in species_common) {
  cat("\n--- Processing:", sp, "---\n")
  bin_r   <- rast(bin_map[[sp]])
  empty_r <- rast(empty_map[[sp]])
  
  # Ensure binary is on the empty raster grid (should already match, but safe)
  #if (!compareGeom(bin_r, empty_r, stopOnError = FALSE, crs = TRUE, extent = TRUE, rowcol = TRUE, res = TRUE)) {
  # bin_r <- resample(bin_r, empty_r, method = "near")  # binary -> nearest neighbor
  #}
  
  merged <- cover(bin_r, empty_r)  # keep bin values; fill remaining cells with empty (0)
  names(merged) <- paste0(sp, "_binary_on_empty")
  
  out_file <- file.path(out_dir, paste0(sp, "_binary_on_empty.tif"))
  writeRaster(merged, out_file, overwrite = TRUE)
  
  cat("Saved:", out_file, "\n")
  # Optional quick check:
  # print(merged)
}

# -----------------------------
# Report any unmatched files
# -----------------------------
only_bin   <- setdiff(names(bin_map), names(empty_map))
only_empty <- setdiff(names(empty_map), names(bin_map))

if (length(only_bin) > 0) {
  cat("\nNo empty raster found for these binaries:\n", paste(only_bin, collapse = ", "), "\n")
}
if (length(only_empty) > 0) {
  cat("\nNo binary raster found for these empties:\n", paste(only_empty, collapse = ", "), "\n")
}




# repeat for 2030
# ssp126

# ---------------------------
# 1) Paths
# ---------------------------

# Binary rasters (your earlier folder)
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2030/ssp126"

# Species ranges (your current folder)
range_dir <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

# Output folder
out_dir <- file.path(raster_root, "clipped_to_range")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# 2) List files
# ---------------------------

tif_files <- list.files(raster_root, pattern="\\.tif$", full.names=TRUE, recursive=TRUE)
shp_files <- list.files(range_dir,  pattern="\\.shp$", full.names=TRUE, recursive=FALSE)

cat("Found", length(tif_files), "raster tif files\n")
cat("Found", length(shp_files), "range shapefiles\n")
stopifnot(length(tif_files) > 0, length(shp_files) > 0)

# ---------------------------
# 3) Build matching keys
#    (assumes ranges are like: blanchardi_range_clipped.shp
#     rasters contain species name like: "Acris blanchardi_mean_pixel_binary.tif"
# ---------------------------

# Key from shapefile names: "blanchardi_range_clipped" -> "blanchardi"
range_key <- tolower(basename(shp_files))
range_key <- str_replace(range_key, "_range\\.shp$", "")
names(shp_files) <- range_key

# Key from raster names: grab the second word in "Genus species_....tif"
# Example: "Acris blanchardi_mean_pixel_binary.tif" -> "blanchardi"
raster_base <- basename(tif_files)
raster_key <- tolower(raster_base)
raster_key <- str_replace(raster_key, "_mean_pixel_binary_2030s_ssp126\\.tif$", "")
raster_key <- str_split_fixed(raster_key, " ", 3)[,2]  # second token = species epithet
names(tif_files) <- raster_key

# Report matching
cat("\nKeys in rasters:\n"); print(sort(unique(names(tif_files))))
cat("\nKeys in ranges:\n");  print(sort(unique(names(shp_files))))

common_keys <- intersect(unique(names(tif_files)), unique(names(shp_files)))
cat("\nCommon keys matched:", length(common_keys), "\n")
print(sort(common_keys))

if (length(common_keys) == 0) stop("No matches found. Check naming conventions.")

# ---------------------------
# 4) Clip each raster to its matching range and save
# ---------------------------

clipped_files <- character(0)

for (k in common_keys) {
  # Read raster
  r <- rast(tif_files[[k]])
  
  # Read range (sf -> terra vect)
  rng_sf <- st_read(shp_files[[k]], quiet = TRUE)
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Crop to bbox, then mask to polygon (strictly inside range)
  r_crop <- crop(r, rng_v)
  r_mask <- mask(r_crop, rng_v)
  
  # Write output
  out_file <- file.path(out_dir, paste0(k, "_binary_clipped_2030s_ssp126.tif"))
  writeRaster(r_mask, out_file, overwrite = TRUE)
  
  clipped_files <- c(clipped_files, out_file)
  
  cat("Saved:", out_file, "\n")
}

cat("\nTotal clipped rasters saved:", length(clipped_files), "\n")

# ---------------------------
# 5) Plot the first clipped raster to verify
# ---------------------------
if (length(clipped_files) > 0) {
  r1 <- rast(clipped_files[1])
  plot(r1, main = paste("Clipped binary raster:", basename(clipped_files[1])))
}


# cover clipped range results to empty raster

# -----------------------------
# Paths
# -----------------------------
root_dir_bin   <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2030/ssp126/clipped_to_range"
root_dir_empty <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/current/clipped_to_range/empty_rasters"

out_dir <- file.path(root_dir_bin, "binary_merged_to_empty")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Helper: robust species id
#   "cyclotis_binary_clipped_2030s_ssp126.tif" -> "cyclotis"
#   "cyclotis_binary_clipped.tif"             -> "cyclotis"
#   "cyclotis_empty_raster.tif"               -> "cyclotis"
# -----------------------------
get_species_id <- function(x) {
  x <- tools::file_path_sans_ext(basename(x))
  x <- sub("_binary_clipped.*$", "", x)  # remove _binary_clipped + anything after
  x <- sub("_empty_raster.*$",  "", x)   # remove _empty_raster + anything after (if any)
  x
}

# -----------------------------
# List files
# -----------------------------
bin_files   <- list.files(root_dir_bin,   pattern = "_binary_clipped.*\\.tif$", full.names = TRUE, recursive = TRUE)
empty_files <- list.files(root_dir_empty, pattern = "_empty_raster\\.tif$",     full.names = TRUE, recursive = FALSE)

cat("Found", length(bin_files), "binary rasters\n")
cat("Found", length(empty_files), "empty rasters\n")

if (length(bin_files) == 0)   stop("No binary rasters found in: ", root_dir_bin)
if (length(empty_files) == 0) stop("No empty rasters found in: ", root_dir_empty)

# Map species -> file
bin_map   <- setNames(bin_files,   vapply(bin_files,   get_species_id, character(1)))
empty_map <- setNames(empty_files, vapply(empty_files, get_species_id, character(1)))

# Shared species
species_common <- intersect(names(bin_map), names(empty_map))
cat("Matched", length(species_common), "species:\n", paste(species_common, collapse = ", "), "\n")

if (length(species_common) == 0) {
  cat("\nBinary IDs:\n");  print(names(bin_map))
  cat("\nEmpty IDs:\n");   print(names(empty_map))
  stop("No matching species names. See IDs printed above.")
}

# -----------------------------
# Merge: binary onto empty grid
# -----------------------------
for (sp in species_common) {
  cat("\n--- Processing:", sp, "---\n")
  
  bin_r   <- rast(bin_map[[sp]])
  empty_r <- rast(empty_map[[sp]])
  
  # Force binary onto empty raster grid (safe even if already matching)
  same_grid <- terra::compareGeom(bin_r, empty_r, stopOnError = FALSE)
  
  if (!isTRUE(same_grid)) {
    bin_r <- terra::resample(bin_r, empty_r, method = "near")  # nearest neighbor for binary
  }
  
  merged <- cover(bin_r, empty_r)  # keep binary where it exists; fill rest with 0s from empty
  names(merged) <- paste0(sp, "_binary_on_empty")
  
  out_file <- file.path(out_dir, paste0(sp, "_binary_on_empty_2030s_ssp126.tif"))
  writeRaster(merged, out_file, overwrite = TRUE)
  
  cat("Saved:", out_file, "\n")
}

# -----------------------------
# Report unmatched
# -----------------------------
only_bin   <- setdiff(names(bin_map), names(empty_map))
only_empty <- setdiff(names(empty_map), names(bin_map))

if (length(only_bin) > 0) {
  cat("\nNo empty raster found for these binaries:\n", paste(only_bin, collapse = ", "), "\n")
}
if (length(only_empty) > 0) {
  cat("\nNo binary raster found for these empties:\n", paste(only_empty, collapse = ", "), "\n")
}


# ---------------------------
# 6) Save raster cells OUTSIDE species range

# Inputs
# -----------------------------
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2030/ssp126"
range_dir   <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

target_folders <- c("bonobos", "chimps", "elephants", "gorillas")

# Only those 4 subfolders
selected_dirs <- file.path(raster_root, target_folders)
selected_dirs <- selected_dirs[dir.exists(selected_dirs)]

# Raster tif files (grab all tifs inside each selected folder)
tif_files <- unlist(lapply(selected_dirs, function(d) {
  list.files(d, pattern = "\\.tif$", full.names = TRUE)
}))
cat("Found", length(tif_files), "tif files\n")

# Range shapefiles
shp_files <- list.files(range_dir, pattern="\\.shp$", full.names=TRUE, recursive=FALSE)
cat("Found", length(shp_files), "range shapefiles\n")
print(basename(shp_files))

# -----------------------------
# Build species -> raster map (folder name as key)
# -----------------------------
get_raster_key <- function(path) basename(dirname(path))
raster_map <- setNames(tif_files, vapply(tif_files, get_raster_key, character(1)))

# -----------------------------
# Build folder -> shapefile map using your naming scheme
# -----------------------------
# folder key      -> shapefile stem (without .shp)
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"   # placeholder: you don't have this shapefile yet
)

# Turn shapefiles into a lookup by stem
# Turn shapefiles into a lookup by stem (strip "_range")
shp_stems  <- tools::file_path_sans_ext(basename(shp_files))
shp_stems  <- sub("_range$", "", shp_stems)
shp_lookup <- setNames(shp_files, shp_stems)

# folder key -> shapefile stem
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"     # forest elephant (Loxodonta cyclotis), per your files
)

# Build shp_map keyed by folder name
shp_map <- setNames(rep(NA_character_, length(target_folders)), target_folders)
for (sp in target_folders) {
  stem <- range_name_map[[sp]]
  if (!is.null(stem) && stem %in% names(shp_lookup)) {
    shp_map[[sp]] <- shp_lookup[[stem]]
  }
}
shp_map <- shp_map[!is.na(shp_map)]

# Shared keys
common_keys <- intersect(names(raster_map), names(shp_map))
cat("Matched", length(common_keys), "folders:", paste(common_keys, collapse=", "), "\n")

# Report missing
missing_ranges <- setdiff(names(raster_map), names(shp_map))
if (length(missing_ranges) > 0) {
  cat("\nNo range shapefile found for:", paste(missing_ranges, collapse=", "), "\n")
}

# -----------------------------
# Output
# -----------------------------
out_dir_outside <- file.path(raster_root, "outside_species_range")
dir.create(out_dir_outside, recursive = TRUE, showWarnings = FALSE)

outside_files <- character(0)

for (k in common_keys) {
  
  r <- rast(raster_map[[k]])
  
  rng_sf <- st_read(shp_map[[k]], quiet = TRUE)
  rng_sf <- st_make_valid(rng_sf)          # helps if geometry is messy
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Keep ONLY cells outside the range polygon
  r_outside <- mask(r, rng_v, inverse = TRUE)
  
  out_file <- file.path(out_dir_outside, paste0(k, "_binary_OUTSIDE_range_2030_ssp126.tif"))
  writeRaster(r_outside, out_file, overwrite = TRUE)
  
  outside_files <- c(outside_files, out_file)
  cat("Saved outside-range raster:", out_file, "\n")
}




# ssp245

# ---------------------------
# 1) Paths
# ---------------------------

# Binary rasters (your earlier folder)
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2030/ssp245"

# Species ranges (your current folder)
range_dir <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

# Output folder
out_dir <- file.path(raster_root, "clipped_to_range")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# 2) List files
# ---------------------------

tif_files <- list.files(raster_root, pattern="\\.tif$", full.names=TRUE, recursive=TRUE)
shp_files <- list.files(range_dir,  pattern="\\.shp$", full.names=TRUE, recursive=FALSE)

cat("Found", length(tif_files), "raster tif files\n")
cat("Found", length(shp_files), "range shapefiles\n")
stopifnot(length(tif_files) > 0, length(shp_files) > 0)

# ---------------------------
# 3) Build matching keys
#    (assumes ranges are like: blanchardi_range_clipped.shp
#     rasters contain species name like: "Acris blanchardi_mean_pixel_binary.tif"
# ---------------------------

# Key from shapefile names: "blanchardi_range_clipped" -> "blanchardi"
range_key <- tolower(basename(shp_files))
range_key <- str_replace(range_key, "_range\\.shp$", "")
names(shp_files) <- range_key

# Key from raster names: grab the second word in "Genus species_....tif"
# Example: "Acris blanchardi_mean_pixel_binary.tif" -> "blanchardi"
raster_base <- basename(tif_files)
raster_key <- tolower(raster_base)
raster_key <- str_replace(raster_key, "_mean_pixel_binary_2030s_ssp245\\.tif$", "")
raster_key <- str_split_fixed(raster_key, " ", 3)[,2]  # second token = species epithet
names(tif_files) <- raster_key

# Report matching
cat("\nKeys in rasters:\n"); print(sort(unique(names(tif_files))))
cat("\nKeys in ranges:\n");  print(sort(unique(names(shp_files))))

common_keys <- intersect(unique(names(tif_files)), unique(names(shp_files)))
cat("\nCommon keys matched:", length(common_keys), "\n")
print(sort(common_keys))

if (length(common_keys) == 0) stop("No matches found. Check naming conventions.")

# ---------------------------
# 4) Clip each raster to its matching range and save
# ---------------------------

clipped_files <- character(0)

for (k in common_keys) {
  # Read raster
  r <- rast(tif_files[[k]])
  
  # Read range (sf -> terra vect)
  rng_sf <- st_read(shp_files[[k]], quiet = TRUE)
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Crop to bbox, then mask to polygon (strictly inside range)
  r_crop <- crop(r, rng_v)
  r_mask <- mask(r_crop, rng_v)
  
  # Write output
  out_file <- file.path(out_dir, paste0(k, "_binary_clipped_2030s_ssp245.tif"))
  writeRaster(r_mask, out_file, overwrite = TRUE)
  
  clipped_files <- c(clipped_files, out_file)
  
  cat("Saved:", out_file, "\n")
}

cat("\nTotal clipped rasters saved:", length(clipped_files), "\n")

# ---------------------------
# 5) Plot the first clipped raster to verify
# ---------------------------
if (length(clipped_files) > 0) {
  r1 <- rast(clipped_files[1])
  plot(r1, main = paste("Clipped binary raster:", basename(clipped_files[1])))
}


# cover clipped range results to empty raster

# -----------------------------
# Paths
# -----------------------------
root_dir_bin   <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2030/ssp245/clipped_to_range"
root_dir_empty <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/current/clipped_to_range/empty_rasters"

out_dir <- file.path(root_dir_bin, "binary_merged_to_empty")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Helper: robust species id
#   "cyclotis_binary_clipped_2030s_ssp126.tif" -> "cyclotis"
#   "cyclotis_binary_clipped.tif"             -> "cyclotis"
#   "cyclotis_empty_raster.tif"               -> "cyclotis"
# -----------------------------
get_species_id <- function(x) {
  x <- tools::file_path_sans_ext(basename(x))
  x <- sub("_binary_clipped.*$", "", x)  # remove _binary_clipped + anything after
  x <- sub("_empty_raster.*$",  "", x)   # remove _empty_raster + anything after (if any)
  x
}

# -----------------------------
# List files
# -----------------------------
bin_files   <- list.files(root_dir_bin,   pattern = "_binary_clipped.*\\.tif$", full.names = TRUE, recursive = TRUE)
empty_files <- list.files(root_dir_empty, pattern = "_empty_raster\\.tif$",     full.names = TRUE, recursive = FALSE)

cat("Found", length(bin_files), "binary rasters\n")
cat("Found", length(empty_files), "empty rasters\n")

if (length(bin_files) == 0)   stop("No binary rasters found in: ", root_dir_bin)
if (length(empty_files) == 0) stop("No empty rasters found in: ", root_dir_empty)

# Map species -> file
bin_map   <- setNames(bin_files,   vapply(bin_files,   get_species_id, character(1)))
empty_map <- setNames(empty_files, vapply(empty_files, get_species_id, character(1)))

# Shared species
species_common <- intersect(names(bin_map), names(empty_map))
cat("Matched", length(species_common), "species:\n", paste(species_common, collapse = ", "), "\n")

if (length(species_common) == 0) {
  cat("\nBinary IDs:\n");  print(names(bin_map))
  cat("\nEmpty IDs:\n");   print(names(empty_map))
  stop("No matching species names. See IDs printed above.")
}

# -----------------------------
# Merge: binary onto empty grid
# -----------------------------
for (sp in species_common) {
  cat("\n--- Processing:", sp, "---\n")
  
  bin_r   <- rast(bin_map[[sp]])
  empty_r <- rast(empty_map[[sp]])
  
  # Force binary onto empty raster grid (safe even if already matching)
  same_grid <- terra::compareGeom(bin_r, empty_r, stopOnError = FALSE)
  
  if (!isTRUE(same_grid)) {
    bin_r <- terra::resample(bin_r, empty_r, method = "near")  # nearest neighbor for binary
  }
  
  merged <- cover(bin_r, empty_r)  # keep binary where it exists; fill rest with 0s from empty
  names(merged) <- paste0(sp, "_binary_on_empty")
  
  out_file <- file.path(out_dir, paste0(sp, "_binary_on_empty_2030s_ssp245.tif"))
  writeRaster(merged, out_file, overwrite = TRUE)
  
  cat("Saved:", out_file, "\n")
}

# -----------------------------
# Report unmatched
# -----------------------------
only_bin   <- setdiff(names(bin_map), names(empty_map))
only_empty <- setdiff(names(empty_map), names(bin_map))

if (length(only_bin) > 0) {
  cat("\nNo empty raster found for these binaries:\n", paste(only_bin, collapse = ", "), "\n")
}
if (length(only_empty) > 0) {
  cat("\nNo binary raster found for these empties:\n", paste(only_empty, collapse = ", "), "\n")
}


# 6) Save raster cells OUTSIDE species range

# Inputs
# -----------------------------
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2030/ssp245"
range_dir   <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

target_folders <- c("bonobos", "chimps", "elephants", "gorillas")

# Only those 4 subfolders
selected_dirs <- file.path(raster_root, target_folders)
selected_dirs <- selected_dirs[dir.exists(selected_dirs)]

# Raster tif files (grab all tifs inside each selected folder)
tif_files <- unlist(lapply(selected_dirs, function(d) {
  list.files(d, pattern = "\\.tif$", full.names = TRUE)
}))
cat("Found", length(tif_files), "tif files\n")

# Range shapefiles
shp_files <- list.files(range_dir, pattern="\\.shp$", full.names=TRUE, recursive=FALSE)
cat("Found", length(shp_files), "range shapefiles\n")
print(basename(shp_files))

# -----------------------------
# Build species -> raster map (folder name as key)
# -----------------------------
get_raster_key <- function(path) basename(dirname(path))
raster_map <- setNames(tif_files, vapply(tif_files, get_raster_key, character(1)))

# -----------------------------
# Build folder -> shapefile map using your naming scheme
# -----------------------------
# folder key      -> shapefile stem (without .shp)
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"   # placeholder: you don't have this shapefile yet
)

# Turn shapefiles into a lookup by stem
# Turn shapefiles into a lookup by stem (strip "_range")
shp_stems  <- tools::file_path_sans_ext(basename(shp_files))
shp_stems  <- sub("_range$", "", shp_stems)
shp_lookup <- setNames(shp_files, shp_stems)

# folder key -> shapefile stem
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"     # forest elephant (Loxodonta cyclotis), per your files
)

# Build shp_map keyed by folder name
shp_map <- setNames(rep(NA_character_, length(target_folders)), target_folders)
for (sp in target_folders) {
  stem <- range_name_map[[sp]]
  if (!is.null(stem) && stem %in% names(shp_lookup)) {
    shp_map[[sp]] <- shp_lookup[[stem]]
  }
}
shp_map <- shp_map[!is.na(shp_map)]

# Shared keys
common_keys <- intersect(names(raster_map), names(shp_map))
cat("Matched", length(common_keys), "folders:", paste(common_keys, collapse=", "), "\n")

# Report missing
missing_ranges <- setdiff(names(raster_map), names(shp_map))
if (length(missing_ranges) > 0) {
  cat("\nNo range shapefile found for:", paste(missing_ranges, collapse=", "), "\n")
}

# -----------------------------
# Output
# -----------------------------
out_dir_outside <- file.path(raster_root, "outside_species_range")
dir.create(out_dir_outside, recursive = TRUE, showWarnings = FALSE)

outside_files <- character(0)

for (k in common_keys) {
  
  r <- rast(raster_map[[k]])
  
  rng_sf <- st_read(shp_map[[k]], quiet = TRUE)
  rng_sf <- st_make_valid(rng_sf)          # helps if geometry is messy
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Keep ONLY cells outside the range polygon
  r_outside <- mask(r, rng_v, inverse = TRUE)
  
  out_file <- file.path(out_dir_outside, paste0(k, "_binary_OUTSIDE_range_2030_ssp245.tif"))
  writeRaster(r_outside, out_file, overwrite = TRUE)
  
  outside_files <- c(outside_files, out_file)
  cat("Saved outside-range raster:", out_file, "\n")
}





# ssp585

# ---------------------------
# 1) Paths
# ---------------------------

# Binary rasters (your earlier folder)
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2030/ssp585"

# Species ranges (your current folder)
range_dir <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

# Output folder
out_dir <- file.path(raster_root, "clipped_to_range")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# 2) List files
# ---------------------------

tif_files <- list.files(raster_root, pattern="\\.tif$", full.names=TRUE, recursive=TRUE)
shp_files <- list.files(range_dir,  pattern="\\.shp$", full.names=TRUE, recursive=FALSE)

cat("Found", length(tif_files), "raster tif files\n")
cat("Found", length(shp_files), "range shapefiles\n")
stopifnot(length(tif_files) > 0, length(shp_files) > 0)

# ---------------------------
# 3) Build matching keys
#    (assumes ranges are like: blanchardi_range_clipped.shp
#     rasters contain species name like: "Acris blanchardi_mean_pixel_binary.tif"
# ---------------------------

# Key from shapefile names: "blanchardi_range_clipped" -> "blanchardi"
range_key <- tolower(basename(shp_files))
range_key <- str_replace(range_key, "_range\\.shp$", "")
names(shp_files) <- range_key

# Key from raster names: grab the second word in "Genus species_....tif"
# Example: "Acris blanchardi_mean_pixel_binary.tif" -> "blanchardi"
raster_base <- basename(tif_files)
raster_key <- tolower(raster_base)
raster_key <- str_replace(raster_key, "_mean_pixel_binary_2030s_ssp585\\.tif$", "")
raster_key <- str_split_fixed(raster_key, " ", 3)[,2]  # second token = species epithet
names(tif_files) <- raster_key

# Report matching
cat("\nKeys in rasters:\n"); print(sort(unique(names(tif_files))))
cat("\nKeys in ranges:\n");  print(sort(unique(names(shp_files))))

common_keys <- intersect(unique(names(tif_files)), unique(names(shp_files)))
cat("\nCommon keys matched:", length(common_keys), "\n")
print(sort(common_keys))

if (length(common_keys) == 0) stop("No matches found. Check naming conventions.")

# ---------------------------
# 4) Clip each raster to its matching range and save
# ---------------------------

clipped_files <- character(0)

for (k in common_keys) {
  # Read raster
  r <- rast(tif_files[[k]])
  
  # Read range (sf -> terra vect)
  rng_sf <- st_read(shp_files[[k]], quiet = TRUE)
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Crop to bbox, then mask to polygon (strictly inside range)
  r_crop <- crop(r, rng_v)
  r_mask <- mask(r_crop, rng_v)
  
  # Write output
  out_file <- file.path(out_dir, paste0(k, "_binary_clipped_2030s_ssp585.tif"))
  writeRaster(r_mask, out_file, overwrite = TRUE)
  
  clipped_files <- c(clipped_files, out_file)
  
  cat("Saved:", out_file, "\n")
}

cat("\nTotal clipped rasters saved:", length(clipped_files), "\n")

# ---------------------------
# 5) Plot the first clipped raster to verify
# ---------------------------
if (length(clipped_files) > 0) {
  r1 <- rast(clipped_files[1])
  plot(r1, main = paste("Clipped binary raster:", basename(clipped_files[1])))
}


# cover clipped range results to empty raster

# -----------------------------
# Paths
# -----------------------------
root_dir_bin   <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2030/ssp585/clipped_to_range"
root_dir_empty <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/current/clipped_to_range/empty_rasters"

out_dir <- file.path(root_dir_bin, "binary_merged_to_empty")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Helper: robust species id
#   "cyclotis_binary_clipped_2030s_ssp126.tif" -> "cyclotis"
#   "cyclotis_binary_clipped.tif"             -> "cyclotis"
#   "cyclotis_empty_raster.tif"               -> "cyclotis"
# -----------------------------
get_species_id <- function(x) {
  x <- tools::file_path_sans_ext(basename(x))
  x <- sub("_binary_clipped.*$", "", x)  # remove _binary_clipped + anything after
  x <- sub("_empty_raster.*$",  "", x)   # remove _empty_raster + anything after (if any)
  x
}

# -----------------------------
# List files
# -----------------------------
bin_files   <- list.files(root_dir_bin,   pattern = "_binary_clipped.*\\.tif$", full.names = TRUE, recursive = TRUE)
empty_files <- list.files(root_dir_empty, pattern = "_empty_raster\\.tif$",     full.names = TRUE, recursive = FALSE)

cat("Found", length(bin_files), "binary rasters\n")
cat("Found", length(empty_files), "empty rasters\n")

if (length(bin_files) == 0)   stop("No binary rasters found in: ", root_dir_bin)
if (length(empty_files) == 0) stop("No empty rasters found in: ", root_dir_empty)

# Map species -> file
bin_map   <- setNames(bin_files,   vapply(bin_files,   get_species_id, character(1)))
empty_map <- setNames(empty_files, vapply(empty_files, get_species_id, character(1)))

# Shared species
species_common <- intersect(names(bin_map), names(empty_map))
cat("Matched", length(species_common), "species:\n", paste(species_common, collapse = ", "), "\n")

if (length(species_common) == 0) {
  cat("\nBinary IDs:\n");  print(names(bin_map))
  cat("\nEmpty IDs:\n");   print(names(empty_map))
  stop("No matching species names. See IDs printed above.")
}

# -----------------------------
# Merge: binary onto empty grid
# -----------------------------
for (sp in species_common) {
  cat("\n--- Processing:", sp, "---\n")
  
  bin_r   <- rast(bin_map[[sp]])
  empty_r <- rast(empty_map[[sp]])
  
  # Force binary onto empty raster grid (safe even if already matching)
  same_grid <- terra::compareGeom(bin_r, empty_r, stopOnError = FALSE)
  
  if (!isTRUE(same_grid)) {
    bin_r <- terra::resample(bin_r, empty_r, method = "near")  # nearest neighbor for binary
  }
  
  merged <- cover(bin_r, empty_r)  # keep binary where it exists; fill rest with 0s from empty
  names(merged) <- paste0(sp, "_binary_on_empty")
  
  out_file <- file.path(out_dir, paste0(sp, "_binary_on_empty_2030s_ssp585.tif"))
  writeRaster(merged, out_file, overwrite = TRUE)
  
  cat("Saved:", out_file, "\n")
}

# -----------------------------
# Report unmatched
# -----------------------------
only_bin   <- setdiff(names(bin_map), names(empty_map))
only_empty <- setdiff(names(empty_map), names(bin_map))

if (length(only_bin) > 0) {
  cat("\nNo empty raster found for these binaries:\n", paste(only_bin, collapse = ", "), "\n")
}
if (length(only_empty) > 0) {
  cat("\nNo binary raster found for these empties:\n", paste(only_empty, collapse = ", "), "\n")
}


# 6) Save raster cells OUTSIDE species range

# Inputs
# -----------------------------
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2030/ssp585"
range_dir   <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

target_folders <- c("bonobos", "chimps", "elephants", "gorillas")

# Only those 4 subfolders
selected_dirs <- file.path(raster_root, target_folders)
selected_dirs <- selected_dirs[dir.exists(selected_dirs)]

# Raster tif files (grab all tifs inside each selected folder)
tif_files <- unlist(lapply(selected_dirs, function(d) {
  list.files(d, pattern = "\\.tif$", full.names = TRUE)
}))
cat("Found", length(tif_files), "tif files\n")

# Range shapefiles
shp_files <- list.files(range_dir, pattern="\\.shp$", full.names=TRUE, recursive=FALSE)
cat("Found", length(shp_files), "range shapefiles\n")
print(basename(shp_files))

# -----------------------------
# Build species -> raster map (folder name as key)
# -----------------------------
get_raster_key <- function(path) basename(dirname(path))
raster_map <- setNames(tif_files, vapply(tif_files, get_raster_key, character(1)))

# -----------------------------
# Build folder -> shapefile map using your naming scheme
# -----------------------------
# folder key      -> shapefile stem (without .shp)
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"   # placeholder: you don't have this shapefile yet
)

# Turn shapefiles into a lookup by stem
# Turn shapefiles into a lookup by stem (strip "_range")
shp_stems  <- tools::file_path_sans_ext(basename(shp_files))
shp_stems  <- sub("_range$", "", shp_stems)
shp_lookup <- setNames(shp_files, shp_stems)

# folder key -> shapefile stem
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"     # forest elephant (Loxodonta cyclotis), per your files
)

# Build shp_map keyed by folder name
shp_map <- setNames(rep(NA_character_, length(target_folders)), target_folders)
for (sp in target_folders) {
  stem <- range_name_map[[sp]]
  if (!is.null(stem) && stem %in% names(shp_lookup)) {
    shp_map[[sp]] <- shp_lookup[[stem]]
  }
}
shp_map <- shp_map[!is.na(shp_map)]

# Shared keys
common_keys <- intersect(names(raster_map), names(shp_map))
cat("Matched", length(common_keys), "folders:", paste(common_keys, collapse=", "), "\n")

# Report missing
missing_ranges <- setdiff(names(raster_map), names(shp_map))
if (length(missing_ranges) > 0) {
  cat("\nNo range shapefile found for:", paste(missing_ranges, collapse=", "), "\n")
}

# -----------------------------
# Output
# -----------------------------
out_dir_outside <- file.path(raster_root, "outside_species_range")
dir.create(out_dir_outside, recursive = TRUE, showWarnings = FALSE)

outside_files <- character(0)

for (k in common_keys) {
  
  r <- rast(raster_map[[k]])
  
  rng_sf <- st_read(shp_map[[k]], quiet = TRUE)
  rng_sf <- st_make_valid(rng_sf)          # helps if geometry is messy
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Keep ONLY cells outside the range polygon
  r_outside <- mask(r, rng_v, inverse = TRUE)
  
  out_file <- file.path(out_dir_outside, paste0(k, "_binary_OUTSIDE_range_2030_ssp585.tif"))
  writeRaster(r_outside, out_file, overwrite = TRUE)
  
  outside_files <- c(outside_files, out_file)
  cat("Saved outside-range raster:", out_file, "\n")
}



#-----------------2050------------

# repeat for 2050
# ssp126

# ---------------------------
# 1) Paths
# ---------------------------

# Binary rasters (your earlier folder)
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2050/ssp126"

# Species ranges (your current folder)
range_dir <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

# Output folder
out_dir <- file.path(raster_root, "clipped_to_range")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# 2) List files
# ---------------------------

tif_files <- list.files(raster_root, pattern="\\.tif$", full.names=TRUE, recursive=TRUE)
shp_files <- list.files(range_dir,  pattern="\\.shp$", full.names=TRUE, recursive=FALSE)

cat("Found", length(tif_files), "raster tif files\n")
cat("Found", length(shp_files), "range shapefiles\n")
stopifnot(length(tif_files) > 0, length(shp_files) > 0)

# ---------------------------
# 3) Build matching keys
#    (assumes ranges are like: blanchardi_range_clipped.shp
#     rasters contain species name like: "Acris blanchardi_mean_pixel_binary.tif"
# ---------------------------

# Key from shapefile names: "blanchardi_range_clipped" -> "blanchardi"
range_key <- tolower(basename(shp_files))
range_key <- str_replace(range_key, "_range\\.shp$", "")
names(shp_files) <- range_key

# Key from raster names: grab the second word in "Genus species_....tif"
# Example: "Acris blanchardi_mean_pixel_binary.tif" -> "blanchardi"
raster_base <- basename(tif_files)
raster_key <- tolower(raster_base)
raster_key <- str_replace(raster_key, "_mean_pixel_binary_2050s_ssp126\\.tif$", "")
raster_key <- str_split_fixed(raster_key, " ", 3)[,2]  # second token = species epithet
names(tif_files) <- raster_key

# Report matching
cat("\nKeys in rasters:\n"); print(sort(unique(names(tif_files))))
cat("\nKeys in ranges:\n");  print(sort(unique(names(shp_files))))

common_keys <- intersect(unique(names(tif_files)), unique(names(shp_files)))
cat("\nCommon keys matched:", length(common_keys), "\n")
print(sort(common_keys))

if (length(common_keys) == 0) stop("No matches found. Check naming conventions.")

# ---------------------------
# 4) Clip each raster to its matching range and save
# ---------------------------

clipped_files <- character(0)

for (k in common_keys) {
  # Read raster
  r <- rast(tif_files[[k]])
  
  # Read range (sf -> terra vect)
  rng_sf <- st_read(shp_files[[k]], quiet = TRUE)
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Crop to bbox, then mask to polygon (strictly inside range)
  r_crop <- crop(r, rng_v)
  r_mask <- mask(r_crop, rng_v)
  
  # Write output
  out_file <- file.path(out_dir, paste0(k, "_binary_clipped_2050s_ssp126.tif"))
  writeRaster(r_mask, out_file, overwrite = TRUE)
  
  clipped_files <- c(clipped_files, out_file)
  
  cat("Saved:", out_file, "\n")
}

cat("\nTotal clipped rasters saved:", length(clipped_files), "\n")

# ---------------------------
# 5) Plot the first clipped raster to verify
# ---------------------------
if (length(clipped_files) > 0) {
  r1 <- rast(clipped_files[1])
  plot(r1, main = paste("Clipped binary raster:", basename(clipped_files[1])))
}


# cover clipped range results to empty raster

# -----------------------------
# Paths
# -----------------------------
root_dir_bin   <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2050/ssp126/clipped_to_range"
root_dir_empty <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/current/clipped_to_range/empty_rasters"

out_dir <- file.path(root_dir_bin, "binary_merged_to_empty")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Helper: robust species id
#   "cyclotis_binary_clipped_2030s_ssp126.tif" -> "cyclotis"
#   "cyclotis_binary_clipped.tif"             -> "cyclotis"
#   "cyclotis_empty_raster.tif"               -> "cyclotis"
# -----------------------------
get_species_id <- function(x) {
  x <- tools::file_path_sans_ext(basename(x))
  x <- sub("_binary_clipped.*$", "", x)  # remove _binary_clipped + anything after
  x <- sub("_empty_raster.*$",  "", x)   # remove _empty_raster + anything after (if any)
  x
}

# -----------------------------
# List files
# -----------------------------
bin_files   <- list.files(root_dir_bin,   pattern = "_binary_clipped.*\\.tif$", full.names = TRUE, recursive = TRUE)
empty_files <- list.files(root_dir_empty, pattern = "_empty_raster\\.tif$",     full.names = TRUE, recursive = FALSE)

cat("Found", length(bin_files), "binary rasters\n")
cat("Found", length(empty_files), "empty rasters\n")

if (length(bin_files) == 0)   stop("No binary rasters found in: ", root_dir_bin)
if (length(empty_files) == 0) stop("No empty rasters found in: ", root_dir_empty)

# Map species -> file
bin_map   <- setNames(bin_files,   vapply(bin_files,   get_species_id, character(1)))
empty_map <- setNames(empty_files, vapply(empty_files, get_species_id, character(1)))

# Shared species
species_common <- intersect(names(bin_map), names(empty_map))
cat("Matched", length(species_common), "species:\n", paste(species_common, collapse = ", "), "\n")

if (length(species_common) == 0) {
  cat("\nBinary IDs:\n");  print(names(bin_map))
  cat("\nEmpty IDs:\n");   print(names(empty_map))
  stop("No matching species names. See IDs printed above.")
}

# -----------------------------
# Merge: binary onto empty grid
# -----------------------------
for (sp in species_common) {
  cat("\n--- Processing:", sp, "---\n")
  
  bin_r   <- rast(bin_map[[sp]])
  empty_r <- rast(empty_map[[sp]])
  
  # Force binary onto empty raster grid (safe even if already matching)
  same_grid <- terra::compareGeom(bin_r, empty_r, stopOnError = FALSE)
  
  if (!isTRUE(same_grid)) {
    bin_r <- terra::resample(bin_r, empty_r, method = "near")  # nearest neighbor for binary
  }
  
  merged <- cover(bin_r, empty_r)  # keep binary where it exists; fill rest with 0s from empty
  names(merged) <- paste0(sp, "_binary_on_empty")
  
  out_file <- file.path(out_dir, paste0(sp, "_binary_on_empty_2050s_ssp126.tif"))
  writeRaster(merged, out_file, overwrite = TRUE)
  
  cat("Saved:", out_file, "\n")
}

# -----------------------------
# Report unmatched
# -----------------------------
only_bin   <- setdiff(names(bin_map), names(empty_map))
only_empty <- setdiff(names(empty_map), names(bin_map))

if (length(only_bin) > 0) {
  cat("\nNo empty raster found for these binaries:\n", paste(only_bin, collapse = ", "), "\n")
}
if (length(only_empty) > 0) {
  cat("\nNo binary raster found for these empties:\n", paste(only_empty, collapse = ", "), "\n")
}


# ---------------------------
# 6) Save raster cells OUTSIDE species range

# Inputs
# -----------------------------
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2050/ssp126"
range_dir   <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

target_folders <- c("bonobos", "chimps", "elephants", "gorillas")

# Only those 4 subfolders
selected_dirs <- file.path(raster_root, target_folders)
selected_dirs <- selected_dirs[dir.exists(selected_dirs)]

# Raster tif files (grab all tifs inside each selected folder)
tif_files <- unlist(lapply(selected_dirs, function(d) {
  list.files(d, pattern = "\\.tif$", full.names = TRUE)
}))
cat("Found", length(tif_files), "tif files\n")

# Range shapefiles
shp_files <- list.files(range_dir, pattern="\\.shp$", full.names=TRUE, recursive=FALSE)
cat("Found", length(shp_files), "range shapefiles\n")
print(basename(shp_files))

# -----------------------------
# Build species -> raster map (folder name as key)
# -----------------------------
get_raster_key <- function(path) basename(dirname(path))
raster_map <- setNames(tif_files, vapply(tif_files, get_raster_key, character(1)))

# -----------------------------
# Build folder -> shapefile map using your naming scheme
# -----------------------------
# folder key      -> shapefile stem (without .shp)
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"   # placeholder: you don't have this shapefile yet
)

# Turn shapefiles into a lookup by stem
# Turn shapefiles into a lookup by stem (strip "_range")
shp_stems  <- tools::file_path_sans_ext(basename(shp_files))
shp_stems  <- sub("_range$", "", shp_stems)
shp_lookup <- setNames(shp_files, shp_stems)

# folder key -> shapefile stem
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"     # forest elephant (Loxodonta cyclotis), per your files
)

# Build shp_map keyed by folder name
shp_map <- setNames(rep(NA_character_, length(target_folders)), target_folders)
for (sp in target_folders) {
  stem <- range_name_map[[sp]]
  if (!is.null(stem) && stem %in% names(shp_lookup)) {
    shp_map[[sp]] <- shp_lookup[[stem]]
  }
}
shp_map <- shp_map[!is.na(shp_map)]

# Shared keys
common_keys <- intersect(names(raster_map), names(shp_map))
cat("Matched", length(common_keys), "folders:", paste(common_keys, collapse=", "), "\n")

# Report missing
missing_ranges <- setdiff(names(raster_map), names(shp_map))
if (length(missing_ranges) > 0) {
  cat("\nNo range shapefile found for:", paste(missing_ranges, collapse=", "), "\n")
}

# -----------------------------
# Output
# -----------------------------
out_dir_outside <- file.path(raster_root, "outside_species_range")
dir.create(out_dir_outside, recursive = TRUE, showWarnings = FALSE)

outside_files <- character(0)

for (k in common_keys) {
  
  r <- rast(raster_map[[k]])
  
  rng_sf <- st_read(shp_map[[k]], quiet = TRUE)
  rng_sf <- st_make_valid(rng_sf)          # helps if geometry is messy
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Keep ONLY cells outside the range polygon
  r_outside <- mask(r, rng_v, inverse = TRUE)
  
  out_file <- file.path(out_dir_outside, paste0(k, "_binary_OUTSIDE_range_2050_ssp126.tif"))
  writeRaster(r_outside, out_file, overwrite = TRUE)
  
  outside_files <- c(outside_files, out_file)
  cat("Saved outside-range raster:", out_file, "\n")
}




# ssp245

# ---------------------------
# 1) Paths
# ---------------------------

# Binary rasters (your earlier folder)
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2050/ssp245"

# Species ranges (your current folder)
range_dir <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

# Output folder
out_dir <- file.path(raster_root, "clipped_to_range")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# 2) List files
# ---------------------------

tif_files <- list.files(raster_root, pattern="\\.tif$", full.names=TRUE, recursive=TRUE)
shp_files <- list.files(range_dir,  pattern="\\.shp$", full.names=TRUE, recursive=FALSE)

cat("Found", length(tif_files), "raster tif files\n")
cat("Found", length(shp_files), "range shapefiles\n")
stopifnot(length(tif_files) > 0, length(shp_files) > 0)

# ---------------------------
# 3) Build matching keys
#    (assumes ranges are like: blanchardi_range_clipped.shp
#     rasters contain species name like: "Acris blanchardi_mean_pixel_binary.tif"
# ---------------------------

# Key from shapefile names: "blanchardi_range_clipped" -> "blanchardi"
range_key <- tolower(basename(shp_files))
range_key <- str_replace(range_key, "_range\\.shp$", "")
names(shp_files) <- range_key

# Key from raster names: grab the second word in "Genus species_....tif"
# Example: "Acris blanchardi_mean_pixel_binary.tif" -> "blanchardi"
raster_base <- basename(tif_files)
raster_key <- tolower(raster_base)
raster_key <- str_replace(raster_key, "_mean_pixel_binary_2050s_ssp245\\.tif$", "")
raster_key <- str_split_fixed(raster_key, " ", 3)[,2]  # second token = species epithet
names(tif_files) <- raster_key

# Report matching
cat("\nKeys in rasters:\n"); print(sort(unique(names(tif_files))))
cat("\nKeys in ranges:\n");  print(sort(unique(names(shp_files))))

common_keys <- intersect(unique(names(tif_files)), unique(names(shp_files)))
cat("\nCommon keys matched:", length(common_keys), "\n")
print(sort(common_keys))

if (length(common_keys) == 0) stop("No matches found. Check naming conventions.")

# ---------------------------
# 4) Clip each raster to its matching range and save
# ---------------------------

clipped_files <- character(0)

for (k in common_keys) {
  # Read raster
  r <- rast(tif_files[[k]])
  
  # Read range (sf -> terra vect)
  rng_sf <- st_read(shp_files[[k]], quiet = TRUE)
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Crop to bbox, then mask to polygon (strictly inside range)
  r_crop <- crop(r, rng_v)
  r_mask <- mask(r_crop, rng_v)
  
  # Write output
  out_file <- file.path(out_dir, paste0(k, "_binary_clipped_2050s_ssp245.tif"))
  writeRaster(r_mask, out_file, overwrite = TRUE)
  
  clipped_files <- c(clipped_files, out_file)
  
  cat("Saved:", out_file, "\n")
}

cat("\nTotal clipped rasters saved:", length(clipped_files), "\n")

# ---------------------------
# 5) Plot the first clipped raster to verify
# ---------------------------
if (length(clipped_files) > 0) {
  r1 <- rast(clipped_files[1])
  plot(r1, main = paste("Clipped binary raster:", basename(clipped_files[1])))
}


# cover clipped range results to empty raster

# -----------------------------
# Paths
# -----------------------------
root_dir_bin   <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2050/ssp245/clipped_to_range"
root_dir_empty <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/current/clipped_to_range/empty_rasters"

out_dir <- file.path(root_dir_bin, "binary_merged_to_empty")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Helper: robust species id
#   "cyclotis_binary_clipped_2030s_ssp126.tif" -> "cyclotis"
#   "cyclotis_binary_clipped.tif"             -> "cyclotis"
#   "cyclotis_empty_raster.tif"               -> "cyclotis"
# -----------------------------
get_species_id <- function(x) {
  x <- tools::file_path_sans_ext(basename(x))
  x <- sub("_binary_clipped.*$", "", x)  # remove _binary_clipped + anything after
  x <- sub("_empty_raster.*$",  "", x)   # remove _empty_raster + anything after (if any)
  x
}

# -----------------------------
# List files
# -----------------------------
bin_files   <- list.files(root_dir_bin,   pattern = "_binary_clipped.*\\.tif$", full.names = TRUE, recursive = TRUE)
empty_files <- list.files(root_dir_empty, pattern = "_empty_raster\\.tif$",     full.names = TRUE, recursive = FALSE)

cat("Found", length(bin_files), "binary rasters\n")
cat("Found", length(empty_files), "empty rasters\n")

if (length(bin_files) == 0)   stop("No binary rasters found in: ", root_dir_bin)
if (length(empty_files) == 0) stop("No empty rasters found in: ", root_dir_empty)

# Map species -> file
bin_map   <- setNames(bin_files,   vapply(bin_files,   get_species_id, character(1)))
empty_map <- setNames(empty_files, vapply(empty_files, get_species_id, character(1)))

# Shared species
species_common <- intersect(names(bin_map), names(empty_map))
cat("Matched", length(species_common), "species:\n", paste(species_common, collapse = ", "), "\n")

if (length(species_common) == 0) {
  cat("\nBinary IDs:\n");  print(names(bin_map))
  cat("\nEmpty IDs:\n");   print(names(empty_map))
  stop("No matching species names. See IDs printed above.")
}

# -----------------------------
# Merge: binary onto empty grid
# -----------------------------
for (sp in species_common) {
  cat("\n--- Processing:", sp, "---\n")
  
  bin_r   <- rast(bin_map[[sp]])
  empty_r <- rast(empty_map[[sp]])
  
  # Force binary onto empty raster grid (safe even if already matching)
  same_grid <- terra::compareGeom(bin_r, empty_r, stopOnError = FALSE)
  
  if (!isTRUE(same_grid)) {
    bin_r <- terra::resample(bin_r, empty_r, method = "near")  # nearest neighbor for binary
  }
  
  merged <- cover(bin_r, empty_r)  # keep binary where it exists; fill rest with 0s from empty
  names(merged) <- paste0(sp, "_binary_on_empty")
  
  out_file <- file.path(out_dir, paste0(sp, "_binary_on_empty_2050s_ssp245.tif"))
  writeRaster(merged, out_file, overwrite = TRUE)
  
  cat("Saved:", out_file, "\n")
}

# -----------------------------
# Report unmatched
# -----------------------------
only_bin   <- setdiff(names(bin_map), names(empty_map))
only_empty <- setdiff(names(empty_map), names(bin_map))

if (length(only_bin) > 0) {
  cat("\nNo empty raster found for these binaries:\n", paste(only_bin, collapse = ", "), "\n")
}
if (length(only_empty) > 0) {
  cat("\nNo binary raster found for these empties:\n", paste(only_empty, collapse = ", "), "\n")
}


# 6) Save raster cells OUTSIDE species range

# Inputs
# -----------------------------
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2050/ssp245"
range_dir   <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

target_folders <- c("bonobos", "chimps", "elephants", "gorillas")

# Only those 4 subfolders
selected_dirs <- file.path(raster_root, target_folders)
selected_dirs <- selected_dirs[dir.exists(selected_dirs)]

# Raster tif files (grab all tifs inside each selected folder)
tif_files <- unlist(lapply(selected_dirs, function(d) {
  list.files(d, pattern = "\\.tif$", full.names = TRUE)
}))
cat("Found", length(tif_files), "tif files\n")

# Range shapefiles
shp_files <- list.files(range_dir, pattern="\\.shp$", full.names=TRUE, recursive=FALSE)
cat("Found", length(shp_files), "range shapefiles\n")
print(basename(shp_files))

# -----------------------------
# Build species -> raster map (folder name as key)
# -----------------------------
get_raster_key <- function(path) basename(dirname(path))
raster_map <- setNames(tif_files, vapply(tif_files, get_raster_key, character(1)))

# -----------------------------
# Build folder -> shapefile map using your naming scheme
# -----------------------------
# folder key      -> shapefile stem (without .shp)
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"   # placeholder: you don't have this shapefile yet
)

# Turn shapefiles into a lookup by stem
# Turn shapefiles into a lookup by stem (strip "_range")
shp_stems  <- tools::file_path_sans_ext(basename(shp_files))
shp_stems  <- sub("_range$", "", shp_stems)
shp_lookup <- setNames(shp_files, shp_stems)

# folder key -> shapefile stem
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"     # forest elephant (Loxodonta cyclotis), per your files
)

# Build shp_map keyed by folder name
shp_map <- setNames(rep(NA_character_, length(target_folders)), target_folders)
for (sp in target_folders) {
  stem <- range_name_map[[sp]]
  if (!is.null(stem) && stem %in% names(shp_lookup)) {
    shp_map[[sp]] <- shp_lookup[[stem]]
  }
}
shp_map <- shp_map[!is.na(shp_map)]

# Shared keys
common_keys <- intersect(names(raster_map), names(shp_map))
cat("Matched", length(common_keys), "folders:", paste(common_keys, collapse=", "), "\n")

# Report missing
missing_ranges <- setdiff(names(raster_map), names(shp_map))
if (length(missing_ranges) > 0) {
  cat("\nNo range shapefile found for:", paste(missing_ranges, collapse=", "), "\n")
}

# -----------------------------
# Output
# -----------------------------
out_dir_outside <- file.path(raster_root, "outside_species_range")
dir.create(out_dir_outside, recursive = TRUE, showWarnings = FALSE)

outside_files <- character(0)

for (k in common_keys) {
  
  r <- rast(raster_map[[k]])
  
  rng_sf <- st_read(shp_map[[k]], quiet = TRUE)
  rng_sf <- st_make_valid(rng_sf)          # helps if geometry is messy
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Keep ONLY cells outside the range polygon
  r_outside <- mask(r, rng_v, inverse = TRUE)
  
  out_file <- file.path(out_dir_outside, paste0(k, "_binary_OUTSIDE_range_2050_ssp245.tif"))
  writeRaster(r_outside, out_file, overwrite = TRUE)
  
  outside_files <- c(outside_files, out_file)
  cat("Saved outside-range raster:", out_file, "\n")
}



# ssp585

# ---------------------------
# 1) Paths
# ---------------------------

# Binary rasters (your earlier folder)
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2050/ssp585"

# Species ranges (your current folder)
range_dir <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

# Output folder
out_dir <- file.path(raster_root, "clipped_to_range")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# 2) List files
# ---------------------------

tif_files <- list.files(raster_root, pattern="\\.tif$", full.names=TRUE, recursive=TRUE)
shp_files <- list.files(range_dir,  pattern="\\.shp$", full.names=TRUE, recursive=FALSE)

cat("Found", length(tif_files), "raster tif files\n")
cat("Found", length(shp_files), "range shapefiles\n")
stopifnot(length(tif_files) > 0, length(shp_files) > 0)

# ---------------------------
# 3) Build matching keys
#    (assumes ranges are like: blanchardi_range_clipped.shp
#     rasters contain species name like: "Acris blanchardi_mean_pixel_binary.tif"
# ---------------------------

# Key from shapefile names: "blanchardi_range_clipped" -> "blanchardi"
range_key <- tolower(basename(shp_files))
range_key <- str_replace(range_key, "_range\\.shp$", "")
names(shp_files) <- range_key

# Key from raster names: grab the second word in "Genus species_....tif"
# Example: "Acris blanchardi_mean_pixel_binary.tif" -> "blanchardi"
raster_base <- basename(tif_files)
raster_key <- tolower(raster_base)
raster_key <- str_replace(raster_key, "_mean_pixel_binary_2050s_ssp585\\.tif$", "")
raster_key <- str_split_fixed(raster_key, " ", 3)[,2]  # second token = species epithet
names(tif_files) <- raster_key

# Report matching
cat("\nKeys in rasters:\n"); print(sort(unique(names(tif_files))))
cat("\nKeys in ranges:\n");  print(sort(unique(names(shp_files))))

common_keys <- intersect(unique(names(tif_files)), unique(names(shp_files)))
cat("\nCommon keys matched:", length(common_keys), "\n")
print(sort(common_keys))

if (length(common_keys) == 0) stop("No matches found. Check naming conventions.")

# ---------------------------
# 4) Clip each raster to its matching range and save
# ---------------------------

clipped_files <- character(0)

for (k in common_keys) {
  # Read raster
  r <- rast(tif_files[[k]])
  
  # Read range (sf -> terra vect)
  rng_sf <- st_read(shp_files[[k]], quiet = TRUE)
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Crop to bbox, then mask to polygon (strictly inside range)
  r_crop <- crop(r, rng_v)
  r_mask <- mask(r_crop, rng_v)
  
  # Write output
  out_file <- file.path(out_dir, paste0(k, "_binary_clipped_2050s_ssp585.tif"))
  writeRaster(r_mask, out_file, overwrite = TRUE)
  
  clipped_files <- c(clipped_files, out_file)
  
  cat("Saved:", out_file, "\n")
}

cat("\nTotal clipped rasters saved:", length(clipped_files), "\n")

# ---------------------------
# 5) Plot the first clipped raster to verify
# ---------------------------
if (length(clipped_files) > 0) {
  r1 <- rast(clipped_files[1])
  plot(r1, main = paste("Clipped binary raster:", basename(clipped_files[1])))
}


# cover clipped range results to empty raster

# -----------------------------
# Paths
# -----------------------------
root_dir_bin   <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2050/ssp585/clipped_to_range"
root_dir_empty <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/current/clipped_to_range/empty_rasters"

out_dir <- file.path(root_dir_bin, "binary_merged_to_empty")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Helper: robust species id
#   "cyclotis_binary_clipped_2030s_ssp126.tif" -> "cyclotis"
#   "cyclotis_binary_clipped.tif"             -> "cyclotis"
#   "cyclotis_empty_raster.tif"               -> "cyclotis"
# -----------------------------
get_species_id <- function(x) {
  x <- tools::file_path_sans_ext(basename(x))
  x <- sub("_binary_clipped.*$", "", x)  # remove _binary_clipped + anything after
  x <- sub("_empty_raster.*$",  "", x)   # remove _empty_raster + anything after (if any)
  x
}

# -----------------------------
# List files
# -----------------------------
bin_files   <- list.files(root_dir_bin,   pattern = "_binary_clipped.*\\.tif$", full.names = TRUE, recursive = TRUE)
empty_files <- list.files(root_dir_empty, pattern = "_empty_raster\\.tif$",     full.names = TRUE, recursive = FALSE)

cat("Found", length(bin_files), "binary rasters\n")
cat("Found", length(empty_files), "empty rasters\n")

if (length(bin_files) == 0)   stop("No binary rasters found in: ", root_dir_bin)
if (length(empty_files) == 0) stop("No empty rasters found in: ", root_dir_empty)

# Map species -> file
bin_map   <- setNames(bin_files,   vapply(bin_files,   get_species_id, character(1)))
empty_map <- setNames(empty_files, vapply(empty_files, get_species_id, character(1)))

# Shared species
species_common <- intersect(names(bin_map), names(empty_map))
cat("Matched", length(species_common), "species:\n", paste(species_common, collapse = ", "), "\n")

if (length(species_common) == 0) {
  cat("\nBinary IDs:\n");  print(names(bin_map))
  cat("\nEmpty IDs:\n");   print(names(empty_map))
  stop("No matching species names. See IDs printed above.")
}

# -----------------------------
# Merge: binary onto empty grid
# -----------------------------
for (sp in species_common) {
  cat("\n--- Processing:", sp, "---\n")
  
  bin_r   <- rast(bin_map[[sp]])
  empty_r <- rast(empty_map[[sp]])
  
  # Force binary onto empty raster grid (safe even if already matching)
  same_grid <- terra::compareGeom(bin_r, empty_r, stopOnError = FALSE)
  
  if (!isTRUE(same_grid)) {
    bin_r <- terra::resample(bin_r, empty_r, method = "near")  # nearest neighbor for binary
  }
  
  merged <- cover(bin_r, empty_r)  # keep binary where it exists; fill rest with 0s from empty
  names(merged) <- paste0(sp, "_binary_on_empty")
  
  out_file <- file.path(out_dir, paste0(sp, "_binary_on_empty_2050s_ssp585.tif"))
  writeRaster(merged, out_file, overwrite = TRUE)
  
  cat("Saved:", out_file, "\n")
}

# -----------------------------
# Report unmatched
# -----------------------------
only_bin   <- setdiff(names(bin_map), names(empty_map))
only_empty <- setdiff(names(empty_map), names(bin_map))

if (length(only_bin) > 0) {
  cat("\nNo empty raster found for these binaries:\n", paste(only_bin, collapse = ", "), "\n")
}
if (length(only_empty) > 0) {
  cat("\nNo binary raster found for these empties:\n", paste(only_empty, collapse = ", "), "\n")
}


# 6) Save raster cells OUTSIDE species range

# Inputs
# -----------------------------
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2050/ssp585"
range_dir   <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

target_folders <- c("bonobos", "chimps", "elephants", "gorillas")

# Only those 4 subfolders
selected_dirs <- file.path(raster_root, target_folders)
selected_dirs <- selected_dirs[dir.exists(selected_dirs)]

# Raster tif files (grab all tifs inside each selected folder)
tif_files <- unlist(lapply(selected_dirs, function(d) {
  list.files(d, pattern = "\\.tif$", full.names = TRUE)
}))
cat("Found", length(tif_files), "tif files\n")

# Range shapefiles
shp_files <- list.files(range_dir, pattern="\\.shp$", full.names=TRUE, recursive=FALSE)
cat("Found", length(shp_files), "range shapefiles\n")
print(basename(shp_files))

# -----------------------------
# Build species -> raster map (folder name as key)
# -----------------------------
get_raster_key <- function(path) basename(dirname(path))
raster_map <- setNames(tif_files, vapply(tif_files, get_raster_key, character(1)))

# -----------------------------
# Build folder -> shapefile map using your naming scheme
# -----------------------------
# folder key      -> shapefile stem (without .shp)
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"   # placeholder: you don't have this shapefile yet
)

# Turn shapefiles into a lookup by stem
# Turn shapefiles into a lookup by stem (strip "_range")
shp_stems  <- tools::file_path_sans_ext(basename(shp_files))
shp_stems  <- sub("_range$", "", shp_stems)
shp_lookup <- setNames(shp_files, shp_stems)

# folder key -> shapefile stem
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"     # forest elephant (Loxodonta cyclotis), per your files
)

# Build shp_map keyed by folder name
shp_map <- setNames(rep(NA_character_, length(target_folders)), target_folders)
for (sp in target_folders) {
  stem <- range_name_map[[sp]]
  if (!is.null(stem) && stem %in% names(shp_lookup)) {
    shp_map[[sp]] <- shp_lookup[[stem]]
  }
}
shp_map <- shp_map[!is.na(shp_map)]

# Shared keys
common_keys <- intersect(names(raster_map), names(shp_map))
cat("Matched", length(common_keys), "folders:", paste(common_keys, collapse=", "), "\n")

# Report missing
missing_ranges <- setdiff(names(raster_map), names(shp_map))
if (length(missing_ranges) > 0) {
  cat("\nNo range shapefile found for:", paste(missing_ranges, collapse=", "), "\n")
}

# -----------------------------
# Output
# -----------------------------
out_dir_outside <- file.path(raster_root, "outside_species_range")
dir.create(out_dir_outside, recursive = TRUE, showWarnings = FALSE)

outside_files <- character(0)

for (k in common_keys) {
  
  r <- rast(raster_map[[k]])
  
  rng_sf <- st_read(shp_map[[k]], quiet = TRUE)
  rng_sf <- st_make_valid(rng_sf)          # helps if geometry is messy
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Keep ONLY cells outside the range polygon
  r_outside <- mask(r, rng_v, inverse = TRUE)
  
  out_file <- file.path(out_dir_outside, paste0(k, "_binary_OUTSIDE_range_2050_ssp585.tif"))
  writeRaster(r_outside, out_file, overwrite = TRUE)
  
  outside_files <- c(outside_files, out_file)
  cat("Saved outside-range raster:", out_file, "\n")
}


# ---------------------------
# 7) Plot first outside raster to verify
# ---------------------------
if (length(outside_files) > 0) {
  r_out1 <- rast(outside_files[1])
  plot(r_out1, main = paste("Binary raster OUTSIDE range:", basename(outside_files[3])))
}



#-----------------2070------------

# repeat for 2070
# ssp126

# ---------------------------
# 1) Paths
# ---------------------------

# Binary rasters (your earlier folder)
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2070/ssp126"

# Species ranges (your current folder)
range_dir <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

# Output folder
out_dir <- file.path(raster_root, "clipped_to_range")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# 2) List files
# ---------------------------

tif_files <- list.files(raster_root, pattern="\\.tif$", full.names=TRUE, recursive=TRUE)
shp_files <- list.files(range_dir,  pattern="\\.shp$", full.names=TRUE, recursive=FALSE)

cat("Found", length(tif_files), "raster tif files\n")
cat("Found", length(shp_files), "range shapefiles\n")
stopifnot(length(tif_files) > 0, length(shp_files) > 0)

# ---------------------------
# 3) Build matching keys
#    (assumes ranges are like: blanchardi_range_clipped.shp
#     rasters contain species name like: "Acris blanchardi_mean_pixel_binary.tif"
# ---------------------------

# Key from shapefile names: "blanchardi_range_clipped" -> "blanchardi"
range_key <- tolower(basename(shp_files))
range_key <- str_replace(range_key, "_range\\.shp$", "")
names(shp_files) <- range_key

# Key from raster names: grab the second word in "Genus species_....tif"
# Example: "Acris blanchardi_mean_pixel_binary.tif" -> "blanchardi"
raster_base <- basename(tif_files)
raster_key <- tolower(raster_base)
raster_key <- str_replace(raster_key, "_mean_pixel_binary_2070s_ssp126\\.tif$", "")
raster_key <- str_split_fixed(raster_key, " ", 3)[,2]  # second token = species epithet
names(tif_files) <- raster_key

# Report matching
cat("\nKeys in rasters:\n"); print(sort(unique(names(tif_files))))
cat("\nKeys in ranges:\n");  print(sort(unique(names(shp_files))))

common_keys <- intersect(unique(names(tif_files)), unique(names(shp_files)))
cat("\nCommon keys matched:", length(common_keys), "\n")
print(sort(common_keys))

if (length(common_keys) == 0) stop("No matches found. Check naming conventions.")

# ---------------------------
# 4) Clip each raster to its matching range and save
# ---------------------------

clipped_files <- character(0)

for (k in common_keys) {
  # Read raster
  r <- rast(tif_files[[k]])
  
  # Read range (sf -> terra vect)
  rng_sf <- st_read(shp_files[[k]], quiet = TRUE)
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Crop to bbox, then mask to polygon (strictly inside range)
  r_crop <- crop(r, rng_v)
  r_mask <- mask(r_crop, rng_v)
  
  # Write output
  out_file <- file.path(out_dir, paste0(k, "_binary_clipped_2070s_ssp126.tif"))
  writeRaster(r_mask, out_file, overwrite = TRUE)
  
  clipped_files <- c(clipped_files, out_file)
  
  cat("Saved:", out_file, "\n")
}

cat("\nTotal clipped rasters saved:", length(clipped_files), "\n")

# ---------------------------
# 5) Plot the first clipped raster to verify
# ---------------------------
if (length(clipped_files) > 0) {
  r1 <- rast(clipped_files[1])
  plot(r1, main = paste("Clipped binary raster:", basename(clipped_files[1])))
}


# cover clipped range results to empty raster

# -----------------------------
# Paths
# -----------------------------
root_dir_bin   <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2070/ssp126/clipped_to_range"
root_dir_empty <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/current/clipped_to_range/empty_rasters"

out_dir <- file.path(root_dir_bin, "binary_merged_to_empty")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Helper: robust species id
#   "cyclotis_binary_clipped_2030s_ssp126.tif" -> "cyclotis"
#   "cyclotis_binary_clipped.tif"             -> "cyclotis"
#   "cyclotis_empty_raster.tif"               -> "cyclotis"
# -----------------------------
get_species_id <- function(x) {
  x <- tools::file_path_sans_ext(basename(x))
  x <- sub("_binary_clipped.*$", "", x)  # remove _binary_clipped + anything after
  x <- sub("_empty_raster.*$",  "", x)   # remove _empty_raster + anything after (if any)
  x
}

# -----------------------------
# List files
# -----------------------------
bin_files   <- list.files(root_dir_bin,   pattern = "_binary_clipped.*\\.tif$", full.names = TRUE, recursive = TRUE)
empty_files <- list.files(root_dir_empty, pattern = "_empty_raster\\.tif$",     full.names = TRUE, recursive = FALSE)

cat("Found", length(bin_files), "binary rasters\n")
cat("Found", length(empty_files), "empty rasters\n")

if (length(bin_files) == 0)   stop("No binary rasters found in: ", root_dir_bin)
if (length(empty_files) == 0) stop("No empty rasters found in: ", root_dir_empty)

# Map species -> file
bin_map   <- setNames(bin_files,   vapply(bin_files,   get_species_id, character(1)))
empty_map <- setNames(empty_files, vapply(empty_files, get_species_id, character(1)))

# Shared species
species_common <- intersect(names(bin_map), names(empty_map))
cat("Matched", length(species_common), "species:\n", paste(species_common, collapse = ", "), "\n")

if (length(species_common) == 0) {
  cat("\nBinary IDs:\n");  print(names(bin_map))
  cat("\nEmpty IDs:\n");   print(names(empty_map))
  stop("No matching species names. See IDs printed above.")
}

# -----------------------------
# Merge: binary onto empty grid
# -----------------------------
for (sp in species_common) {
  cat("\n--- Processing:", sp, "---\n")
  
  bin_r   <- rast(bin_map[[sp]])
  empty_r <- rast(empty_map[[sp]])
  
  # Force binary onto empty raster grid (safe even if already matching)
  same_grid <- terra::compareGeom(bin_r, empty_r, stopOnError = FALSE)
  
  if (!isTRUE(same_grid)) {
    bin_r <- terra::resample(bin_r, empty_r, method = "near")  # nearest neighbor for binary
  }
  
  merged <- cover(bin_r, empty_r)  # keep binary where it exists; fill rest with 0s from empty
  names(merged) <- paste0(sp, "_binary_on_empty")
  
  out_file <- file.path(out_dir, paste0(sp, "_binary_on_empty_2070s_ssp126.tif"))
  writeRaster(merged, out_file, overwrite = TRUE)
  
  cat("Saved:", out_file, "\n")
}

# -----------------------------
# Report unmatched
# -----------------------------
only_bin   <- setdiff(names(bin_map), names(empty_map))
only_empty <- setdiff(names(empty_map), names(bin_map))

if (length(only_bin) > 0) {
  cat("\nNo empty raster found for these binaries:\n", paste(only_bin, collapse = ", "), "\n")
}
if (length(only_empty) > 0) {
  cat("\nNo binary raster found for these empties:\n", paste(only_empty, collapse = ", "), "\n")
}


# ---------------------------
# 6) Save raster cells OUTSIDE species range

# Inputs
# -----------------------------
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2070/ssp126"
range_dir   <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

target_folders <- c("bonobos", "chimps", "elephants", "gorillas")

# Only those 4 subfolders
selected_dirs <- file.path(raster_root, target_folders)
selected_dirs <- selected_dirs[dir.exists(selected_dirs)]

# Raster tif files (grab all tifs inside each selected folder)
tif_files <- unlist(lapply(selected_dirs, function(d) {
  list.files(d, pattern = "\\.tif$", full.names = TRUE)
}))
cat("Found", length(tif_files), "tif files\n")

# Range shapefiles
shp_files <- list.files(range_dir, pattern="\\.shp$", full.names=TRUE, recursive=FALSE)
cat("Found", length(shp_files), "range shapefiles\n")
print(basename(shp_files))

# -----------------------------
# Build species -> raster map (folder name as key)
# -----------------------------
get_raster_key <- function(path) basename(dirname(path))
raster_map <- setNames(tif_files, vapply(tif_files, get_raster_key, character(1)))

# -----------------------------
# Build folder -> shapefile map using your naming scheme
# -----------------------------
# folder key      -> shapefile stem (without .shp)
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"   # placeholder: you don't have this shapefile yet
)

# Turn shapefiles into a lookup by stem
# Turn shapefiles into a lookup by stem (strip "_range")
shp_stems  <- tools::file_path_sans_ext(basename(shp_files))
shp_stems  <- sub("_range$", "", shp_stems)
shp_lookup <- setNames(shp_files, shp_stems)

# folder key -> shapefile stem
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"     # forest elephant (Loxodonta cyclotis), per your files
)

# Build shp_map keyed by folder name
shp_map <- setNames(rep(NA_character_, length(target_folders)), target_folders)
for (sp in target_folders) {
  stem <- range_name_map[[sp]]
  if (!is.null(stem) && stem %in% names(shp_lookup)) {
    shp_map[[sp]] <- shp_lookup[[stem]]
  }
}
shp_map <- shp_map[!is.na(shp_map)]

# Shared keys
common_keys <- intersect(names(raster_map), names(shp_map))
cat("Matched", length(common_keys), "folders:", paste(common_keys, collapse=", "), "\n")

# Report missing
missing_ranges <- setdiff(names(raster_map), names(shp_map))
if (length(missing_ranges) > 0) {
  cat("\nNo range shapefile found for:", paste(missing_ranges, collapse=", "), "\n")
}

# -----------------------------
# Output
# -----------------------------
out_dir_outside <- file.path(raster_root, "outside_species_range")
dir.create(out_dir_outside, recursive = TRUE, showWarnings = FALSE)

outside_files <- character(0)

for (k in common_keys) {
  
  r <- rast(raster_map[[k]])
  
  rng_sf <- st_read(shp_map[[k]], quiet = TRUE)
  rng_sf <- st_make_valid(rng_sf)          # helps if geometry is messy
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Keep ONLY cells outside the range polygon
  r_outside <- mask(r, rng_v, inverse = TRUE)
  
  out_file <- file.path(out_dir_outside, paste0(k, "_binary_OUTSIDE_range_2070_ssp126.tif"))
  writeRaster(r_outside, out_file, overwrite = TRUE)
  
  outside_files <- c(outside_files, out_file)
  cat("Saved outside-range raster:", out_file, "\n")
}




# ssp245

# ---------------------------
# 1) Paths
# ---------------------------

# Binary rasters (your earlier folder)
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2070/ssp245"

# Species ranges (your current folder)
range_dir <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

# Output folder
out_dir <- file.path(raster_root, "clipped_to_range")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# 2) List files
# ---------------------------

tif_files <- list.files(raster_root, pattern="\\.tif$", full.names=TRUE, recursive=TRUE)
shp_files <- list.files(range_dir,  pattern="\\.shp$", full.names=TRUE, recursive=FALSE)

cat("Found", length(tif_files), "raster tif files\n")
cat("Found", length(shp_files), "range shapefiles\n")
stopifnot(length(tif_files) > 0, length(shp_files) > 0)

# ---------------------------
# 3) Build matching keys
#    (assumes ranges are like: blanchardi_range_clipped.shp
#     rasters contain species name like: "Acris blanchardi_mean_pixel_binary.tif"
# ---------------------------

# Key from shapefile names: "blanchardi_range_clipped" -> "blanchardi"
range_key <- tolower(basename(shp_files))
range_key <- str_replace(range_key, "_range\\.shp$", "")
names(shp_files) <- range_key

# Key from raster names: grab the second word in "Genus species_....tif"
# Example: "Acris blanchardi_mean_pixel_binary.tif" -> "blanchardi"
raster_base <- basename(tif_files)
raster_key <- tolower(raster_base)
raster_key <- str_replace(raster_key, "_mean_pixel_binary_2070s_ssp245\\.tif$", "")
raster_key <- str_split_fixed(raster_key, " ", 3)[,2]  # second token = species epithet
names(tif_files) <- raster_key

# Report matching
cat("\nKeys in rasters:\n"); print(sort(unique(names(tif_files))))
cat("\nKeys in ranges:\n");  print(sort(unique(names(shp_files))))

common_keys <- intersect(unique(names(tif_files)), unique(names(shp_files)))
cat("\nCommon keys matched:", length(common_keys), "\n")
print(sort(common_keys))

if (length(common_keys) == 0) stop("No matches found. Check naming conventions.")

# ---------------------------
# 4) Clip each raster to its matching range and save
# ---------------------------

clipped_files <- character(0)

for (k in common_keys) {
  # Read raster
  r <- rast(tif_files[[k]])
  
  # Read range (sf -> terra vect)
  rng_sf <- st_read(shp_files[[k]], quiet = TRUE)
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Crop to bbox, then mask to polygon (strictly inside range)
  r_crop <- crop(r, rng_v)
  r_mask <- mask(r_crop, rng_v)
  
  # Write output
  out_file <- file.path(out_dir, paste0(k, "_binary_clipped_2070s_ssp245.tif"))
  writeRaster(r_mask, out_file, overwrite = TRUE)
  
  clipped_files <- c(clipped_files, out_file)
  
  cat("Saved:", out_file, "\n")
}

cat("\nTotal clipped rasters saved:", length(clipped_files), "\n")

# ---------------------------
# 5) Plot the first clipped raster to verify
# ---------------------------
if (length(clipped_files) > 0) {
  r1 <- rast(clipped_files[1])
  plot(r1, main = paste("Clipped binary raster:", basename(clipped_files[1])))
}


# cover clipped range results to empty raster

# -----------------------------
# Paths
# -----------------------------
root_dir_bin   <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2070/ssp245/clipped_to_range"
root_dir_empty <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/current/clipped_to_range/empty_rasters"

out_dir <- file.path(root_dir_bin, "binary_merged_to_empty")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Helper: robust species id
#   "cyclotis_binary_clipped_2030s_ssp126.tif" -> "cyclotis"
#   "cyclotis_binary_clipped.tif"             -> "cyclotis"
#   "cyclotis_empty_raster.tif"               -> "cyclotis"
# -----------------------------
get_species_id <- function(x) {
  x <- tools::file_path_sans_ext(basename(x))
  x <- sub("_binary_clipped.*$", "", x)  # remove _binary_clipped + anything after
  x <- sub("_empty_raster.*$",  "", x)   # remove _empty_raster + anything after (if any)
  x
}

# -----------------------------
# List files
# -----------------------------
bin_files   <- list.files(root_dir_bin,   pattern = "_binary_clipped.*\\.tif$", full.names = TRUE, recursive = TRUE)
empty_files <- list.files(root_dir_empty, pattern = "_empty_raster\\.tif$",     full.names = TRUE, recursive = FALSE)

cat("Found", length(bin_files), "binary rasters\n")
cat("Found", length(empty_files), "empty rasters\n")

if (length(bin_files) == 0)   stop("No binary rasters found in: ", root_dir_bin)
if (length(empty_files) == 0) stop("No empty rasters found in: ", root_dir_empty)

# Map species -> file
bin_map   <- setNames(bin_files,   vapply(bin_files,   get_species_id, character(1)))
empty_map <- setNames(empty_files, vapply(empty_files, get_species_id, character(1)))

# Shared species
species_common <- intersect(names(bin_map), names(empty_map))
cat("Matched", length(species_common), "species:\n", paste(species_common, collapse = ", "), "\n")

if (length(species_common) == 0) {
  cat("\nBinary IDs:\n");  print(names(bin_map))
  cat("\nEmpty IDs:\n");   print(names(empty_map))
  stop("No matching species names. See IDs printed above.")
}

# -----------------------------
# Merge: binary onto empty grid
# -----------------------------
for (sp in species_common) {
  cat("\n--- Processing:", sp, "---\n")
  
  bin_r   <- rast(bin_map[[sp]])
  empty_r <- rast(empty_map[[sp]])
  
  # Force binary onto empty raster grid (safe even if already matching)
  same_grid <- terra::compareGeom(bin_r, empty_r, stopOnError = FALSE)
  
  if (!isTRUE(same_grid)) {
    bin_r <- terra::resample(bin_r, empty_r, method = "near")  # nearest neighbor for binary
  }
  
  merged <- cover(bin_r, empty_r)  # keep binary where it exists; fill rest with 0s from empty
  names(merged) <- paste0(sp, "_binary_on_empty")
  
  out_file <- file.path(out_dir, paste0(sp, "_binary_on_empty_2070s_ssp245.tif"))
  writeRaster(merged, out_file, overwrite = TRUE)
  
  cat("Saved:", out_file, "\n")
}

# -----------------------------
# Report unmatched
# -----------------------------
only_bin   <- setdiff(names(bin_map), names(empty_map))
only_empty <- setdiff(names(empty_map), names(bin_map))

if (length(only_bin) > 0) {
  cat("\nNo empty raster found for these binaries:\n", paste(only_bin, collapse = ", "), "\n")
}
if (length(only_empty) > 0) {
  cat("\nNo binary raster found for these empties:\n", paste(only_empty, collapse = ", "), "\n")
}


# 6) Save raster cells OUTSIDE species range

# Inputs
# -----------------------------
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2070/ssp245"
range_dir   <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

target_folders <- c("bonobos", "chimps", "elephants", "gorillas")

# Only those 4 subfolders
selected_dirs <- file.path(raster_root, target_folders)
selected_dirs <- selected_dirs[dir.exists(selected_dirs)]

# Raster tif files (grab all tifs inside each selected folder)
tif_files <- unlist(lapply(selected_dirs, function(d) {
  list.files(d, pattern = "\\.tif$", full.names = TRUE)
}))
cat("Found", length(tif_files), "tif files\n")

# Range shapefiles
shp_files <- list.files(range_dir, pattern="\\.shp$", full.names=TRUE, recursive=FALSE)
cat("Found", length(shp_files), "range shapefiles\n")
print(basename(shp_files))

# -----------------------------
# Build species -> raster map (folder name as key)
# -----------------------------
get_raster_key <- function(path) basename(dirname(path))
raster_map <- setNames(tif_files, vapply(tif_files, get_raster_key, character(1)))

# -----------------------------
# Build folder -> shapefile map using your naming scheme
# -----------------------------
# folder key      -> shapefile stem (without .shp)
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"   # placeholder: you don't have this shapefile yet
)

# Turn shapefiles into a lookup by stem
# Turn shapefiles into a lookup by stem (strip "_range")
shp_stems  <- tools::file_path_sans_ext(basename(shp_files))
shp_stems  <- sub("_range$", "", shp_stems)
shp_lookup <- setNames(shp_files, shp_stems)

# folder key -> shapefile stem
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"     # forest elephant (Loxodonta cyclotis), per your files
)

# Build shp_map keyed by folder name
shp_map <- setNames(rep(NA_character_, length(target_folders)), target_folders)
for (sp in target_folders) {
  stem <- range_name_map[[sp]]
  if (!is.null(stem) && stem %in% names(shp_lookup)) {
    shp_map[[sp]] <- shp_lookup[[stem]]
  }
}
shp_map <- shp_map[!is.na(shp_map)]

# Shared keys
common_keys <- intersect(names(raster_map), names(shp_map))
cat("Matched", length(common_keys), "folders:", paste(common_keys, collapse=", "), "\n")

# Report missing
missing_ranges <- setdiff(names(raster_map), names(shp_map))
if (length(missing_ranges) > 0) {
  cat("\nNo range shapefile found for:", paste(missing_ranges, collapse=", "), "\n")
}

# -----------------------------
# Output
# -----------------------------
out_dir_outside <- file.path(raster_root, "outside_species_range")
dir.create(out_dir_outside, recursive = TRUE, showWarnings = FALSE)

outside_files <- character(0)

for (k in common_keys) {
  
  r <- rast(raster_map[[k]])
  
  rng_sf <- st_read(shp_map[[k]], quiet = TRUE)
  rng_sf <- st_make_valid(rng_sf)          # helps if geometry is messy
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Keep ONLY cells outside the range polygon
  r_outside <- mask(r, rng_v, inverse = TRUE)
  
  out_file <- file.path(out_dir_outside, paste0(k, "_binary_OUTSIDE_range_2070_ssp245.tif"))
  writeRaster(r_outside, out_file, overwrite = TRUE)
  
  outside_files <- c(outside_files, out_file)
  cat("Saved outside-range raster:", out_file, "\n")
}



# ssp585

# ---------------------------
# 1) Paths
# ---------------------------

# Binary rasters (your earlier folder)
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2070/ssp585"

# Species ranges (your current folder)
range_dir <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

# Output folder
out_dir <- file.path(raster_root, "clipped_to_range")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# 2) List files
# ---------------------------

tif_files <- list.files(raster_root, pattern="\\.tif$", full.names=TRUE, recursive=TRUE)
shp_files <- list.files(range_dir,  pattern="\\.shp$", full.names=TRUE, recursive=FALSE)

cat("Found", length(tif_files), "raster tif files\n")
cat("Found", length(shp_files), "range shapefiles\n")
stopifnot(length(tif_files) > 0, length(shp_files) > 0)

# ---------------------------
# 3) Build matching keys
#    (assumes ranges are like: blanchardi_range_clipped.shp
#     rasters contain species name like: "Acris blanchardi_mean_pixel_binary.tif"
# ---------------------------

# Key from shapefile names: "blanchardi_range_clipped" -> "blanchardi"
range_key <- tolower(basename(shp_files))
range_key <- str_replace(range_key, "_range\\.shp$", "")
names(shp_files) <- range_key

# Key from raster names: grab the second word in "Genus species_....tif"
# Example: "Acris blanchardi_mean_pixel_binary.tif" -> "blanchardi"
raster_base <- basename(tif_files)
raster_key <- tolower(raster_base)
raster_key <- str_replace(raster_key, "_mean_pixel_binary_2070s_ssp585\\.tif$", "")
raster_key <- str_split_fixed(raster_key, " ", 3)[,2]  # second token = species epithet
names(tif_files) <- raster_key

# Report matching
cat("\nKeys in rasters:\n"); print(sort(unique(names(tif_files))))
cat("\nKeys in ranges:\n");  print(sort(unique(names(shp_files))))

common_keys <- intersect(unique(names(tif_files)), unique(names(shp_files)))
cat("\nCommon keys matched:", length(common_keys), "\n")
print(sort(common_keys))

if (length(common_keys) == 0) stop("No matches found. Check naming conventions.")

# ---------------------------
# 4) Clip each raster to its matching range and save
# ---------------------------

clipped_files <- character(0)

for (k in common_keys) {
  # Read raster
  r <- rast(tif_files[[k]])
  
  # Read range (sf -> terra vect)
  rng_sf <- st_read(shp_files[[k]], quiet = TRUE)
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Crop to bbox, then mask to polygon (strictly inside range)
  r_crop <- crop(r, rng_v)
  r_mask <- mask(r_crop, rng_v)
  
  # Write output
  out_file <- file.path(out_dir, paste0(k, "_binary_clipped_2070s_ssp585.tif"))
  writeRaster(r_mask, out_file, overwrite = TRUE)
  
  clipped_files <- c(clipped_files, out_file)
  
  cat("Saved:", out_file, "\n")
}

cat("\nTotal clipped rasters saved:", length(clipped_files), "\n")

# ---------------------------
# 5) Plot the first clipped raster to verify
# ---------------------------
if (length(clipped_files) > 0) {
  r1 <- rast(clipped_files[2])
  plot(r1, main = paste("Clipped binary raster:", basename(clipped_files[2])))
}


# cover clipped range results to empty raster

# -----------------------------
# Paths
# -----------------------------
root_dir_bin   <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2070/ssp585/clipped_to_range"
root_dir_empty <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/current/clipped_to_range/empty_rasters"

out_dir <- file.path(root_dir_bin, "binary_merged_to_empty")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Helper: robust species id
#   "cyclotis_binary_clipped_2030s_ssp126.tif" -> "cyclotis"
#   "cyclotis_binary_clipped.tif"             -> "cyclotis"
#   "cyclotis_empty_raster.tif"               -> "cyclotis"
# -----------------------------
get_species_id <- function(x) {
  x <- tools::file_path_sans_ext(basename(x))
  x <- sub("_binary_clipped.*$", "", x)  # remove _binary_clipped + anything after
  x <- sub("_empty_raster.*$",  "", x)   # remove _empty_raster + anything after (if any)
  x
}

# -----------------------------
# List files
# -----------------------------
bin_files   <- list.files(root_dir_bin,   pattern = "_binary_clipped.*\\.tif$", full.names = TRUE, recursive = TRUE)
empty_files <- list.files(root_dir_empty, pattern = "_empty_raster\\.tif$",     full.names = TRUE, recursive = FALSE)

cat("Found", length(bin_files), "binary rasters\n")
cat("Found", length(empty_files), "empty rasters\n")

if (length(bin_files) == 0)   stop("No binary rasters found in: ", root_dir_bin)
if (length(empty_files) == 0) stop("No empty rasters found in: ", root_dir_empty)

# Map species -> file
bin_map   <- setNames(bin_files,   vapply(bin_files,   get_species_id, character(1)))
empty_map <- setNames(empty_files, vapply(empty_files, get_species_id, character(1)))

# Shared species
species_common <- intersect(names(bin_map), names(empty_map))
cat("Matched", length(species_common), "species:\n", paste(species_common, collapse = ", "), "\n")

if (length(species_common) == 0) {
  cat("\nBinary IDs:\n");  print(names(bin_map))
  cat("\nEmpty IDs:\n");   print(names(empty_map))
  stop("No matching species names. See IDs printed above.")
}

# -----------------------------
# Merge: binary onto empty grid
# -----------------------------
for (sp in species_common) {
  cat("\n--- Processing:", sp, "---\n")
  
  bin_r   <- rast(bin_map[[sp]])
  empty_r <- rast(empty_map[[sp]])
  
  # Force binary onto empty raster grid (safe even if already matching)
  same_grid <- terra::compareGeom(bin_r, empty_r, stopOnError = FALSE)
  
  if (!isTRUE(same_grid)) {
    bin_r <- terra::resample(bin_r, empty_r, method = "near")  # nearest neighbor for binary
  }
  
  merged <- cover(bin_r, empty_r)  # keep binary where it exists; fill rest with 0s from empty
  names(merged) <- paste0(sp, "_binary_on_empty")
  
  out_file <- file.path(out_dir, paste0(sp, "_binary_on_empty_2070s_ssp585.tif"))
  writeRaster(merged, out_file, overwrite = TRUE)
  
  cat("Saved:", out_file, "\n")
}

# -----------------------------
# Report unmatched
# -----------------------------
only_bin   <- setdiff(names(bin_map), names(empty_map))
only_empty <- setdiff(names(empty_map), names(bin_map))

if (length(only_bin) > 0) {
  cat("\nNo empty raster found for these binaries:\n", paste(only_bin, collapse = ", "), "\n")
}
if (length(only_empty) > 0) {
  cat("\nNo binary raster found for these empties:\n", paste(only_empty, collapse = ", "), "\n")
}


# 6) Save raster cells OUTSIDE species range

# Inputs
# -----------------------------
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2070/ssp585"
range_dir   <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

target_folders <- c("bonobos", "chimps", "elephants", "gorillas")

# Only those 4 subfolders
selected_dirs <- file.path(raster_root, target_folders)
selected_dirs <- selected_dirs[dir.exists(selected_dirs)]

# Raster tif files (grab all tifs inside each selected folder)
tif_files <- unlist(lapply(selected_dirs, function(d) {
  list.files(d, pattern = "\\.tif$", full.names = TRUE)
}))
cat("Found", length(tif_files), "tif files\n")

# Range shapefiles
shp_files <- list.files(range_dir, pattern="\\.shp$", full.names=TRUE, recursive=FALSE)
cat("Found", length(shp_files), "range shapefiles\n")
print(basename(shp_files))

# -----------------------------
# Build species -> raster map (folder name as key)
# -----------------------------
get_raster_key <- function(path) basename(dirname(path))
raster_map <- setNames(tif_files, vapply(tif_files, get_raster_key, character(1)))

# -----------------------------
# Build folder -> shapefile map using your naming scheme
# -----------------------------
# folder key      -> shapefile stem (without .shp)
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"   # placeholder: you don't have this shapefile yet
)

# Turn shapefiles into a lookup by stem
# Turn shapefiles into a lookup by stem (strip "_range")
shp_stems  <- tools::file_path_sans_ext(basename(shp_files))
shp_stems  <- sub("_range$", "", shp_stems)
shp_lookup <- setNames(shp_files, shp_stems)

# folder key -> shapefile stem
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"     # forest elephant (Loxodonta cyclotis), per your files
)

# Build shp_map keyed by folder name
shp_map <- setNames(rep(NA_character_, length(target_folders)), target_folders)
for (sp in target_folders) {
  stem <- range_name_map[[sp]]
  if (!is.null(stem) && stem %in% names(shp_lookup)) {
    shp_map[[sp]] <- shp_lookup[[stem]]
  }
}
shp_map <- shp_map[!is.na(shp_map)]

# Shared keys
common_keys <- intersect(names(raster_map), names(shp_map))
cat("Matched", length(common_keys), "folders:", paste(common_keys, collapse=", "), "\n")

# Report missing
missing_ranges <- setdiff(names(raster_map), names(shp_map))
if (length(missing_ranges) > 0) {
  cat("\nNo range shapefile found for:", paste(missing_ranges, collapse=", "), "\n")
}

# -----------------------------
# Output
# -----------------------------
out_dir_outside <- file.path(raster_root, "outside_species_range")
dir.create(out_dir_outside, recursive = TRUE, showWarnings = FALSE)

outside_files <- character(0)

for (k in common_keys) {
  
  r <- rast(raster_map[[k]])
  
  rng_sf <- st_read(shp_map[[k]], quiet = TRUE)
  rng_sf <- st_make_valid(rng_sf)          # helps if geometry is messy
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Keep ONLY cells outside the range polygon
  r_outside <- mask(r, rng_v, inverse = TRUE)
  
  out_file <- file.path(out_dir_outside, paste0(k, "_binary_OUTSIDE_range_2070_ssp585.tif"))
  writeRaster(r_outside, out_file, overwrite = TRUE)
  
  outside_files <- c(outside_files, out_file)
  cat("Saved outside-range raster:", out_file, "\n")
}


# ---------------------------
# 7) Plot first outside raster to verify
# ---------------------------
if (length(outside_files) > 0) {
  r_out1 <- rast(outside_files[1])
  plot(r_out1, main = paste("Binary raster OUTSIDE range:", basename(outside_files[3])))
}




#-----------------2100------------

# repeat for 2100
# ssp126

# ---------------------------
# 1) Paths
# ---------------------------

# Binary rasters (your earlier folder)
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2100/ssp126"

# Species ranges (your current folder)
range_dir <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

# Output folder
out_dir <- file.path(raster_root, "clipped_to_range")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# 2) List files
# ---------------------------

tif_files <- list.files(raster_root, pattern="\\.tif$", full.names=TRUE, recursive=TRUE)
shp_files <- list.files(range_dir,  pattern="\\.shp$", full.names=TRUE, recursive=FALSE)

cat("Found", length(tif_files), "raster tif files\n")
cat("Found", length(shp_files), "range shapefiles\n")
stopifnot(length(tif_files) > 0, length(shp_files) > 0)

# ---------------------------
# 3) Build matching keys
#    (assumes ranges are like: blanchardi_range_clipped.shp
#     rasters contain species name like: "Acris blanchardi_mean_pixel_binary.tif"
# ---------------------------

# Key from shapefile names: "blanchardi_range_clipped" -> "blanchardi"
range_key <- tolower(basename(shp_files))
range_key <- str_replace(range_key, "_range\\.shp$", "")
names(shp_files) <- range_key

# Key from raster names: grab the second word in "Genus species_....tif"
# Example: "Acris blanchardi_mean_pixel_binary.tif" -> "blanchardi"
raster_base <- basename(tif_files)
raster_key <- tolower(raster_base)
raster_key <- str_replace(raster_key, "_mean_pixel_binary_2100s_ssp126\\.tif$", "")
raster_key <- str_split_fixed(raster_key, " ", 3)[,2]  # second token = species epithet
names(tif_files) <- raster_key

# Report matching
cat("\nKeys in rasters:\n"); print(sort(unique(names(tif_files))))
cat("\nKeys in ranges:\n");  print(sort(unique(names(shp_files))))

common_keys <- intersect(unique(names(tif_files)), unique(names(shp_files)))
cat("\nCommon keys matched:", length(common_keys), "\n")
print(sort(common_keys))

if (length(common_keys) == 0) stop("No matches found. Check naming conventions.")

# ---------------------------
# 4) Clip each raster to its matching range and save
# ---------------------------

clipped_files <- character(0)

for (k in common_keys) {
  # Read raster
  r <- rast(tif_files[[k]])
  
  # Read range (sf -> terra vect)
  rng_sf <- st_read(shp_files[[k]], quiet = TRUE)
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Crop to bbox, then mask to polygon (strictly inside range)
  r_crop <- crop(r, rng_v)
  r_mask <- mask(r_crop, rng_v)
  
  # Write output
  out_file <- file.path(out_dir, paste0(k, "_binary_clipped_2100s_ssp126.tif"))
  writeRaster(r_mask, out_file, overwrite = TRUE)
  
  clipped_files <- c(clipped_files, out_file)
  
  cat("Saved:", out_file, "\n")
}

cat("\nTotal clipped rasters saved:", length(clipped_files), "\n")

# ---------------------------
# 5) Plot the first clipped raster to verify
# ---------------------------
if (length(clipped_files) > 0) {
  r1 <- rast(clipped_files[1])
  plot(r1, main = paste("Clipped binary raster:", basename(clipped_files[1])))
}


# cover clipped range results to empty raster

# -----------------------------
# Paths
# -----------------------------
root_dir_bin   <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2100/ssp126/clipped_to_range"
root_dir_empty <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/current/clipped_to_range/empty_rasters"

out_dir <- file.path(root_dir_bin, "binary_merged_to_empty")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Helper: robust species id
#   "cyclotis_binary_clipped_2030s_ssp126.tif" -> "cyclotis"
#   "cyclotis_binary_clipped.tif"             -> "cyclotis"
#   "cyclotis_empty_raster.tif"               -> "cyclotis"
# -----------------------------
get_species_id <- function(x) {
  x <- tools::file_path_sans_ext(basename(x))
  x <- sub("_binary_clipped.*$", "", x)  # remove _binary_clipped + anything after
  x <- sub("_empty_raster.*$",  "", x)   # remove _empty_raster + anything after (if any)
  x
}

# -----------------------------
# List files
# -----------------------------
bin_files   <- list.files(root_dir_bin,   pattern = "_binary_clipped.*\\.tif$", full.names = TRUE, recursive = TRUE)
empty_files <- list.files(root_dir_empty, pattern = "_empty_raster\\.tif$",     full.names = TRUE, recursive = FALSE)

cat("Found", length(bin_files), "binary rasters\n")
cat("Found", length(empty_files), "empty rasters\n")

if (length(bin_files) == 0)   stop("No binary rasters found in: ", root_dir_bin)
if (length(empty_files) == 0) stop("No empty rasters found in: ", root_dir_empty)

# Map species -> file
bin_map   <- setNames(bin_files,   vapply(bin_files,   get_species_id, character(1)))
empty_map <- setNames(empty_files, vapply(empty_files, get_species_id, character(1)))

# Shared species
species_common <- intersect(names(bin_map), names(empty_map))
cat("Matched", length(species_common), "species:\n", paste(species_common, collapse = ", "), "\n")

if (length(species_common) == 0) {
  cat("\nBinary IDs:\n");  print(names(bin_map))
  cat("\nEmpty IDs:\n");   print(names(empty_map))
  stop("No matching species names. See IDs printed above.")
}

# -----------------------------
# Merge: binary onto empty grid
# -----------------------------
for (sp in species_common) {
  cat("\n--- Processing:", sp, "---\n")
  
  bin_r   <- rast(bin_map[[sp]])
  empty_r <- rast(empty_map[[sp]])
  
  # Force binary onto empty raster grid (safe even if already matching)
  same_grid <- terra::compareGeom(bin_r, empty_r, stopOnError = FALSE)
  
  if (!isTRUE(same_grid)) {
    bin_r <- terra::resample(bin_r, empty_r, method = "near")  # nearest neighbor for binary
  }
  
  merged <- cover(bin_r, empty_r)  # keep binary where it exists; fill rest with 0s from empty
  names(merged) <- paste0(sp, "_binary_on_empty")
  
  out_file <- file.path(out_dir, paste0(sp, "_binary_on_empty_2100s_ssp126.tif"))
  writeRaster(merged, out_file, overwrite = TRUE)
  
  cat("Saved:", out_file, "\n")
}

# -----------------------------
# Report unmatched
# -----------------------------
only_bin   <- setdiff(names(bin_map), names(empty_map))
only_empty <- setdiff(names(empty_map), names(bin_map))

if (length(only_bin) > 0) {
  cat("\nNo empty raster found for these binaries:\n", paste(only_bin, collapse = ", "), "\n")
}
if (length(only_empty) > 0) {
  cat("\nNo binary raster found for these empties:\n", paste(only_empty, collapse = ", "), "\n")
}


# ---------------------------
# 6) Save raster cells OUTSIDE species range

# Inputs
# -----------------------------
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2100/ssp126"
range_dir   <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

target_folders <- c("bonobos", "chimps", "elephants", "gorillas")

# Only those 4 subfolders
selected_dirs <- file.path(raster_root, target_folders)
selected_dirs <- selected_dirs[dir.exists(selected_dirs)]

# Raster tif files (grab all tifs inside each selected folder)
tif_files <- unlist(lapply(selected_dirs, function(d) {
  list.files(d, pattern = "\\.tif$", full.names = TRUE)
}))
cat("Found", length(tif_files), "tif files\n")

# Range shapefiles
shp_files <- list.files(range_dir, pattern="\\.shp$", full.names=TRUE, recursive=FALSE)
cat("Found", length(shp_files), "range shapefiles\n")
print(basename(shp_files))

# -----------------------------
# Build species -> raster map (folder name as key)
# -----------------------------
get_raster_key <- function(path) basename(dirname(path))
raster_map <- setNames(tif_files, vapply(tif_files, get_raster_key, character(1)))

# -----------------------------
# Build folder -> shapefile map using your naming scheme
# -----------------------------
# folder key      -> shapefile stem (without .shp)
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"   # placeholder: you don't have this shapefile yet
)

# Turn shapefiles into a lookup by stem
# Turn shapefiles into a lookup by stem (strip "_range")
shp_stems  <- tools::file_path_sans_ext(basename(shp_files))
shp_stems  <- sub("_range$", "", shp_stems)
shp_lookup <- setNames(shp_files, shp_stems)

# folder key -> shapefile stem
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"     # forest elephant (Loxodonta cyclotis), per your files
)

# Build shp_map keyed by folder name
shp_map <- setNames(rep(NA_character_, length(target_folders)), target_folders)
for (sp in target_folders) {
  stem <- range_name_map[[sp]]
  if (!is.null(stem) && stem %in% names(shp_lookup)) {
    shp_map[[sp]] <- shp_lookup[[stem]]
  }
}
shp_map <- shp_map[!is.na(shp_map)]

# Shared keys
common_keys <- intersect(names(raster_map), names(shp_map))
cat("Matched", length(common_keys), "folders:", paste(common_keys, collapse=", "), "\n")

# Report missing
missing_ranges <- setdiff(names(raster_map), names(shp_map))
if (length(missing_ranges) > 0) {
  cat("\nNo range shapefile found for:", paste(missing_ranges, collapse=", "), "\n")
}

# -----------------------------
# Output
# -----------------------------
out_dir_outside <- file.path(raster_root, "outside_species_range")
dir.create(out_dir_outside, recursive = TRUE, showWarnings = FALSE)

outside_files <- character(0)

for (k in common_keys) {
  
  r <- rast(raster_map[[k]])
  
  rng_sf <- st_read(shp_map[[k]], quiet = TRUE)
  rng_sf <- st_make_valid(rng_sf)          # helps if geometry is messy
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Keep ONLY cells outside the range polygon
  r_outside <- mask(r, rng_v, inverse = TRUE)
  
  out_file <- file.path(out_dir_outside, paste0(k, "_binary_OUTSIDE_range_2100_ssp126.tif"))
  writeRaster(r_outside, out_file, overwrite = TRUE)
  
  outside_files <- c(outside_files, out_file)
  cat("Saved outside-range raster:", out_file, "\n")
}




# ssp245

# ---------------------------
# 1) Paths
# ---------------------------

# Binary rasters (your earlier folder)
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2100/ssp245"

# Species ranges (your current folder)
range_dir <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

# Output folder
out_dir <- file.path(raster_root, "clipped_to_range")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# 2) List files
# ---------------------------

tif_files <- list.files(raster_root, pattern="\\.tif$", full.names=TRUE, recursive=TRUE)
shp_files <- list.files(range_dir,  pattern="\\.shp$", full.names=TRUE, recursive=FALSE)

cat("Found", length(tif_files), "raster tif files\n")
cat("Found", length(shp_files), "range shapefiles\n")
stopifnot(length(tif_files) > 0, length(shp_files) > 0)

# ---------------------------
# 3) Build matching keys
#    (assumes ranges are like: blanchardi_range_clipped.shp
#     rasters contain species name like: "Acris blanchardi_mean_pixel_binary.tif"
# ---------------------------

# Key from shapefile names: "blanchardi_range_clipped" -> "blanchardi"
range_key <- tolower(basename(shp_files))
range_key <- str_replace(range_key, "_range\\.shp$", "")
names(shp_files) <- range_key

# Key from raster names: grab the second word in "Genus species_....tif"
# Example: "Acris blanchardi_mean_pixel_binary.tif" -> "blanchardi"
raster_base <- basename(tif_files)
raster_key <- tolower(raster_base)
raster_key <- str_replace(raster_key, "_mean_pixel_binary_2100s_ssp245\\.tif$", "")
raster_key <- str_split_fixed(raster_key, " ", 3)[,2]  # second token = species epithet
names(tif_files) <- raster_key

# Report matching
cat("\nKeys in rasters:\n"); print(sort(unique(names(tif_files))))
cat("\nKeys in ranges:\n");  print(sort(unique(names(shp_files))))

common_keys <- intersect(unique(names(tif_files)), unique(names(shp_files)))
cat("\nCommon keys matched:", length(common_keys), "\n")
print(sort(common_keys))

if (length(common_keys) == 0) stop("No matches found. Check naming conventions.")

# ---------------------------
# 4) Clip each raster to its matching range and save
# ---------------------------

clipped_files <- character(0)

for (k in common_keys) {
  # Read raster
  r <- rast(tif_files[[k]])
  
  # Read range (sf -> terra vect)
  rng_sf <- st_read(shp_files[[k]], quiet = TRUE)
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Crop to bbox, then mask to polygon (strictly inside range)
  r_crop <- crop(r, rng_v)
  r_mask <- mask(r_crop, rng_v)
  
  # Write output
  out_file <- file.path(out_dir, paste0(k, "_binary_clipped_2100s_ssp245.tif"))
  writeRaster(r_mask, out_file, overwrite = TRUE)
  
  clipped_files <- c(clipped_files, out_file)
  
  cat("Saved:", out_file, "\n")
}

cat("\nTotal clipped rasters saved:", length(clipped_files), "\n")

# ---------------------------
# 5) Plot the first clipped raster to verify
# ---------------------------
if (length(clipped_files) > 0) {
  r1 <- rast(clipped_files[1])
  plot(r1, main = paste("Clipped binary raster:", basename(clipped_files[1])))
}


# cover clipped range results to empty raster

# -----------------------------
# Paths
# -----------------------------
root_dir_bin   <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2100/ssp245/clipped_to_range"
root_dir_empty <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/current/clipped_to_range/empty_rasters"

out_dir <- file.path(root_dir_bin, "binary_merged_to_empty")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Helper: robust species id
#   "cyclotis_binary_clipped_2030s_ssp126.tif" -> "cyclotis"
#   "cyclotis_binary_clipped.tif"             -> "cyclotis"
#   "cyclotis_empty_raster.tif"               -> "cyclotis"
# -----------------------------
get_species_id <- function(x) {
  x <- tools::file_path_sans_ext(basename(x))
  x <- sub("_binary_clipped.*$", "", x)  # remove _binary_clipped + anything after
  x <- sub("_empty_raster.*$",  "", x)   # remove _empty_raster + anything after (if any)
  x
}

# -----------------------------
# List files
# -----------------------------
bin_files   <- list.files(root_dir_bin,   pattern = "_binary_clipped.*\\.tif$", full.names = TRUE, recursive = TRUE)
empty_files <- list.files(root_dir_empty, pattern = "_empty_raster\\.tif$",     full.names = TRUE, recursive = FALSE)

cat("Found", length(bin_files), "binary rasters\n")
cat("Found", length(empty_files), "empty rasters\n")

if (length(bin_files) == 0)   stop("No binary rasters found in: ", root_dir_bin)
if (length(empty_files) == 0) stop("No empty rasters found in: ", root_dir_empty)

# Map species -> file
bin_map   <- setNames(bin_files,   vapply(bin_files,   get_species_id, character(1)))
empty_map <- setNames(empty_files, vapply(empty_files, get_species_id, character(1)))

# Shared species
species_common <- intersect(names(bin_map), names(empty_map))
cat("Matched", length(species_common), "species:\n", paste(species_common, collapse = ", "), "\n")

if (length(species_common) == 0) {
  cat("\nBinary IDs:\n");  print(names(bin_map))
  cat("\nEmpty IDs:\n");   print(names(empty_map))
  stop("No matching species names. See IDs printed above.")
}

# -----------------------------
# Merge: binary onto empty grid
# -----------------------------
for (sp in species_common) {
  cat("\n--- Processing:", sp, "---\n")
  
  bin_r   <- rast(bin_map[[sp]])
  empty_r <- rast(empty_map[[sp]])
  
  # Force binary onto empty raster grid (safe even if already matching)
  same_grid <- terra::compareGeom(bin_r, empty_r, stopOnError = FALSE)
  
  if (!isTRUE(same_grid)) {
    bin_r <- terra::resample(bin_r, empty_r, method = "near")  # nearest neighbor for binary
  }
  
  merged <- cover(bin_r, empty_r)  # keep binary where it exists; fill rest with 0s from empty
  names(merged) <- paste0(sp, "_binary_on_empty")
  
  out_file <- file.path(out_dir, paste0(sp, "_binary_on_empty_2100s_ssp245.tif"))
  writeRaster(merged, out_file, overwrite = TRUE)
  
  cat("Saved:", out_file, "\n")
}

# -----------------------------
# Report unmatched
# -----------------------------
only_bin   <- setdiff(names(bin_map), names(empty_map))
only_empty <- setdiff(names(empty_map), names(bin_map))

if (length(only_bin) > 0) {
  cat("\nNo empty raster found for these binaries:\n", paste(only_bin, collapse = ", "), "\n")
}
if (length(only_empty) > 0) {
  cat("\nNo binary raster found for these empties:\n", paste(only_empty, collapse = ", "), "\n")
}


# 6) Save raster cells OUTSIDE species range

# Inputs
# -----------------------------
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2100/ssp245"
range_dir   <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

target_folders <- c("bonobos", "chimps", "elephants", "gorillas")

# Only those 4 subfolders
selected_dirs <- file.path(raster_root, target_folders)
selected_dirs <- selected_dirs[dir.exists(selected_dirs)]

# Raster tif files (grab all tifs inside each selected folder)
tif_files <- unlist(lapply(selected_dirs, function(d) {
  list.files(d, pattern = "\\.tif$", full.names = TRUE)
}))
cat("Found", length(tif_files), "tif files\n")

# Range shapefiles
shp_files <- list.files(range_dir, pattern="\\.shp$", full.names=TRUE, recursive=FALSE)
cat("Found", length(shp_files), "range shapefiles\n")
print(basename(shp_files))

# -----------------------------
# Build species -> raster map (folder name as key)
# -----------------------------
get_raster_key <- function(path) basename(dirname(path))
raster_map <- setNames(tif_files, vapply(tif_files, get_raster_key, character(1)))

# -----------------------------
# Build folder -> shapefile map using your naming scheme
# -----------------------------
# folder key      -> shapefile stem (without .shp)
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"   # placeholder: you don't have this shapefile yet
)

# Turn shapefiles into a lookup by stem
# Turn shapefiles into a lookup by stem (strip "_range")
shp_stems  <- tools::file_path_sans_ext(basename(shp_files))
shp_stems  <- sub("_range$", "", shp_stems)
shp_lookup <- setNames(shp_files, shp_stems)

# folder key -> shapefile stem
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"     # forest elephant (Loxodonta cyclotis), per your files
)

# Build shp_map keyed by folder name
shp_map <- setNames(rep(NA_character_, length(target_folders)), target_folders)
for (sp in target_folders) {
  stem <- range_name_map[[sp]]
  if (!is.null(stem) && stem %in% names(shp_lookup)) {
    shp_map[[sp]] <- shp_lookup[[stem]]
  }
}
shp_map <- shp_map[!is.na(shp_map)]

# Shared keys
common_keys <- intersect(names(raster_map), names(shp_map))
cat("Matched", length(common_keys), "folders:", paste(common_keys, collapse=", "), "\n")

# Report missing
missing_ranges <- setdiff(names(raster_map), names(shp_map))
if (length(missing_ranges) > 0) {
  cat("\nNo range shapefile found for:", paste(missing_ranges, collapse=", "), "\n")
}

# -----------------------------
# Output
# -----------------------------
out_dir_outside <- file.path(raster_root, "outside_species_range")
dir.create(out_dir_outside, recursive = TRUE, showWarnings = FALSE)

outside_files <- character(0)

for (k in common_keys) {
  
  r <- rast(raster_map[[k]])
  
  rng_sf <- st_read(shp_map[[k]], quiet = TRUE)
  rng_sf <- st_make_valid(rng_sf)          # helps if geometry is messy
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Keep ONLY cells outside the range polygon
  r_outside <- mask(r, rng_v, inverse = TRUE)
  
  out_file <- file.path(out_dir_outside, paste0(k, "_binary_OUTSIDE_range_2100_ssp245.tif"))
  writeRaster(r_outside, out_file, overwrite = TRUE)
  
  outside_files <- c(outside_files, out_file)
  cat("Saved outside-range raster:", out_file, "\n")
}



# ssp585

# ---------------------------
# 1) Paths
# ---------------------------

# Binary rasters (your earlier folder)
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2100/ssp585"

# Species ranges (your current folder)
range_dir <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

# Output folder
out_dir <- file.path(raster_root, "clipped_to_range")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# 2) List files
# ---------------------------

tif_files <- list.files(raster_root, pattern="\\.tif$", full.names=TRUE, recursive=TRUE)
shp_files <- list.files(range_dir,  pattern="\\.shp$", full.names=TRUE, recursive=FALSE)

cat("Found", length(tif_files), "raster tif files\n")
cat("Found", length(shp_files), "range shapefiles\n")
stopifnot(length(tif_files) > 0, length(shp_files) > 0)

# ---------------------------
# 3) Build matching keys
#    (assumes ranges are like: blanchardi_range_clipped.shp
#     rasters contain species name like: "Acris blanchardi_mean_pixel_binary.tif"
# ---------------------------

# Key from shapefile names: "blanchardi_range_clipped" -> "blanchardi"
range_key <- tolower(basename(shp_files))
range_key <- str_replace(range_key, "_range\\.shp$", "")
names(shp_files) <- range_key

# Key from raster names: grab the second word in "Genus species_....tif"
# Example: "Acris blanchardi_mean_pixel_binary.tif" -> "blanchardi"
raster_base <- basename(tif_files)
raster_key <- tolower(raster_base)
raster_key <- str_replace(raster_key, "_mean_pixel_binary_2100s_ssp585\\.tif$", "")
raster_key <- str_split_fixed(raster_key, " ", 3)[,2]  # second token = species epithet
names(tif_files) <- raster_key

# Report matching
cat("\nKeys in rasters:\n"); print(sort(unique(names(tif_files))))
cat("\nKeys in ranges:\n");  print(sort(unique(names(shp_files))))

common_keys <- intersect(unique(names(tif_files)), unique(names(shp_files)))
cat("\nCommon keys matched:", length(common_keys), "\n")
print(sort(common_keys))

if (length(common_keys) == 0) stop("No matches found. Check naming conventions.")

# ---------------------------
# 4) Clip each raster to its matching range and save
# ---------------------------

clipped_files <- character(0)

for (k in common_keys) {
  # Read raster
  r <- rast(tif_files[[k]])
  
  # Read range (sf -> terra vect)
  rng_sf <- st_read(shp_files[[k]], quiet = TRUE)
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Crop to bbox, then mask to polygon (strictly inside range)
  r_crop <- crop(r, rng_v)
  r_mask <- mask(r_crop, rng_v)
  
  # Write output
  out_file <- file.path(out_dir, paste0(k, "_binary_clipped_2100s_ssp585.tif"))
  writeRaster(r_mask, out_file, overwrite = TRUE)
  
  clipped_files <- c(clipped_files, out_file)
  
  cat("Saved:", out_file, "\n")
}

cat("\nTotal clipped rasters saved:", length(clipped_files), "\n")

# ---------------------------
# 5) Plot the first clipped raster to verify
# ---------------------------
if (length(clipped_files) > 0) {
  r1 <- rast(clipped_files[2])
  plot(r1, main = paste("Clipped binary raster:", basename(clipped_files[2])))
}


# cover clipped range results to empty raster

# -----------------------------
# Paths
# -----------------------------
root_dir_bin   <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2100/ssp585/clipped_to_range"
root_dir_empty <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/current/clipped_to_range/empty_rasters"

out_dir <- file.path(root_dir_bin, "binary_merged_to_empty")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Helper: robust species id
#   "cyclotis_binary_clipped_2030s_ssp126.tif" -> "cyclotis"
#   "cyclotis_binary_clipped.tif"             -> "cyclotis"
#   "cyclotis_empty_raster.tif"               -> "cyclotis"
# -----------------------------
get_species_id <- function(x) {
  x <- tools::file_path_sans_ext(basename(x))
  x <- sub("_binary_clipped.*$", "", x)  # remove _binary_clipped + anything after
  x <- sub("_empty_raster.*$",  "", x)   # remove _empty_raster + anything after (if any)
  x
}

# -----------------------------
# List files
# -----------------------------
bin_files   <- list.files(root_dir_bin,   pattern = "_binary_clipped.*\\.tif$", full.names = TRUE, recursive = TRUE)
empty_files <- list.files(root_dir_empty, pattern = "_empty_raster\\.tif$",     full.names = TRUE, recursive = FALSE)

cat("Found", length(bin_files), "binary rasters\n")
cat("Found", length(empty_files), "empty rasters\n")

if (length(bin_files) == 0)   stop("No binary rasters found in: ", root_dir_bin)
if (length(empty_files) == 0) stop("No empty rasters found in: ", root_dir_empty)

# Map species -> file
bin_map   <- setNames(bin_files,   vapply(bin_files,   get_species_id, character(1)))
empty_map <- setNames(empty_files, vapply(empty_files, get_species_id, character(1)))

# Shared species
species_common <- intersect(names(bin_map), names(empty_map))
cat("Matched", length(species_common), "species:\n", paste(species_common, collapse = ", "), "\n")

if (length(species_common) == 0) {
  cat("\nBinary IDs:\n");  print(names(bin_map))
  cat("\nEmpty IDs:\n");   print(names(empty_map))
  stop("No matching species names. See IDs printed above.")
}

# -----------------------------
# Merge: binary onto empty grid
# -----------------------------
for (sp in species_common) {
  cat("\n--- Processing:", sp, "---\n")
  
  bin_r   <- rast(bin_map[[sp]])
  empty_r <- rast(empty_map[[sp]])
  
  # Force binary onto empty raster grid (safe even if already matching)
  same_grid <- terra::compareGeom(bin_r, empty_r, stopOnError = FALSE)
  
  if (!isTRUE(same_grid)) {
    bin_r <- terra::resample(bin_r, empty_r, method = "near")  # nearest neighbor for binary
  }
  
  merged <- cover(bin_r, empty_r)  # keep binary where it exists; fill rest with 0s from empty
  names(merged) <- paste0(sp, "_binary_on_empty")
  
  out_file <- file.path(out_dir, paste0(sp, "_binary_on_empty_2100s_ssp585.tif"))
  writeRaster(merged, out_file, overwrite = TRUE)
  
  cat("Saved:", out_file, "\n")
}

# -----------------------------
# Report unmatched
# -----------------------------
only_bin   <- setdiff(names(bin_map), names(empty_map))
only_empty <- setdiff(names(empty_map), names(bin_map))

if (length(only_bin) > 0) {
  cat("\nNo empty raster found for these binaries:\n", paste(only_bin, collapse = ", "), "\n")
}
if (length(only_empty) > 0) {
  cat("\nNo binary raster found for these empties:\n", paste(only_empty, collapse = ", "), "\n")
}


# 6) Save raster cells OUTSIDE species range

# Inputs
# -----------------------------
raster_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/2100/ssp585"
range_dir   <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"

target_folders <- c("bonobos", "chimps", "elephants", "gorillas")

# Only those 4 subfolders
selected_dirs <- file.path(raster_root, target_folders)
selected_dirs <- selected_dirs[dir.exists(selected_dirs)]

# Raster tif files (grab all tifs inside each selected folder)
tif_files <- unlist(lapply(selected_dirs, function(d) {
  list.files(d, pattern = "\\.tif$", full.names = TRUE)
}))
cat("Found", length(tif_files), "tif files\n")

# Range shapefiles
shp_files <- list.files(range_dir, pattern="\\.shp$", full.names=TRUE, recursive=FALSE)
cat("Found", length(shp_files), "range shapefiles\n")
print(basename(shp_files))

# -----------------------------
# Build species -> raster map (folder name as key)
# -----------------------------
get_raster_key <- function(path) basename(dirname(path))
raster_map <- setNames(tif_files, vapply(tif_files, get_raster_key, character(1)))

# -----------------------------
# Build folder -> shapefile map using your naming scheme
# -----------------------------
# folder key      -> shapefile stem (without .shp)
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"   # placeholder: you don't have this shapefile yet
)

# Turn shapefiles into a lookup by stem
# Turn shapefiles into a lookup by stem (strip "_range")
shp_stems  <- tools::file_path_sans_ext(basename(shp_files))
shp_stems  <- sub("_range$", "", shp_stems)
shp_lookup <- setNames(shp_files, shp_stems)

# folder key -> shapefile stem
range_name_map <- c(
  bonobos   = "paniscus",
  chimps    = "troglodytes",
  gorillas  = "gorilla",
  elephants = "cyclotis"     # forest elephant (Loxodonta cyclotis), per your files
)

# Build shp_map keyed by folder name
shp_map <- setNames(rep(NA_character_, length(target_folders)), target_folders)
for (sp in target_folders) {
  stem <- range_name_map[[sp]]
  if (!is.null(stem) && stem %in% names(shp_lookup)) {
    shp_map[[sp]] <- shp_lookup[[stem]]
  }
}
shp_map <- shp_map[!is.na(shp_map)]

# Shared keys
common_keys <- intersect(names(raster_map), names(shp_map))
cat("Matched", length(common_keys), "folders:", paste(common_keys, collapse=", "), "\n")

# Report missing
missing_ranges <- setdiff(names(raster_map), names(shp_map))
if (length(missing_ranges) > 0) {
  cat("\nNo range shapefile found for:", paste(missing_ranges, collapse=", "), "\n")
}

# -----------------------------
# Output
# -----------------------------
out_dir_outside <- file.path(raster_root, "outside_species_range")
dir.create(out_dir_outside, recursive = TRUE, showWarnings = FALSE)

outside_files <- character(0)

for (k in common_keys) {
  
  r <- rast(raster_map[[k]])
  
  rng_sf <- st_read(shp_map[[k]], quiet = TRUE)
  rng_sf <- st_make_valid(rng_sf)          # helps if geometry is messy
  rng_v  <- vect(rng_sf)
  
  # Reproject range to raster CRS if needed
  if (!same.crs(rng_v, r)) {
    rng_v <- project(rng_v, crs(r))
  }
  
  # Keep ONLY cells outside the range polygon
  r_outside <- mask(r, rng_v, inverse = TRUE)
  
  out_file <- file.path(out_dir_outside, paste0(k, "_binary_OUTSIDE_range_2100_ssp585.tif"))
  writeRaster(r_outside, out_file, overwrite = TRUE)
  
  outside_files <- c(outside_files, out_file)
  cat("Saved outside-range raster:", out_file, "\n")
}


# ---------------------------
# 7) Plot first outside raster to verify
# ---------------------------
if (length(outside_files) > 0) {
  r_out1 <- rast(outside_files[1])
  plot(r_out1, main = paste("Binary raster OUTSIDE range:", basename(outside_files[3])))
}





#---------------------------------------------------------------------------

#---------------Area and change detection calculations-----------------------

# read all binary raster for area calculation and change detection analysis

# Settings
# =============================
current_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/current/clipped_to_range/binary_merged_to_empty"
future_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future"
out_csv     <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/suitable_area_summary_2020_2100.csv"

years <- c(2030, 2050, 2070, 2100)
ssps  <- c("ssp126", "ssp245", "ssp585")

# =============================
# Helpers
# =============================
get_species_current <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub("_binary_on_empty$", "", nm)
}

get_species_future <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub("_binary_on_empty_\\d{4}s_.*$", "", nm)
}

cell_area_km2 <- function(r) {
  resm <- terra::res(r)
  abs(resm[1] * resm[2]) / 1e6
}

# total range km2 from a raster grid (non-NA cells)
range_km2_from_raster <- function(r) {
  a_cell <- cell_area_km2(r)
  n_total <- as.numeric(global(!is.na(r), "sum", na.rm = TRUE)[1, 1])
  n_total * a_cell
}

# suitable km2 from a binary raster (cells == 1)
suitable_km2_from_raster <- function(r) {
  a_cell <- cell_area_km2(r)
  n_suit <- as.numeric(global(r == 1, "sum", na.rm = TRUE)[1, 1])
  n_suit * a_cell
}

# =============================
# 1) CURRENT: build species -> current range area lookup
# =============================
cur_files <- list.files(current_dir, pattern = "\\.tif$", full.names = TRUE, recursive = FALSE)
if (length(cur_files) == 0) stop("No current rasters found in: ", current_dir)

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

# =============================
# 2) Compile results (CURRENT + FUTURE)
# =============================
res <- list()

# ---- CURRENT rows (2020) ----
for (f in cur_files) {
  r <- rast(f)
  sp <- get_species_current(f)
  
  suit_km2 <- suitable_km2_from_raster(r)
  range_km2_current <- current_range_lookup[[sp]]
  pct <- ifelse(range_km2_current > 0, (suit_km2 / range_km2_current) * 100, NA_real_)
  
  res[[length(res) + 1]] <- data.frame(
    species = sp,
    year = 2020,
    scenario = "current",
    cell_km2 = current_cell_lookup[[sp]],
    range_km2_current = range_km2_current,
    suitable_km2 = suit_km2,
    pct_of_current_range = pct,
    file = f,
    stringsAsFactors = FALSE
  )
}

# ---- FUTURE rows ----
for (yr in years) {
  for (ssp in ssps) {
    
    dir_i <- file.path(future_root, as.character(yr), ssp, "clipped_to_range", "binary_merged_to_empty")
    if (!dir.exists(dir_i)) {
      warning("Missing folder, skipping: ", dir_i)
      next
    }
    
    fut_files <- list.files(dir_i, pattern = "\\.tif$", full.names = TRUE, recursive = FALSE)
    if (length(fut_files) == 0) {
      warning("No future rasters found in: ", dir_i)
      next
    }
    
    for (f in fut_files) {
      r <- rast(f)
      sp <- get_species_future(f)
      
      # Denominator MUST be current range area
      if (!sp %in% names(current_range_lookup)) {
        warning("Species not found in current lookup (skipping): ", sp, " | file: ", f)
        next
      }
      
      suit_km2 <- suitable_km2_from_raster(r)
      range_km2_current <- current_range_lookup[[sp]]
      pct <- ifelse(range_km2_current > 0, (suit_km2 / range_km2_current) * 100, NA_real_)
      
      res[[length(res) + 1]] <- data.frame(
        species = sp,
        year = yr,
        scenario = ssp,
        cell_km2 = cell_area_km2(r),
        range_km2_current = range_km2_current,
        suitable_km2 = suit_km2,
        pct_of_current_range = pct,
        file = f,
        stringsAsFactors = FALSE
      )
    }
  }
}

# =============================
# Combine + save
# =============================
df <- do.call(rbind, res)
df <- df[order(df$species, df$year, df$scenario), ]

print(df)
write.csv(df, out_csv, row.names = FALSE)

cat("\nSaved CSV to:\n", out_csv, "\n")




# plot line graph

library(ggplot2)

# test one species

# 1) Filter species
df_cyc <- subset(df, species == "cyclotis")

# 2) Round to 1 decimal
df_cyc$pct_1dp <- round(df_cyc$pct_of_current_range, 1)

# 3) Get current (2020) row
current_row <- df_cyc[df_cyc$year == 2020 & df_cyc$scenario == "current", ]

# 4) Duplicate current row for each future scenario
future_scenarios <- c("ssp126", "ssp245", "ssp585")
current_expanded <- do.call(rbind, lapply(future_scenarios, function(sc) {
  tmp <- current_row
  tmp$scenario <- sc
  tmp
}))

# 5) Remove original "current" row and combine
df_plot <- rbind(
  subset(df_cyc, scenario != "current"),
  current_expanded
)

# 6) Order scenarios
df_plot$scenario <- factor(df_plot$scenario, levels = future_scenarios)

# 7) Plot (each scenario now starts at 2020)
p <- ggplot(df_plot, aes(x = year, y = pct_1dp, color = scenario, group = scenario)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(2020, 2030, 2050, 2070, 2100)) +
  labs(
    title = "Cyclotis % Suitable Area Over Time",
    x = "Year",
    y = "% Suitable Area (of 2020 range)",
    color = "Scenario"
  ) +
  theme_bw()

print(p)

# Optional save
# ggsave("F:/WWF_data/SDMs_LC_climate/maps_statistics/cyclotis_pct_suitability_lineplot.png",
#        plot = p, width = 8, height = 5, dpi = 300)



# plot all species in one pannel

library(ggplot2)

# -----------------------------
# 1) Rename species to common names
# -----------------------------
df2 <- df
df2$species <- as.character(df2$species)
df2$species[df2$species == "cyclotis"]     <- "elephants"
df2$species[df2$species == "troglodytes"]  <- "chimpanzees"
df2$species[df2$species == "paniscus"]     <- "bonobos"
# gorilla remains unchanged (change to "gorillas" if desired)

# -----------------------------
# 2) Prepare plotting data (connect lines from 2020)
# -----------------------------
future_scenarios <- c("ssp126", "ssp245", "ssp585")

df_plot <- df2[, c("species", "year", "scenario", "pct_of_current_range")]
df_plot$pct_1dp <- round(df_plot$pct_of_current_range, 1)

# Extract 2020 baseline
cur_2020 <- subset(df_plot, year == 2020 & scenario == "current")

# Duplicate baseline for each scenario
cur_expanded <- do.call(rbind, lapply(future_scenarios, function(sc) {
  tmp <- cur_2020
  tmp$scenario <- sc
  tmp
}))

# Keep only future scenarios + baseline
df_plot <- subset(df_plot, scenario %in% future_scenarios)
df_plot <- rbind(df_plot, cur_expanded)

# Order factors
df_plot$scenario <- factor(df_plot$scenario, levels = future_scenarios)
df_plot$species  <- factor(df_plot$species, levels = c("bonobos", "chimpanzees", "gorilla", "elephants"))

# -----------------------------
# 3) Plot (straight lines, no labels)
# -----------------------------
p <- ggplot(df_plot, aes(x = year, y = pct_1dp, color = scenario, group = scenario)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(2020, 2030, 2050, 2070, 2100)) +
  scale_color_manual(values = c(ssp126 = "green", ssp245 = "blue", ssp585 = "red")) +
  labs(
    title = "% Suitable Area Over Time (relative to 2020 range)",
    x = "Year",
    y = "% Suitable Area",
    color = "Scenario"
  ) +
  facet_wrap(~ species, scales = "free_y") +
  theme_bw()

print(p)

# Optional: save
# ggsave("F:/WWF_data/SDMs_LC_climate/maps_statistics/pct_suitability_all_species_lines.png",
#        plot = p, width = 11, height = 6, dpi = 300)



# recalculate by taking off 2030

library(ggplot2)

# -----------------------------
# 1) Rename species to common names
# -----------------------------
df2 <- df
df2$species <- as.character(df2$species)
df2$species[df2$species == "cyclotis"]     <- "elephants"
df2$species[df2$species == "troglodytes"]  <- "chimpanzees"
df2$species[df2$species == "paniscus"]     <- "bonobos"
# gorilla remains unchanged (change to "gorillas" if desired)

# -----------------------------
# 2) Prepare plotting data (connect lines from 2020)
# -----------------------------
future_scenarios <- c("ssp126", "ssp245", "ssp585")

df_plot <- df2[, c("species", "year", "scenario", "pct_of_current_range")]
df_plot$pct_1dp <- round(df_plot$pct_of_current_range, 1)

# Remove 2030
df_plot <- subset(df_plot, year != 2030)

# Extract 2020 baseline
cur_2020 <- subset(df_plot, year == 2020 & scenario == "current")

# Duplicate baseline for each scenario
cur_expanded <- do.call(rbind, lapply(future_scenarios, function(sc) {
  tmp <- cur_2020
  tmp$scenario <- sc
  tmp
}))

# Keep only future scenarios + baseline
df_plot <- subset(df_plot, scenario %in% future_scenarios)
df_plot <- rbind(df_plot, cur_expanded)

# Order factors
df_plot$scenario <- factor(df_plot$scenario, levels = future_scenarios)
df_plot$species  <- factor(df_plot$species, levels = c("bonobos", "chimpanzees", "gorilla", "elephants"))

# -----------------------------
# 3) Plot
# -----------------------------
p <- ggplot(df_plot, aes(x = year, y = pct_1dp, color = scenario, group = scenario)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(2020, 2050, 2070, 2100)) +
  scale_color_manual(values = c(ssp126 = "green", ssp245 = "blue", ssp585 = "red")) +
  labs(
    title = "Percentage Trends in Suitable Ape and Elephant Habitats",
    x = "Year",
    y = "% Suitable Area",
    color = "Scenario"
  ) +
  facet_wrap(~ species, scales = "free_y") +
  theme_bw()

print(p)

#  save
#ggsave("F:/WWF_data/SDMs_LC_climate/maps_statistics/plots/pct_suitability_no2030.png",
#plot = p, width = 11, height = 6, dpi = 300)




# Change detection -------------------

# =============================
# PATHS
# =============================
current_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/current/clipped_to_range/binary_merged_to_empty"
future_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future"

# Output folders
out_change_dir  <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/change_rasters_2020_to_future"
out_refugia_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/refugia_rasters_stable"
out_loss_dir    <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/exposure_rasters_loss"
dir.create(out_change_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(out_refugia_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_loss_dir,    recursive = TRUE, showWarnings = FALSE)

out_csv <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/refugia_exposure_summary_2020_to_future.csv"

years <- c(2050, 2070, 2100)
ssps  <- c("ssp126", "ssp245", "ssp585")

# =============================
# HELPERS
# =============================
get_species_current <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub("_binary_on_empty$", "", nm)
}
get_species_future <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  sub("_binary_on_empty_\\d{4}s_.*$", "", nm)
}

cell_area_km2 <- function(r) {
  rr <- res(r)
  abs(rr[1] * rr[2]) / 1e6
}

# Current range area (km2) = count non-NA cells * cell area
current_range_km2 <- function(r) {
  a <- cell_area_km2(r)
  n <- as.numeric(global(!is.na(r), "sum", na.rm = TRUE)[1,1])
  n * a
}

# =============================
# 1) READ CURRENT RASTERS (2020 baseline)
# =============================
cur_files <- list.files(current_dir, pattern = "\\.tif$", full.names = TRUE, recursive = FALSE)
if (length(cur_files) == 0) stop("No current rasters found in: ", current_dir)

cur_map <- setNames(cur_files, vapply(cur_files, get_species_current, character(1)))

# Pre-compute current range area lookup (km2)
cur_range_lookup <- sapply(names(cur_map), function(sp) {
  r <- rast(cur_map[[sp]])
  current_range_km2(r)
}, simplify = TRUE)

# =============================
# 2) LOOP: build change rasters + refugia/loss rasters + stats
# Change coding:
#  1 = stable (1->1)
#  2 = gain   (0->1)
# -1 = loss   (1->0)
#  0 = unchanged (0->0)
# =============================
results <- list()

# Pick one species to plot for verification (change if you want)
species_to_plot <- "cyclotis"   # e.g., "cyclotis", "gorilla", "paniscus", "troglodytes"
plotted_once <- FALSE

for (sp in names(cur_map)) {
  
  r_cur <- rast(cur_map[[sp]])
  
  # loop years/scenarios
  for (yr in years) {
    for (ssp in ssps) {
      
      fut_dir <- file.path(future_root, as.character(yr), ssp, "clipped_to_range", "binary_merged_to_empty")
      if (!dir.exists(fut_dir)) {
        warning("Missing folder, skipping: ", fut_dir)
        next
      }
      
      fut_files <- list.files(fut_dir, pattern="\\.tif$", full.names=TRUE, recursive=FALSE)
      if (length(fut_files) == 0) next
      
      # find the matching file for this species
      fut_species <- vapply(fut_files, get_species_future, character(1))
      idx <- which(fut_species == sp)
      if (length(idx) == 0) {
        warning("No future raster found for species ", sp, " in: ", fut_dir)
        next
      }
      
      r_fut <- rast(fut_files[idx[1]])
      
      # Ensure same grid (should already match, but safe)
      if (!isTRUE(compareGeom(r_cur, r_fut, stopOnError = FALSE))) {
        r_fut <- resample(r_fut, r_cur, method = "near")
      }
      
      # Change raster: NA-safe by restricting to cells where both are not NA
      ok <- !is.na(r_cur) & !is.na(r_fut)
      
      change <- rast(r_cur)
      values(change) <- NA
      
      # stable (1->1)
      change[ ok & r_cur == 1 & r_fut == 1 ] <-  1
      # gain (0->1)
      change[ ok & r_cur == 0 & r_fut == 1 ] <-  2
      # loss (1->0)
      change[ ok & r_cur == 1 & r_fut == 0 ] <- -1
      # unchanged (0->0)
      change[ ok & r_cur == 0 & r_fut == 0 ] <-  0
      
      names(change) <- paste0(sp, "_change_", yr, "_", ssp)
      
      # Refugia raster (stable pixels only)
      refugia <- mask(change, change == 1, maskvalues = FALSE)  # keeps only value==1, others NA
      names(refugia) <- paste0(sp, "_refugia_", yr, "_", ssp)
      
      # Loss raster (exposure pixels only)
      loss <- mask(change, change == -1, maskvalues = FALSE)    # keeps only value==-1, others NA
      names(loss) <- paste0(sp, "_loss_", yr, "_", ssp)
      
      # Save rasters
      out_change <- file.path(out_change_dir,  paste0(sp, "_change_2020_to_", yr, "_", ssp, ".tif"))
      out_ref    <- file.path(out_refugia_dir, paste0(sp, "_refugia_stable_2020_to_", yr, "_", ssp, ".tif"))
      out_loss   <- file.path(out_loss_dir,    paste0(sp, "_exposure_loss_2020_to_", yr, "_", ssp, ".tif"))
      
      writeRaster(change,  out_change, overwrite = TRUE)
      writeRaster(refugia, out_ref,    overwrite = TRUE)
      writeRaster(loss,    out_loss,   overwrite = TRUE)
      
      # Plot one example to verify (once)
      if (!plotted_once && sp == species_to_plot) {
        plot(change, main = paste0(sp, " change (2020 -> ", yr, " ", ssp, ")\n",
                                   "1=stable, 2=gain, -1=loss, 0=unchanged"))
        plotted_once <- TRUE
      }
      
      # Stats: refugia (stable) and exposure (loss)
      a_cell <- cell_area_km2(r_cur)
      stable_n <- as.numeric(global(change == 1, "sum", na.rm = TRUE)[1,1])
      loss_n   <- as.numeric(global(change == -1, "sum", na.rm = TRUE)[1,1])
      
      stable_km2 <- stable_n * a_cell
      loss_km2   <- loss_n   * a_cell
      
      range_km2_2020 <- cur_range_lookup[[sp]]
      
      stable_pct <- ifelse(range_km2_2020 > 0, stable_km2 / range_km2_2020 * 100, NA_real_)
      loss_pct   <- ifelse(range_km2_2020 > 0, loss_km2   / range_km2_2020 * 100, NA_real_)
      
      results[[length(results) + 1]] <- data.frame(
        species = sp,
        baseline_year = 2020,
        future_year = yr,
        scenario = ssp,
        range_km2_2020 = range_km2_2020,
        stable_km2 = stable_km2,
        stable_pct_of_2020_range = stable_pct,
        loss_km2 = loss_km2,
        loss_pct_of_2020_range = loss_pct,
        change_raster = out_change,
        refugia_raster = out_ref,
        loss_raster = out_loss,
        stringsAsFactors = FALSE
      )
    }
  }
}

# =============================
# 3) SAVE RESULTS CSV
# =============================
df_change_stats <- do.call(rbind, results)
df_change_stats <- df_change_stats[order(df_change_stats$species,
                                         df_change_stats$future_year,
                                         df_change_stats$scenario), ]

print(df_change_stats)
write.csv(df_change_stats, out_csv, row.names = FALSE)
cat("\nSaved CSV:\n", out_csv, "\n")
cat("\nSaved change rasters in:\n", out_change_dir, "\n")
cat("Saved refugia rasters in:\n", out_refugia_dir, "\n")
cat("Saved loss rasters in:\n", out_loss_dir, "\n")



# =============================
# Plot STABLE (refugia) % areas from df_change_stats
# =============================

# 1) Copy + rename species to common names (same mapping as before)
d <- df_change_stats
d$species <- as.character(d$species)
d$species[d$species == "cyclotis"]     <- "elephants"
d$species[d$species == "troglodytes"]  <- "chimpanzees"
d$species[d$species == "paniscus"]     <- "bonobos"
# gorilla stays as "gorilla" (change to "gorillas" if you want)
# d$species[d$species == "gorilla"] <- "gorillas"

# 2) Keep only needed cols + round to 1 decimal for plotting
d$pct_1dp <- round(d$stable_pct_of_2020_range, 1)

# 3) Factor order + colors
d$scenario <- factor(d$scenario, levels = c("ssp126", "ssp245", "ssp585"))
d$species  <- factor(d$species, levels = c("bonobos", "chimpanzees", "gorilla", "elephants"))

# 4) Plot: stable % (refugia) over time (2050/2070/2100)
p_stable <- ggplot(d, aes(x = future_year, y = pct_1dp, color = scenario, group = scenario)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(2050, 2070, 2100)) +
  scale_color_manual(values = c(ssp126 = "green", ssp245 = "blue", ssp585 = "red")) +
  labs(
    title = "Percentage Trends in In Situ Habitat Refugia Areas for Apes and Elephants",
    x = "Year",
    y = "% Refugia Area",
    color = "Scenario"
  ) +
  facet_wrap(~ species, scales = "free_y") +
  theme_bw()

print(p_stable)

# Optional save
# ggsave("F:/WWF_data/SDMs_LC_climate/maps_statistics/stable_refugia_percent_facets.png",
#        plot = p_stable, width = 11, height = 6, dpi = 300)



# =============================
# Plot Exposure % areas from df_change_stats
# =============================

# 1) Copy + rename species to common names (same mapping as before)
d <- df_change_stats
d$species <- as.character(d$species)
d$species[d$species == "cyclotis"]     <- "elephants"
d$species[d$species == "troglodytes"]  <- "chimpanzees"
d$species[d$species == "paniscus"]     <- "bonobos"
# gorilla stays as "gorilla" (change to "gorillas" if you want)
# d$species[d$species == "gorilla"] <- "gorillas"

# 2) Keep only needed cols + round to 1 decimal for plotting
d$pct_1dp <- round(d$loss_pct_of_2020_range, 1)

# 3) Factor order + colors
d$scenario <- factor(d$scenario, levels = c("ssp126", "ssp245", "ssp585"))
d$species  <- factor(d$species, levels = c("bonobos", "chimpanzees", "gorilla", "elephants"))

# 4) Plot: stable % (refugia) over time (2050/2070/2100)
p_stable <- ggplot(d, aes(x = future_year, y = pct_1dp, color = scenario, group = scenario)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(2050, 2070, 2100)) +
  scale_color_manual(values = c(ssp126 = "green", ssp245 = "blue", ssp585 = "red")) +
  labs(
    title = "Percentage Trends in Suitable Habitat Loss Areas for Apes and Elephants",
    x = "Year",
    y = "% Loss Area",
    color = "Scenario"
  ) +
  facet_wrap(~ species, scales = "free_y") +
  theme_bw()

print(p_stable)

# Optional save
# ggsave("F:/WWF_data/SDMs_LC_climate/maps_statistics/stable_refugia_percent_facets.png",
#        plot = p_stable, width = 11, height = 6, dpi = 300)




#----------------Calculate species richness--------------------------

# =============================
# PATHS
# =============================
current_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/current/clipped_to_range/binary_merged_to_empty"
future_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future"

out_rich_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/richness_outputs"
dir.create(out_rich_dir, recursive = TRUE, showWarnings = FALSE)

out_csv <- file.path(out_rich_dir, "species_richness_value_area_summary_pct_vs_currentRange.csv")

years <- c(2030, 2050, 2070, 2100)
ssps  <- c("ssp126", "ssp245", "ssp585")

# =============================
# HELPERS
# =============================
cell_area_km2 <- function(r) {
  rr <- res(r)
  abs(rr[1] * rr[2]) / 1e6
}

make_template <- function(r_list) {
  tpl <- r_list[[1]]
  for (i in 2:length(r_list)) tpl <- extend(tpl, ext(r_list[[i]]))
  tpl
}

to_template <- function(r, tpl) {
  if (!isTRUE(compareGeom(r, tpl, stopOnError = FALSE))) {
    r <- resample(r, tpl, method = "near")
  }
  r
}

# union-of-species-range area from a set of rasters (km2)
# (cells that are non-NA in at least one species raster)
union_range_area_km2 <- function(r_list_aligned, tpl) {
  a_cell <- cell_area_km2(tpl)
  rng_sum <- Reduce(`+`, lapply(r_list_aligned, function(r) !is.na(r)))
  rng <- rng_sum > 0
  n_rng <- as.numeric(global(rng, "sum", na.rm = TRUE)[1,1])
  n_rng * a_cell
}

# area table for richness values 1..max, with % computed vs CURRENT richness range area
area_by_value_vs_currentRange <- function(rich_pos, a_cell, current_rich_range_km2) {
  max_r <- max(values(rich_pos), na.rm = TRUE)
  vals <- 1:max_r
  
  out <- lapply(vals, function(v) {
    n_v <- as.numeric(global(rich_pos == v, "sum", na.rm = TRUE)[1,1])
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

# Compute richness (>0 only) raster + area table (pct vs CURRENT richness range)
calc_richness_vs_currentRange <- function(files, label, current_rich_range_km2,
                                          plot_it = FALSE, out_dir = out_rich_dir) {
  if (length(files) == 0) stop("No rasters for: ", label)
  
  r_list <- lapply(files, rast)
  tpl <- make_template(r_list)
  r_list <- lapply(r_list, to_template, tpl = tpl)
  
  # richness = sum binaries (NA->0)
  r01 <- lapply(r_list, function(r) ifel(is.na(r), 0, r))
  rich <- Reduce(`+`, r01)
  names(rich) <- paste0("richness_", label)
  
  # delete zeros: 0 -> NA
  rich_pos <- ifel(rich == 0, NA, rich)
  names(rich_pos) <- paste0("richness_pos_", label)
  
  if (plot_it) {
    plot(rich_pos, main = paste0("Richness (>0 only): ", label))
  }
  
  # save raster
  out_rich <- file.path(out_dir, paste0("richness_pos_", label, ".tif"))
  writeRaster(rich_pos, out_rich, overwrite = TRUE)
  
  # areas by richness value (percent vs CURRENT richness range area)
  a_cell <- cell_area_km2(tpl)
  tab <- area_by_value_vs_currentRange(rich_pos, a_cell, current_rich_range_km2)
  
  tab$label <- label
  tab$n_species <- length(files)
  tab$cell_km2 <- a_cell
  tab$current_richness_range_area_km2 <- current_rich_range_km2
  tab$richness_raster <- out_rich
  
  tab[, c("label","n_species","cell_km2","current_richness_range_area_km2",
          "richness_value","n_cells","area_km2","pct_of_CURRENT_richness_range",
          "richness_raster")]
}

# =============================
# 1) CURRENT: compute CURRENT richness range area (denominator)
# =============================
cur_files <- list.files(current_dir, pattern = "\\.tif$", full.names = TRUE, recursive = FALSE)
if (length(cur_files) == 0) stop("No current rasters found in: ", current_dir)

# Build current template + aligned rasters (for union range calc)
cur_list <- lapply(cur_files, rast)
cur_tpl  <- make_template(cur_list)
cur_list <- lapply(cur_list, to_template, tpl = cur_tpl)

current_rich_range_km2 <- union_range_area_km2(cur_list, cur_tpl)
cat("CURRENT richness range area (km2): ", current_rich_range_km2, "\n")

# Current richness (>0 only) + plot
df_all <- list()
df_all[[1]] <- calc_richness_vs_currentRange(
  files = cur_files,
  label = "2020_current",
  current_rich_range_km2 = current_rich_range_km2,
  plot_it = TRUE
)

# =============================
# 2) FUTURE: percent vs CURRENT richness range area
# =============================
k <- 2
for (yr in years) {
  for (ssp in ssps) {
    
    dir_i <- file.path(future_root, as.character(yr), ssp, "clipped_to_range", "binary_merged_to_empty")
    if (!dir.exists(dir_i)) {
      warning("Missing folder, skipping: ", dir_i)
      next
    }
    
    files_i <- list.files(dir_i, pattern="\\.tif$", full.names=TRUE, recursive=FALSE)
    if (length(files_i) == 0) {
      warning("No rasters found, skipping: ", dir_i)
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

# =============================
# 3) SAVE CSV
# =============================
df_rich_val_area <- do.call(rbind, df_all)
df_rich_val_area <- df_rich_val_area[order(df_rich_val_area$label, df_rich_val_area$richness_value), ]

print(df_rich_val_area)
write.csv(df_rich_val_area, out_csv, row.names = FALSE)

cat("\nSaved CSV:\n", out_csv, "\n")
cat("Saved richness rasters in:\n", out_rich_dir, "\n")



# plot richness

library(ggplot2)

# df_rich_val_area already in your environment
d <- df_rich_val_area

# % richness (area of richness_value / current richness range * 100)
# since cell_km2 = 1, area_km2 == n_cells, but keep it general:
d$area_km2 <- d$n_cells * d$cell_km2
d$pct_rich <- (d$area_km2 / d$current_richness_range_area_km2) * 100

# Parse label into year + scenario (2020_current special case)
d$year <- ifelse(d$label == "2020_current", 2020, as.integer(sub("_.*$", "", d$label)))
d$scenario <- ifelse(d$label == "2020_current", "current", sub("^\\d{4}_", "", d$label))

# Factors for nice ordering
d$scenario <- factor(d$scenario, levels = c("current", "ssp126", "ssp245", "ssp585"))
d$richness_value <- factor(d$richness_value, levels = sort(unique(d$richness_value)))

# Facet plot: one panel per richness_value
p <- ggplot(d, aes(x = year, y = pct_rich, color = scenario, group = scenario)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(2020, 2030, 2050, 2070, 2100)) +
  scale_color_manual(values = c(current = "black", ssp126 = "green", ssp245 = "blue", ssp585 = "red")) +
  labs(
    title = "Percentage richness area by richness value (denominator = 2020 richness range)",
    x = "Year",
    y = "% of 2020 richness range area",
    color = "Scenario"
  ) +
  facet_wrap(~ richness_value, ncol = 3, scales = "free_y") +
  theme_bw()

print(p)

# Optional save
# ggsave("F:/WWF_data/SDMs_LC_climate/maps_statistics/richness_outputs/pct_richness_facets_by_value.png",
#        plot = p, width = 11, height = 6, dpi = 300)



# plot by removing 2030

d <- df_rich_val_area

# Compute % richness vs CURRENT richness range
d$area_km2 <- d$n_cells * d$cell_km2
d$pct_rich <- (d$area_km2 / d$current_richness_range_area_km2) * 100

# Extract year + scenario
d$year <- ifelse(d$label == "2020_current", 2020, as.integer(sub("_.*$", "", d$label)))
d$scenario <- ifelse(d$label == "2020_current", "current", sub("^\\d{4}_", "", d$label))

# Remove 2030
d <- subset(d, year != 2030)

# ---- DUPLICATE 2020 FOR EACH FUTURE SCENARIO (to link lines) ----
d2020 <- subset(d, year == 2020)

d126 <- d2020; d126$scenario <- "ssp126"
d245 <- d2020; d245$scenario <- "ssp245"
d585 <- d2020; d585$scenario <- "ssp585"

d_plot <- rbind(
  subset(d, year > 2020),
  d126, d245, d585
)

# Factor ordering
d_plot$scenario <- factor(d_plot$scenario, levels = c("ssp126","ssp245","ssp585"))
d_plot$richness_value <- factor(d_plot$richness_value,
                                levels = sort(unique(d_plot$richness_value)))

# Plot
p <- ggplot(d_plot, aes(x = year, y = pct_rich, color = scenario, group = scenario)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(2020, 2050, 2070, 2100)) +
  scale_color_manual(values = c(ssp126 = "green",
                                ssp245 = "blue",
                                ssp585 = "red")) +
  labs(
    title = "Percentage richness area (future linked to 2020 baseline)",
    x = "Year",
    y = "% of 2020 richness range area",
    color = "Scenario"
  ) +
  facet_wrap(~ richness_value, ncol = 3, scales = "free_y") +
  theme_bw()

print(p)

# Optional save
# ggsave("F:/WWF_data/SDMs_LC_climate/maps_statistics/richness_outputs/pct_richness_linked_to_2020.png",
#        plot = p, width = 11, height = 6, dpi = 300)





# extract top 5% richness sites

# -----------------------------
# Where your richness rasters were saved
# (from earlier code)
# -----------------------------
rich_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/richness_outputs"

# Read ALL richness rasters you created (richness_pos_*.tif)
rich_files <- list.files(rich_dir, pattern = "^richness_pos_.*\\.tif$", full.names = TRUE)
cat("Found", length(rich_files), "richness rasters\n")
if (length(rich_files) == 0) stop("No richness_pos_*.tif found in: ", rich_dir)

# Output folder for top-5% masks
out_top5_dir <- file.path(rich_dir, "top5pct_richness")
dir.create(out_top5_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Helper: top 5% extraction
# -----------------------------
top5_mask <- function(r, probs = 0.95) {
  v <- values(r, mat = FALSE)
  v <- v[!is.na(v)]
  if (length(v) == 0) stop("Raster has no non-NA values.")
  thr <- as.numeric(quantile(v, probs = probs, names = FALSE, type = 7))
  mask <- r >= thr
  mask <- ifel(mask, 1, NA)  # keep only top cells (1), others NA
  list(mask = mask, threshold = thr)
}

# -----------------------------
# Build + save masks for all rasters
# -----------------------------
mask_files <- character(0)
thresholds <- data.frame(label = character(0), threshold = numeric(0))

for (f in rich_files) {
  r <- rast(f)
  lbl <- sub("^richness_pos_", "", tools::file_path_sans_ext(basename(f)))  # e.g. 2020_current, 2050_ssp245...
  
  res_top <- top5_mask(r, probs = 0.95)
  m <- res_top$mask
  thr <- res_top$threshold
  
  out_mask <- file.path(out_top5_dir, paste0("top5pct_", lbl, ".tif"))
  writeRaster(m, out_mask, overwrite = TRUE)
  
  mask_files <- c(mask_files, out_mask)
  thresholds <- rbind(thresholds, data.frame(label = lbl, threshold = thr))
  
  cat("Saved:", out_mask, " (threshold >= ", thr, ")\n")
}

# Save thresholds table (optional but useful)
write.csv(thresholds, file.path(out_top5_dir, "top5pct_thresholds.csv"), row.names = FALSE)

# -----------------------------
# Plot ONE example to verify (current)
# -----------------------------
f_cur <- file.path(rich_dir, "richness_pos_2020_current.tif")
if (!file.exists(f_cur)) {
  # fallback: just plot first raster found
  f_cur <- rich_files[1]
}
r_cur <- rast(f_cur)
lbl_cur <- sub("^richness_pos_", "", tools::file_path_sans_ext(basename(f_cur)))

res_cur <- top5_mask(r_cur, probs = 0.95)
m_cur <- res_cur$mask
thr_cur <- res_cur$threshold

plot(r_cur, main = paste0("Richness (>0), ", lbl_cur))
plot(m_cur, main = paste0("Top 5% richness mask (>= ", thr_cur, "), ", lbl_cur))




# calculate and plot top5% richness sites

library(terra)
library(ggplot2)

# -----------------------------
# Inputs
# -----------------------------
rich_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/richness_outputs"
rich_files <- list.files(rich_dir, pattern = "^richness_pos_.*\\.tif$", full.names = TRUE)
cat("Found", length(rich_files), "richness_pos rasters\n")
if (length(rich_files) == 0) stop("No richness_pos_*.tif found in: ", rich_dir)

out_top5_dir <- file.path(rich_dir, "top5pct_area")
dir.create(out_top5_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Helper: parse label -> year/scenario
# -----------------------------
parse_label <- function(lbl) {
  if (lbl == "2020_current") return(list(year = 2020L, scenario = "current"))
  year <- as.integer(sub("_.*$", "", lbl))
  scenario <- sub("^\\d{4}_", "", lbl)
  list(year = year, scenario = scenario)
}

# -----------------------------
# Helper: exact top 5% by AREA (cells)
# Keeps exactly ceil(0.05 * N) cells among richness>0 (non-NA)
# -----------------------------
top5_area_mask <- function(r, p = 0.05) {
  v <- values(r, mat = FALSE)
  ok <- which(!is.na(v))
  if (length(ok) == 0) stop("Raster has no non-NA values (richness>0).")
  
  n <- length(ok)
  n_top <- ceiling(p * n)
  
  vv <- v[ok]
  
  # Rank descending (highest richness first)
  ord <- order(vv, decreasing = TRUE, na.last = NA)
  
  # Determine cutoff value at the boundary
  cutoff_val <- vv[ord[n_top]]
  
  # Start with all > cutoff
  keep <- ok[vv > cutoff_val]
  
  # Add enough == cutoff to reach exactly n_top
  eq_idx <- ok[vv == cutoff_val]
  needed <- n_top - length(keep)
  if (needed > 0) {
    # deterministic selection: take first 'needed' cells by stable ordering (index order)
    eq_idx <- sort(eq_idx)
    keep <- c(keep, eq_idx[seq_len(min(needed, length(eq_idx)))])
  }
  
  # Build mask (1 for top cells, NA otherwise)
  m <- rast(r)
  values(m) <- NA
  m[keep] <- 1
  
  list(mask = m, n_top = n_top, cutoff_val = cutoff_val, n_total = n)
}

# -----------------------------
# Loop: compute top5% area + save masks + plot current for verification
# -----------------------------
rows <- list()
plotted <- FALSE

for (f in rich_files) {
  r <- rast(f)
  lbl <- sub("^richness_pos_", "", tools::file_path_sans_ext(basename(f)))
  
  # area per cell (km2)
  a_cell <- abs(res(r)[1] * res(r)[2]) / 1e6
  
  res5 <- top5_area_mask(r, p = 0.05)
  m <- res5$mask
  
  top5_area_km2 <- res5$n_top * a_cell
  top5_pct_of_pos <- (res5$n_top / res5$n_total) * 100  # should be ~5% (exact with ceiling)
  
  # Save mask raster
  out_mask <- file.path(out_top5_dir, paste0("top5pct_area_", lbl, ".tif"))
  writeRaster(m, out_mask, overwrite = TRUE)
  
  # Plot current to verify (once)
  if (!plotted && lbl == "2020_current") {
    plot(r, main = "Richness (>0) 2020_current")
    plot(m, main = paste0("Top 5% area mask (exact) 2020_current\ncutoff=", res5$cutoff_val))
    plotted <- TRUE
  }
  
  # Parse year/scenario
  ps <- parse_label(lbl)
  
  rows[[length(rows) + 1]] <- data.frame(
    label = lbl,
    year = ps$year,
    scenario = ps$scenario,
    cell_km2 = a_cell,
    n_total_pos_cells = res5$n_total,
    n_top5_cells = res5$n_top,
    cutoff_richness_value = res5$cutoff_val,
    top5_area_km2 = top5_area_km2,
    top5_pct_of_positive_cells = top5_pct_of_pos,
    top5_mask_raster = out_mask,
    stringsAsFactors = FALSE
  )
  
  cat("Done:", lbl, " top5_area_km2=", top5_area_km2, "\n")
}

df_top5 <- do.call(rbind, rows)
df_top5 <- df_top5[order(df_top5$year, df_top5$scenario), ]
print(df_top5)

# Save table
out_csv <- file.path(out_top5_dir, "top5pct_area_summary.csv")
write.csv(df_top5, out_csv, row.names = FALSE)
cat("\nSaved:", out_csv, "\n")

# -----------------------------
# Line graph: Top 5% area through time
# Link SSP lines to 2020 baseline (like before)
# -----------------------------

future_scenarios <- c("ssp126", "ssp245", "ssp585")

# Remove 2030
df_plot <- subset(df_top5, year != 2030)

# Baseline row (2020 current)
base <- subset(df_plot, year == 2020 & scenario == "current")

# Duplicate baseline for each SSP
base_exp <- do.call(rbind, lapply(future_scenarios, function(sc) {
  tmp <- base
  tmp$scenario <- sc
  tmp
}))

# Keep only future SSPs + expanded baseline
df_plot <- rbind(
  subset(df_plot, year > 2020 & scenario %in% future_scenarios),
  base_exp
)

df_plot$scenario <- factor(df_plot$scenario, levels = future_scenarios)

# Plot
p <- ggplot(df_plot, aes(x = year, y = top5_area_km2, color = scenario, group = scenario)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(2020, 2050, 2070, 2100)) +
  scale_color_manual(values = c(ssp126 = "green",
                                ssp245 = "blue",
                                ssp585 = "red")) +
  labs(
    title = "Top 5% richness area (2030 removed)",
    x = "Year",
    y = "Top 5% area (km²)",
    color = "Scenario"
  ) +
  theme_bw()

print(p)


#----------------------------------------------------------------------------------

#extract and calculate Insitu + exsitu refugia (2050-2100)-------------

# Calculate area first

# read all binary raster for area calculation and change detection analysis

# Root folder
root_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future"

# -------------------------------------------------
# 1. Find ALL folders named "outside_species_range"
# -------------------------------------------------
all_dirs <- list.dirs(root_dir, recursive = TRUE, full.names = TRUE)

outside_dirs <- all_dirs[basename(all_dirs) == "outside_species_range"]

cat("Found", length(outside_dirs), "outside_species_range folders\n")
print(outside_dirs)

if (length(outside_dirs) == 0) {
  stop("No 'outside_species_range' folders found inside: ", root_dir)
}

# -------------------------------------------------
# 2. Read all .tif files inside those folders
# -------------------------------------------------
tif_files <- unlist(lapply(outside_dirs, function(d) {
  list.files(d, pattern = "\\.tif$", full.names = TRUE)
}))

cat("Found", length(tif_files), "outside-range raster tif files\n")
print(basename(tif_files))

if (length(tif_files) == 0) {
  stop("No .tif files found inside outside_species_range folders.")
}

# -------------------------------------------------
# 3. Load rasters
# -------------------------------------------------
rasters_list <- lapply(tif_files, rast)

# Optional: name each raster by file
names(rasters_list) <- tools::file_path_sans_ext(basename(tif_files))

# -------------------------------------------------
# 4. Print raster info for verification
# -------------------------------------------------
for (i in seq_along(rasters_list)) {
  cat("\n--- Raster", i, "of", length(rasters_list), "---\n")
  cat("File:", tif_files[i], "\n")
  print(rasters_list[[i]])
}

cat("\nAll outside-range rasters successfully loaded.\n")




# Now, calculate area
# Inputs
# -----------------------------
root_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future"

# Output CSV
out_csv <- file.path(root_dir, "outside_species_range_suitable_area_km2.csv")

# -----------------------------
# Helper: parse species/year/scenario from filename
#   bonobos_binary_OUTSIDE_range_2050_ssp245.tif
# -----------------------------
parse_meta <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  m  <- regexec("^(.+?)_binary_OUTSIDE_range_(\\d{4})_(ssp\\d{3})$", nm)
  hit <- regmatches(nm, m)[[1]]
  if (length(hit) == 0) return(list(species=NA, year=NA, scenario=NA))
  list(species = hit[2], year = as.integer(hit[3]), scenario = hit[4])
}

# -----------------------------
# 1) Find all outside_species_range folders
# -----------------------------
all_dirs <- list.dirs(root_dir, recursive = TRUE, full.names = TRUE)
outside_dirs <- all_dirs[basename(all_dirs) == "outside_species_range"]
if (length(outside_dirs) == 0) stop("No 'outside_species_range' folders found in: ", root_dir)

# -----------------------------
# 2) List all outside-range tif files
# -----------------------------
tif_files <- unlist(lapply(outside_dirs, function(d) {
  list.files(d, pattern = "\\.tif$", full.names = TRUE)
}))
if (length(tif_files) == 0) stop("No .tif files found inside outside_species_range folders.")

# -----------------------------
# 3) Compute suitable area (sum of 1s * cell area)
# -----------------------------
res_list <- vector("list", length(tif_files))

for (i in seq_along(tif_files)) {
  f <- tif_files[i]
  meta <- parse_meta(f)
  
  r <- rast(f)
  
  # cell area in km2 (your rasters are 1000m x 1000m, so this should be 1)
  cell_km2 <- prod(res(r)) / 1e6
  
  # count 1s (NA-safe)
  # If your rasters have NA outside mask, keep na.rm=TRUE
  n1 <- global(r == 1, "sum", na.rm = TRUE)[1, 1]
  if (is.na(n1)) n1 <- 0
  
  suitable_km2 <- as.numeric(n1) * cell_km2
  
  res_list[[i]] <- data.frame(
    species = meta$species,
    year = meta$year,
    scenario = meta$scenario,
    cell_km2 = cell_km2,
    n_suitable_cells = as.numeric(n1),
    suitable_area_km2 = suitable_km2,
    file = f,
    stringsAsFactors = FALSE
  )
  
  cat("Done:", basename(f), "->", suitable_km2, "km2\n")
}

df_outside_area <- do.call(rbind, res_list)

# Clean ordering
df_outside_area <- df_outside_area[order(df_outside_area$species, df_outside_area$year, df_outside_area$scenario), ]

# -----------------------------
# 4) Print + save
# -----------------------------
print(df_outside_area)

write.csv(df_outside_area, out_csv, row.names = FALSE)
cat("\nSaved CSV:", out_csv, "\n")


# calculate by adding percent values

root_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future"
out_csv  <- file.path(root_dir, "outside_species_range_suitable_area_km2.csv")

# -------------------------------------------------
# 1. Locate all outside_species_range folders
# -------------------------------------------------
all_dirs <- list.dirs(root_dir, recursive = TRUE, full.names = TRUE)
outside_dirs <- all_dirs[basename(all_dirs) == "outside_species_range"]

tif_files <- unlist(lapply(outside_dirs, function(d) {
  list.files(d, pattern="\\.tif$", full.names=TRUE)
}))

# -------------------------------------------------
# 2. Extract metadata from filename
# -------------------------------------------------
parse_meta <- function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  m  <- regexec("^(.+?)_binary_OUTSIDE_range_(\\d{4})_(ssp\\d{3})$", nm)
  hit <- regmatches(nm, m)[[1]]
  list(
    species = hit[2],
    year    = as.integer(hit[3]),
    scenario= hit[4]
  )
}

# -------------------------------------------------
# 3. FIRST PASS → compute suitable area + outside area
# -------------------------------------------------
res_list <- vector("list", length(tif_files))

for (i in seq_along(tif_files)) {
  
  f <- tif_files[i]
  meta <- parse_meta(f)
  r <- rast(f)
  
  cell_km2 <- prod(res(r)) / 1e6
  
  # outside area = all non-NA cells
  n_outside <- global(!is.na(r), "sum", na.rm=FALSE)[1,1]
  outside_km2 <- as.numeric(n_outside) * cell_km2
  
  # suitable area = count of 1s
  n_suitable <- global(r == 1, "sum", na.rm=TRUE)[1,1]
  suitable_km2 <- as.numeric(n_suitable) * cell_km2
  
  res_list[[i]] <- data.frame(
    species = meta$species,
    year = meta$year,
    scenario = meta$scenario,
    cell_km2 = cell_km2,
    outside_range_km2 = outside_km2,
    suitable_km2 = suitable_km2,
    stringsAsFactors = FALSE
  )
}

df_outside <- do.call(rbind, res_list)

# -------------------------------------------------
# 4. Get BASELINE outside area per species (2030 ssp126)
# -------------------------------------------------
baseline <- subset(df_outside, year == 2030 & scenario == "ssp126",
                   select = c("species", "outside_range_km2"))
names(baseline)[2] <- "baseline_outside_2030ssp126_km2"

# join baseline to full table
df_outside <- merge(df_outside, baseline, by="species", all.x=TRUE)

# -------------------------------------------------
# 5. Compute % relative to baseline outside area
# -------------------------------------------------
df_outside$pct_suitable_vs_2030ssp126 <- with(df_outside,
                                              ifelse(baseline_outside_2030ssp126_km2 > 0,
                                                     suitable_km2 / baseline_outside_2030ssp126_km2 * 100,
                                                     NA)
)

# order nicely
df_outside <- df_outside[order(df_outside$species, df_outside$year, df_outside$scenario), ]

# -------------------------------------------------
# 6. Print + Save
# -------------------------------------------------
print(df_outside)

write.csv(df_outside, out_csv, row.names = FALSE)
cat("\nSaved CSV:", out_csv, "\n")


#------------------------------------------------

# Calculate Insitu + Exsitu climate refugia
# =============================
future_root <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future"

library(terra)

# Root folder
root_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future"

# List ALL tif files recursively
all_tifs <- list.files(
  path = root_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

# Exclude unwanted subfolders
exclude_patterns <- c("clipped_to_range", "outside_species_range")

tif_files <- all_tifs[!grepl(
  paste(exclude_patterns, collapse = "|"),
  all_tifs,
  ignore.case = TRUE
)]

cat("Total tif files found:", length(tif_files), "\n")

# Read rasters into a list (safer than stacking if extents/resolutions differ)
rasters_list <- lapply(tif_files, function(f) {
  r <- rast(f)
  names(r) <- tools::file_path_sans_ext(basename(f))
  return(r)
})

# Optional: check
rasters_list


# extract and claculate now

# -----------------------------
# Inputs you already have
# -----------------------------
# tif_files      # character vector of file paths
# rasters_list   # list of SpatRaster (already read)

# Root output folder (change if you want)
out_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/insitu_plus_exsitu_richness_outputs"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1) Read rasters into a list (safe if extents differ)
# ------------------------------------------------------------
rasters_list <- lapply(tif_files, function(f) {
  r <- rast(f)
  names(r) <- tools::file_path_sans_ext(basename(f))
  r
})

# Plot the first raster to verify
plot(rasters_list[[1]], main = names(rasters_list[[1]]))

# ------------------------------------------------------------
# 1) Read rasters into a list
# ------------------------------------------------------------
rasters_list <- lapply(tif_files, function(f) {
  r <- rast(f)
  names(r) <- tools::file_path_sans_ext(basename(f))
  r
})

# Plot the first raster to verify
plot(rasters_list[[1]], main = names(rasters_list[[1]]))

# ------------------------------------------------------------
# 2) Parse group (e.g., 2030s_ssp126) from filename
# ------------------------------------------------------------
get_group <- function(x) {
  b <- basename(x)
  m <- regmatches(b, regexpr("(2030s|2050s|2070s|2100s)_ssp(126|245|585)", b))
  if (length(m) == 0 || m == "") NA_character_ else m
}

groups <- vapply(tif_files, get_group, character(1))

if (any(is.na(groups))) {
  bad <- unique(basename(tif_files[is.na(groups)]))
  stop("Could not parse group from some filenames:\n", paste(bad, collapse = "\n"))
}

print(table(groups))

# ------------------------------------------------------------
# 3) Area-by-richness WITHOUT freq() (terra-version proof)
# ------------------------------------------------------------
area_table_from_richness <- function(rich_rast, include_zero = FALSE) {
  
  # Unique richness values present (excluding NA)
  vals <- unique(values(rich_rast))
  vals <- vals[!is.na(vals)]
  vals <- sort(vals)
  
  if (!include_zero) vals <- vals[vals >= 1]
  
  # cell area
  cell_area_m2 <- prod(res(rich_rast))
  
  # For each richness value, count cells robustly
  out <- lapply(vals, function(v) {
    # mask of cells equal to v
    m <- rich_rast == v
    
    # count cells (sum of TRUEs); global() exists in all terra versions
    cnt <- as.numeric(global(m, "sum", na.rm = TRUE)[1,1])
    
    data.frame(
      value = v,
      count = cnt,
      area_m2  = cnt * cell_area_m2,
      area_km2 = (cnt * cell_area_m2) / 1e6,
      area_ha  = (cnt * cell_area_m2) / 1e4
    )
  })
  
  do.call(rbind, out)
}

# ------------------------------------------------------------
# 4) Compute richness per (year × scenario), save raster + CSV
# ------------------------------------------------------------
unique_groups <- sort(unique(groups))
all_area_tables <- list()

for (g in unique_groups) {
  message("Processing group: ", g)
  
  idx <- which(groups == g)
  rlist_g <- rasters_list[idx]
  
  # Stack and sum across species (binary 0/1 layers)
  rstack <- rast(rlist_g)
  richness <- app(rstack, fun = sum, na.rm = TRUE)
  names(richness) <- paste0("richness_", g)
  
  # Save richness raster
  out_tif <- file.path(out_dir, paste0("richness_", g, ".tif"))
  writeRaster(richness, out_tif, overwrite = TRUE)
  
  # Plot quick check
  plot(richness, main = paste0("Richness: ", g))
  
  # Area table
  area_tab <- area_table_from_richness(richness, include_zero = FALSE)
  area_tab$group <- g
  area_tab <- area_tab[, c("group", "value", "count", "area_m2", "area_km2", "area_ha")]
  
  print(area_tab)
  
  # Save per-group CSV
  out_csv <- file.path(out_dir, paste0("area_by_richness_", g, ".csv"))
  write.csv(area_tab, out_csv, row.names = FALSE)
  
  all_area_tables[[g]] <- area_tab
  
  rm(rstack, richness)
  gc()
}

# ------------------------------------------------------------
# 5) Save combined CSV
# ------------------------------------------------------------
area_all <- do.call(rbind, all_area_tables)
out_csv_all <- file.path(out_dir, "area_by_richness_ALL_GROUPS.csv")
write.csv(area_all, out_csv_all, row.names = FALSE)

message("DONE. Outputs saved to: ", out_dir)





# Now, get top 5% richness sites

library(terra)

# Folder where richness rasters were saved in the previous step
rich_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/insitu_plus_exsitu_richness_outputs"

# Output folder for top 5% hotspots
hot_dir <- file.path(rich_dir, "top5pct_hotspots")
dir.create(hot_dir, recursive = TRUE, showWarnings = FALSE)

# List richness rasters
rich_files <- list.files(rich_dir, pattern = "^richness_.*\\.tif$", full.names = TRUE)

# Helper: compute top p% hotspot raster (1/0) using cell-count cutoff
top_pct_hotspot <- function(r, p = 0.05) {
  v <- values(r)
  v <- v[!is.na(v)]
  if (length(v) == 0) stop("Raster has no non-NA values.")
  
  # number of cells to keep
  n_keep <- ceiling(length(v) * p)
  
  # cutoff is the n_keep-th largest value
  v_sorted <- sort(v, decreasing = TRUE)
  cutoff <- v_sorted[n_keep]
  
  # hotspot mask (ties at cutoff included)
  hs <- r >= cutoff
  hs <- classify(hs, rcl = matrix(c(0,0, 1,1), ncol=2, byrow=TRUE), include.lowest=TRUE)
  # ensure numeric 0/1
  hs <- hs * 1
  names(hs) <- paste0(names(r), "_top", p*100, "pct")
  
  list(hotspot = hs, cutoff = cutoff, n_keep = n_keep, n_total = length(v))
}

# Helper: area of hotspot (cells==1)
hotspot_area <- function(hs_rast) {
  cell_area_m2 <- prod(res(hs_rast))
  n_hot <- as.numeric(global(hs_rast == 1, "sum", na.rm = TRUE)[1,1])
  
  data.frame(
    hotspot_cells = n_hot,
    area_m2  = n_hot * cell_area_m2,
    area_km2 = (n_hot * cell_area_m2) / 1e6,
    area_ha  = (n_hot * cell_area_m2) / 1e4
  )
}

# Run for all richness rasters
hotspot_summary <- list()

for (f in rich_files) {
  r <- rast(f)
  g <- sub("^richness_", "", tools::file_path_sans_ext(basename(f)))  # group label
  
  message("Top 5% hotspots for: ", g)
  
  hs_info <- top_pct_hotspot(r, p = 0.05)
  hs <- hs_info$hotspot
  
  # Save hotspot tif
  out_tif <- file.path(hot_dir, paste0("top5pct_hotspots_", g, ".tif"))
  writeRaster(hs, out_tif, overwrite = TRUE)
  
  # Area
  a <- hotspot_area(hs)
  a$group <- g
  a$cutoff_value <- hs_info$cutoff
  a$total_nonNA_cells <- hs_info$n_total
  a$target_top5pct_cells <- hs_info$n_keep
  
  hotspot_summary[[g]] <- a
  
  # Optional quick plot
  plot(hs, main = paste0("Top 5% hotspots: ", g))
  
  rm(r, hs)
  gc()
}

# Save per-group + combined CSV
hot_df <- do.call(rbind, hotspot_summary)
hot_df <- hot_df[, c("group", "cutoff_value", "total_nonNA_cells", "target_top5pct_cells",
                     "hotspot_cells", "area_m2", "area_km2", "area_ha")]

print(hot_df)

write.csv(hot_df, file.path(hot_dir, "top5pct_hotspot_area_ALL_GROUPS.csv"), row.names = FALSE)

message("DONE. Hotspot rasters + area table saved to: ", hot_dir)



#--------------------------------------------------------------------------------------

# Calculate suitable, richness, and refugia area within landscapes

library(terra)
library(sf)
library(dplyr)

# -----------------------------
# INPUTS
# -----------------------------
landscapes <- st_read("F:/WWF_data/climate_species_analysis/defined_study_area/landscapes.shp")

root_current <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/current/clipped_to_range/binary_merged_to_empty"
root_future  <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future"

out_csv <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/suitable_area_by_landscape.csv"

# -----------------------------
# Convert landscapes → terra
# -----------------------------
land_v <- vect(landscapes)
land_names <- landscapes$NAME

# -----------------------------
# Helper to compute suitable area by landscape
# -----------------------------
calc_landscape_area <- function(r, species, year, scenario, total_current_suitable) {
  
  # ensure same CRS
  if (!same.crs(r, land_v)) {
    land_proj <- project(land_v, crs(r))
  } else {
    land_proj <- land_v
  }
  
  cell_km2 <- prod(res(r)) / 1e6
  
  # Extract per landscape
  e <- extract(r, land_proj)
  
  # Count suitable cells (==1) per polygon
  suitable_cells <- tapply(e[,2] == 1, e[,1], sum, na.rm=TRUE)
  
  suitable_km2 <- suitable_cells * cell_km2
  
  df <- data.frame(
    species = species,
    landscape = land_names[as.integer(names(suitable_km2))],
    year = year,
    scenario = scenario,
    suitable_km2 = suitable_km2,
    pct_of_current_total = suitable_km2 / total_current_suitable * 100
  )
  
  return(df)
}

# -----------------------------
# 1. CURRENT — compute TOTAL suitable per species
# -----------------------------
current_files <- list.files(root_current, pattern="\\.tif$", full.names=TRUE)

current_totals <- list()
current_results <- list()

for (f in current_files) {
  
  r <- rast(f)
  
  species <- sub("_binary_on_empty.*", "", basename(f))
  
  cell_km2 <- prod(res(r)) / 1e6
  total_cells <- global(r == 1, "sum", na.rm=TRUE)[1,1]
  total_km2 <- as.numeric(total_cells) * cell_km2
  
  current_totals[[species]] <- total_km2
  
  # Landscape stats
  df <- calc_landscape_area(r, species, 2020, "current", total_km2)
  current_results[[species]] <- df
}

df_current <- do.call(rbind, current_results)
df_current

# -----------------------------
# 2. FUTURE — compute landscape suitable
# -----------------------------
future_dirs <- list.dirs(root_future, recursive=TRUE, full.names=TRUE)
future_dirs <- future_dirs[grepl("binary_merged_to_empty$", future_dirs)]

future_results <- list()

for (d in future_dirs) {
  
  tif_files <- list.files(d, pattern="\\.tif$", full.names=TRUE)
  
  for (f in tif_files) {
    
    r <- rast(f)
    
    # Parse metadata
    nm <- tools::file_path_sans_ext(basename(f))
    m  <- regexec("^(.+?)_binary_on_empty_(\\d{4})s_(ssp\\d{3})$", nm)
    hit <- regmatches(nm, m)[[1]]
    
    species <- hit[2]
    year    <- as.integer(hit[3])
    scenario<- hit[4]
    
    total_current <- current_totals[[species]]
    
    df <- calc_landscape_area(r, species, year, scenario, total_current)
    
    future_results[[length(future_results)+1]] <- df
  }
}

df_future <- do.call(rbind, future_results)

# -----------------------------
# Combine + Save
# -----------------------------
df_all <- rbind(df_current, df_future)
df_all <- df_all[order(df_all$species, df_all$year, df_all$scenario, df_all$landscape), ]

print(df_all)

write.csv(df_all, out_csv, row.names=FALSE)
cat("\nSaved:", out_csv, "\n")



library(dplyr)
library(ggplot2)

# ---- choose ONE species here ----
sp_pick <- "cyclotis"  # change to "gorilla", "paniscus", "troglodytes", etc.

df_plot <- df_all %>%
  filter(species == sp_pick, year != 2030) %>%
  mutate(
    pct_1dp = round(pct_of_current_total, 1),
    scenario = factor(scenario, levels = c("current","ssp126","ssp245","ssp585")),
    year = as.integer(year),
    landscape_short = recode(landscape,
                             "Mount Cameroon-Korup-Bakossi" = "MtCam-Korup",
                             "Campo-Ma'an" = "Campo",
                             "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
                             "Bas-Oogué" = "Bas-Ogo",
                             "Gamba-Mayumba-Conkouati" = "Gamba",
                             "Lac Télé-Lac Tumba" = "LacTele",
                             "Shanga Trii-National" = "TNS",
                             .default = landscape
    )
  )

ggplot(df_plot, aes(x = year, y = pct_1dp, color = scenario, group = scenario)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ landscape_short, scales = "free_y") +
  scale_x_continuous(breaks = c(2020, 2050, 2070, 2100)) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  labs(
    title = paste0(sp_pick, " — % of current suitable habitat by landscape"),
    x = "Year",
    y = "% of current suitable habitat",
    color = "Scenario"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")




# plot suitability for each species within landscapes

library(dplyr)
library(ggplot2)
library(tibble)

# ---- choose ONE species here ----
sp_pick <- "paniscus"

# 1) Prep the base data (remove 2030)
base <- df_all %>%
  filter(species == sp_pick, year != 2030,
         landscape != "Mount Cameroon-Korup-Bakossi") %>%
  mutate(
    scenario = factor(scenario, levels = c("ssp126","ssp245","ssp585","current")),
    year = as.integer(year),
    #pct_1dp = round(pct_of_current_total, 1),
    pct_1dp = round(suitable_km2, 1),
    landscape_short = recode(landscape,
                             "Campo-Ma'an" = "Campo",
                             "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
                             "Bas-Oogué" = "Bas-Ogoue",
                             "Gamba-Mayumba-Conkouati" = "Gamba",
                             "Lac Télé-Lac Tumba" = "LacTele",
                             "Shanga Trii-National" = "TNS",
                             .default = landscape
    )
  )

# 2) Replicate the 2020 current point into each SSP so future lines connect to 2020
cur <- base %>%
  filter(year == 2020, scenario == "current") %>%
  select(species, landscape, landscape_short, year, pct_1dp) %>%
  distinct()

fut_scen <- tibble(
  scenario = factor(c("ssp126","ssp245","ssp585"),
                    levels = c("ssp126","ssp245","ssp585","current"))
)

cur_rep <- merge(cur, fut_scen, by = NULL) %>%  # cartesian join (safe + simple)
  select(species, landscape, landscape_short, year, scenario, pct_1dp)

df_plot <- bind_rows(
  base %>% select(species, landscape, landscape_short, year, scenario, pct_1dp),
  cur_rep
)

# ---- plot ----
ggplot(df_plot, aes(x = year, y = pct_1dp, color = scenario, group = scenario)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ landscape_short, scales = "free_y") +
  scale_x_continuous(breaks = c(2020, 2050, 2070, 2100)) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_color_manual(
    values = c("ssp126" = "green", "ssp245" = "blue", "ssp585" = "red", "current" = "black"),
    breaks = c("ssp126","ssp245","ssp585","current")
  ) +
  labs(
    title = paste0("Suitable Habitat Area per Conservation Landscape for Bonobos"),
    x = "Year", y = "Suitable Habitat Area (km2)", color = "Scenario"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")





# calculate richness within landscapes 

rich_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/richness_outputs"

all_files <- list.files(
  path = rich_dir,
  recursive = TRUE,
  full.names = TRUE
)

cat("Total files found:", length(all_files), "\n\n")
print(all_files)

tif_files <- list.files(
  path = rich_dir,
  pattern = "\\.tif$",
  recursive = TRUE,
  full.names = TRUE
)

cat("Total tif files found:", length(tif_files), "\n\n")
print(tif_files)

top5_dir <- file.path(rich_dir, "top5pct_richness")

top5_files <- list.files(
  path = top5_dir,
  recursive = TRUE,
  full.names = TRUE
)

cat("Top 5% folder files:", length(top5_files), "\n\n")
print(top5_files)


# Do calculations
# -----------------------------
rich_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/richness_outputs"
landscapes <- st_read("F:/WWF_data/climate_species_analysis/defined_study_area/landscapes.shp", quiet = TRUE)
land_v <- vect(landscapes)

# Only richness_pos rasters
rich_files <- list.files(
  rich_dir,
  pattern = "^richness_pos_.*\\.tif$",
  full.names = TRUE
)

# Helper to parse label
parse_label <- function(x){
  nm <- tools::file_path_sans_ext(basename(x))
  nm <- sub("richness_pos_", "", nm)
  parts <- strsplit(nm, "_")[[1]]
  data.frame(year=as.integer(parts[1]), scenario=parts[2])
}

results <- list()

for(i in seq_along(rich_files)){
  
  r <- rast(rich_files[i])
  
  if(!same.crs(r, land_v)){
    land_v2 <- project(land_v, crs(r))
  } else {
    land_v2 <- land_v
  }
  
  info <- parse_label(rich_files[i])
  
  for(j in 1:nrow(land_v2)){
    
    land_j <- land_v2[j]
    land_name <- landscapes$NAME[j]
    
    r_crop <- crop(r, land_j)
    r_mask <- mask(r_crop, land_j)
    
    vals <- values(r_mask, na.rm=TRUE)
    vals <- vals[vals > 0]
    
    if(length(vals)==0) next
    
    tab <- as.data.frame(table(vals))
    colnames(tab) <- c("richness_value","n_cells")
    
    cell_km2 <- prod(res(r_mask))/1e6
    tab$area_km2 <- tab$n_cells * cell_km2
    
    tab$year <- info$year
    tab$scenario <- info$scenario
    tab$landscape <- land_name
    
    results[[length(results)+1]] <- tab
  }
}

df_rich_landscape <- bind_rows(results)
df_rich_landscape

# Save CSV
write.csv(
  df_rich_landscape,
  file.path(rich_dir, "richness_area_by_landscape_and_value.csv"),
  row.names = FALSE
)

cat("Saved: richness_area_by_landscape_and_value.csv\n")



# repeat for top 5% richness

top5_dir <- file.path(rich_dir, "top5pct_richness")

top5_files <- list.files(
  top5_dir,
  pattern = "\\.tif$",
  full.names = TRUE
)

parse_top5 <- function(x){
  nm <- tools::file_path_sans_ext(basename(x))
  nm <- sub("top5pct_", "", nm)
  parts <- strsplit(nm, "_")[[1]]
  data.frame(year=as.integer(parts[1]), scenario=parts[2])
}

top5_results <- list()

for(i in seq_along(top5_files)){
  
  r <- rast(top5_files[i])
  
  if(!same.crs(r, land_v)){
    land_v2 <- project(land_v, crs(r))
  } else {
    land_v2 <- land_v
  }
  
  info <- parse_top5(top5_files[i])
  
  for(j in 1:nrow(land_v2)){
    
    land_j <- land_v2[j]
    land_name <- landscapes$NAME[j]
    
    r_crop <- crop(r, land_j)
    r_mask <- mask(r_crop, land_j)
    
    n_cells <- global(r_mask == 1, "sum", na.rm=TRUE)[1,1]
    if(is.na(n_cells) || n_cells == 0) next
    
    cell_km2 <- prod(res(r_mask))/1e6
    area_km2 <- n_cells * cell_km2
    
    top5_results[[length(top5_results)+1]] <- data.frame(
      year = info$year,
      scenario = info$scenario,
      landscape = land_name,
      area_km2 = area_km2
    )
  }
}

df_top5_land <- bind_rows(top5_results)
df_top5_land

write.csv(
  df_top5_land,
  file.path(rich_dir, "top5pct_richness_area_by_landscape.csv"),
  row.names = FALSE
)

cat("Saved: top5pct_richness_area_by_landscape.csv\n")


# plot top 5%

library(dplyr)
library(ggplot2)

# 1) filter to desired years only
df_plot <- df_top5_land %>%
  filter(year %in% c(2020, 2050, 2070, 2100)) %>%
  mutate(
    year = as.integer(year),
    scenario = factor(scenario, levels = c("ssp126","ssp245","ssp585","current")),
    landscape_short = recode(landscape,
                             "Campo-Ma'an" = "Campo",
                             "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
                             "Gamba-Mayumba-Conkouati" = "Gamba",
                             "Lac Télé-Lac Tumba" = "LacTele",
                             "Shanga Trii-National" = "TNS",
                             .default = landscape
    )
  )

# 2) replicate the 2020 current point into each future scenario so lines connect
cur <- df_plot %>%
  filter(year == 2020, scenario == "current") %>%
  select(year, landscape_short, area_km2) %>%
  distinct()

cur_rep <- cur %>%
  tidyr::crossing(
    scenario = factor(c("ssp126","ssp245","ssp585"),
                      levels = c("ssp126","ssp245","ssp585","current"))
  )

df_plot2 <- bind_rows(
  df_plot %>% select(year, scenario, landscape_short, area_km2),
  cur_rep
)

# 3) plot
ggplot(df_plot2, aes(x = year, y = area_km2, color = scenario, group = scenario)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ landscape_short, scales = "free_y") +
  scale_x_continuous(breaks = c(2020, 2050, 2070, 2100)) +
  scale_color_manual(values = c(
    "ssp126" = "green",
    "ssp245" = "blue",
    "ssp585" = "red",
    "current" = "black"
  )) +
  labs(
    title = "Top 5% Richness Area by Landscape",
    x = "Year",
    y = "Richness Area (km²)",
    color = "Scenario"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")


# ---------------------------------------------------

# Now, calculate Insitu refugia for each species

ref_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/refugia_rasters_stable"

all_files <- list.files(
  path = ref_dir,
  recursive = TRUE,
  full.names = TRUE
)

cat("Total files found:", length(all_files), "\n\n")
print(all_files)

tif_files <- list.files(
  path = ref_dir,
  pattern = "\\.tif$",
  recursive = TRUE,
  full.names = TRUE
)

cat("Total tif files found:", length(tif_files), "\n\n")
print(tif_files)


# calculate refugia

library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# -----------------------------
# Inputs
# -----------------------------
ref_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/refugia_rasters_stable"
tif_files <- list.files(ref_dir, pattern = "\\.tif$", full.names = TRUE)

landscapes <- st_read("F:/WWF_data/climate_species_analysis/defined_study_area/landscapes.shp")

# -----------------------------
# Prep landscapes (drop Mount Cameroon-Korup-Bakossi)
# -----------------------------
land_sf <- landscapes %>%
  rename(landscape = NAME) %>%
  filter(landscape != "Mount Cameroon-Korup-Bakossi") %>%
  mutate(
    landscape_short = recode(landscape,
                             "Campo-Ma'an" = "Campo",
                             "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
                             "Bas-Oogué" = "Bas-Ogo",
                             "Gamba-Mayumba-Conkouati" = "Gamba",
                             "Lac Télé-Lac Tumba" = "LacTele",
                             "Shanga Trii-National" = "TNS",
                             .default = landscape
    )
  )

land_v <- vect(land_sf)

# -----------------------------
# Helper: parse filename metadata
# e.g. cyclotis_refugia_stable_2020_to_2050_ssp126.tif
# -----------------------------
parse_meta <- function(f) {
  nm <- basename(f)
  m <- str_match(nm, "^(.+?)_refugia_stable_2020_to_(\\d{4})_(ssp\\d{3})\\.tif$")
  if (any(is.na(m))) return(NULL)
  tibble(
    file = f,
    species = m[,2],
    baseline_year = 2020L,
    future_year = as.integer(m[,3]),
    scenario = m[,4]
  )
}

meta <- bind_rows(lapply(tif_files, parse_meta))
if (nrow(meta) == 0) stop("No files matched expected pattern *_refugia_stable_2020_to_YYYY_sspNNN.tif")

# -----------------------------
# Compute area by landscape for each raster
# -----------------------------
calc_one <- function(file, species, baseline_year, future_year, scenario) {
  r <- rast(file)
  
  # cell area (km2) from raster resolution (m)
  resm <- res(r)
  cell_km2 <- (resm[1] * resm[2]) / 1e6
  
  # ensure same CRS
  # ensure same CRS (SpatVector -> raster CRS)
  land_use <- land_v
  if (!terra::same.crs(r, land_use)) {
    land_use <- terra::project(land_use, terra::crs(r))
  }
  
  # extract sum of values (1s) per polygon
  ex <- terra::extract(r, land_use, fun = sum, na.rm = TRUE, touches = TRUE)
  # ex has: ID + layer column
  val_col <- names(ex)[2]
  
  out <- tibble(
    species = species,
    baseline_year = baseline_year,
    future_year = future_year,
    scenario = scenario,
    landscape = land_sf$landscape,
    landscape_short = land_sf$landscape_short,
    n_cells_1 = as.numeric(ex[[val_col]]),
    cell_km2 = cell_km2,
    refugia_km2 = n_cells_1 * cell_km2
  )
  
  out
}

df_refugia_land <- purrr::pmap_dfr(
  meta,
  ~calc_one(..1, ..2, ..3, ..4, ..5)
)

df_refugia_land

# Save CSV
out_csv <- file.path(ref_dir, "refugia_area_by_landscape.csv")
write.csv(df_refugia_land, out_csv, row.names = FALSE)

print(df_refugia_land)

# -----------------------------
# Plot: one species (facet by landscape)
# Connect scenario lines to 2020 point (replicate 2020 across scenarios)
# -----------------------------
sp_pick <- "gorilla"   # change species as needed

df_plot <- df_refugia_land %>%
  filter(
    species == sp_pick,
    future_year %in% c(2050, 2070, 2100)
  ) %>%
  mutate(
    year = future_year,
    scenario = factor(scenario, levels = c("ssp126","ssp245","ssp585"))
  )

ggplot(df_plot,
       aes(x = year, y = refugia_km2,
           color = scenario, group = scenario)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ landscape_short, scales = "free_y") +
  scale_x_continuous(breaks = c(2050, 2070, 2100)) +
  scale_color_manual(values = c(
    "ssp126" = "green",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    title = paste0("In Situ Refugia Area per Landscape for Gorillas"),
    x = "Year",
    y = "Refugia Area (km²)",
    color = "Scenario"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )





#------------repeat calculations for exsitu Refugia

# Root folder
root_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future"

# 1) Find ALL folders named "outside_species_range"
all_dirs <- list.dirs(root_dir, recursive = TRUE, full.names = TRUE)
outside_dirs <- all_dirs[basename(all_dirs) == "outside_species_range"]

cat("Found", length(outside_dirs), "outside_species_range folders\n\n")
print(outside_dirs)

if (length(outside_dirs) == 0) {
  stop("No 'outside_species_range' folders found inside: ", root_dir)
}

# 2) List ALL .tif files inside those folders (and print to verify)
tif_files <- unlist(lapply(outside_dirs, function(d) {
  list.files(d, pattern = "\\.tif$", full.names = TRUE)
}))

cat("\nFound", length(tif_files), "raster tif files in outside_species_range folders\n\n")
print(tif_files)

# Optional: print just filenames
cat("\nFilenames only:\n")
print(basename(tif_files))



# calculate and plot
# -----------------------------
# Inputs
# -----------------------------
root_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future"
landscape_shp <- "F:/WWF_data/climate_species_analysis/defined_study_area/landscapes.shp"
out_csv <- file.path(root_dir, "outside_range_suitable_area_by_landscape.csv")

# -----------------------------
# 1) Find outside_species_range rasters
# -----------------------------
all_dirs <- list.dirs(root_dir, recursive = TRUE, full.names = TRUE)
outside_dirs <- all_dirs[basename(all_dirs) == "outside_species_range"]
stopifnot(length(outside_dirs) > 0)

tif_files <- unlist(lapply(outside_dirs, function(d) {
  list.files(d, pattern = "\\.tif$", full.names = TRUE)
}))
stopifnot(length(tif_files) > 0)

# -----------------------------
# 2) Read landscapes and prep vectors (terra)
# -----------------------------
land_sf <- st_read(landscape_shp, quiet = TRUE)

# keep NAME only + geometry
land_sf <- land_sf %>% select(NAME, geometry)

# short names (edit as you like)
land_sf <- land_sf %>%
  mutate(
    landscape_short = recode(
      NAME,
      "Mount Cameroon-Korup-Bakossi" = "MtCam-Korup",
      "Campo-Ma'an" = "Campo",
      "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
      "Bas-Oogué" = "Bas-Ogo",
      "Gamba-Mayumba-Conkouati" = "Gamba",
      "Lac Télé-Lac Tumba" = "LacTele",
      "Shanga Trii-National" = "TNS",
      .default = NAME
    )
  )

# Convert to terra vect
land_v <- vect(land_sf)

# -----------------------------
# 3) Helpers
# -----------------------------
# parse species/year/scenario from filename like:
# "bonobos_binary_OUTSIDE_range_2050_ssp245.tif"
parse_meta <- function(fp) {
  nm <- basename(fp)
  nm <- str_remove(nm, "\\.tif$")
  m <- str_match(nm, "^(.+?)_binary_OUTSIDE_range_(\\d{4})_(ssp\\d+)$")
  if (any(is.na(m))) {
    stop("Filename does not match expected pattern: ", nm)
  }
  list(
    species = m[,2],
    year = as.integer(m[,3]),
    scenario = m[,4]
  )
}

# sum of 1's area (km2) inside a polygon
sum_ones_area_km2 <- function(r, poly) {
  # mask/crop to polygon
  rr <- crop(r, poly, snap = "out")
  rr <- mask(rr, poly)
  
  # count 1's (ignore NAs)
  n1 <- global(rr == 1, "sum", na.rm = TRUE)[1,1]
  if (is.na(n1)) n1 <- 0
  
  # cell area in km2 (robust even if resolution changes)
  # expanse returns area in map units; for projected CRS in meters, unit="km"
  cell_km2 <- as.numeric(expanse(rr[[1]], unit = "km", transform = FALSE)[1]) /
    ncell(rr[[1]])  # average per cell (safe if regular grid)
  
  # In your case 1000m raster => cell_km2 ~ 1
  as.numeric(n1) * cell_km2
}

# -----------------------------
# 4) Loop over rasters -> landscape stats
# -----------------------------
results <- vector("list", length(tif_files))

for (i in seq_along(tif_files)) {
  fp <- tif_files[i]
  meta <- parse_meta(fp)
  
  r <- rast(fp)
  
  # make sure landscapes CRS matches raster CRS
  # Reproject landscapes only if CRS differs
  if (!same.crs(r, land_v)) {
    land_v_i <- project(land_v, crs(r))
  } else {
    land_v_i <- land_v
  }
  
  # total outside-range area for this raster (non-NA cells) in km2
  # (since outside-range rasters are already clipped to "outside range",
  #  this is your denominator for % outside-range suitability)
  tot_out_km2 <- as.numeric(global(!is.na(r), "sum", na.rm = TRUE)[1,1]) *
    (as.numeric(res(r)[1]) * as.numeric(res(r)[2]) / 1e6)
  
  # landscape-wise
  df_i <- lapply(seq_len(nrow(land_v_i)), function(j) {
    poly <- land_v_i[j]
    suitable_km2 <- sum_ones_area_km2(r, poly)
    
    tibble(
      species = meta$species,
      year = meta$year,
      scenario = meta$scenario,
      landscape = land_v_i$NAME[j],
      landscape_short = land_v_i$landscape_short[j],
      suitable_km2 = suitable_km2,
      outside_range_area_km2 = tot_out_km2,
      pct_of_outside_range = ifelse(tot_out_km2 > 0, suitable_km2 / tot_out_km2 * 100, 0)
    )
  }) %>% bind_rows()
  
  results[[i]] <- df_i
  rm(r); gc(FALSE)
}

df_outside_land <- bind_rows(results) %>%
  mutate(
    pct_of_outside_range_1dp = round(pct_of_outside_range, 1)
  )

df_outside_land
# -----------------------------
# 5) Save CSV
# -----------------------------
write.csv(df_outside_land, out_csv, row.names = FALSE)
cat("Saved:", out_csv, "\n")

# -----------------------------
# 6) Plot for one species (facet by landscape)
#    - connect future lines to 2030 baseline within each scenario
# -----------------------------

sp_pick <- "gorillas"   # change species when needed

# shorten landscape names
short_landscape <- function(x) {
  dplyr::recode(x,
                "Mount Cameroon-Korup-Bakossi" = "MtCam-Korup",
                "Campo-Ma'an" = "Campo",
                "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
                "Bas-Oogué" = "Bas-Ogo",
                "Gamba-Mayumba-Conkouati" = "Gamba",
                "Lac Télé-Lac Tumba" = "LacTele",
                "Shanga Trii-National" = "TNS",
                .default = x
  )
}

df_plot <- df_outside_land %>%
  filter(
    species == sp_pick,
    landscape != "Mount Cameroon-Korup-Bakossi",
    year %in% c(2050, 2070, 2100),
    scenario %in% c("ssp126","ssp245","ssp585")
  ) %>%
  mutate(
    landscape_short = short_landscape(landscape),
    year = as.integer(year),
    suitable_km2 = round(suitable_km2, 1),
    scenario = factor(scenario, levels = c("ssp126","ssp245","ssp585"))
  )

ggplot(df_plot,
       aes(x = year, y = suitable_km2,
           group = scenario, color = scenario)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ landscape_short, scales = "free_y") +
  scale_color_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  scale_x_continuous(breaks = c(2050, 2070, 2100)) +
  labs(
    x = "Year",
    y = "Refugia Area (km²)",
    color = "Scenario",
    title = paste0("Ex Situ Refugia Area per Conservation Landscape for Gorillas")
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10)
  )




#--------Now perform calculations for insitu + exsitu refugia richness----


rich_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/insitu_plus_exsitu_richness_outputs"

all_files <- list.files(
  path = rich_dir,
  recursive = TRUE,
  full.names = TRUE
)

cat("Total files found:", length(all_files), "\n\n")
print(all_files)

tif_files <- list.files(
  path = rich_dir,
  pattern = "\\.tif$",
  recursive = TRUE,
  full.names = TRUE
)

cat("Total tif files found:", length(tif_files), "\n\n")
print(tif_files)

top5_dir <- file.path(rich_dir, "top5pct_hotspots")

top5_files <- list.files(
  path = top5_dir,
  recursive = TRUE,
  full.names = TRUE
)

cat("Top 5% folder files:", length(top5_files), "\n\n")
print(top5_files)



# ------------Now, calculate----------
# Paths
# -----------------------------
rich_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/insitu_plus_exsitu_richness_outputs"
land_path <- "F:/WWF_data/climate_species_analysis/defined_study_area/landscapes.shp"

# -----------------------------
# Read landscapes (as SpatVector for terra)
# -----------------------------
land_sf <- st_read(land_path, quiet = TRUE)
land_sf$NAME <- as.character(land_sf$NAME)

# remove Mount Cameroon-Korup-Bakossi
land_sf <- land_sf %>% filter(NAME != "Mount Cameroon-Korup-Bakossi")
land_v  <- terra::vect(land_sf)

# Optional short names for plots/tables
short_landscape <- function(x) {
  dplyr::recode(x,
                "Campo-Ma'an" = "Campo",
                "Dja-Odzala-Minkébé Tri-National (Tridom)" = "TRIDOM",
                "Bas-Oogué" = "Bas-Ogoue",
                "Gamba-Mayumba-Conkouati" = "Gamba",
                "Lac Télé-Lac Tumba" = "LacTele",
                "Shanga Trii-National" = "TNS",
                .default = x
  )
}

# -----------------------------
# List richness rasters (2050/2070/2100 only)
# -----------------------------
rich_files <- list.files(
  path = rich_dir,
  pattern = "^richness_(2050s|2070s|2100s)_ssp(126|245|585)\\.tif$",
  full.names = TRUE
)

cat("Richness rasters found (2050/2070/2100):", length(rich_files), "\n")
print(basename(rich_files))

# -----------------------------
# Helper: extract meta from filename
# richness_2050s_ssp126.tif
# -----------------------------
parse_meta <- function(f) {
  b <- basename(f)
  m <- str_match(b, "^richness_(2050s|2070s|2100s)_ssp(126|245|585)\\.tif$")
  year <- as.integer(str_replace(m[2], "s", ""))  # "2050s" -> 2050
  scenario <- paste0("ssp", m[3])
  tibble(file = f, year = year, scenario = scenario)
}

meta_rich <- map_dfr(rich_files, parse_meta)

# -----------------------------
# Compute area by richness value per landscape
# -----------------------------
calc_rich_area_one <- function(file, year, scenario, land_v) {
  
  r <- rast(file)
  
  # Reproject landscapes only if CRS differs
  if (!same.crs(r, land_v)) {
    land_use <- project(land_v, crs(r))
  } else {
    land_use <- land_v
  }
  
  # Extract by polygon: returns a data.frame with ID + cell values
  ex <- terra::extract(r, land_use)
  
  # ex has columns: ID, layer (or richness name)
  val_col <- setdiff(names(ex), "ID")[1]
  
  out <- ex %>%
    as_tibble() %>%
    rename(richness_value = all_of(val_col)) %>%
    filter(!is.na(richness_value), richness_value > 0) %>%
    mutate(richness_value = as.integer(round(richness_value))) %>%
    count(ID, richness_value, name = "n_cells") %>%
    mutate(area_km2 = n_cells * (prod(res(r)) / 1e6)) %>%
    left_join(
      tibble(ID = 1:nrow(land_use),
             landscape = land_use$NAME),
      by = "ID"
    ) %>%
    mutate(
      landscape_short = short_landscape(landscape),
      year = year,
      scenario = scenario
    ) %>%
    select(year, scenario, landscape, landscape_short, richness_value, n_cells, area_km2)
  
  out
}

df_rich_land_val <- purrr::pmap_dfr(
  list(meta_rich$file, meta_rich$year, meta_rich$scenario),
  ~calc_rich_area_one(..1, ..2, ..3, land_v = land_v)
)

print(df_rich_land_val)

# Save
out_csv_rich <- file.path(rich_dir, "richness_area_by_landscape_value_2050_2070_2100.csv")
write.csv(df_rich_land_val, out_csv_rich, row.names = FALSE)
cat("Saved:", out_csv_rich, "\n")


# -----------------------------
# List top5% hotspot rasters (2050/2070/2100 only)
# top5pct_hotspots_2050s_ssp126.tif
# -----------------------------
top5_dir <- file.path(rich_dir, "top5pct_hotspots")

top5_files <- list.files(
  path = top5_dir,
  pattern = "^top5pct_hotspots_(2050s|2070s|2100s)_ssp(126|245|585)\\.tif$",
  full.names = TRUE
)

cat("Top5% rasters found (2050/2070/2100):", length(top5_files), "\n")
print(basename(top5_files))

parse_meta_top5 <- function(f) {
  b <- basename(f)
  m <- str_match(b, "^top5pct_hotspots_(2050s|2070s|2100s)_ssp(126|245|585)\\.tif$")
  year <- as.integer(str_replace(m[2], "s", ""))
  scenario <- paste0("ssp", m[3])
  tibble(file = f, year = year, scenario = scenario)
}

meta_top5 <- map_dfr(top5_files, parse_meta_top5)

calc_top5_area_one <- function(file, year, scenario, land_v) {
  r <- rast(file)
  
  # Ensure same CRS
  if (!same.crs(r, land_v)) {
    land_use <- project(land_v, crs(r))
  } else {
    land_use <- land_v
  }
  
  ex <- terra::extract(r, land_use)
  val_col <- setdiff(names(ex), "ID")[1]
  
  out <- ex %>%
    as_tibble() %>%
    rename(val = all_of(val_col)) %>%
    filter(!is.na(val), val == 1) %>%
    count(ID, name = "n_cells") %>%
    mutate(area_km2 = n_cells * (prod(res(r)) / 1e6)) %>%
    left_join(
      tibble(ID = 1:nrow(land_use),
             landscape = land_use$NAME),
      by = "ID"
    ) %>%
    mutate(
      landscape_short = short_landscape(landscape),
      year = year,
      scenario = factor(scenario, levels = c("ssp126","ssp245","ssp585"))
    ) %>%
    select(year, scenario, landscape, landscape_short, n_cells, area_km2)
  
  # ensure landscapes with zero get included
  all_ids <- tibble(ID = 1:nrow(land_use),
                    landscape = land_use$NAME,
                    landscape_short = short_landscape(land_use$NAME)) %>%
    mutate(year = year,
           scenario = factor(scenario, levels = c("ssp126","ssp245","ssp585")))
  
  out2 <- all_ids %>%
    left_join(out %>% select(year, scenario, landscape, landscape_short, n_cells, area_km2),
              by = c("year","scenario","landscape","landscape_short")) %>%
    mutate(
      n_cells = ifelse(is.na(n_cells), 0, n_cells),
      area_km2 = ifelse(is.na(area_km2), 0, area_km2)
    )
  
  out2
}

df_top5_land <- purrr::pmap_dfr(
  list(meta_top5$file, meta_top5$year, meta_top5$scenario),
  ~calc_top5_area_one(..1, ..2, ..3, land_v = land_v)
)

print(df_top5_land)

# Save
out_csv_top5 <- file.path(top5_dir, "top5pct_hotspot_area_by_landscape_2050_2070_2100.csv")
write.csv(df_top5_land, out_csv_top5, row.names = FALSE)
cat("Saved:", out_csv_top5, "\n")

# -----------------------------
# Plot: facet by landscape, start from 2050 (already filtered), remove MtCam already done
# -----------------------------
ggplot(df_top5_land,
       aes(x = year, y = area_km2, color = scenario, group = scenario)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ landscape_short, scales = "free_y") +
  scale_color_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  scale_x_continuous(breaks = c(2050, 2070, 2100)) +
  labs(
    x = "Year",
    y = "Top 5% Refugia Hotspot Area (km²)",
    color = "Scenario",
    title = "Top 5% In Situ + Ex Situ Refugia Richness Hotspots per Conservation Landscape"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")



#--------------------------------------------------------------------------------------

# Calculate suitable, richness, and refugia area within Protected Areas


library(sf)
library(terra)

# -----------------------------
# INPUTS
# -----------------------------
landscapes <- st_read("F:/WWF_data/SDMs_LC_climate/PA/PA_clipped_to_study_area.shp")

root_current <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/current/clipped_to_range/binary_merged_to_empty"
root_future  <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future"
out_csv <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/suitable_area_by_PA.csv"

# -----------------------------
# Convert landscapes → terra
# -----------------------------
land_v <- terra::vect(landscapes)

# pick the field that contains your PA names
# (you showed landscapes$PA exists)
name_col <- "PA"
land_v$PA_name <- landscapes[[name_col]]   # keep name attached to polygons

# -----------------------------
# Helper: suitable area by polygon
# -----------------------------
calc_landscape_area <- function(r, species, year, scenario, total_current_suitable) {
  
  # ensure CRS match
  land_proj <- if (!terra::same.crs(r, land_v)) terra::project(land_v, terra::crs(r)) else land_v
  
  cell_km2 <- prod(terra::res(r)) / 1e6
  
  # IMPORTANT: force terra::extract
  e <- terra::extract(r, land_proj)
  
  # e has: ID, layer values
  suitable_cells <- tapply(e[, 2] == 1, e[, 1], sum, na.rm = TRUE)
  suitable_km2   <- suitable_cells * cell_km2
  
  # match polygon IDs back to polygon names safely
  ids <- as.integer(names(suitable_km2))
  pa_names <- land_proj$PA_name[ids]
  
  df <- data.frame(
    species = species,
    landscape = pa_names,
    year = year,
    scenario = scenario,
    suitable_km2 = as.numeric(suitable_km2),
    pct_of_current_total = as.numeric(suitable_km2) / total_current_suitable * 100
  )
  
  df
}

# -----------------------------
# 1) CURRENT totals + by-PA
# -----------------------------
current_files <- list.files(root_current, pattern = "\\.tif$", full.names = TRUE)

current_totals  <- list()
current_results <- list()

for (f in current_files) {
  
  r <- terra::rast(f)
  species <- sub("_binary_on_empty.*", "", basename(f))
  
  cell_km2 <- prod(terra::res(r)) / 1e6
  total_cells <- terra::global(r == 1, "sum", na.rm = TRUE)[1, 1]
  total_km2 <- as.numeric(total_cells) * cell_km2
  
  current_totals[[species]] <- total_km2
  
  df <- calc_landscape_area(r, species, 2020, "current", total_km2)
  current_results[[length(current_results) + 1]] <- df
}

df_current <- do.call(rbind, current_results)

# -----------------------------
# 2) FUTURE by-PA
# -----------------------------
future_dirs <- list.dirs(root_future, recursive = TRUE, full.names = TRUE)
future_dirs <- future_dirs[grepl("binary_merged_to_empty$", future_dirs)]

future_results <- list()

for (d in future_dirs) {
  
  tif_files <- list.files(d, pattern = "\\.tif$", full.names = TRUE)
  
  for (f in tif_files) {
    
    r <- terra::rast(f)
    
    nm <- tools::file_path_sans_ext(basename(f))
    m  <- regexec("^(.+?)_binary_on_empty_(\\d{4})s_(ssp\\d{3})$", nm)
    hit <- regmatches(nm, m)[[1]]
    
    # skip files that don't match your naming convention
    if (length(hit) == 0) next
    
    species  <- hit[2]
    year     <- as.integer(hit[3])
    scenario <- hit[4]
    
    total_current <- current_totals[[species]]
    if (is.null(total_current) || is.na(total_current) || total_current == 0) next
    
    df <- calc_landscape_area(r, species, year, scenario, total_current)
    future_results[[length(future_results) + 1]] <- df
  }
}

df_future <- do.call(rbind, future_results)

# -----------------------------
# Combine + Save
# -----------------------------
df_all <- rbind(df_current, df_future)
df_all <- df_all[order(df_all$species, df_all$year, df_all$scenario, df_all$landscape), ]

write.csv(df_all, out_csv, row.names = FALSE)
df_all

# group results per PA

df_grouped <- df_all %>%
  group_by(species, year, scenario, landscape) %>%
  summarise(
    suitable_km2 = sum(suitable_km2, na.rm = TRUE),
    pct_of_current_total = sum(pct_of_current_total, na.rm = TRUE),
    n_polygons = dplyr::n(),
    .groups = "drop"
  ) %>%
  arrange(species, year, scenario, landscape)

df_grouped
write.csv(df_grouped,
          "F:/WWF_data/SDMs_LC_climate/maps_statistics/suitable_area_by_PA_GROUPED.csv",
          row.names = FALSE)


# ---- choose ONE species here ----
sp_pick <- "cyclotis"  # change to "gorilla", "paniscus", "troglodytes", etc.

df_plot <- df_grouped %>%
  filter(species == sp_pick, year != 2030) %>%
  mutate(
    pct_1dp = round(pct_of_current_total, 1),
    scenario = factor(scenario, levels = c("current","ssp126","ssp245","ssp585")),
    year = as.integer(year),
    landscape_short = recode(landscape,
                             "National Park" = "National Park",
                             "Forest Management Unit" = "Forest Management Unit",
                             "Community Hunting Zone" = "Community Hunting Zone",
                             "Wildlife Sanctuary" = "Wildlife Sanctuary"
    )
  )

ggplot(df_plot, aes(x = year, y = pct_1dp, color = scenario, group = scenario)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ landscape_short, scales = "free_y") +
  scale_x_continuous(breaks = c(2020, 2050, 2070, 2100)) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  labs(
    title = paste0(sp_pick, " — % of current suitable habitat by landscape"),
    x = "Year",
    y = "% of current suitable habitat",
    color = "Scenario"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")




# plot suitability for each species within PAs

library(dplyr)
library(ggplot2)
library(tibble)

# ---- choose ONE species here ----
sp_pick <- "gorilla"

# 1) Prep the base data (remove 2030)
base <- df_grouped %>%
  filter(species == sp_pick, year != 2030,
         landscape != "Mount Cameroon-Korup-Bakossi") %>%
  mutate(
    scenario = factor(scenario, levels = c("ssp126","ssp245","ssp585","current")),
    year = as.integer(year),
    #pct_1dp = round(pct_of_current_total, 1),
    pct_1dp = round(suitable_km2, 1),
    landscape_short = recode(landscape,
                             "National Park" = "National Park",
                             "Forest Management Unit" = "Forest Management Unit",
                             "Community Hunting Zone" = "Community Hunting Zone",
                             "Wildlife Sanctuary" = "Wildlife Sanctuary",
                             .default = landscape
    )
  )

# 2) Replicate the 2020 current point into each SSP so future lines connect to 2020
cur <- base %>%
  filter(year == 2020, scenario == "current") %>%
  select(species, landscape, landscape_short, year, pct_1dp) %>%
  distinct()

fut_scen <- tibble(
  scenario = factor(c("ssp126","ssp245","ssp585"),
                    levels = c("ssp126","ssp245","ssp585","current"))
)

cur_rep <- merge(cur, fut_scen, by = NULL) %>%  # cartesian join (safe + simple)
  select(species, landscape, landscape_short, year, scenario, pct_1dp)

df_plot <- bind_rows(
  base %>% select(species, landscape, landscape_short, year, scenario, pct_1dp),
  cur_rep
)

# ---- plot ----
ggplot(df_plot, aes(x = year, y = pct_1dp, color = scenario, group = scenario)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ landscape_short, scales = "free_y") +
  scale_x_continuous(breaks = c(2020, 2050, 2070, 2100)) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  scale_color_manual(
    values = c("ssp126" = "green", "ssp245" = "blue", "ssp585" = "red", "current" = "black"),
    breaks = c("ssp126","ssp245","ssp585","current")
  ) +
  labs(
    title = paste0("Suitable Habitat Area per Protected Area for Gorillas"),
    x = "Year", y = "Suitable Habitat Area (km2)", color = "Scenario"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")




# calculate richness within landscapes 

rich_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/richness_outputs"

all_files <- list.files(
  path = rich_dir,
  recursive = TRUE,
  full.names = TRUE
)

cat("Total files found:", length(all_files), "\n\n")
print(all_files)

tif_files <- list.files(
  path = rich_dir,
  pattern = "\\.tif$",
  recursive = TRUE,
  full.names = TRUE
)

cat("Total tif files found:", length(tif_files), "\n\n")
print(tif_files)

top5_dir <- file.path(rich_dir, "top5pct_richness")

top5_files <- list.files(
  path = top5_dir,
  recursive = TRUE,
  full.names = TRUE
)

cat("Top 5% folder files:", length(top5_files), "\n\n")
print(top5_files)


# Do calculations

rich_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/richness_outputs"

# --- read PAs
landscapes <- st_read("F:/WWF_data/SDMs_LC_climate/PA/PA_clipped_to_study_area.shp")
land_v <- terra::vect(landscapes)

# choose the PA-type field you want to group by
pa_type_col <- "PA"          # <- change if needed (e.g., "TYPE2_AP" or "type_ap")
land_v$pa_type <- landscapes[[pa_type_col]]

# --- only richness_pos rasters
rich_files <- list.files(
  rich_dir,
  pattern = "^richness_pos_.*\\.tif$",
  full.names = TRUE
)

parse_label <- function(x){
  nm <- tools::file_path_sans_ext(basename(x))
  nm <- sub("richness_pos_", "", nm)
  parts <- strsplit(nm, "_")[[1]]
  data.frame(year = as.integer(parts[1]), scenario = parts[2], stringsAsFactors = FALSE)
}

results <- list()

for (f in rich_files) {
  
  r <- terra::rast(f)
  
  land_v2 <- if (!terra::same.crs(r, land_v)) terra::project(land_v, terra::crs(r)) else land_v
  info <- parse_label(f)
  
  # Extract all raster values for all polygons at once
  e <- terra::extract(r, land_v2)   # columns: ID, layer
  names(e)[1:2] <- c("ID","richness_value")
  
  # keep only >0 richness
  e <- e[!is.na(e$richness_value) & e$richness_value > 0, , drop = FALSE]
  if (nrow(e) == 0) next
  
  # count cells per polygon per richness value
  tab <- as.data.frame(table(e$ID, e$richness_value), stringsAsFactors = FALSE)
  names(tab) <- c("ID","richness_value","n_cells")
  tab$n_cells <- as.numeric(tab$n_cells)
  tab$ID <- as.integer(tab$ID)
  tab$richness_value <- as.integer(tab$richness_value)
  
  # area
  cell_km2 <- prod(terra::res(r)) / 1e6
  tab$area_km2 <- tab$n_cells * cell_km2
  
  # attach PA type
  tab$pa_type <- land_v2$pa_type[tab$ID]
  
  # attach time/scenario
  tab$year <- info$year
  tab$scenario <- info$scenario
  
  results[[length(results) + 1]] <- tab
}

df_rich_feature <- bind_rows(results)

# --- GROUPED by PA type (this is what you want)
df_rich_pa_type <- df_rich_feature %>%
  group_by(year, scenario, pa_type, richness_value) %>%
  summarise(
    n_cells = sum(n_cells, na.rm = TRUE),
    area_km2 = sum(area_km2, na.rm = TRUE),
    n_polygons = n_distinct(ID),
    .groups = "drop"
  ) %>%
  arrange(year, scenario, pa_type, richness_value)

# Save both (feature-level optional)
write.csv(df_rich_feature,
          file.path(rich_dir, "richness_area_by_PAfeature_and_value.csv"),
          row.names = FALSE)

write.csv(df_rich_pa_type,
          file.path(rich_dir, "richness_area_by_PAtype_and_value_GROUPED.csv"),
          row.names = FALSE)

df_rich_pa_type
cat("Saved: richness_area_by_PAtype_and_value_GROUPED.csv\n")


# plot top 5%

library(dplyr)
library(terra)

top5_dir <- file.path(rich_dir, "top5pct_richness")

top5_files <- list.files(
  top5_dir,
  pattern = "\\.tif$",
  full.names = TRUE
)

parse_top5 <- function(x){
  nm <- tools::file_path_sans_ext(basename(x))
  nm <- sub("top5pct_", "", nm)
  parts <- strsplit(nm, "_")[[1]]
  data.frame(year = as.integer(parts[1]), scenario = parts[2], stringsAsFactors = FALSE)
}

top5_results <- list()

for (f in top5_files) {
  
  r <- terra::rast(f)
  
  land_v2 <- if (!terra::same.crs(r, land_v)) terra::project(land_v, terra::crs(r)) else land_v
  info <- parse_top5(f)
  
  # Extract all values for all polygons at once
  e <- terra::extract(r, land_v2)
  names(e)[1:2] <- c("ID","top5")
  
  e <- e[!is.na(e$top5), , drop = FALSE]
  if (nrow(e) == 0) next
  
  # count top5 cells (=1) per polygon
  n_cells <- tapply(e$top5 == 1, e$ID, sum, na.rm = TRUE)
  n_cells <- n_cells[!is.na(n_cells) & n_cells > 0]
  if (length(n_cells) == 0) next
  
  cell_km2 <- prod(terra::res(r)) / 1e6
  df <- data.frame(
    ID = as.integer(names(n_cells)),
    n_cells = as.numeric(n_cells),
    area_km2 = as.numeric(n_cells) * cell_km2,
    year = info$year,
    scenario = info$scenario,
    stringsAsFactors = FALSE
  )
  
  df$pa_type <- land_v2$pa_type[df$ID]
  
  top5_results[[length(top5_results) + 1]] <- df
}

df_top5_feature <- bind_rows(top5_results)

# GROUPED by PA type
df_top5_pa_type <- df_top5_feature %>%
  group_by(year, scenario, pa_type) %>%
  summarise(
    n_cells = sum(n_cells, na.rm = TRUE),
    area_km2 = sum(area_km2, na.rm = TRUE),
    n_polygons = n_distinct(ID),
    .groups = "drop"
  ) %>%
  arrange(year, scenario, pa_type)

write.csv(df_top5_feature,
          file.path(rich_dir, "top5pct_richness_area_by_PAfeature.csv"),
          row.names = FALSE)

write.csv(df_top5_pa_type,
          file.path(rich_dir, "top5pct_richness_area_by_PAtype_GROUPED.csv"),
          row.names = FALSE)

df_top5_pa_type
cat("Saved: top5pct_richness_area_by_PAtype_GROUPED.csv\n")


# 1) filter to desired years only + set factor order
df_plot <- df_top5_pa_type %>%
  filter(year %in% c(2020, 2050, 2070, 2100)) %>%
  mutate(
    year = as.integer(year),
    scenario = factor(scenario, levels = c("ssp126","ssp245","ssp585","current")),
    pa_type = as.character(pa_type)
  )

# 2) replicate the 2020 current point into each future scenario so lines connect
cur <- df_plot %>%
  filter(year == 2020, scenario == "current") %>%
  select(year, pa_type, area_km2) %>%
  distinct()

cur_rep <- cur %>%
  tidyr::crossing(
    scenario = factor(c("ssp126","ssp245","ssp585"),
                      levels = c("ssp126","ssp245","ssp585","current"))
  )

df_plot2 <- bind_rows(
  df_plot %>% select(year, scenario, pa_type, area_km2),
  cur_rep
) %>%
  arrange(pa_type, scenario, year)

# 3) plot
ggplot(df_plot2, aes(x = year, y = area_km2, color = scenario, group = scenario)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ pa_type, scales = "free_y") +
  scale_x_continuous(breaks = c(2020, 2050, 2070, 2100)) +
  scale_color_manual(values = c(
    "ssp126" = "green",
    "ssp245" = "blue",
    "ssp585" = "red",
    "current" = "black"
  )) +
  labs(
    title = "Top 5% Richness Area by Protected Area Type",
    x = "Year",
    y = "Top 5% Richness Area (km²)",
    color = "Scenario"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")


# ---------------------------------------------------

# Now, calculate Insitu refugia for each species

ref_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/refugia_rasters_stable"

all_files <- list.files(
  path = ref_dir,
  recursive = TRUE,
  full.names = TRUE
)

cat("Total files found:", length(all_files), "\n\n")
print(all_files)

tif_files <- list.files(
  path = ref_dir,
  pattern = "\\.tif$",
  recursive = TRUE,
  full.names = TRUE
)

cat("Total tif files found:", length(tif_files), "\n\n")
print(tif_files)


#--------------- calculate refugia--------------------
# Inputs
# -----------------------------
ref_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/refugia_rasters_stable"
tif_files <- list.files(ref_dir, pattern = "\\.tif$", full.names = TRUE)

landscapes <- st_read("F:/WWF_data/SDMs_LC_climate/PA/PA_clipped_to_study_area.shp")

# -----------------------------
# Prep landscapes (optional drop)
# If you really need to drop this, you must ensure the NAME field exists.
# If NAME does not exist, set name_col correctly (e.g., "nom_ap").
# -----------------------------
name_col <- "PA"   # <- change if your PA-name field is different (e.g., "nom_ap")
pa_type_col <- "PA"  # <- change if needed (e.g., "TYPE2_AP", "type_ap")

land_sf <- landscapes %>%
  mutate(
    pa_name = .data[[name_col]],
    pa_type = .data[[pa_type_col]]
  )

# Optionally drop one landscape by name (only if it exists in your data)
land_sf <- land_sf %>%
  filter(pa_name != "Mount Cameroon-Korup-Bakossi")

land_v <- terra::vect(land_sf)

# -----------------------------
# Helper: parse filename metadata
# e.g. cyclotis_refugia_stable_2020_to_2050_ssp126.tif
# -----------------------------
parse_meta <- function(f) {
  nm <- basename(f)
  m <- str_match(nm, "^(.+?)_refugia_stable_2020_to_(\\d{4})_(ssp\\d{3})\\.tif$")
  if (any(is.na(m))) return(NULL)
  
  data.frame(
    file = f,
    species = m[, 2],
    baseline_year = 2020L,
    future_year = as.integer(m[, 3]),
    scenario = m[, 4],
    stringsAsFactors = FALSE
  )
}

meta <- bind_rows(lapply(tif_files, parse_meta))
if (nrow(meta) == 0) {
  stop("No files matched expected pattern *_refugia_stable_2020_to_YYYY_sspNNN.tif")
}

# -----------------------------
# Compute refugia area by polygon (feature-level) for each raster
# -----------------------------
calc_one <- function(file, species, baseline_year, future_year, scenario) {
  
  r <- terra::rast(file)
  
  # cell area (km²)
  cell_km2 <- prod(terra::res(r)) / 1e6
  
  # ensure CRS match
  land_use <- if (!terra::same.crs(r, land_v)) terra::project(land_v, terra::crs(r)) else land_v
  
  # sum of 1s per polygon (touches=TRUE matches your earlier intent)
  ex <- terra::extract(r, land_use, fun = sum, na.rm = TRUE, touches = TRUE)
  val_col <- names(ex)[2]
  
  # build output using polygon IDs to pull attributes from land_use
  out <- data.frame(
    species = species,
    baseline_year = baseline_year,
    future_year = future_year,
    scenario = scenario,
    ID = ex$ID,
    pa_name = land_use$pa_name[ex$ID],
    pa_type = land_use$pa_type[ex$ID],
    n_cells_1 = as.numeric(ex[[val_col]]),
    cell_km2 = cell_km2,
    refugia_km2 = as.numeric(ex[[val_col]]) * cell_km2,
    stringsAsFactors = FALSE
  )
  
  out
}

df_refugia_feature <- bind_rows(
  lapply(seq_len(nrow(meta)), function(i) {
    calc_one(meta$file[i], meta$species[i], meta$baseline_year[i],
             meta$future_year[i], meta$scenario[i])
  })
)

# -----------------------------
# GROUPED by PA type (sum across polygons)
# -----------------------------
df_refugia_pa_type <- df_refugia_feature %>%
  group_by(species, baseline_year, future_year, scenario, pa_type) %>%
  summarise(
    refugia_km2 = sum(refugia_km2, na.rm = TRUE),
    n_cells_1   = sum(n_cells_1, na.rm = TRUE),
    n_polygons  = n_distinct(ID),
    .groups = "drop"
  ) %>%
  arrange(species, future_year, scenario, pa_type)

# -----------------------------
# Save CSVs
# -----------------------------
write.csv(df_refugia_feature,
          file.path(ref_dir, "refugia_area_by_PAfeature.csv"),
          row.names = FALSE)

write.csv(df_refugia_pa_type,
          file.path(ref_dir, "refugia_area_by_PAtype_GROUPED.csv"),
          row.names = FALSE)

df_refugia_pa_type
cat("Saved: refugia_area_by_PAtype_GROUPED.csv\n")

# -----------------------------
# Plot: one species (facet by PA)
# Connect scenario lines to 2020 point (replicate 2020 across scenarios)
# -----------------------------

sp_pick <- "troglodytes"   # change species as needed

df_plot <- df_refugia_pa_type %>%
  filter(
    species == sp_pick,
    future_year %in% c(2050, 2070, 2100)
  ) %>%
  mutate(
    year = future_year,
    scenario = factor(scenario, levels = c("ssp126","ssp245","ssp585")),
    pa_type = as.character(pa_type)
  )

ggplot(df_plot,
       aes(x = year,
           y = refugia_km2,
           color = scenario,
           group = scenario)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ pa_type, scales = "free_y") +
  scale_x_continuous(breaks = c(2050, 2070, 2100)) +
  scale_color_manual(values = c(
    "ssp126" = "green",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    title = paste0("In Situ Refugia Area by Protected Area Type for Chimpanzee"),
    x = "Year",
    y = "Refugia Area (km²)",
    color = "Scenario"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )



#------------repeat calculations for exsitu Refugia

# Root folder
root_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future"

# 1) Find ALL folders named "outside_species_range"
all_dirs <- list.dirs(root_dir, recursive = TRUE, full.names = TRUE)
outside_dirs <- all_dirs[basename(all_dirs) == "outside_species_range"]

cat("Found", length(outside_dirs), "outside_species_range folders\n\n")
print(outside_dirs)

if (length(outside_dirs) == 0) {
  stop("No 'outside_species_range' folders found inside: ", root_dir)
}

# 2) List ALL .tif files inside those folders (and print to verify)
tif_files <- unlist(lapply(outside_dirs, function(d) {
  list.files(d, pattern = "\\.tif$", full.names = TRUE)
}))

cat("\nFound", length(tif_files), "raster tif files in outside_species_range folders\n\n")
print(tif_files)

# Optional: print just filenames
cat("\nFilenames only:\n")
print(basename(tif_files))



# calculate and plot
# -----------------------------
# Inputs
# -----------------------------
root_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future"
landscapes <- st_read("F:/WWF_data/SDMs_LC_climate/PA/PA_clipped_to_study_area.shp")

# choose PA-type field used for grouping
pa_type_col <- "PA"   # <- change if needed (e.g., "TYPE2_AP", "type_ap")

# outputs
out_csv_feature <- file.path(root_dir, "exsitu_refugia_area_by_PAfeature.csv")
out_csv_grouped <- file.path(root_dir, "exsitu_refugia_area_by_PAtype_GROUPED.csv")

# -----------------------------
# 1) Find outside_species_range rasters
# -----------------------------
all_dirs <- list.dirs(root_dir, recursive = TRUE, full.names = TRUE)
outside_dirs <- all_dirs[basename(all_dirs) == "outside_species_range"]
stopifnot(length(outside_dirs) > 0)

tif_files <- unlist(lapply(outside_dirs, function(d) {
  list.files(d, pattern = "\\.tif$", full.names = TRUE)
}))
stopifnot(length(tif_files) > 0)

# -----------------------------
# 2) Prep PA polygons as terra vector
# -----------------------------
land_v <- terra::vect(landscapes)
land_v$pa_type <- landscapes[[pa_type_col]]

# -----------------------------
# 3) Helpers
# -----------------------------
# filename like: "bonobos_binary_OUTSIDE_range_2050_ssp245.tif"
parse_meta <- function(fp) {
  nm <- tools::file_path_sans_ext(basename(fp))
  m <- str_match(nm, "^(.+?)_binary_OUTSIDE_range_(\\d{4})_(ssp\\d+)$")
  if (any(is.na(m))) return(NULL)
  
  data.frame(
    file = fp,
    species = m[, 2],
    year = as.integer(m[, 3]),
    scenario = m[, 4],
    stringsAsFactors = FALSE
  )
}

meta <- bind_rows(lapply(tif_files, parse_meta))
if (nrow(meta) == 0) stop("No files matched pattern *_binary_OUTSIDE_range_YYYY_sspNNN.tif")

# -----------------------------
# 4) Main loop (fast extract)
# -----------------------------
all_res <- vector("list", nrow(meta))

for (i in seq_len(nrow(meta))) {
  
  fp <- meta$file[i]
  r  <- terra::rast(fp)
  
  # reproject polygons if needed
  land_v2 <- if (!terra::same.crs(r, land_v)) terra::project(land_v, terra::crs(r)) else land_v
  
  # cell area (km²)
  cell_km2 <- prod(terra::res(r)) / 1e6
  
  # denominator: total outside-range footprint area (non-NA cells)
  n_out_cells <- terra::global(!is.na(r), "sum", na.rm = TRUE)[1, 1]
  outside_range_area_km2 <- as.numeric(n_out_cells) * cell_km2
  if (is.na(outside_range_area_km2)) outside_range_area_km2 <- 0
  
  # extract raster values for all polygons
  e <- terra::extract(r, land_v2)    # columns: ID, layer
  names(e)[1:2] <- c("ID", "val")
  
  # count 1's per polygon (treat NA as not suitable)
  n1 <- tapply(e$val == 1, e$ID, sum, na.rm = TRUE)
  n1[is.na(n1)] <- 0
  
  df_i <- data.frame(
    species = meta$species[i],
    year = meta$year[i],
    scenario = meta$scenario[i],
    ID = as.integer(names(n1)),
    pa_type = land_v2$pa_type[as.integer(names(n1))],
    n_cells_1 = as.numeric(n1),
    suitable_km2 = as.numeric(n1) * cell_km2,
    outside_range_area_km2 = outside_range_area_km2,
    pct_of_outside_range = ifelse(outside_range_area_km2 > 0,
                                  (as.numeric(n1) * cell_km2) / outside_range_area_km2 * 100,
                                  0),
    stringsAsFactors = FALSE
  )
  
  all_res[[i]] <- df_i
  
  rm(r); gc(FALSE)
}

df_exsitu_feature <- bind_rows(all_res)

# -----------------------------
# 5) Group by PA type (sum areas & percents)
# -----------------------------
df_exsitu_pa_type <- df_exsitu_feature %>%
  group_by(species, year, scenario, pa_type) %>%
  summarise(
    suitable_km2 = sum(suitable_km2, na.rm = TRUE),
    pct_of_outside_range = sum(pct_of_outside_range, na.rm = TRUE),
    n_cells_1 = sum(n_cells_1, na.rm = TRUE),
    n_polygons = n_distinct(ID),
    outside_range_area_km2 = first(outside_range_area_km2),
    .groups = "drop"
  ) %>%
  arrange(species, year, scenario, pa_type) %>%
  mutate(pct_of_outside_range_1dp = round(pct_of_outside_range, 1))

# -----------------------------
# 6) Save
# -----------------------------
write.csv(df_exsitu_feature, out_csv_feature, row.names = FALSE)
write.csv(df_exsitu_pa_type, out_csv_grouped, row.names = FALSE)

df_exsitu_pa_type
cat("Saved:\n", out_csv_feature, "\n", out_csv_grouped, "\n")


# -----------------------------
# 6) Plot for one species (facet by PA)
#    - connect future lines to 2030 baseline within each scenario
# -----------------------------

sp_pick <- "chimps"   # change species as needed

df_plot <- df_exsitu_pa_type %>%
  filter(
    species == sp_pick,
    year %in% c(2050, 2070, 2100)
  ) %>%
  mutate(
    year = year,
    scenario = factor(scenario, levels = c("ssp126","ssp245","ssp585")),
    pa_type = as.character(pa_type)
  )

ggplot(df_plot,
       aes(x = year,
           y = suitable_km2,
           color = scenario,
           group = scenario)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ pa_type, scales = "free_y") +
  scale_x_continuous(breaks = c(2050, 2070, 2100)) +
  scale_color_manual(values = c(
    "ssp126" = "green",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  labs(
    title = paste0("Ex Situ Refugia Area by Protected Area Type for Chimpanzee"),
    x = "Year",
    y = "Refugia Area (km²)",
    color = "Scenario"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )



#--------Now perform calculations for insitu + exsitu refugia richness----


rich_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/insitu_plus_exsitu_richness_outputs"

all_files <- list.files(
  path = rich_dir,
  recursive = TRUE,
  full.names = TRUE
)

cat("Total files found:", length(all_files), "\n\n")
print(all_files)

tif_files <- list.files(
  path = rich_dir,
  pattern = "\\.tif$",
  recursive = TRUE,
  full.names = TRUE
)

cat("Total tif files found:", length(tif_files), "\n\n")
print(tif_files)

top5_dir <- file.path(rich_dir, "top5pct_hotspots")

top5_files <- list.files(
  path = top5_dir,
  recursive = TRUE,
  full.names = TRUE
)

cat("Top 5% folder files:", length(top5_files), "\n\n")
print(top5_files)



# ------------Now, calculate----------

# Inputs
# -----------------------------
rich_dir <- "F:/WWF_data/SDMs_LC_climate/maps_statistics/future/insitu_plus_exsitu_richness_outputs"
top5_dir <- file.path(rich_dir, "top5pct_hotspots")

landscapes <- st_read("F:/WWF_data/SDMs_LC_climate/PA/PA_clipped_to_study_area.shp")

# PA grouping column (change if needed)
pa_type_col <- "PA"   # e.g., "PA", "TYPE2_AP", "type_ap"

# Optional: PA name column only used if you want to drop one by name
# If you don’t have NAME, set to "nom_ap" or just comment out the drop block
name_col <- "PA"

# -----------------------------
# Prep landscapes -> terra
# -----------------------------
land_sf <- landscapes %>%
  mutate(
    pa_type = .data[[pa_type_col]],
    pa_name = if (name_col %in% names(.)) as.character(.data[[name_col]]) else NA_character_
  )

# Optional drop (safe even if pa_name is NA)
land_sf <- land_sf %>% filter(is.na(pa_name) | pa_name != "Mount Cameroon-Korup-Bakossi")

land_v <- terra::vect(land_sf)

# -----------------------------
# Helpers: parse metadata
# -----------------------------
parse_rich_meta <- function(fp) {
  nm <- tools::file_path_sans_ext(basename(fp))
  m <- str_match(nm, "^richness_(\\d{4})s_(ssp\\d{3})$")
  if (any(is.na(m))) return(NULL)
  data.frame(file = fp, year = as.integer(m[,2]), scenario = m[,3], stringsAsFactors = FALSE)
}

parse_top5_meta <- function(fp) {
  nm <- tools::file_path_sans_ext(basename(fp))
  m <- str_match(nm, "^top5pct_hotspots_(\\d{4})s_(ssp\\d{3})$")
  if (any(is.na(m))) return(NULL)
  data.frame(file = fp, year = as.integer(m[,2]), scenario = m[,3], stringsAsFactors = FALSE)
}

# -----------------------------
# 1A) Richness rasters
# -----------------------------
rich_files <- list.files(
  path = rich_dir,
  pattern = "^richness_\\d{4}s_ssp\\d{3}\\.tif$",
  full.names = TRUE
)

meta_rich <- bind_rows(lapply(rich_files, parse_rich_meta))
stopifnot(nrow(meta_rich) > 0)

calc_rich_one <- function(file, year, scenario) {
  r <- terra::rast(file)
  cell_km2 <- prod(terra::res(r)) / 1e6
  
  land_use <- if (!terra::same.crs(r, land_v)) terra::project(land_v, terra::crs(r)) else land_v
  
  e <- terra::extract(r, land_use)
  names(e)[1:2] <- c("ID", "richness_value")
  
  e <- e[!is.na(e$richness_value) & e$richness_value > 0, , drop = FALSE]
  if (nrow(e) == 0) return(NULL)
  
  tab <- as.data.frame(table(e$ID, e$richness_value), stringsAsFactors = FALSE)
  names(tab) <- c("ID", "richness_value", "n_cells")
  tab$ID <- as.integer(tab$ID)
  tab$richness_value <- as.integer(tab$richness_value)
  tab$n_cells <- as.numeric(tab$n_cells)
  
  tab$area_km2 <- tab$n_cells * cell_km2
  tab$pa_type <- land_use$pa_type[tab$ID]
  tab$year <- as.integer(year)
  tab$scenario <- scenario
  tab
}

df_rich_feature <- bind_rows(lapply(seq_len(nrow(meta_rich)), function(i) {
  calc_rich_one(meta_rich$file[i], meta_rich$year[i], meta_rich$scenario[i])
}))

df_rich_pa_type <- df_rich_feature %>%
  group_by(year, scenario, pa_type, richness_value) %>%
  summarise(
    n_cells = sum(n_cells, na.rm = TRUE),
    area_km2 = sum(area_km2, na.rm = TRUE),
    n_polygons = n_distinct(ID),
    .groups = "drop"
  ) %>%
  arrange(year, scenario, pa_type, richness_value)

df_rich_pa_type

write.csv(df_rich_feature,
          file.path(rich_dir, "insitu_exsitu_richness_area_by_PAfeature_and_value.csv"),
          row.names = FALSE)

write.csv(df_rich_pa_type,
          file.path(rich_dir, "insitu_exsitu_richness_area_by_PAtype_and_value_GROUPED.csv"),
          row.names = FALSE)

# -----------------------------
# 1B) Top 5% hotspot rasters (binary 1/0)
# -----------------------------
top5_files <- list.files(
  path = top5_dir,
  pattern = "^top5pct_hotspots_\\d{4}s_ssp\\d{3}\\.tif$",
  full.names = TRUE
)

meta_top5 <- bind_rows(lapply(top5_files, parse_top5_meta))
stopifnot(nrow(meta_top5) > 0)

calc_top5_one <- function(file, year, scenario) {
  r <- terra::rast(file)
  cell_km2 <- prod(terra::res(r)) / 1e6
  
  land_use <- if (!terra::same.crs(r, land_v)) terra::project(land_v, terra::crs(r)) else land_v
  
  e <- terra::extract(r, land_use)
  names(e)[1:2] <- c("ID", "top5")
  
  e <- e[!is.na(e$top5), , drop = FALSE]
  if (nrow(e) == 0) return(NULL)
  
  n1 <- tapply(e$top5 == 1, e$ID, sum, na.rm = TRUE)
  n1[is.na(n1)] <- 0
  
  df <- data.frame(
    ID = as.integer(names(n1)),
    n_cells_1 = as.numeric(n1),
    area_km2 = as.numeric(n1) * cell_km2,
    pa_type = land_use$pa_type[as.integer(names(n1))],
    year = as.integer(year),
    scenario = scenario,
    stringsAsFactors = FALSE
  )
  
  df
}

df_top5_feature <- bind_rows(lapply(seq_len(nrow(meta_top5)), function(i) {
  calc_top5_one(meta_top5$file[i], meta_top5$year[i], meta_top5$scenario[i])
}))

df_top5_pa_type <- df_top5_feature %>%
  group_by(year, scenario, pa_type) %>%
  summarise(
    n_cells_1 = sum(n_cells_1, na.rm = TRUE),
    area_km2 = sum(area_km2, na.rm = TRUE),
    n_polygons = n_distinct(ID),
    .groups = "drop"
  ) %>%
  arrange(year, scenario, pa_type)

df_top5_pa_type

write.csv(df_top5_feature,
          file.path(top5_dir, "insitu_exsitu_top5pct_hotspots_area_by_PAfeature.csv"),
          row.names = FALSE)

write.csv(df_top5_pa_type,
          file.path(top5_dir, "insitu_exsitu_top5pct_hotspots_area_by_PAtype_GROUPED.csv"),
          row.names = FALSE)

cat("Done. Saved richness + top5 hotspot outputs.\n")


# -----------------------------
# Plot: facet by landscape, start from 2050 (already filtered), remove MtCam already done
# -----------------------------
library(dplyr)
library(ggplot2)

df_plot <- df_top5_pa_type %>%
  filter(year %in% c(2050, 2070, 2100)) %>%
  mutate(
    year = as.integer(year),
    scenario = factor(scenario, levels = c("ssp126","ssp245","ssp585"))
  )

ggplot(df_plot,
       aes(x = year, y = area_km2, color = scenario, group = scenario)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ pa_type, scales = "free_y") +
  scale_color_manual(values = c(
    "ssp126" = "green3",
    "ssp245" = "blue",
    "ssp585" = "red"
  )) +
  scale_x_continuous(breaks = c(2050, 2070, 2100)) +
  labs(
    x = "Year",
    y = "Top 5% Refugia Hotspot Area (km²)",
    color = "Scenario",
    title = "Top 5% In Situ + Ex Situ Refugia Richness Hotspots per Protected Area"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")







