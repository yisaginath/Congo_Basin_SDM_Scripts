
install.packages("rgdal")
library(dismo) # for maxent modelling
library(raster) # for raster creation, manipulation, and analysis
library(dplyr) # tidy data
library(magrittr) # for streamlining manipulated data and making codes easily readable with the pipe operator
library(rgdal) # for handling spatial data in different formats and facilitating projection
library(sp) # for handling spatial data, especially vector layers
library(maxnet) # for MaxEnt modeliing
library(ggplot2) # plotting results
library(rnaturalearth)# for handling background map data
library(rnaturalearthdata) # for handling background map data
Sys.setenv("PROJ_NETWORK" = "ON")
library(sf)# for handling spatial data in different formats and facilitating projection
library(terra) #for raster creation, manipulation, and analysis
library(blockCV)# for spatial cross-validation with presence-absence data
library(spatialEco)# provide importnt functions for manipulating, querying, and modeling spatial datasets in ecology
library(pROC)
library(randomForest)
library(PresenceAbsence)
library(corrplot)
library(tidyr)
library(readr)
library(RStoolbox) # has rasterPCA

# Clean all raster temp files to free memory
clean_temp_rasters <- function() {
  # raster temp
  removeTmpFiles(h = 0)
  
  # terra temp
  terra::tmpFiles(remove = TRUE)
  
  cat("✅ All raster/terra temp files removed.\n")
}

# Run cleanup
clean_temp_rasters()

terraOptions(tempdir = "F:/R_temp")   # change to a disk with >20GB free
rasterOptions(tmpdir = "F:/R_temp")   # for raster package if used


# prepare stable land cover

forest_old <- rast("F:/WWF_data/SDMs_LC_climate/predictors/forest_2020.tif")
forest_old
LC2020_6_8 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC1_6/LC6_8_2020.tif")
LC2020_6_8
plot(LC2020_6_8)

# Load future land cover and cover to resampled data
LC2030_ssp126 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC1_6/LC2030_ssp126.tif")
LC2030_ssp126
LC2030_ssp126_res <- resample(LC2030_ssp126, LC2020_6_8, method = "near")
LC2030_ssp126_res
plot(LC2030_ssp126_res)

LC2030_ssp126_cover <- cover(LC2030_ssp126_res, LC2020_6_8)
LC2030_ssp126_cover
plot(LC2030_ssp126_cover)

# ---- Save rasters ----
writeRaster(LC2030_ssp126_cover, "F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2030_ssp126_stable.tif", overwrite=TRUE)


LC2030_ssp245 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC1_6/LC2030_ssp245.tif")
LC2030_ssp245
LC2030_ssp245_res <- resample(LC2030_ssp245, LC2020_6_8, method = "near")
LC2030_ssp245_res
plot(LC2030_ssp245_res)

LC2030_ssp245_cover <- cover(LC2030_ssp245_res, LC2020_6_8)
LC2030_ssp245_cover
plot(LC2030_ssp245_cover)

# ---- Save rasters ----
writeRaster(LC2030_ssp245_cover, "F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2030_ssp245_stable.tif", overwrite=TRUE)


LC2030_ssp585 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC1_6/LC2030_ssp585.tif")
LC2030_ssp585
LC2030_ssp585_res <- resample(LC2030_ssp585, LC2020_6_8, method = "near")
LC2030_ssp585_res
plot(LC2030_ssp585_res)

LC2030_ssp585_cover <- cover(LC2030_ssp585_res, LC2020_6_8)
LC2030_ssp585_cover
plot(LC2030_ssp585_cover)

# ---- Save rasters ----
writeRaster(LC2030_ssp585_cover, "F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2030_ssp585_stable.tif", overwrite=TRUE)



LC2050_ssp126 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC1_6/LC2050_ssp126.tif")
LC2050_ssp126
LC2050_ssp126_res <- resample(LC2050_ssp126, LC2020_6_8, method = "near")
LC2050_ssp126_res
plot(LC2050_ssp126_res)

LC2050_ssp126_cover <- cover(LC2050_ssp126_res, LC2020_6_8)
LC2050_ssp126_cover
plot(LC2050_ssp126_cover)

# ---- Save rasters ----
writeRaster(LC2050_ssp126_cover, "F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2050_ssp126_stable.tif", overwrite=TRUE)


LC2050_ssp245 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC1_6/LC2050_ssp245.tif")
LC2050_ssp245
LC2050_ssp245_res <- resample(LC2050_ssp245, LC2020_6_8, method = "near")
LC2050_ssp245_res
plot(LC2050_ssp245_res)

LC2050_ssp245_cover <- cover(LC2050_ssp245_res, LC2020_6_8)
LC2050_ssp245_cover
plot(LC2050_ssp245_cover)

# ---- Save rasters ----
writeRaster(LC2050_ssp245_cover, "F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2050_ssp245_stable.tif", overwrite=TRUE)


LC2050_ssp585 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC1_6/LC2050_ssp585.tif")
LC2050_ssp585
LC2050_ssp585_res <- resample(LC2050_ssp585, LC2020_6_8, method = "near")
LC2050_ssp585_res
plot(LC2050_ssp585_res)

LC2050_ssp585_cover <- cover(LC2050_ssp585_res, LC2020_6_8)
LC2050_ssp585_cover
plot(LC2050_ssp585_cover)

# ---- Save rasters ----
writeRaster(LC2050_ssp585_cover, "F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2050_ssp585_stable.tif", overwrite=TRUE)


LC2070_ssp126 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC1_6/LC2070_ssp126.tif")
LC2070_ssp126
LC2070_ssp126_res <- resample(LC2070_ssp126, LC2020_6_8, method = "near")
LC2070_ssp126_res
plot(LC2070_ssp126_res)

LC2070_ssp126_cover <- cover(LC2070_ssp126_res, LC2020_6_8)
LC2070_ssp126_cover
plot(LC2070_ssp126_cover)

# ---- Save rasters ----
writeRaster(LC2070_ssp126_cover, "F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2070_ssp126_stable.tif", overwrite=TRUE)


LC2070_ssp245 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC1_6/LC2070_ssp245.tif")
LC2070_ssp245
LC2070_ssp245_res <- resample(LC2070_ssp245, LC2020_6_8, method = "near")
LC2070_ssp245_res
plot(LC2070_ssp245_res)

LC2070_ssp245_cover <- cover(LC2070_ssp245_res, LC2020_6_8)
LC2070_ssp245_cover
plot(LC2070_ssp245_cover)

# ---- Save rasters ----
writeRaster(LC2070_ssp245_cover, "F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2070_ssp245_stable.tif", overwrite=TRUE)


LC2070_ssp585 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC1_6/LC2070_ssp585.tif")
LC2070_ssp585
LC2070_ssp585_res <- resample(LC2070_ssp585, LC2020_6_8, method = "near")
LC2070_ssp585_res
plot(LC2070_ssp585_res)

LC2070_ssp585_cover <- cover(LC2070_ssp585_res, LC2020_6_8)
LC2070_ssp585_cover
plot(LC2070_ssp585_cover)

# ---- Save rasters ----
writeRaster(LC2070_ssp585_cover, "F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2070_ssp585_stable.tif", overwrite=TRUE)


LC2100_ssp126 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC1_6/LC2100_ssp126.tif")
LC2100_ssp126
LC2100_ssp126_res <- resample(LC2100_ssp126, LC2020_6_8, method = "near")
LC2100_ssp126_res
plot(LC2100_ssp126_res)

LC2100_ssp126_cover <- cover(LC2100_ssp126_res, LC2020_6_8)
LC2100_ssp126_cover
plot(LC2100_ssp126_cover)

# ---- Save rasters ----
writeRaster(LC2100_ssp126_cover, "F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2100_ssp126_stable.tif", overwrite=TRUE)


LC2100_ssp245 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC1_6/LC2100_ssp245.tif")
LC2100_ssp245
LC2100_ssp245_res <- resample(LC2100_ssp245, LC2020_6_8, method = "near")
LC2100_ssp245_res
plot(LC2100_ssp245_res)

LC2100_ssp245_cover <- cover(LC2100_ssp245_res, LC2020_6_8)
LC2100_ssp245_cover
plot(LC2100_ssp245_cover)

# ---- Save rasters ----
writeRaster(LC2100_ssp245_cover, "F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2100_ssp245_stable.tif", overwrite=TRUE)


LC2100_ssp585 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC1_6/LC2100_ssp585.tif")
LC2100_ssp585
LC2100_ssp585_res <- resample(LC2100_ssp585, LC2020_6_8, method = "near")
LC2100_ssp585_res
plot(LC2100_ssp585_res)

LC2100_ssp585_cover <- cover(LC2100_ssp585_res, LC2020_6_8)
LC2100_ssp585_cover
plot(LC2100_ssp585_cover)

# ---- Save rasters ----
writeRaster(LC2100_ssp585_cover, "F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2100_ssp585_stable.tif", overwrite=TRUE)


# -------------------------------------------------------------------------------

# Calculate amount of land cover persite at 1km2 spatial scale

# Load the study area for masking land cover data
#setwd("F:/Uillinois_data/study_area")# Set working directory to location of study area
#study_area <- sf::st_read(dsn = "study_area.shp")
study_area <- sf::st_read(dsn = "F:/WWF_data/climate_species_analysis/defined_study_area/study_area_fit.shp")

# Load the land cover datasets and visualize
#SECanadaLandCover <- rast("E:/Uottawa_data/study_area/land_cover_data/Eastern Expansion/resampled_data2/SECanada_landcover_resampled_30m.tif")
SECanadaLandCover <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/CB_LC2020_30m.tif")
plot(SECanadaLandCover)
SECanadaLandCover

# Create a base raster with some land use values for each pixel
# raster x (blank but with 1000m cells) can be used to "resample" rasters of different resolution or calculate variables
# Use CRS of the land cover data, and insert list of values based on the number of cells defined within the base raster
x <- terra::rast(resolution = 1000,
                 extent = ext(SECanadaLandCover),
                 crs = terra::crs(SECanadaLandCover),
                 vals = 1:1189953)
names(x) <- "cell_id"

ncell(x)# check number of raster cells to be inserted within the vals range of the base raster and modify the base raster values


##### 3. Use land cover raster to measure proportion of habitat per site -----------------

# Start by matching the CRS of the study area to that of the land cover data, and convert to a vector object
SECanadaVectTemp <- study_area %>%
  st_transform(., terra::crs(SECanadaLandCover)) %>%
  vect(.)

# Mask to the land cover data to the new stucy area (vector object) so that aggregate doesn't have to do so many cells
# use the terra crop and mask functions in a 2 step process
SECanadaLandCoverCropped <- terra::crop(x = SECanadaLandCover, 
                                        y = SECanadaVectTemp)
plot(SECanadaLandCoverCropped, main = "cropped")

SECanadaLandCover <- terra::mask(x = SECanadaLandCoverCropped, 
                                 mask = SECanadaVectTemp)
plot(SECanadaLandCover, main = "masked")



# Mask (if landcover is incomplete or to adjust to border of the study area)
x_lc <- x %>%
  terra::project(., terra::crs(SECanadaLandCover)) %>%
  terra::mask(., 
              mask = SECanadaVectTemp)
plot(x_lc)

# List the legend of the land cover data to ease selection of classes to be aggregated and calculate proportion of habitat per site
# land cover Legend 
# 1 = Forest
# 2 = Shrubland
# 3 = Grassland
# 4 = Wetland
# 5 =  Cropland
# 6 = Barren
# 7 = Built-up
# 8 = Water

# Calculate ratio for aggregating data from smaller cells into bigger cells
Nratio <- as.integer(terra::res(x_lc)/terra::res(SECanadaLandCover))[1] 
Nratio

# -------------------proportion of habitat per site calculations--------------

# A. Forest

# Lets start with calculating habitat per site for forest
# code = 1

# Now, calculate the proportion of habitat per site for native grass
system.time(
  forest_temp <- aggregate(SECanadaLandCover,
                           Nratio, 
                           # Function sums the number of forest cells x cell area (100 x 100) / area of environmental layer raster cells (1000 x 1000)
                           fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(forest_temp)

# Resample data to correct resolution and extent.
forest_resampled <- terra::resample(forest_temp, x_lc, method = "bilinear") 

# Before saving, make the name of the layer correspond to the variable
names(forest_resampled) <- "forest"

# Need to mask landcover resampled rasters (so that 0 within mask, NA outside)
# Make NA values 0, then re-mask by study area outline
forest_resampled[is.na(forest_resampled)] <- 0
forest <- terra::mask(x = forest_resampled, 
                      mask = study_area %>%
                        st_transform(., terra::crs(forest_resampled)) %>%
                        vect(.))
# Plot layer
plot(forest)

forest <- resample(forest, forest_old, method = "near")
forest
forest_old

# Save results
output_dir <- "F:/WWF_data/SDMs_LC_climate/predictors1/"
writeRaster(forest, 
            filename = paste0(output_dir, "forest_proportion_1km.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

# First roll-up to 81 times (805/10 m)
Nratio_step <- as.integer(1000/raster::res(SECanadaLandCover)[1])

# Need base raster in lc CRC (and as rasterLayer)
# Mask (if landcover is incomplete or to adjust to border of prairies)
x_lc <- x %>%
  terra::project(., terra::crs(SECanadaLandCover)) %>%
  terra::mask(., 
              mask = SECanadaVectTemp %>%
                st_as_sf(.) %>%
                st_transform(., terra::crs(SECanadaLandCover)) %>%
                vect(.))

mat_10km <- matrix(1, ncol = 11, nrow = 11) # 10000/1000 = 10+1 to make it even


system.time(
  forest_1000_temp <- terra::aggregate(SECanadaLandCover,
                                       Nratio_step, 
                                       fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)}) 
)


# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(forest_10km <- terra::focal(forest_1000_temp, 
                                        w = mat_10km,
                                        fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
forest_10km_resampled <- terra::resample(forest_10km, 
                                         x_lc, 
                                         method = "bilinear")

# Project raster
forest_10km <- terra::project(x = forest_10km_resampled,
                              y = x)

# Before saving, make the name of the value correspond to the variable
names(forest_10km) <- "forest_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
forest_10km[is.na(forest_10km)] <- 0
forest_10km <- terra::mask(x = forest_10km, 
                           mask = SECanadaVectTemp %>% 
                             st_as_sf(.) %>%
                             vect(.))

plot(forest_10km)

forest_10km <- resample(forest_10km, forest_old, method = "near")
forest_10km
# Save results
writeRaster(forest_10km, 
            filename = paste0(output_dir, "forest_10km.tif"), 
            overwrite = TRUE)



# use the below function to clear memory if too full
#terra::tmpFiles(remove=TRUE)  # Remove temporary raster files
#rm(list=ls())  # Remove all objects from the work space
#gc()

# ---------------------------------------------------------------------------

# B. Shrubland

# codes = 2
system.time(
  shrubland_temp <- aggregate(SECanadaLandCover,
                              Nratio, 
                              
                              fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(shrubland_temp)

# Resample data to correct resolution and extent.
shrubland_resampled <- terra::resample(shrubland_temp, x_lc, method = "bilinear") 

# Before saving, make the name of the layer correspond to the variable
names(shrubland_resampled) <- "opforest"

# Need to mask landcover resampled rasters (so that 0 within mask, NA outside)
# Make NA values 0, then re-mask by study area outline
shrubland_resampled[is.na(shrubland_resampled)] <- 0
shrubland <- terra::mask(x = shrubland_resampled, 
                         mask = study_area %>%
                           st_transform(., terra::crs(shrubland_resampled)) %>%
                           vect(.))
# Plot layer
plot(shrubland)

shrubland <- resample(shrubland, forest_old, method = "near")

# Save results
writeRaster(shrubland, 
            filename = paste0(output_dir, "opforest_proportion_1km.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  shrubland_1000_temp <- terra::aggregate(SECanadaLandCover,
                                          Nratio_step, 
                                          fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)}) 
)


# Find area of grassland within 10000/1000 = 10 cells and add +1  = fact 10 + 1
system.time(shrubland_10km <- terra::focal(shrubland_1000_temp, 
                                           w = mat_10km,
                                           fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
shrubland_10km_resampled <- terra::resample(shrubland_10km, 
                                            x_lc, 
                                            method = "bilinear")

# Project raster
shrubland_10km <- terra::project(x = shrubland_10km_resampled,
                                 y = x)

# Before saving, make the name of the value correspond to the variable
names(shrubland_10km) <- "opforest_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
shrubland_10km[is.na(shrubland_10km)] <- 0
shrubland_10km <- terra::mask(x = shrubland_10km, 
                              mask = SECanadaVectTemp %>% 
                                st_as_sf(.) %>%
                                vect(.))

plot(shrubland_10km)

shrubland_10km <- resample(shrubland_10km, forest_old, method = "near")

# Save results
writeRaster(shrubland_10km, 
            filename = paste0(output_dir, "opforest_10km.tif"), 
            overwrite = TRUE)




# ---------------------------------------------------------------------------

# C. Wetlands

# Third, calculate the proportion of habitat per site for wetlands
system.time(
  SEwetlands_temp <- aggregate(SECanadaLandCover,
                               Nratio, 
                               
                               fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)})
)


plot(SEwetlands_temp)

# Resample data to correct resolution and extent.
SEwetlands_resampled <- terra::resample(SEwetlands_temp, x_lc, method = "bilinear") 

# Before saving, make the name of the layer correspond to the variable
names(SEwetlands_resampled) <- "wetland"

# Need to mask landcover resampled rasters (so that 0 within mask, NA outside)
# Make NA values 0, then re-mask by study area outline
SEwetlands_resampled[is.na(SEwetlands_resampled)] <- 0
SEwetlands <- terra::mask(x = SEwetlands_resampled, 
                          mask = study_area %>%
                            st_transform(., terra::crs(SEwetlands_resampled)) %>%
                            vect(.))
# Plot layer
plot(SEwetlands)

SEwetlands <- resample(SEwetlands, forest_old, method = "near")

# Save results
writeRaster(SEwetlands, 
            filename = paste0(output_dir, "wetlands_proportion_1km.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  SE_wetlands_1000_temp <- terra::aggregate(SECanadaLandCover,
                                            Nratio_step, 
                                            fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)}) 
)


# Find area of wetland within 10000/1000 = 10 cells and add +1  = fact 10 + 1
system.time(SE_wetlands_10km <- terra::focal(SE_wetlands_1000_temp, 
                                             w = mat_10km,
                                             fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
SE_wetlands_10km_resampled <- terra::resample(SE_wetlands_10km, 
                                              x_lc, 
                                              method = "bilinear")

# Project raster
SE_wetlands_10km <- terra::project(x = SE_wetlands_10km_resampled,
                                   y = x)

# Before saving, make the name of the value correspond to the variable
names(SE_wetlands_10km) <- "wetland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
SE_wetlands_10km[is.na(SE_wetlands_10km)] <- 0
SE_wetlands_10km <- terra::mask(x = SE_wetlands_10km, 
                                mask = SECanadaVectTemp %>% 
                                  st_as_sf(.) %>%
                                  vect(.))

plot(SE_wetlands_10km)

SEwetlands_10km <- resample(SE_wetlands_10km, forest_old, method = "near")

# Save results
writeRaster(SE_wetlands_10km, 
            filename = paste0(output_dir, "wetlands_10km.tif"), 
            overwrite = TRUE)



# ---------------------------------------------------------------------------

# D. Water
# Water categories = 6

# Third, calculate the proportion of habitat per site for wetlands
system.time(
  SEwater_temp <- aggregate(SECanadaLandCover,
                            Nratio, 
                            # Function sums the number of water body cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                            fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)})
)


plot(SEwater_temp)

# Resample data to correct resolution and extent.
SEwater_resampled <- terra::resample(SEwater_temp, x_lc, method = "bilinear") 

# Before saving, make the name of the layer correspond to the variable
names(SEwater_resampled) <- "water"

# Need to mask landcover resampled rasters (so that 0 within mask, NA outside)
# Make NA values 0, then re-mask by study area outline
SEwater_resampled[is.na(SEwater_resampled)] <- 0
SEwater <- terra::mask(x = SEwater_resampled, 
                       mask = study_area %>%
                         st_transform(., terra::crs(SEwater_resampled)) %>%
                         vect(.))
# Plot layer
plot(SEwater)

SEwater <- resample(SEwater, forest_old, method = "near")

# Save results
writeRaster(SEwater, 
            filename = paste0(output_dir, "water_proportion_1km.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  SE_freshwater_1000_temp <- terra::aggregate(SECanadaLandCover,
                                              Nratio_step, 
                                              fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)}) 
)


# Find area of grassland within 10000/1000 = 10 cells and add +1  = fact 10 + 1
system.time(SE_freshwater_10km <- terra::focal(SE_freshwater_1000_temp, 
                                               w = mat_10km,
                                               fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
SE_freshwater_10km_resampled <- terra::resample(SE_freshwater_10km, 
                                                x_lc, 
                                                method = "bilinear")

# Project raster
SE_freshwater_10km <- terra::project(x = SE_freshwater_10km_resampled,
                                     y = x)

# Before saving, make the name of the value correspond to the variable
names(SE_freshwater_10km) <- "water_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
SE_freshwater_10km[is.na(SE_freshwater_10km)] <- 0
SE_freshwater_10km <- terra::mask(x = SE_freshwater_10km, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(SE_freshwater_10km)

SE_freshwater_10km <- resample(SE_freshwater_10km, forest_old, method = "near")

# Save results
writeRaster(SE_freshwater_10km, 
            filename = paste0(output_dir, "water_10km.tif"), 
            overwrite = TRUE)


# ---------------------------------------------------------------------------

# E. built_up
# settlements codes = 5

system.time(
  SEsettlements_temp <- aggregate(SECanadaLandCover,
                                  Nratio, 
                                  # Function sums the number of forest cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                  fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(SEsettlements_temp)


# Resample data to correct resolution and extent.
SEsettlements_resampled <- terra::resample(SEsettlements_temp, x_lc, method = "bilinear") 

# Before saving, make the name of the layer correspond to the variable
names(SEsettlements_resampled) <- "built_up"

# Need to mask landcover resampled rasters (so that 0 within mask, NA outside)
# Make NA values 0, then re-mask by study area outline
SEsettlements_resampled[is.na(SEsettlements_resampled)] <- 0
SEsettlements <- terra::mask(x = SEsettlements_resampled, 
                             mask = study_area %>%
                               st_transform(., terra::crs(SEsettlements_resampled)) %>%
                               vect(.))
# Plot layer
plot(SEsettlements)

SEsettlements <- resample(SEsettlements, forest_old, method = "near")

# Save results
writeRaster(SEsettlements, 
            filename = paste0(output_dir, "built_up_proportion_1km.tif"), 
            overwrite = TRUE)


# Calculate proportion of habitat within 10 km  for settlement-----------------------------

system.time(
  SE_settlements_1000_temp <- terra::aggregate(SECanadaLandCover,
                                               Nratio_step, 
                                               fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)}) 
)


# Find area of grassland within 10000/1000 = 10 cells and add +1  = fact 10 + 1
system.time(SE_settlements_10km <- terra::focal(SE_settlements_1000_temp, 
                                                w = mat_10km,
                                                fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
SE_settlements_10km_resampled <- terra::resample(SE_settlements_10km, 
                                                 x_lc, 
                                                 method = "bilinear")

# Project raster
SE_settlements_10km <- terra::project(x = SE_settlements_10km_resampled,
                                      y = x)

# Before saving, make the name of the value correspond to the variable
names(SE_settlements_10km) <- "built_up_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
SE_settlements_10km[is.na(SE_settlements_10km)] <- 0
SE_settlements_10km <- terra::mask(x = SE_settlements_10km, 
                                   mask = SECanadaVectTemp %>% 
                                     st_as_sf(.) %>%
                                     vect(.))

plot(SE_settlements_10km)

SE_settlements_10km <- resample(SE_settlements_10km, forest_old, method = "near")

# Save results
writeRaster(SE_settlements_10km, 
            filename = paste0(output_dir, "built_up_10km.tif"), 
            overwrite = TRUE)



# ---------------------------------------------------------------------------

# F.  croplands
# cropland codes = 1
system.time(
  SEannual_croplands_temp <- aggregate(SECanadaLandCover,
                                       Nratio, 
                                       # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                       fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(SEannual_croplands_temp)

# Resample data to correct resolution and extent.
SEannual_croplands_resampled <- terra::resample(SEannual_croplands_temp, x_lc, method = "bilinear") 

# Before saving, make the name of the layer correspond to the variable
names(SEannual_croplands_resampled) <- "croplands"

# Need to mask landcover resampled rasters (so that 0 within mask, NA outside)
# Make NA values 0, then re-mask by study area outline
SEannual_croplands_resampled[is.na(SEannual_croplands_resampled)] <- 0
SEannual_croplands <- terra::mask(x = SEannual_croplands_resampled, 
                                  mask = study_area %>%
                                    st_transform(., terra::crs(SEannual_croplands_resampled)) %>%
                                    vect(.))
# Plot layer
plot(SEannual_croplands)

SEannual_croplands <- resample(SEannual_croplands, forest_old, method = "near")

# Save results
writeRaster(SEannual_croplands, 
            filename = paste0(output_dir, "croplands_proportion_1km.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  SE_croplands_1000_temp <- terra::aggregate(SECanadaLandCover,
                                             Nratio_step, 
                                             fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)}) 
)


# Find area of grassland within 10000/1000 = 10 cells and add +1  = fact 10 + 1
system.time(SE_croplands_10km <- terra::focal(SE_croplands_1000_temp, 
                                              w = mat_10km,
                                              fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
SE_croplands_10km_resampled <- terra::resample(SE_croplands_10km, 
                                               x_lc, 
                                               method = "bilinear")

# Project raster
SE_croplands_10km <- terra::project(x = SE_croplands_10km_resampled,
                                    y = x)

# Before saving, make the name of the value correspond to the variable
names(SE_croplands_10km) <- "croplands_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
SE_croplands_10km[is.na(SE_croplands_10km)] <- 0
SE_croplands_10km <- terra::mask(x = SE_croplands_10km, 
                                 mask = SECanadaVectTemp %>% 
                                   st_as_sf(.) %>%
                                   vect(.))

plot(SE_croplands_10km)

SE_croplands_10km <- resample(SE_croplands_10km, forest_old, method = "near")

# Save results
writeRaster(SE_croplands_10km, 
            filename = paste0(output_dir, "croplands_10km.tif"), 
            overwrite = TRUE)


# ---------------------------------------------------------------------------
# H. Barren areas
# Barren land = 4

# Calculate the proportion of habitat per site for barren lands
system.time(
  SEbarren_lands_temp <- aggregate(SECanadaLandCover,
                                   Nratio, 
                                   # Function sums the number of barren land cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                   fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(SEbarren_lands_temp)

# Resample data to correct resolution and extent.
SEbarren_lands_resampled <- terra::resample(SEbarren_lands_temp, x_lc, method = "bilinear") 

# Before saving, make the name of the layer correspond to the variable
names(SEbarren_lands_resampled) <- "barren"

# Need to mask landcover resampled rasters (so that 0 within mask, NA outside)
# Make NA values 0, then re-mask by study area outline
SEbarren_lands_resampled[is.na(SEbarren_lands_resampled)] <- 0
SEbarren_lands <- terra::mask(x = SEbarren_lands_resampled, 
                              mask = study_area %>%
                                st_transform(., terra::crs(SEbarren_lands_resampled)) %>%
                                vect(.))
# Plot layer
plot(SEbarren_lands)

SEbarren_lands <- resample(SEbarren_lands, forest_old, method = "near")

# Save results
writeRaster(SEbarren_lands, 
            filename = paste0(output_dir, "barren_lands_proportion_1km.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  SE_barren_1000_temp <- terra::aggregate(SECanadaLandCover,
                                          Nratio_step, 
                                          fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)}) 
)


# Find area of grassland within 10000/1000 = 10 cells and add +1  = fact 10 + 1
system.time(SE_barren_10km <- terra::focal(SE_barren_1000_temp, 
                                           w = mat_10km,
                                           fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells

# Resample at correct resolution
SE_barren_10km_resampled <- terra::resample(SE_barren_10km, 
                                            x_lc, 
                                            method = "bilinear")

# Project raster
SE_barren_10km <- terra::project(x = SE_barren_10km_resampled,
                                 y = x)

# Before saving, make the name of the value correspond to the variable
names(SE_barren_10km) <- "barren_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
SE_barren_10km[is.na(SE_barren_10km)] <- 0
SE_barren_10km <- terra::mask(x = SE_barren_10km, 
                              mask = SECanadaVectTemp %>% 
                                st_as_sf(.) %>%
                                vect(.))

plot(SE_barren_10km)

SE_barren_10km <- resample(SE_barren_10km, forest_old, method = "near")

# Save results
writeRaster(SE_barren_10km, 
            filename = paste0(output_dir, "barren_10km.tif"), 
            overwrite = TRUE)



# ---------------------------------------------------------------------------

# I. grassland
#  (category 3)

# Calculate the proportion of habitat per site for roads
system.time(
  grassland_temp <- aggregate(SECanadaLandCover,
                              Nratio, 
                              # Function sums the number of road cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                              fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(grassland_temp)

# Resample data to correct resolution and extent.
grassland_resampled <- terra::resample(grassland_temp, x_lc, method = "bilinear") 

# Before saving, make the name of the layer correspond to the variable
names(grassland_resampled) <- "grassland"

# Need to mask landcover resampled rasters (so that 0 within mask, NA outside)
# Make NA values 0, then re-mask by study area outline
grassland_resampled[is.na(grassland_resampled)] <- 0
grassland <- terra::mask(x = grassland_resampled, 
                         mask = study_area %>%
                           st_transform(., terra::crs(grassland_resampled)) %>%
                           vect(.))
# Plot layer
plot(grassland)

grassland <- resample(grassland, forest_old, method = "near")

# Save results
writeRaster(grassland, 
            filename = paste0(output_dir, "grassland_proportion_1km.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  grassland_1000_temp <- terra::aggregate(SECanadaLandCover,
                                          Nratio_step, 
                                          fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)}) 
)


# Find area of grassland within 10000/1000 = 10 cells and add +1  = fact 10 + 1
system.time(grassland_10km <- terra::focal(grassland_1000_temp, 
                                           w = mat_10km,
                                           fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
grassland_10km_resampled <- terra::resample(grassland_10km, 
                                            x_lc, 
                                            method = "bilinear")

# Project raster
grassland_10km <- terra::project(x = grassland_10km_resampled,
                                 y = x)

# Before saving, make the name of the value correspond to the variable
names(grassland_10km) <- "grassland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
grassland_10km[is.na(grassland_10km)] <- 0
grassland_10km <- terra::mask(x = grassland_10km, 
                              mask = SECanadaVectTemp %>% 
                                st_as_sf(.) %>%
                                vect(.))

plot(grassland_10km)

grassland_10km <- resample(grassland_10km, forest_old, method = "near")

# Save results
writeRaster(grassland_10km, 
            filename = paste0(output_dir, "grassland_10km.tif"), 
            overwrite = TRUE)



# --------------------------------End---------------------------------------




# -----proportion of habitat per site for future land cover--------------

#--------------------------------Grasslands-------------------------

# 2050s (2041 - 2070) and 2100 (2071 - 2100)

# Load land cover data to be analyzed

landcover_2030_ssp126 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2030_ssp126_stable.tif")
landcover_2030_ssp126
plot(landcover_2030_ssp126)
landcover_2030_ssp245 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2030_ssp245_stable.tif")
landcover_2030_ssp585 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2030_ssp585_stable.tif")

landcover_2050_ssp126 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2050_ssp126_stable.tif")
landcover_2050_ssp126
plot(landcover_2050_ssp126)
landcover_2050_ssp245 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2050_ssp245_stable.tif")
landcover_2050_ssp585 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2050_ssp585_stable.tif")

landcover_2070_ssp126 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2070_ssp126_stable.tif")
landcover_2070_ssp126
plot(landcover_2070_ssp126)
landcover_2070_ssp245 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2070_ssp245_stable.tif")
landcover_2070_ssp585 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2070_ssp585_stable.tif")

landcover_2100_ssp126 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2100_ssp126_stable.tif")
landcover_2100_ssp126
plot(landcover_2100_ssp126)
landcover_2100_ssp245 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2100_ssp245_stable.tif")
landcover_2100_ssp585 <- rast("F:/global_land_cover/zahng_et_al_20223/data_2020_2100/CB/stable1/LC2100_ssp585_stable.tif")


# Mask to the land cover data to the new stucy area (vector object) so that aggregate doesn't have to do so many cells
# use the terra crop and mask functions in a 2 step process

#  2030 ssp126

landcover_2030_ssp126_cropped <- terra::crop(x = landcover_2030_ssp126, 
                                             y = SECanadaVectTemp)
plot(landcover_2030_ssp126_cropped, main = "cropped")

landcover_2030_ssp126 <- terra::mask(x = landcover_2030_ssp126_cropped, 
                                     mask = SECanadaVectTemp)
plot(landcover_2030_ssp126, main = "masked")


#  2030 ssp245

landcover_2030_ssp245_cropped <- terra::crop(x = landcover_2030_ssp245, 
                                             y = SECanadaVectTemp)
plot(landcover_2030_ssp245_cropped, main = "cropped")

landcover_2030_ssp245 <- terra::mask(x = landcover_2030_ssp245_cropped, 
                                     mask = SECanadaVectTemp)
plot(landcover_2030_ssp245, main = "masked")


landcover_2030_ssp585_cropped <- terra::crop(x = landcover_2030_ssp585, 
                                             y = SECanadaVectTemp)
plot(landcover_2030_ssp585_cropped, main = "cropped")

landcover_2030_ssp585 <- terra::mask(x = landcover_2030_ssp585_cropped, 
                                     mask = SECanadaVectTemp)
plot(landcover_2030_ssp585, main = "masked")



#  2050 ssp126

landcover_2050_ssp126_cropped <- terra::crop(x = landcover_2050_ssp126, 
                                             y = SECanadaVectTemp)
plot(landcover_2050_ssp126_cropped, main = "cropped")

landcover_2050_ssp126 <- terra::mask(x = landcover_2050_ssp126_cropped, 
                                     mask = SECanadaVectTemp)
plot(landcover_2050_ssp126, main = "masked")


#  2050 ssp245

landcover_2050_ssp245_cropped <- terra::crop(x = landcover_2050_ssp245, 
                                             y = SECanadaVectTemp)
plot(landcover_2050_ssp245_cropped, main = "cropped")

landcover_2050_ssp245 <- terra::mask(x = landcover_2050_ssp245_cropped, 
                                     mask = SECanadaVectTemp)
plot(landcover_2050_ssp245, main = "masked")


#  2050 ssp585

landcover_2050_ssp585_cropped <- terra::crop(x = landcover_2050_ssp585, 
                                             y = SECanadaVectTemp)
plot(landcover_2050_ssp585_cropped, main = "cropped")

landcover_2050_ssp585 <- terra::mask(x = landcover_2050_ssp585_cropped, 
                                     mask = SECanadaVectTemp)
plot(landcover_2050_ssp585, main = "masked")


#  2070 ssp126

landcover_2070_ssp126_cropped <- terra::crop(x = landcover_2070_ssp126, 
                                             y = SECanadaVectTemp)
plot(landcover_2070_ssp126_cropped, main = "cropped")

landcover_2070_ssp126 <- terra::mask(x = landcover_2070_ssp126_cropped, 
                                     mask = SECanadaVectTemp)
plot(landcover_2070_ssp126, main = "masked")



#  2050 ssp245

landcover_2070_ssp245_cropped <- terra::crop(x = landcover_2070_ssp245, 
                                             y = SECanadaVectTemp)
plot(landcover_2070_ssp245_cropped, main = "cropped")

landcover_2070_ssp245 <- terra::mask(x = landcover_2070_ssp245_cropped, 
                                     mask = SECanadaVectTemp)
plot(landcover_2070_ssp245, main = "masked")



#  2070 ssp585

landcover_2070_ssp585_cropped <- terra::crop(x = landcover_2070_ssp585, 
                                             y = SECanadaVectTemp)
plot(landcover_2070_ssp585_cropped, main = "cropped")

landcover_2070_ssp585 <- terra::mask(x = landcover_2070_ssp585_cropped, 
                                     mask = SECanadaVectTemp)
plot(landcover_2070_ssp585, main = "masked")



#  2100 ssp126

landcover_2100_ssp126_cropped <- terra::crop(x = landcover_2100_ssp126, 
                                             y = SECanadaVectTemp)
plot(landcover_2100_ssp126_cropped, main = "cropped")

landcover_2100_ssp126 <- terra::mask(x = landcover_2100_ssp126_cropped, 
                                     mask = SECanadaVectTemp)
plot(landcover_2100_ssp126, main = "masked")



#  2100 ssp245

landcover_2100_ssp245_cropped <- terra::crop(x = landcover_2100_ssp245, 
                                             y = SECanadaVectTemp)
plot(landcover_2100_ssp245_cropped, main = "cropped")

landcover_2100_ssp245 <- terra::mask(x = landcover_2100_ssp245_cropped, 
                                     mask = SECanadaVectTemp)
plot(landcover_2100_ssp245, main = "masked")



#  2100 ssp585

landcover_2100_ssp585_cropped <- terra::crop(x = landcover_2100_ssp585, 
                                             y = SECanadaVectTemp)
plot(landcover_2100_ssp585_cropped, main = "cropped")

landcover_2100_ssp585 <- terra::mask(x = landcover_2100_ssp585_cropped, 
                                     mask = SECanadaVectTemp)
plot(landcover_2100_ssp585, main = "masked")



# -------------------proportion of habitat per site calculations--------------


# 2050 ssp126

# Now, calculate the proportion of habitat per site for forest
system.time(
  forest_temp_2050_ssp126 <- aggregate(landcover_2050_ssp126,
                                       Nratio, 
                                       # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                       fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(forest_temp_2050_ssp126 )
forest_temp_2050_ssp126

# Resample at correct resolution
forest_resampled_2050_ssp126 <- terra::resample(forest_temp_2050_ssp126, 
                                                x_lc, 
                                                method = "bilinear")

# Project raster
forest_2050_ssp126 <- terra::project(x = forest_resampled_2050_ssp126,
                                     y = x)

# Before saving, make the name of the value correspond to the variable
names(forest_2050_ssp126) <- "forest"
forest_2050_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
forest_2050_ssp126[is.na(forest_2050_ssp126)] <- 0
forest_2050_ssp126 <- terra::mask(x = forest_2050_ssp126, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(forest_2050_ssp126)

forest_2050_ssp126 <- resample(forest_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(forest_2050_ssp126, 
            filename = paste0(output_dir, "forest_proportion_1km_2050_ssp126.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  forest_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(forest_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(forest_10km_2050_ssp126 <- terra::focal(forest_1000_temp_2050_ssp126, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
forest_10km_resampled_2050_ssp126 <- terra::resample(forest_10km_2050_ssp126, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
forest_10km_2050_ssp126 <- terra::project(x = forest_10km_resampled_2050_ssp126,
                                          y = x)

# Before saving, make the name of the value correspond to the variable
names(forest_10km_2050_ssp126) <- "forest_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
forest_10km_2050_ssp126[is.na(forest_10km_2050_ssp126)] <- 0
forest_10km_2050_ssp126 <- terra::mask(x = forest_10km_2050_ssp126, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(forest_10km_2050_ssp126)

forest_10km_2050_ssp126 <- resample(forest_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(forest_10km_2050_ssp126, 
            filename = paste0(output_dir, "forest_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# shrublands

system.time(
  shrublands_temp_2050_ssp126 <- aggregate(landcover_2050_ssp126,
                                           Nratio, 
                                           # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                           fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(shrublands_temp_2050_ssp126 )
shrublands_temp_2050_ssp126

# Resample at correct resolution
shrublands_resampled_2050_ssp126 <- terra::resample(shrublands_temp_2050_ssp126, 
                                                    x_lc, 
                                                    method = "bilinear")

# Project raster
shrublands_2050_ssp126 <- terra::project(x = shrublands_resampled_2050_ssp126,
                                         y = x)

# Before saving, make the name of the value correspond to the variable
names(shrublands_2050_ssp126) <- "opforest"
shrublands_2050_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
shrublands_2050_ssp126[is.na(shrublands_2050_ssp126)] <- 0
shrublands_2050_ssp126 <- terra::mask(x = shrublands_2050_ssp126, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(shrublands_2050_ssp126)

shrublands_2050_ssp126 <- resample(shrublands_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(shrublands_2050_ssp126, 
            filename = paste0(output_dir, "opforest_proportion_1km_2050_ssp126.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  shrublands_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                       Nratio_step, 
                                                       fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(shrublands_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(shrublands_10km_2050_ssp126 <- terra::focal(shrublands_1000_temp_2050_ssp126, 
                                                        w = mat_10km,
                                                        fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
shrublands_10km_resampled_2050_ssp126 <- terra::resample(shrublands_10km_2050_ssp126, 
                                                         x_lc, 
                                                         method = "bilinear")

# Project raster
shrublands_10km_2050_ssp126 <- terra::project(x = shrublands_10km_resampled_2050_ssp126,
                                              y = x)

# Before saving, make the name of the value correspond to the variable
names(shrublands_10km_2050_ssp126) <- "opforest_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
shrublands_10km_2050_ssp126[is.na(shrublands_10km_2050_ssp126)] <- 0
shrublands_10km_2050_ssp126 <- terra::mask(x = shrublands_10km_2050_ssp126, 
                                           mask = SECanadaVectTemp %>% 
                                             st_as_sf(.) %>%
                                             vect(.))

plot(shrublands_10km_2050_ssp126)

shrublands_10km_2050_ssp126 <- resample(shrublands_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(shrublands_10km_2050_ssp126, 
            filename = paste0(output_dir, "opforest_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# grassland

system.time(
  grassland_temp_2050_ssp126 <- aggregate(landcover_2050_ssp126,
                                          Nratio, 
                                          # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                          fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(grassland_temp_2050_ssp126 )
grassland_temp_2050_ssp126

# Resample at correct resolution
grassland_resampled_2050_ssp126 <- terra::resample(grassland_temp_2050_ssp126, 
                                                   x_lc, 
                                                   method = "bilinear")

# Project raster
grassland_2050_ssp126 <- terra::project(x = grassland_resampled_2050_ssp126,
                                        y = x)

# Before saving, make the name of the value correspond to the variable
names(grassland_2050_ssp126) <- "grassland"
grassland_2050_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
grassland_2050_ssp126[is.na(grassland_2050_ssp126)] <- 0
grassland_2050_ssp126 <- terra::mask(x = grassland_2050_ssp126, 
                                     mask = SECanadaVectTemp %>% 
                                       st_as_sf(.) %>%
                                       vect(.))

plot(grassland_2050_ssp126)

grassland_2050_ssp126 <- resample(grassland_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(grassland_2050_ssp126, 
            filename = paste0(output_dir, "grassland_proportion_1km_2050_ssp126.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  grassland_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                      Nratio_step, 
                                                      fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(grassland_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(grassland_10km_2050_ssp126 <- terra::focal(grassland_1000_temp_2050_ssp126, 
                                                       w = mat_10km,
                                                       fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
grassland_10km_resampled_2050_ssp126 <- terra::resample(grassland_10km_2050_ssp126, 
                                                        x_lc, 
                                                        method = "bilinear")

# Project raster
grassland_10km_2050_ssp126 <- terra::project(x = grassland_10km_resampled_2050_ssp126,
                                             y = x)

# Before saving, make the name of the value correspond to the variable
names(grassland_10km_2050_ssp126) <- "grassland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
grassland_10km_2050_ssp126[is.na(grassland_10km_2050_ssp126)] <- 0
grassland_10km_2050_ssp126 <- terra::mask(x = grassland_10km_2050_ssp126, 
                                          mask = SECanadaVectTemp %>% 
                                            st_as_sf(.) %>%
                                            vect(.))

plot(grassland_10km_2050_ssp126)

grassland_10km_2050_ssp126 <- resample(grassland_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(grassland_10km_2050_ssp126, 
            filename = paste0(output_dir, "grassland_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# wetland

system.time(
  wetland_temp_2050_ssp126 <- aggregate(landcover_2050_ssp126,
                                        Nratio, 
                                        # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                        fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(wetland_temp_2050_ssp126 )
wetland_temp_2050_ssp126

# Resample at correct resolution
wetland_resampled_2050_ssp126 <- terra::resample(wetland_temp_2050_ssp126, 
                                                 x_lc, 
                                                 method = "bilinear")

# Project raster
wetland_2050_ssp126 <- terra::project(x = wetland_resampled_2050_ssp126,
                                      y = x)

# Before saving, make the name of the value correspond to the variable
names(wetland_2050_ssp126) <- "wetland"
wetland_2050_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
wetland_2050_ssp126[is.na(wetland_2050_ssp126)] <- 0
wetland_2050_ssp126 <- terra::mask(x = wetland_2050_ssp126, 
                                   mask = SECanadaVectTemp %>% 
                                     st_as_sf(.) %>%
                                     vect(.))

plot(wetland_2050_ssp126)
wetland_2050_ssp126 <- resample(wetland_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(wetland_2050_ssp126, 
            filename = paste0(output_dir, "wetland_proportion_1km_2050_ssp126.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  wetland_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                    Nratio_step, 
                                                    fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(wetland_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(wetland_10km_2050_ssp126 <- terra::focal(wetland_1000_temp_2050_ssp126, 
                                                     w = mat_10km,
                                                     fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
wetland_10km_resampled_2050_ssp126 <- terra::resample(wetland_10km_2050_ssp126, 
                                                      x_lc, 
                                                      method = "bilinear")

# Project raster
wetland_10km_2050_ssp126 <- terra::project(x = wetland_10km_resampled_2050_ssp126,
                                           y = x)

# Before saving, make the name of the value correspond to the variable
names(wetland_10km_2050_ssp126) <- "wetland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
wetland_10km_2050_ssp126[is.na(wetland_10km_2050_ssp126)] <- 0
wetland_10km_2050_ssp126 <- terra::mask(x = wetland_10km_2050_ssp126, 
                                        mask = SECanadaVectTemp %>% 
                                          st_as_sf(.) %>%
                                          vect(.))

plot(wetland_10km_2050_ssp126)

wetland_10km_2050_ssp126 <- resample(wetland_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(wetland_10km_2050_ssp126, 
            filename = paste0(output_dir, "wetland_10km_2050_ssp126.tif"), 
            overwrite = TRUE)




# cropland

system.time(
  cropland_temp_2050_ssp126 <- aggregate(landcover_2050_ssp126,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(cropland_temp_2050_ssp126 )
cropland_temp_2050_ssp126

# Resample at correct resolution
cropland_resampled_2050_ssp126 <- terra::resample(cropland_temp_2050_ssp126, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
cropland_2050_ssp126 <- terra::project(x = cropland_resampled_2050_ssp126,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(cropland_2050_ssp126) <- "cropland"
cropland_2050_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
cropland_2050_ssp126[is.na(cropland_2050_ssp126)] <- 0
cropland_2050_ssp126 <- terra::mask(x = cropland_2050_ssp126, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(cropland_2050_ssp126)
cropland_2050_ssp126 <- resample(cropland_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(cropland_2050_ssp126, 
            filename = paste0(output_dir, "cropland_proportion_1km_2050_ssp126.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  cropland_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(cropland_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(cropland_10km_2050_ssp126 <- terra::focal(cropland_1000_temp_2050_ssp126, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
cropland_10km_resampled_2050_ssp126 <- terra::resample(cropland_10km_2050_ssp126, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
cropland_10km_2050_ssp126 <- terra::project(x = cropland_10km_resampled_2050_ssp126,
                                            y = x)

# Before saving, make the name of the value correspond to the variable
names(cropland_10km_2050_ssp126) <- "cropland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
cropland_10km_2050_ssp126[is.na(cropland_10km_2050_ssp126)] <- 0
cropland_10km_2050_ssp126 <- terra::mask(x = cropland_10km_2050_ssp126, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(cropland_10km_2050_ssp126)

cropland_10km_2050_ssp126 <- resample(cropland_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(cropland_10km_2050_ssp126, 
            filename = paste0(output_dir, "cropland_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# barren

system.time(
  barren_temp_2050_ssp126 <- aggregate(landcover_2050_ssp126,
                                       Nratio, 
                                       # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                       fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(barren_temp_2050_ssp126 )
barren_temp_2050_ssp126

# Resample at correct resolution
barren_resampled_2050_ssp126 <- terra::resample(barren_temp_2050_ssp126, 
                                                x_lc, 
                                                method = "bilinear")

# Project raster
barren_2050_ssp126 <- terra::project(x = barren_resampled_2050_ssp126,
                                     y = x)

# Before saving, make the name of the value correspond to the variable
names(barren_2050_ssp126) <- "barren"
barren_2050_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
barren_2050_ssp126[is.na(barren_2050_ssp126)] <- 0
barren_2050_ssp126 <- terra::mask(x = barren_2050_ssp126, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(barren_2050_ssp126)
barren_2050_ssp126 <- resample(barren_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(barren_2050_ssp126, 
            filename = paste0(output_dir, "barren_proportion_1km_2050_ssp126.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  barren_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(barren_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(barren_10km_2050_ssp126 <- terra::focal(barren_1000_temp_2050_ssp126, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells


# Resample at correct resolution
barren_10km_resampled_2050_ssp126 <- terra::resample(barren_10km_2050_ssp126, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
barren_10km_2050_ssp126 <- terra::project(x = barren_10km_resampled_2050_ssp126,
                                          y = x)

# Before saving, make the name of the value correspond to the variable
names(barren_10km_2050_ssp126) <- "barren_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
barren_10km_2050_ssp126[is.na(barren_10km_2050_ssp126)] <- 0
barren_10km_2050_ssp126 <- terra::mask(x = barren_10km_2050_ssp126, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(barren_10km_2050_ssp126)

barren_10km_2050_ssp126 <- resample(barren_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(barren_10km_2050_ssp126, 
            filename = paste0(output_dir, "barren_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# built_up

system.time(
  built_up_temp_2050_ssp126 <- aggregate(landcover_2050_ssp126,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(built_up_temp_2050_ssp126 )
built_up_temp_2050_ssp126

# Resample at correct resolution
built_up_resampled_2050_ssp126 <- terra::resample(built_up_temp_2050_ssp126, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
built_up_2050_ssp126 <- terra::project(x = built_up_resampled_2050_ssp126,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(built_up_2050_ssp126) <- "built_up"
built_up_2050_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
built_up_2050_ssp126[is.na(built_up_2050_ssp126)] <- 0
built_up_2050_ssp126 <- terra::mask(x = built_up_2050_ssp126, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(built_up_2050_ssp126)
built_up_2050_ssp126 <- resample(built_up_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(built_up_2050_ssp126, 
            filename = paste0(output_dir, "built_up_proportion_1km_2050_ssp126.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  built_up_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(built_up_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(built_up_10km_2050_ssp126 <- terra::focal(built_up_1000_temp_2050_ssp126, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
built_up_10km_resampled_2050_ssp126 <- terra::resample(built_up_10km_2050_ssp126, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
built_up_10km_2050_ssp126 <- terra::project(x = built_up_10km_resampled_2050_ssp126,
                                            y = x)

# Before saving, make the name of the value correspond to the variable
names(built_up_10km_2050_ssp126) <- "built_up_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
built_up_10km_2050_ssp126[is.na(built_up_10km_2050_ssp126)] <- 0
built_up_10km_2050_ssp126 <- terra::mask(x = built_up_10km_2050_ssp126, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(built_up_10km_2050_ssp126)

built_up_10km_2050_ssp126 <- resample(built_up_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(built_up_10km_2050_ssp126, 
            filename = paste0(output_dir, "built_up_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# water

system.time(
  water_temp_2050_ssp126 <- aggregate(landcover_2050_ssp126,
                                      Nratio, 
                                      # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                      fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(water_temp_2050_ssp126 )
water_temp_2050_ssp126

# Resample at correct resolution
water_resampled_2050_ssp126 <- terra::resample(water_temp_2050_ssp126, 
                                               x_lc, 
                                               method = "bilinear")

# Project raster
water_2050_ssp126 <- terra::project(x = water_resampled_2050_ssp126,
                                    y = x)

# Before saving, make the name of the value correspond to the variable
names(water_2050_ssp126) <- "water"
water_2050_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
water_2050_ssp126[is.na(water_2050_ssp126)] <- 0
water_2050_ssp126 <- terra::mask(x = water_2050_ssp126, 
                                 mask = SECanadaVectTemp %>% 
                                   st_as_sf(.) %>%
                                   vect(.))

plot(water_2050_ssp126)
water_2050_ssp126 <- resample(water_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(water_2050_ssp126, 
            filename = paste0(output_dir, "water_proportion_1km_2050_ssp126.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  water_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                  Nratio_step, 
                                                  fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(water_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(water_10km_2050_ssp126 <- terra::focal(water_1000_temp_2050_ssp126, 
                                                   w = mat_10km,
                                                   fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
water_10km_resampled_2050_ssp126 <- terra::resample(water_10km_2050_ssp126, 
                                                    x_lc, 
                                                    method = "bilinear")

# Project raster
water_10km_2050_ssp126 <- terra::project(x = water_10km_resampled_2050_ssp126,
                                         y = x)

# Before saving, make the name of the value correspond to the variable
names(water_10km_2050_ssp126) <- "water_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
water_10km_2050_ssp126[is.na(water_10km_2050_ssp126)] <- 0
water_10km_2050_ssp126 <- terra::mask(x = water_10km_2050_ssp126, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(water_10km_2050_ssp126)

water_10km_2050_ssp126 <- resample(water_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(water_10km_2050_ssp126, 
            filename = paste0(output_dir, "water_10km_2050_ssp126.tif"), 
            overwrite = TRUE)




# 2050 ssp245

# Now, calculate the proportion of habitat per site for forest
system.time(
  forest_temp_2050_ssp245 <- aggregate(landcover_2050_ssp245,
                                       Nratio, 
                                       # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                       fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(forest_temp_2050_ssp245 )
forest_temp_2050_ssp245

# Resample at correct resolution
forest_resampled_2050_ssp245 <- terra::resample(forest_temp_2050_ssp245, 
                                                x_lc, 
                                                method = "bilinear")

# Project raster
forest_2050_ssp245 <- terra::project(x = forest_resampled_2050_ssp245,
                                     y = x)

# Before saving, make the name of the value correspond to the variable
names(forest_2050_ssp245) <- "forest"
forest_2050_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
forest_2050_ssp245[is.na(forest_2050_ssp245)] <- 0
forest_2050_ssp245 <- terra::mask(x = forest_2050_ssp245, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(forest_2050_ssp245)

forest_2050_ssp245 <- resample(forest_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(forest_2050_ssp245, 
            filename = paste0(output_dir, "forest_proportion_1km_2050_ssp245.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  forest_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(forest_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(forest_10km_2050_ssp245 <- terra::focal(forest_1000_temp_2050_ssp245, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
forest_10km_resampled_2050_ssp245 <- terra::resample(forest_10km_2050_ssp245, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
forest_10km_2050_ssp245 <- terra::project(x = forest_10km_resampled_2050_ssp245,
                                          y = x)

# Before saving, make the name of the value correspond to the variable
names(forest_10km_2050_ssp245) <- "forest_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
forest_10km_2050_ssp245[is.na(forest_10km_2050_ssp245)] <- 0
forest_10km_2050_ssp245 <- terra::mask(x = forest_10km_2050_ssp245, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(forest_10km_2050_ssp245)

forest_10km_2050_ssp245 <- resample(forest_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(forest_10km_2050_ssp245, 
            filename = paste0(output_dir, "forest_10km_2050_ssp245.tif"), 
            overwrite = TRUE)



# shrublands

system.time(
  shrublands_temp_2050_ssp245 <- aggregate(landcover_2050_ssp245,
                                           Nratio, 
                                           # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                           fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(shrublands_temp_2050_ssp245 )
shrublands_temp_2050_ssp245

# Resample at correct resolution
shrublands_resampled_2050_ssp245 <- terra::resample(shrublands_temp_2050_ssp245, 
                                                    x_lc, 
                                                    method = "bilinear")

# Project raster
shrublands_2050_ssp245 <- terra::project(x = shrublands_resampled_2050_ssp245,
                                         y = x)

# Before saving, make the name of the value correspond to the variable
names(shrublands_2050_ssp245) <- "opforest"
shrublands_2050_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
shrublands_2050_ssp245[is.na(shrublands_2050_ssp245)] <- 0
shrublands_2050_ssp245 <- terra::mask(x = shrublands_2050_ssp245, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(shrublands_2050_ssp245)

shrublands_2050_ssp245 <- resample(shrublands_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(shrublands_2050_ssp245, 
            filename = paste0(output_dir, "opforest_proportion_1km_2050_ssp245.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  shrublands_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                       Nratio_step, 
                                                       fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(shrublands_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(shrublands_10km_2050_ssp245 <- terra::focal(shrublands_1000_temp_2050_ssp245, 
                                                        w = mat_10km,
                                                        fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
shrublands_10km_resampled_2050_ssp245 <- terra::resample(shrublands_10km_2050_ssp245, 
                                                         x_lc, 
                                                         method = "bilinear")

# Project raster
shrublands_10km_2050_ssp245 <- terra::project(x = shrublands_10km_resampled_2050_ssp245,
                                              y = x)

# Before saving, make the name of the value correspond to the variable
names(shrublands_10km_2050_ssp245) <- "opforest_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
shrublands_10km_2050_ssp245[is.na(shrublands_10km_2050_ssp245)] <- 0
shrublands_10km_2050_ssp245 <- terra::mask(x = shrublands_10km_2050_ssp245, 
                                           mask = SECanadaVectTemp %>% 
                                             st_as_sf(.) %>%
                                             vect(.))

plot(shrublands_10km_2050_ssp245)

shrublands_10km_2050_ssp245 <- resample(shrublands_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(shrublands_10km_2050_ssp245, 
            filename = paste0(output_dir, "opforest_10km_2050_ssp245.tif"), 
            overwrite = TRUE)


# grassland

system.time(
  grassland_temp_2050_ssp245 <- aggregate(landcover_2050_ssp245,
                                          Nratio, 
                                          # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                          fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(grassland_temp_2050_ssp245 )
grassland_temp_2050_ssp245

# Resample at correct resolution
grassland_resampled_2050_ssp245 <- terra::resample(grassland_temp_2050_ssp245, 
                                                   x_lc, 
                                                   method = "bilinear")

# Project raster
grassland_2050_ssp245 <- terra::project(x = grassland_resampled_2050_ssp245,
                                        y = x)

# Before saving, make the name of the value correspond to the variable
names(grassland_2050_ssp245) <- "grassland"
grassland_2050_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
grassland_2050_ssp245[is.na(grassland_2050_ssp245)] <- 0
grassland_2050_ssp245 <- terra::mask(x = grassland_2050_ssp245, 
                                     mask = SECanadaVectTemp %>% 
                                       st_as_sf(.) %>%
                                       vect(.))

plot(grassland_2050_ssp245)

grassland_2050_ssp245 <- resample(grassland_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(grassland_2050_ssp245, 
            filename = paste0(output_dir, "grassland_proportion_1km_2050_ssp245.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  grassland_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                      Nratio_step, 
                                                      fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(grassland_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(grassland_10km_2050_ssp245 <- terra::focal(grassland_1000_temp_2050_ssp245, 
                                                       w = mat_10km,
                                                       fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
grassland_10km_resampled_2050_ssp245 <- terra::resample(grassland_10km_2050_ssp245, 
                                                        x_lc, 
                                                        method = "bilinear")

# Project raster
grassland_10km_2050_ssp245 <- terra::project(x = grassland_10km_resampled_2050_ssp245,
                                             y = x)

# Before saving, make the name of the value correspond to the variable
names(grassland_10km_2050_ssp245) <- "grassland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
grassland_10km_2050_ssp245[is.na(grassland_10km_2050_ssp245)] <- 0
grassland_10km_2050_ssp245 <- terra::mask(x = grassland_10km_2050_ssp245, 
                                          mask = SECanadaVectTemp %>% 
                                            st_as_sf(.) %>%
                                            vect(.))

plot(grassland_10km_2050_ssp245)

grassland_10km_2050_ssp245 <- resample(grassland_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(grassland_10km_2050_ssp245, 
            filename = paste0(output_dir, "grassland_10km_2050_ssp245.tif"), 
            overwrite = TRUE)


# wetland

system.time(
  wetland_temp_2050_ssp245 <- aggregate(landcover_2050_ssp245,
                                        Nratio, 
                                        # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                        fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(wetland_temp_2050_ssp245 )
wetland_temp_2050_ssp245

# Resample at correct resolution
wetland_resampled_2050_ssp245 <- terra::resample(wetland_temp_2050_ssp245, 
                                                 x_lc, 
                                                 method = "bilinear")

# Project raster
wetland_2050_ssp245 <- terra::project(x = wetland_resampled_2050_ssp245,
                                      y = x)

# Before saving, make the name of the value correspond to the variable
names(wetland_2050_ssp245) <- "wetland"
wetland_2050_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
wetland_2050_ssp245[is.na(wetland_2050_ssp245)] <- 0
wetland_2050_ssp245 <- terra::mask(x = wetland_2050_ssp245, 
                                   mask = SECanadaVectTemp %>% 
                                     st_as_sf(.) %>%
                                     vect(.))

plot(wetland_2050_ssp245)
wetland_2050_ssp245 <- resample(wetland_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(wetland_2050_ssp245, 
            filename = paste0(output_dir, "wetland_proportion_1km_2050_ssp245.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  wetland_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                    Nratio_step, 
                                                    fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(wetland_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(wetland_10km_2050_ssp245 <- terra::focal(wetland_1000_temp_2050_ssp245, 
                                                     w = mat_10km,
                                                     fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
wetland_10km_resampled_2050_ssp245 <- terra::resample(wetland_10km_2050_ssp245, 
                                                      x_lc, 
                                                      method = "bilinear")

# Project raster
wetland_10km_2050_ssp245 <- terra::project(x = wetland_10km_resampled_2050_ssp245,
                                           y = x)

# Before saving, make the name of the value correspond to the variable
names(wetland_10km_2050_ssp245) <- "wetland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
wetland_10km_2050_ssp245[is.na(wetland_10km_2050_ssp245)] <- 0
wetland_10km_2050_ssp245 <- terra::mask(x = wetland_10km_2050_ssp245, 
                                        mask = SECanadaVectTemp %>% 
                                          st_as_sf(.) %>%
                                          vect(.))

plot(wetland_10km_2050_ssp245)

wetland_10km_2050_ssp245 <- resample(wetland_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(wetland_10km_2050_ssp245, 
            filename = paste0(output_dir, "wetland_10km_2050_ssp245.tif"), 
            overwrite = TRUE)


# cropland

system.time(
  cropland_temp_2050_ssp245 <- aggregate(landcover_2050_ssp245,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(cropland_temp_2050_ssp245 )
cropland_temp_2050_ssp245

# Resample at correct resolution
cropland_resampled_2050_ssp245 <- terra::resample(cropland_temp_2050_ssp245, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
cropland_2050_ssp245 <- terra::project(x = cropland_resampled_2050_ssp245,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(cropland_2050_ssp245) <- "cropland"
cropland_2050_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
cropland_2050_ssp245[is.na(cropland_2050_ssp245)] <- 0
cropland_2050_ssp245 <- terra::mask(x = cropland_2050_ssp245, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(cropland_2050_ssp245)
cropland_2050_ssp245 <- resample(cropland_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(cropland_2050_ssp245, 
            filename = paste0(output_dir, "cropland_proportion_1km_2050_ssp245.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  cropland_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(cropland_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(cropland_10km_2050_ssp245 <- terra::focal(cropland_1000_temp_2050_ssp245, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
cropland_10km_resampled_2050_ssp245 <- terra::resample(cropland_10km_2050_ssp245, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
cropland_10km_2050_ssp245 <- terra::project(x = cropland_10km_resampled_2050_ssp245,
                                            y = x)

# Before saving, make the name of the value correspond to the variable
names(cropland_10km_2050_ssp245) <- "cropland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
cropland_10km_2050_ssp245[is.na(cropland_10km_2050_ssp245)] <- 0
cropland_10km_2050_ssp245 <- terra::mask(x = cropland_10km_2050_ssp245, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(cropland_10km_2050_ssp245)

cropland_10km_2050_ssp245 <- resample(cropland_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(cropland_10km_2050_ssp245, 
            filename = paste0(output_dir, "cropland_10km_2050_ssp245.tif"), 
            overwrite = TRUE)


# barren

system.time(
  barren_temp_2050_ssp245 <- aggregate(landcover_2050_ssp245,
                                       Nratio, 
                                       # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                       fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(barren_temp_2050_ssp245 )
barren_temp_2050_ssp245

# Resample at correct resolution
barren_resampled_2050_ssp245 <- terra::resample(barren_temp_2050_ssp245, 
                                                x_lc, 
                                                method = "bilinear")

# Project raster
barren_2050_ssp245 <- terra::project(x = barren_resampled_2050_ssp245,
                                     y = x)

# Before saving, make the name of the value correspond to the variable
names(barren_2050_ssp245) <- "barren"
barren_2050_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
barren_2050_ssp245[is.na(barren_2050_ssp245)] <- 0
barren_2050_ssp245 <- terra::mask(x = barren_2050_ssp245, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(barren_2050_ssp245)
barren_2050_ssp245 <- resample(barren_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(barren_2050_ssp245, 
            filename = paste0(output_dir, "barren_proportion_1km_2050_ssp245.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  barren_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(barren_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(barren_10km_2050_ssp126 <- terra::focal(barren_1000_temp_2050_ssp126, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells


# Resample at correct resolution
barren_10km_resampled_2050_ssp126 <- terra::resample(barren_10km_2050_ssp126, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
barren_10km_2050_ssp126 <- terra::project(x = barren_10km_resampled_2050_ssp126,
                                          y = x)

# Before saving, make the name of the value correspond to the variable
names(barren_10km_2050_ssp126) <- "barren_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
barren_10km_2050_ssp126[is.na(barren_10km_2050_ssp126)] <- 0
barren_10km_2050_ssp126 <- terra::mask(x = barren_10km_2050_ssp126, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(barren_10km_2050_ssp126)

barren_10km_2050_ssp126 <- resample(barren_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(barren_10km_2050_ssp126, 
            filename = paste0(output_dir, "barren_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# built_up

system.time(
  built_up_temp_2050_ssp245 <- aggregate(landcover_2050_ssp245,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(built_up_temp_2050_ssp245 )
built_up_temp_2050_ssp245

# Resample at correct resolution
built_up_resampled_2050_ssp245 <- terra::resample(built_up_temp_2050_ssp245, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
built_up_2050_ssp245 <- terra::project(x = built_up_resampled_2050_ssp245,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(built_up_2050_ssp245) <- "built_up"
built_up_2050_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
built_up_2050_ssp245[is.na(built_up_2050_ssp245)] <- 0
built_up_2050_ssp245 <- terra::mask(x = built_up_2050_ssp245, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(built_up_2050_ssp245)
built_up_2050_ssp245 <- resample(built_up_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(built_up_2050_ssp245, 
            filename = paste0(output_dir, "built_up_proportion_1km_2050_ssp245.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  built_up_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(built_up_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(built_up_10km_2050_ssp126 <- terra::focal(built_up_1000_temp_2050_ssp126, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
built_up_10km_resampled_2050_ssp126 <- terra::resample(built_up_10km_2050_ssp126, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
built_up_10km_2050_ssp126 <- terra::project(x = built_up_10km_resampled_2050_ssp126,
                                            y = x)

# Before saving, make the name of the value correspond to the variable
names(built_up_10km_2050_ssp126) <- "built_up_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
built_up_10km_2050_ssp126[is.na(built_up_10km_2050_ssp126)] <- 0
built_up_10km_2050_ssp126 <- terra::mask(x = built_up_10km_2050_ssp126, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(built_up_10km_2050_ssp126)

built_up_10km_2050_ssp126 <- resample(built_up_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(built_up_10km_2050_ssp126, 
            filename = paste0(output_dir, "built_up_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# water

system.time(
  water_temp_2050_ssp245 <- aggregate(landcover_2050_ssp245,
                                      Nratio, 
                                      # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                      fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(water_temp_2050_ssp245 )
water_temp_2050_ssp245

# Resample at correct resolution
water_resampled_2050_ssp245 <- terra::resample(water_temp_2050_ssp245, 
                                               x_lc, 
                                               method = "bilinear")

# Project raster
water_2050_ssp245 <- terra::project(x = water_resampled_2050_ssp245,
                                    y = x)

# Before saving, make the name of the value correspond to the variable
names(water_2050_ssp245) <- "water"
water_2050_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
water_2050_ssp245[is.na(water_2050_ssp245)] <- 0
water_2050_ssp245 <- terra::mask(x = water_2050_ssp245, 
                                 mask = SECanadaVectTemp %>% 
                                   st_as_sf(.) %>%
                                   vect(.))

plot(water_2050_ssp245)
water_2050_ssp245 <- resample(water_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(water_2050_ssp245, 
            filename = paste0(output_dir, "water_proportion_1km_2050_ssp245.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  water_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                  Nratio_step, 
                                                  fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(water_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(water_10km_2050_ssp126 <- terra::focal(water_1000_temp_2050_ssp126, 
                                                   w = mat_10km,
                                                   fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
water_10km_resampled_2050_ssp126 <- terra::resample(water_10km_2050_ssp126, 
                                                    x_lc, 
                                                    method = "bilinear")

# Project raster
water_10km_2050_ssp126 <- terra::project(x = water_10km_resampled_2050_ssp126,
                                         y = x)

# Before saving, make the name of the value correspond to the variable
names(water_10km_2050_ssp126) <- "water_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
water_10km_2050_ssp126[is.na(water_10km_2050_ssp126)] <- 0
water_10km_2050_ssp126 <- terra::mask(x = water_10km_2050_ssp126, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(water_10km_2050_ssp126)

water_10km_2050_ssp126 <- resample(water_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(water_10km_2050_ssp126, 
            filename = paste0(output_dir, "water_10km_2050_ssp126.tif"), 
            overwrite = TRUE)





# 2050 ssp585

# Now, calculate the proportion of habitat per site for forest
system.time(
  forest_temp_2050_ssp585 <- aggregate(landcover_2050_ssp585,
                                       Nratio, 
                                       # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                       fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(forest_temp_2050_ssp585 )
forest_temp_2050_ssp585

# Resample at correct resolution
forest_resampled_2050_ssp585 <- terra::resample(forest_temp_2050_ssp585, 
                                                x_lc, 
                                                method = "bilinear")

# Project raster
forest_2050_ssp585 <- terra::project(x = forest_resampled_2050_ssp585,
                                     y = x)

# Before saving, make the name of the value correspond to the variable
names(forest_2050_ssp585) <- "forest"
forest_2050_ssp585

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
forest_2050_ssp585[is.na(forest_2050_ssp585)] <- 0
forest_2050_ssp585 <- terra::mask(x = forest_2050_ssp585, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(forest_2050_ssp585)

forest_2050_ssp585 <- resample(forest_2050_ssp585, forest_old, method = "near")

# Save results
writeRaster(forest_2050_ssp585, 
            filename = paste0(output_dir, "forest_proportion_1km_2050_ssp585.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  forest_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(forest_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(forest_10km_2050_ssp245 <- terra::focal(forest_1000_temp_2050_ssp245, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
forest_10km_resampled_2050_ssp245 <- terra::resample(forest_10km_2050_ssp245, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
forest_10km_2050_ssp245 <- terra::project(x = forest_10km_resampled_2050_ssp245,
                                          y = x)

# Before saving, make the name of the value correspond to the variable
names(forest_10km_2050_ssp245) <- "forest_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
forest_10km_2050_ssp245[is.na(forest_10km_2050_ssp245)] <- 0
forest_10km_2050_ssp245 <- terra::mask(x = forest_10km_2050_ssp245, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(forest_10km_2050_ssp245)

forest_10km_2050_ssp245 <- resample(forest_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(forest_10km_2050_ssp245, 
            filename = paste0(output_dir, "forest_10km_2050_ssp245.tif"), 
            overwrite = TRUE)



# shrublands

system.time(
  shrublands_temp_2050_ssp585 <- aggregate(landcover_2050_ssp585,
                                           Nratio, 
                                           # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                           fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(shrublands_temp_2050_ssp585 )
shrublands_temp_2050_ssp585

# Resample at correct resolution
shrublands_resampled_2050_ssp585 <- terra::resample(shrublands_temp_2050_ssp585, 
                                                    x_lc, 
                                                    method = "bilinear")

# Project raster
shrublands_2050_ssp585 <- terra::project(x = shrublands_resampled_2050_ssp585,
                                         y = x)

# Before saving, make the name of the value correspond to the variable
names(shrublands_2050_ssp585) <- "opforest"
shrublands_2050_ssp585

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
shrublands_2050_ssp585[is.na(shrublands_2050_ssp585)] <- 0
shrublands_2050_ssp585 <- terra::mask(x = shrublands_2050_ssp585, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(shrublands_2050_ssp585)

shrublands_2050_ssp585 <- resample(shrublands_2050_ssp585, forest_old, method = "near")

# Save results
writeRaster(shrublands_2050_ssp585, 
            filename = paste0(output_dir, "opforest_proportion_1km_2050_ssp585.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  shrublands_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                       Nratio_step, 
                                                       fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(shrublands_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(shrublands_10km_2050_ssp245 <- terra::focal(shrublands_1000_temp_2050_ssp245, 
                                                        w = mat_10km,
                                                        fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
shrublands_10km_resampled_2050_ssp245 <- terra::resample(shrublands_10km_2050_ssp245, 
                                                         x_lc, 
                                                         method = "bilinear")

# Project raster
shrublands_10km_2050_ssp245 <- terra::project(x = shrublands_10km_resampled_2050_ssp245,
                                              y = x)

# Before saving, make the name of the value correspond to the variable
names(shrublands_10km_2050_ssp245) <- "opforest_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
shrublands_10km_2050_ssp245[is.na(shrublands_10km_2050_ssp245)] <- 0
shrublands_10km_2050_ssp245 <- terra::mask(x = shrublands_10km_2050_ssp245, 
                                           mask = SECanadaVectTemp %>% 
                                             st_as_sf(.) %>%
                                             vect(.))

plot(shrublands_10km_2050_ssp245)

shrublands_10km_2050_ssp245 <- resample(shrublands_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(shrublands_10km_2050_ssp245, 
            filename = paste0(output_dir, "opforest_10km_2050_ssp245.tif"), 
            overwrite = TRUE)


# grassland

system.time(
  grassland_temp_2050_ssp585 <- aggregate(landcover_2050_ssp585,
                                          Nratio, 
                                          # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                          fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(grassland_temp_2050_ssp585 )
grassland_temp_2050_ssp585

# Resample at correct resolution
grassland_resampled_2050_ssp585 <- terra::resample(grassland_temp_2050_ssp585, 
                                                   x_lc, 
                                                   method = "bilinear")

# Project raster
grassland_2050_ssp585 <- terra::project(x = grassland_resampled_2050_ssp585,
                                        y = x)

# Before saving, make the name of the value correspond to the variable
names(grassland_2050_ssp585) <- "grassland"
grassland_2050_ssp585

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
grassland_2050_ssp585[is.na(grassland_2050_ssp585)] <- 0
grassland_2050_ssp585 <- terra::mask(x = grassland_2050_ssp585, 
                                     mask = SECanadaVectTemp %>% 
                                       st_as_sf(.) %>%
                                       vect(.))

plot(grassland_2050_ssp585)

grassland_2050_ssp585 <- resample(grassland_2050_ssp585, forest_old, method = "near")

# Save results
writeRaster(grassland_2050_ssp585, 
            filename = paste0(output_dir, "grassland_proportion_1km_2050_ssp585.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  grassland_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                      Nratio_step, 
                                                      fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(grassland_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(grassland_10km_2050_ssp245 <- terra::focal(grassland_1000_temp_2050_ssp245, 
                                                       w = mat_10km,
                                                       fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
grassland_10km_resampled_2050_ssp245 <- terra::resample(grassland_10km_2050_ssp245, 
                                                        x_lc, 
                                                        method = "bilinear")

# Project raster
grassland_10km_2050_ssp245 <- terra::project(x = grassland_10km_resampled_2050_ssp245,
                                             y = x)

# Before saving, make the name of the value correspond to the variable
names(grassland_10km_2050_ssp245) <- "grassland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
grassland_10km_2050_ssp245[is.na(grassland_10km_2050_ssp245)] <- 0
grassland_10km_2050_ssp245 <- terra::mask(x = grassland_10km_2050_ssp245, 
                                          mask = SECanadaVectTemp %>% 
                                            st_as_sf(.) %>%
                                            vect(.))

plot(grassland_10km_2050_ssp245)

grassland_10km_2050_ssp245 <- resample(grassland_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(grassland_10km_2050_ssp245, 
            filename = paste0(output_dir, "grassland_10km_2050_ssp245.tif"), 
            overwrite = TRUE)


# wetland

system.time(
  wetland_temp_2050_ssp585 <- aggregate(landcover_2050_ssp585,
                                        Nratio, 
                                        # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                        fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(wetland_temp_2050_ssp585 )
wetland_temp_2050_ssp585

# Resample at correct resolution
wetland_resampled_2050_ssp585 <- terra::resample(wetland_temp_2050_ssp585, 
                                                 x_lc, 
                                                 method = "bilinear")

# Project raster
wetland_2050_ssp585 <- terra::project(x = wetland_resampled_2050_ssp585,
                                      y = x)

# Before saving, make the name of the value correspond to the variable
names(wetland_2050_ssp585) <- "wetland"
wetland_2050_ssp585

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
wetland_2050_ssp585[is.na(wetland_2050_ssp585)] <- 0
wetland_2050_ssp585 <- terra::mask(x = wetland_2050_ssp585, 
                                   mask = SECanadaVectTemp %>% 
                                     st_as_sf(.) %>%
                                     vect(.))

plot(wetland_2050_ssp585)
wetland_2050_ssp585 <- resample(wetland_2050_ssp585, forest_old, method = "near")

# Save results
writeRaster(wetland_2050_ssp585, 
            filename = paste0(output_dir, "wetland_proportion_1km_2050_ssp585.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  wetland_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                    Nratio_step, 
                                                    fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(wetland_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(wetland_10km_2050_ssp245 <- terra::focal(wetland_1000_temp_2050_ssp245, 
                                                     w = mat_10km,
                                                     fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
wetland_10km_resampled_2050_ssp245 <- terra::resample(wetland_10km_2050_ssp245, 
                                                      x_lc, 
                                                      method = "bilinear")

# Project raster
wetland_10km_2050_ssp245 <- terra::project(x = wetland_10km_resampled_2050_ssp245,
                                           y = x)

# Before saving, make the name of the value correspond to the variable
names(wetland_10km_2050_ssp245) <- "wetland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
wetland_10km_2050_ssp245[is.na(wetland_10km_2050_ssp245)] <- 0
wetland_10km_2050_ssp245 <- terra::mask(x = wetland_10km_2050_ssp245, 
                                        mask = SECanadaVectTemp %>% 
                                          st_as_sf(.) %>%
                                          vect(.))

plot(wetland_10km_2050_ssp245)

wetland_10km_2050_ssp245 <- resample(wetland_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(wetland_10km_2050_ssp245, 
            filename = paste0(output_dir, "wetland_10km_2050_ssp245.tif"), 
            overwrite = TRUE)


# cropland

system.time(
  cropland_temp_2050_ssp585 <- aggregate(landcover_2050_ssp585,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(cropland_temp_2050_ssp585 )
cropland_temp_2050_ssp585

# Resample at correct resolution
cropland_resampled_2050_ssp585 <- terra::resample(cropland_temp_2050_ssp585, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
cropland_2050_ssp585 <- terra::project(x = cropland_resampled_2050_ssp585,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(cropland_2050_ssp585) <- "cropland"
cropland_2050_ssp585

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
cropland_2050_ssp585[is.na(cropland_2050_ssp585)] <- 0
cropland_2050_ssp585 <- terra::mask(x = cropland_2050_ssp585, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(cropland_2050_ssp585)
cropland_2050_ssp585 <- resample(cropland_2050_ssp585, forest_old, method = "near")

# Save results
writeRaster(cropland_2050_ssp585, 
            filename = paste0(output_dir, "cropland_proportion_1km_2050_ssp585.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  cropland_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(cropland_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(cropland_10km_2050_ssp245 <- terra::focal(cropland_1000_temp_2050_ssp245, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
cropland_10km_resampled_2050_ssp245 <- terra::resample(cropland_10km_2050_ssp245, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
cropland_10km_2050_ssp245 <- terra::project(x = cropland_10km_resampled_2050_ssp245,
                                            y = x)

# Before saving, make the name of the value correspond to the variable
names(cropland_10km_2050_ssp245) <- "cropland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
cropland_10km_2050_ssp245[is.na(cropland_10km_2050_ssp245)] <- 0
cropland_10km_2050_ssp245 <- terra::mask(x = cropland_10km_2050_ssp245, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(cropland_10km_2050_ssp245)

cropland_10km_2050_ssp245 <- resample(cropland_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(cropland_10km_2050_ssp245, 
            filename = paste0(output_dir, "cropland_10km_2050_ssp245.tif"), 
            overwrite = TRUE)


# barren

system.time(
  barren_temp_2050_ssp585 <- aggregate(landcover_2050_ssp585,
                                       Nratio, 
                                       # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                       fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(barren_temp_2050_ssp585 )
barren_temp_2050_ssp585

# Resample at correct resolution
barren_resampled_2050_ssp585 <- terra::resample(barren_temp_2050_ssp585, 
                                                x_lc, 
                                                method = "bilinear")

# Project raster
barren_2050_ssp585 <- terra::project(x = barren_resampled_2050_ssp585,
                                     y = x)

# Before saving, make the name of the value correspond to the variable
names(barren_2050_ssp585) <- "barren"
barren_2050_ssp585

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
barren_2050_ssp585[is.na(barren_2050_ssp585)] <- 0
barren_2050_ssp585 <- terra::mask(x = barren_2050_ssp585, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(barren_2050_ssp585)
barren_2050_ssp585 <- resample(barren_2050_ssp585, forest_old, method = "near")

# Save results
writeRaster(barren_2050_ssp585, 
            filename = paste0(output_dir, "barren_proportion_1km_2050_ssp585.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  barren_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(barren_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(barren_10km_2050_ssp126 <- terra::focal(barren_1000_temp_2050_ssp126, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells


# Resample at correct resolution
barren_10km_resampled_2050_ssp126 <- terra::resample(barren_10km_2050_ssp126, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
barren_10km_2050_ssp126 <- terra::project(x = barren_10km_resampled_2050_ssp126,
                                          y = x)

# Before saving, make the name of the value correspond to the variable
names(barren_10km_2050_ssp126) <- "barren_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
barren_10km_2050_ssp126[is.na(barren_10km_2050_ssp126)] <- 0
barren_10km_2050_ssp126 <- terra::mask(x = barren_10km_2050_ssp126, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(barren_10km_2050_ssp126)

barren_10km_2050_ssp126 <- resample(barren_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(barren_10km_2050_ssp126, 
            filename = paste0(output_dir, "barren_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# built_up

system.time(
  built_up_temp_2050_ssp585 <- aggregate(landcover_2050_ssp585,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(built_up_temp_2050_ssp585 )
built_up_temp_2050_ssp585

# Resample at correct resolution
built_up_resampled_2050_ssp585 <- terra::resample(built_up_temp_2050_ssp585, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
built_up_2050_ssp585 <- terra::project(x = built_up_resampled_2050_ssp585,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(built_up_2050_ssp585) <- "built_up"
built_up_2050_ssp585

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
built_up_2050_ssp585[is.na(built_up_2050_ssp585)] <- 0
built_up_2050_ssp585 <- terra::mask(x = built_up_2050_ssp585, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(built_up_2050_ssp585)
built_up_2050_ssp585 <- resample(built_up_2050_ssp585, forest_old, method = "near")

# Save results
writeRaster(built_up_2050_ssp585, 
            filename = paste0(output_dir, "built_up_proportion_1km_2050_ssp585.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  built_up_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(built_up_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(built_up_10km_2050_ssp126 <- terra::focal(built_up_1000_temp_2050_ssp126, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
built_up_10km_resampled_2050_ssp126 <- terra::resample(built_up_10km_2050_ssp126, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
built_up_10km_2050_ssp126 <- terra::project(x = built_up_10km_resampled_2050_ssp126,
                                            y = x)

# Before saving, make the name of the value correspond to the variable
names(built_up_10km_2050_ssp126) <- "built_up_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
built_up_10km_2050_ssp126[is.na(built_up_10km_2050_ssp126)] <- 0
built_up_10km_2050_ssp126 <- terra::mask(x = built_up_10km_2050_ssp126, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(built_up_10km_2050_ssp126)

built_up_10km_2050_ssp126 <- resample(built_up_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(built_up_10km_2050_ssp126, 
            filename = paste0(output_dir, "built_up_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# water

system.time(
  water_temp_2050_ssp585 <- aggregate(landcover_2050_ssp585,
                                      Nratio, 
                                      # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                      fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(water_temp_2050_ssp585 )
water_temp_2050_ssp585

# Resample at correct resolution
water_resampled_2050_ssp585 <- terra::resample(water_temp_2050_ssp585, 
                                               x_lc, 
                                               method = "bilinear")

# Project raster
water_2050_ssp585 <- terra::project(x = water_resampled_2050_ssp585,
                                    y = x)

# Before saving, make the name of the value correspond to the variable
names(water_2050_ssp585) <- "water"
water_2050_ssp585

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
water_2050_ssp585[is.na(water_2050_ssp585)] <- 0
water_2050_ssp585 <- terra::mask(x = water_2050_ssp585, 
                                 mask = SECanadaVectTemp %>% 
                                   st_as_sf(.) %>%
                                   vect(.))

plot(water_2050_ssp585)
water_2050_ssp585 <- resample(water_2050_ssp585, forest_old, method = "near")

# Save results
writeRaster(water_2050_ssp585, 
            filename = paste0(output_dir, "water_proportion_1km_2050_ssp585.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  water_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                  Nratio_step, 
                                                  fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(water_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(water_10km_2050_ssp126 <- terra::focal(water_1000_temp_2050_ssp126, 
                                                   w = mat_10km,
                                                   fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
water_10km_resampled_2050_ssp126 <- terra::resample(water_10km_2050_ssp126, 
                                                    x_lc, 
                                                    method = "bilinear")

# Project raster
water_10km_2050_ssp126 <- terra::project(x = water_10km_resampled_2050_ssp126,
                                         y = x)

# Before saving, make the name of the value correspond to the variable
names(water_10km_2050_ssp126) <- "water_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
water_10km_2050_ssp126[is.na(water_10km_2050_ssp126)] <- 0
water_10km_2050_ssp126 <- terra::mask(x = water_10km_2050_ssp126, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(water_10km_2050_ssp126)

water_10km_2050_ssp126 <- resample(water_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(water_10km_2050_ssp126, 
            filename = paste0(output_dir, "water_10km_2050_ssp126.tif"), 
            overwrite = TRUE)




#----------------2070--------------

# -------------------proportion of habitat per site calculations--------------

# Now, calculate the proportion of habitat per site for forest
system.time(
  forest_temp_2070_ssp126 <- aggregate(landcover_2070_ssp126,
                                       Nratio, 
                                       # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                       fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(forest_temp_2070_ssp126 )
forest_temp_2070_ssp126

# Resample at correct resolution
forest_resampled_2070_ssp126 <- terra::resample(forest_temp_2070_ssp126, 
                                                x_lc, 
                                                method = "bilinear")

# Project raster
forest_2070_ssp126 <- terra::project(x = forest_resampled_2070_ssp126,
                                     y = x)

# Before saving, make the name of the value correspond to the variable
names(forest_2070_ssp126) <- "forest"
forest_2070_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
forest_2070_ssp126[is.na(forest_2070_ssp126)] <- 0
forest_2070_ssp126 <- terra::mask(x = forest_2070_ssp126, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(forest_2070_ssp126)

forest_2070_ssp126 <- resample(forest_2070_ssp126, forest_old, method = "near")

# Save results
writeRaster(forest_2070_ssp126, 
            filename = paste0(output_dir, "forest_proportion_1km_2070_ssp126.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  forest_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(forest_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(forest_10km_2050_ssp126 <- terra::focal(forest_1000_temp_2050_ssp126, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
forest_10km_resampled_2050_ssp126 <- terra::resample(forest_10km_2050_ssp126, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
forest_10km_2050_ssp126 <- terra::project(x = forest_10km_resampled_2050_ssp126,
                                          y = x)

# Before saving, make the name of the value correspond to the variable
names(forest_10km_2050_ssp126) <- "forest_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
forest_10km_2050_ssp126[is.na(forest_10km_2050_ssp126)] <- 0
forest_10km_2050_ssp126 <- terra::mask(x = forest_10km_2050_ssp126, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(forest_10km_2050_ssp126)

forest_10km_2050_ssp126 <- resample(forest_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(forest_10km_2050_ssp126, 
            filename = paste0(output_dir, "forest_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# shrublands

system.time(
  shrublands_temp_2070_ssp126 <- aggregate(landcover_2070_ssp126,
                                           Nratio, 
                                           # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                           fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(shrublands_temp_2070_ssp126 )
shrublands_temp_2070_ssp126

# Resample at correct resolution
shrublands_resampled_2070_ssp126 <- terra::resample(shrublands_temp_2070_ssp126, 
                                                    x_lc, 
                                                    method = "bilinear")

# Project raster
shrublands_2070_ssp126 <- terra::project(x = shrublands_resampled_2070_ssp126,
                                         y = x)

# Before saving, make the name of the value correspond to the variable
names(shrublands_2070_ssp126) <- "opforest"
shrublands_2070_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
shrublands_2070_ssp126[is.na(shrublands_2070_ssp126)] <- 0
shrublands_2070_ssp126 <- terra::mask(x = shrublands_2070_ssp126, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(shrublands_2070_ssp126)

shrublands_2070_ssp126 <- resample(shrublands_2070_ssp126, forest_old, method = "near")

# Save results
writeRaster(shrublands_2070_ssp126, 
            filename = paste0(output_dir, "opforest_proportion_1km_2070_ssp126.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  shrublands_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                       Nratio_step, 
                                                       fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(shrublands_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(shrublands_10km_2050_ssp126 <- terra::focal(shrublands_1000_temp_2050_ssp126, 
                                                        w = mat_10km,
                                                        fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
shrublands_10km_resampled_2050_ssp126 <- terra::resample(shrublands_10km_2050_ssp126, 
                                                         x_lc, 
                                                         method = "bilinear")

# Project raster
shrublands_10km_2050_ssp126 <- terra::project(x = shrublands_10km_resampled_2050_ssp126,
                                              y = x)

# Before saving, make the name of the value correspond to the variable
names(shrublands_10km_2050_ssp126) <- "opforest_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
shrublands_10km_2050_ssp126[is.na(shrublands_10km_2050_ssp126)] <- 0
shrublands_10km_2050_ssp126 <- terra::mask(x = shrublands_10km_2050_ssp126, 
                                           mask = SECanadaVectTemp %>% 
                                             st_as_sf(.) %>%
                                             vect(.))

plot(shrublands_10km_2050_ssp126)

shrublands_10km_2050_ssp126 <- resample(shrublands_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(shrublands_10km_2050_ssp126, 
            filename = paste0(output_dir, "opforest_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# grassland

system.time(
  grassland_temp_2070_ssp126 <- aggregate(landcover_2070_ssp126,
                                          Nratio, 
                                          # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                          fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(grassland_temp_2070_ssp126 )
grassland_temp_2070_ssp126

# Resample at correct resolution
grassland_resampled_2070_ssp126 <- terra::resample(grassland_temp_2070_ssp126, 
                                                   x_lc, 
                                                   method = "bilinear")

# Project raster
grassland_2070_ssp126 <- terra::project(x = grassland_resampled_2070_ssp126,
                                        y = x)

# Before saving, make the name of the value correspond to the variable
names(grassland_2070_ssp126) <- "grassland"
grassland_2070_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
grassland_2070_ssp126[is.na(grassland_2070_ssp126)] <- 0
grassland_2070_ssp126 <- terra::mask(x = grassland_2070_ssp126, 
                                     mask = SECanadaVectTemp %>% 
                                       st_as_sf(.) %>%
                                       vect(.))

plot(grassland_2070_ssp126)

grassland_2070_ssp126 <- resample(grassland_2070_ssp126, forest_old, method = "near")

# Save results
writeRaster(grassland_2070_ssp126, 
            filename = paste0(output_dir, "grassland_proportion_1km_2070_ssp126.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  grassland_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                      Nratio_step, 
                                                      fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(grassland_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(grassland_10km_2050_ssp126 <- terra::focal(grassland_1000_temp_2050_ssp126, 
                                                       w = mat_10km,
                                                       fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
grassland_10km_resampled_2050_ssp126 <- terra::resample(grassland_10km_2050_ssp126, 
                                                        x_lc, 
                                                        method = "bilinear")

# Project raster
grassland_10km_2050_ssp126 <- terra::project(x = grassland_10km_resampled_2050_ssp126,
                                             y = x)

# Before saving, make the name of the value correspond to the variable
names(grassland_10km_2050_ssp126) <- "grassland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
grassland_10km_2050_ssp126[is.na(grassland_10km_2050_ssp126)] <- 0
grassland_10km_2050_ssp126 <- terra::mask(x = grassland_10km_2050_ssp126, 
                                          mask = SECanadaVectTemp %>% 
                                            st_as_sf(.) %>%
                                            vect(.))

plot(grassland_10km_2050_ssp126)

grassland_10km_2050_ssp126 <- resample(grassland_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(grassland_10km_2050_ssp126, 
            filename = paste0(output_dir, "grassland_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# wetland

system.time(
  wetland_temp_2070_ssp126 <- aggregate(landcover_2070_ssp126,
                                        Nratio, 
                                        # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                        fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(wetland_temp_2070_ssp126 )
wetland_temp_2070_ssp126

# Resample at correct resolution
wetland_resampled_2070_ssp126 <- terra::resample(wetland_temp_2070_ssp126, 
                                                 x_lc, 
                                                 method = "bilinear")

# Project raster
wetland_2070_ssp126 <- terra::project(x = wetland_resampled_2070_ssp126,
                                      y = x)

# Before saving, make the name of the value correspond to the variable
names(wetland_2070_ssp126) <- "wetland"
wetland_2070_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
wetland_2070_ssp126[is.na(wetland_2070_ssp126)] <- 0
wetland_2070_ssp126 <- terra::mask(x = wetland_2070_ssp126, 
                                   mask = SECanadaVectTemp %>% 
                                     st_as_sf(.) %>%
                                     vect(.))

plot(wetland_2070_ssp126)
wetland_2070_ssp126 <- resample(wetland_2070_ssp126, forest_old, method = "near")

# Save results
writeRaster(wetland_2070_ssp126, 
            filename = paste0(output_dir, "wetland_proportion_1km_2070_ssp126.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  wetland_1000_temp_2070_ssp126 <- terra::aggregate(landcover_2070_ssp126,
                                                    Nratio_step, 
                                                    fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(wetland_1000_temp_2070_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(wetland_10km_2070_ssp126 <- terra::focal(wetland_1000_temp_2070_ssp126, 
                                                     w = mat_10km,
                                                     fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
wetland_10km_resampled_2070_ssp126 <- terra::resample(wetland_10km_2070_ssp126, 
                                                      x_lc, 
                                                      method = "bilinear")

# Project raster
wetland_10km_2070_ssp126 <- terra::project(x = wetland_10km_resampled_2070_ssp126,
                                           y = x)

# Before saving, make the name of the value correspond to the variable
names(wetland_10km_2070_ssp126) <- "wetland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
wetland_10km_2070_ssp126[is.na(wetland_10km_2070_ssp126)] <- 0
wetland_10km_2070_ssp126 <- terra::mask(x = wetland_10km_2070_ssp126, 
                                        mask = SECanadaVectTemp %>% 
                                          st_as_sf(.) %>%
                                          vect(.))

plot(wetland_10km_2070_ssp126)

wetland_10km_2070_ssp126 <- resample(wetland_10km_2070_ssp126, forest_old, method = "near")

# Save results
writeRaster(wetland_10km_2070_ssp126, 
            filename = paste0(output_dir, "wetland_10km_2070_ssp126.tif"), 
            overwrite = TRUE)




# cropland

system.time(
  cropland_temp_2070_ssp126 <- aggregate(landcover_2070_ssp126,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(cropland_temp_2070_ssp126 )
cropland_temp_2070_ssp126

# Resample at correct resolution
cropland_resampled_2070_ssp126 <- terra::resample(cropland_temp_2070_ssp126, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
cropland_2070_ssp126 <- terra::project(x = cropland_resampled_2070_ssp126,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(cropland_2070_ssp126) <- "cropland"
cropland_2070_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
cropland_2070_ssp126[is.na(cropland_2070_ssp126)] <- 0
cropland_2070_ssp126 <- terra::mask(x = cropland_2070_ssp126, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(cropland_2070_ssp126)
cropland_2070_ssp126 <- resample(cropland_2070_ssp126, forest_old, method = "near")

# Save results
writeRaster(cropland_2070_ssp126, 
            filename = paste0(output_dir, "cropland_proportion_1km_2070_ssp126.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  cropland_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(cropland_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(cropland_10km_2050_ssp126 <- terra::focal(cropland_1000_temp_2050_ssp126, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
cropland_10km_resampled_2050_ssp126 <- terra::resample(cropland_10km_2050_ssp126, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
cropland_10km_2050_ssp126 <- terra::project(x = cropland_10km_resampled_2050_ssp126,
                                            y = x)

# Before saving, make the name of the value correspond to the variable
names(cropland_10km_2050_ssp126) <- "cropland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
cropland_10km_2050_ssp126[is.na(cropland_10km_2050_ssp126)] <- 0
cropland_10km_2050_ssp126 <- terra::mask(x = cropland_10km_2050_ssp126, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(cropland_10km_2050_ssp126)

cropland_10km_2050_ssp126 <- resample(cropland_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(cropland_10km_2050_ssp126, 
            filename = paste0(output_dir, "cropland_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# barren

system.time(
  barren_temp_2070_ssp126 <- aggregate(landcover_2070_ssp126,
                                       Nratio, 
                                       # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                       fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(barren_temp_2070_ssp126 )
barren_temp_2070_ssp126

# Resample at correct resolution
barren_resampled_2070_ssp126 <- terra::resample(barren_temp_2070_ssp126, 
                                                x_lc, 
                                                method = "bilinear")

# Project raster
barren_2070_ssp126 <- terra::project(x = barren_resampled_2070_ssp126,
                                     y = x)

# Before saving, make the name of the value correspond to the variable
names(barren_2070_ssp126) <- "barren"
barren_2070_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
barren_2070_ssp126[is.na(barren_2070_ssp126)] <- 0
barren_2070_ssp126 <- terra::mask(x = barren_2070_ssp126, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(barren_2070_ssp126)
barren_2070_ssp126 <- resample(barren_2070_ssp126, forest_old, method = "near")

# Save results
writeRaster(barren_2070_ssp126, 
            filename = paste0(output_dir, "barren_proportion_1km_2070_ssp126.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  barren_1000_temp_2070_ssp126 <- terra::aggregate(landcover_2070_ssp126,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(barren_1000_temp_2070_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(barren_10km_2070_ssp126 <- terra::focal(barren_1000_temp_2070_ssp126, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells


# Resample at correct resolution
barren_10km_resampled_2070_ssp126 <- terra::resample(barren_10km_2070_ssp126, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
barren_10km_2070_ssp126 <- terra::project(x = barren_10km_resampled_2070_ssp126,
                                          y = x)

# Before saving, make the name of the value correspond to the variable
names(barren_10km_2070_ssp126) <- "barren_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
barren_10km_2070_ssp126[is.na(barren_10km_2070_ssp126)] <- 0
barren_10km_2070_ssp126 <- terra::mask(x = barren_10km_2070_ssp126, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(barren_10km_2070_ssp126)

barren_10km_2070_ssp126 <- resample(barren_10km_2070_ssp126, forest_old, method = "near")

# Save results
writeRaster(barren_10km_2070_ssp126, 
            filename = paste0(output_dir, "barren_10km_2070_ssp126.tif"), 
            overwrite = TRUE)



# built_up

system.time(
  built_up_temp_2070_ssp126 <- aggregate(landcover_2070_ssp126,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(built_up_temp_2070_ssp126 )
built_up_temp_2070_ssp126

# Resample at correct resolution
built_up_resampled_2070_ssp126 <- terra::resample(built_up_temp_2070_ssp126, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
built_up_2070_ssp126 <- terra::project(x = built_up_resampled_2070_ssp126,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(built_up_2070_ssp126) <- "built_up"
built_up_2070_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
built_up_2070_ssp126[is.na(built_up_2070_ssp126)] <- 0
built_up_2070_ssp126 <- terra::mask(x = built_up_2070_ssp126, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(built_up_2070_ssp126)
built_up_2070_ssp126 <- resample(built_up_2070_ssp126, forest_old, method = "near")

# Save results
writeRaster(built_up_2070_ssp126, 
            filename = paste0(output_dir, "built_up_proportion_1km_2070_ssp126.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  built_up_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(built_up_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(built_up_10km_2050_ssp126 <- terra::focal(built_up_1000_temp_2050_ssp126, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
built_up_10km_resampled_2050_ssp126 <- terra::resample(built_up_10km_2050_ssp126, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
built_up_10km_2050_ssp126 <- terra::project(x = built_up_10km_resampled_2050_ssp126,
                                            y = x)

# Before saving, make the name of the value correspond to the variable
names(built_up_10km_2050_ssp126) <- "built_up_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
built_up_10km_2050_ssp126[is.na(built_up_10km_2050_ssp126)] <- 0
built_up_10km_2050_ssp126 <- terra::mask(x = built_up_10km_2050_ssp126, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(built_up_10km_2050_ssp126)

built_up_10km_2050_ssp126 <- resample(built_up_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(built_up_10km_2050_ssp126, 
            filename = paste0(output_dir, "built_up_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# water

system.time(
  water_temp_2070_ssp126 <- aggregate(landcover_2070_ssp126,
                                      Nratio, 
                                      # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                      fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(water_temp_2070_ssp126 )
water_temp_2070_ssp126

# Resample at correct resolution
water_resampled_2070_ssp126 <- terra::resample(water_temp_2070_ssp126, 
                                               x_lc, 
                                               method = "bilinear")

# Project raster
water_2070_ssp126 <- terra::project(x = water_resampled_2070_ssp126,
                                    y = x)

# Before saving, make the name of the value correspond to the variable
names(water_2070_ssp126) <- "water"
water_2070_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
water_2070_ssp126[is.na(water_2070_ssp126)] <- 0
water_2070_ssp126 <- terra::mask(x = water_2070_ssp126, 
                                 mask = SECanadaVectTemp %>% 
                                   st_as_sf(.) %>%
                                   vect(.))

plot(water_2070_ssp126)
water_2070_ssp126 <- resample(water_2070_ssp126, forest_old, method = "near")

# Save results
writeRaster(water_2070_ssp126, 
            filename = paste0(output_dir, "water_proportion_1km_2070_ssp126.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  water_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                  Nratio_step, 
                                                  fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(water_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(water_10km_2050_ssp126 <- terra::focal(water_1000_temp_2050_ssp126, 
                                                   w = mat_10km,
                                                   fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
water_10km_resampled_2050_ssp126 <- terra::resample(water_10km_2050_ssp126, 
                                                    x_lc, 
                                                    method = "bilinear")

# Project raster
water_10km_2050_ssp126 <- terra::project(x = water_10km_resampled_2050_ssp126,
                                         y = x)

# Before saving, make the name of the value correspond to the variable
names(water_10km_2050_ssp126) <- "water_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
water_10km_2050_ssp126[is.na(water_10km_2050_ssp126)] <- 0
water_10km_2050_ssp126 <- terra::mask(x = water_10km_2050_ssp126, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(water_10km_2050_ssp126)

water_10km_2050_ssp126 <- resample(water_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(water_10km_2050_ssp126, 
            filename = paste0(output_dir, "water_10km_2050_ssp126.tif"), 
            overwrite = TRUE)




# 2070 ssp245

# Now, calculate the proportion of habitat per site for forest
system.time(
  forest_temp_2070_ssp245 <- aggregate(landcover_2070_ssp245,
                                       Nratio, 
                                       # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                       fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(forest_temp_2070_ssp245 )
forest_temp_2070_ssp245

# Resample at correct resolution
forest_resampled_2070_ssp245 <- terra::resample(forest_temp_2070_ssp245, 
                                                x_lc, 
                                                method = "bilinear")

# Project raster
forest_2070_ssp245 <- terra::project(x = forest_resampled_2070_ssp245,
                                     y = x)

# Before saving, make the name of the value correspond to the variable
names(forest_2070_ssp245) <- "forest"
forest_2070_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
forest_2070_ssp245[is.na(forest_2070_ssp245)] <- 0
forest_2070_ssp245 <- terra::mask(x = forest_2070_ssp245, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(forest_2070_ssp245)

forest_2070_ssp245 <- resample(forest_2070_ssp245, forest_old, method = "near")

# Save results
writeRaster(forest_2070_ssp245, 
            filename = paste0(output_dir, "forest_proportion_1km_2070_ssp245.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  forest_1000_temp_2070_ssp245 <- terra::aggregate(landcover_2070_ssp245,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(forest_1000_temp_2070_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(forest_10km_2070_ssp245 <- terra::focal(forest_1000_temp_2070_ssp245, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
forest_10km_resampled_2070_ssp245 <- terra::resample(forest_10km_2070_ssp245, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
forest_10km_2070_ssp245 <- terra::project(x = forest_10km_resampled_2070_ssp245,
                                          y = x)

# Before saving, make the name of the value correspond to the variable
names(forest_10km_2070_ssp245) <- "forest_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
forest_10km_2070_ssp245[is.na(forest_10km_2070_ssp245)] <- 0
forest_10km_2070_ssp245 <- terra::mask(x = forest_10km_2070_ssp245, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(forest_10km_2070_ssp245)

forest_10km_2070_ssp245 <- resample(forest_10km_2070_ssp245, forest_old, method = "near")

# Save results
writeRaster(forest_10km_2070_ssp245, 
            filename = paste0(output_dir, "forest_10km_2070_ssp245.tif"), 
            overwrite = TRUE)



# shrublands

system.time(
  shrublands_temp_2070_ssp245 <- aggregate(landcover_2070_ssp245,
                                           Nratio, 
                                           # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                           fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(shrublands_temp_2070_ssp245 )
shrublands_temp_2070_ssp245

# Resample at correct resolution
shrublands_resampled_2070_ssp245 <- terra::resample(shrublands_temp_2070_ssp245, 
                                                    x_lc, 
                                                    method = "bilinear")

# Project raster
shrublands_2070_ssp245 <- terra::project(x = shrublands_resampled_2070_ssp245,
                                         y = x)

# Before saving, make the name of the value correspond to the variable
names(shrublands_2070_ssp245) <- "opforest"
shrublands_2070_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
shrublands_2070_ssp245[is.na(shrublands_2070_ssp245)] <- 0
shrublands_2070_ssp245 <- terra::mask(x = shrublands_2070_ssp245, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(shrublands_2070_ssp245)

shrublands_2070_ssp245 <- resample(shrublands_2070_ssp245, forest_old, method = "near")

# Save results
writeRaster(shrublands_2070_ssp245, 
            filename = paste0(output_dir, "opforest_proportion_1km_2070_ssp245.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  shrublands_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                       Nratio_step, 
                                                       fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(shrublands_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(shrublands_10km_2050_ssp245 <- terra::focal(shrublands_1000_temp_2050_ssp245, 
                                                        w = mat_10km,
                                                        fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
shrublands_10km_resampled_2050_ssp245 <- terra::resample(shrublands_10km_2050_ssp245, 
                                                         x_lc, 
                                                         method = "bilinear")

# Project raster
shrublands_10km_2050_ssp245 <- terra::project(x = shrublands_10km_resampled_2050_ssp245,
                                              y = x)

# Before saving, make the name of the value correspond to the variable
names(shrublands_10km_2050_ssp245) <- "opforest_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
shrublands_10km_2050_ssp245[is.na(shrublands_10km_2050_ssp245)] <- 0
shrublands_10km_2050_ssp245 <- terra::mask(x = shrublands_10km_2050_ssp245, 
                                           mask = SECanadaVectTemp %>% 
                                             st_as_sf(.) %>%
                                             vect(.))

plot(shrublands_10km_2050_ssp245)

shrublands_10km_2050_ssp245 <- resample(shrublands_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(shrublands_10km_2050_ssp245, 
            filename = paste0(output_dir, "opforest_10km_2050_ssp245.tif"), 
            overwrite = TRUE)


# grassland

system.time(
  grassland_temp_2070_ssp245 <- aggregate(landcover_2070_ssp245,
                                          Nratio, 
                                          # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                          fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(grassland_temp_2070_ssp245 )
grassland_temp_2070_ssp245

# Resample at correct resolution
grassland_resampled_2070_ssp245 <- terra::resample(grassland_temp_2070_ssp245, 
                                                   x_lc, 
                                                   method = "bilinear")

# Project raster
grassland_2070_ssp245 <- terra::project(x = grassland_resampled_2070_ssp245,
                                        y = x)

# Before saving, make the name of the value correspond to the variable
names(grassland_2070_ssp245) <- "grassland"
grassland_2070_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
grassland_2070_ssp245[is.na(grassland_2070_ssp245)] <- 0
grassland_2070_ssp245 <- terra::mask(x = grassland_2070_ssp245, 
                                     mask = SECanadaVectTemp %>% 
                                       st_as_sf(.) %>%
                                       vect(.))

plot(grassland_2070_ssp245)

grassland_2070_ssp245 <- resample(grassland_2070_ssp245, forest_old, method = "near")

# Save results
writeRaster(grassland_2070_ssp245, 
            filename = paste0(output_dir, "grassland_proportion_1km_2070_ssp245.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  grassland_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                      Nratio_step, 
                                                      fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(grassland_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(grassland_10km_2050_ssp245 <- terra::focal(grassland_1000_temp_2050_ssp245, 
                                                       w = mat_10km,
                                                       fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
grassland_10km_resampled_2050_ssp245 <- terra::resample(grassland_10km_2050_ssp245, 
                                                        x_lc, 
                                                        method = "bilinear")

# Project raster
grassland_10km_2050_ssp245 <- terra::project(x = grassland_10km_resampled_2050_ssp245,
                                             y = x)

# Before saving, make the name of the value correspond to the variable
names(grassland_10km_2050_ssp245) <- "grassland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
grassland_10km_2050_ssp245[is.na(grassland_10km_2050_ssp245)] <- 0
grassland_10km_2050_ssp245 <- terra::mask(x = grassland_10km_2050_ssp245, 
                                          mask = SECanadaVectTemp %>% 
                                            st_as_sf(.) %>%
                                            vect(.))

plot(grassland_10km_2050_ssp245)

grassland_10km_2050_ssp245 <- resample(grassland_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(grassland_10km_2050_ssp245, 
            filename = paste0(output_dir, "grassland_10km_2050_ssp245.tif"), 
            overwrite = TRUE)


# wetland

system.time(
  wetland_temp_2070_ssp245 <- aggregate(landcover_2070_ssp245,
                                        Nratio, 
                                        # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                        fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(wetland_temp_2070_ssp245 )
wetland_temp_2070_ssp245

# Resample at correct resolution
wetland_resampled_2070_ssp245 <- terra::resample(wetland_temp_2070_ssp245, 
                                                 x_lc, 
                                                 method = "bilinear")

# Project raster
wetland_2070_ssp245 <- terra::project(x = wetland_resampled_2070_ssp245,
                                      y = x)

# Before saving, make the name of the value correspond to the variable
names(wetland_2070_ssp245) <- "wetland"
wetland_2070_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
wetland_2070_ssp245[is.na(wetland_2070_ssp245)] <- 0
wetland_2070_ssp245 <- terra::mask(x = wetland_2070_ssp245, 
                                   mask = SECanadaVectTemp %>% 
                                     st_as_sf(.) %>%
                                     vect(.))

plot(wetland_2070_ssp245)
wetland_2070_ssp245 <- resample(wetland_2070_ssp245, forest_old, method = "near")

# Save results
writeRaster(wetland_2070_ssp245, 
            filename = paste0(output_dir, "wetland_proportion_1km_2070_ssp245.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  wetland_1000_temp_2070_ssp245 <- terra::aggregate(landcover_2070_ssp245,
                                                    Nratio_step, 
                                                    fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(wetland_1000_temp_2070_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(wetland_10km_2070_ssp245 <- terra::focal(wetland_1000_temp_2070_ssp245, 
                                                     w = mat_10km,
                                                     fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
wetland_10km_resampled_2070_ssp245 <- terra::resample(wetland_10km_2070_ssp245, 
                                                      x_lc, 
                                                      method = "bilinear")

# Project raster
wetland_10km_2070_ssp245 <- terra::project(x = wetland_10km_resampled_2070_ssp245,
                                           y = x)

# Before saving, make the name of the value correspond to the variable
names(wetland_10km_2070_ssp245) <- "wetland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
wetland_10km_2070_ssp245[is.na(wetland_10km_2070_ssp245)] <- 0
wetland_10km_2070_ssp245 <- terra::mask(x = wetland_10km_2070_ssp245, 
                                        mask = SECanadaVectTemp %>% 
                                          st_as_sf(.) %>%
                                          vect(.))

plot(wetland_10km_2070_ssp245)

wetland_10km_2070_ssp245 <- resample(wetland_10km_2070_ssp245, forest_old, method = "near")

# Save results
writeRaster(wetland_10km_2070_ssp245, 
            filename = paste0(output_dir, "wetland_10km_2070_ssp245.tif"), 
            overwrite = TRUE)


# cropland

system.time(
  cropland_temp_2070_ssp245 <- aggregate(landcover_2070_ssp245,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(cropland_temp_2070_ssp245 )
cropland_temp_2070_ssp245

# Resample at correct resolution
cropland_resampled_2070_ssp245 <- terra::resample(cropland_temp_2070_ssp245, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
cropland_2070_ssp245 <- terra::project(x = cropland_resampled_2070_ssp245,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(cropland_2070_ssp245) <- "cropland"
cropland_2070_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
cropland_2070_ssp245[is.na(cropland_2070_ssp245)] <- 0
cropland_2070_ssp245 <- terra::mask(x = cropland_2070_ssp245, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(cropland_2070_ssp245)
cropland_2070_ssp245 <- resample(cropland_2070_ssp245, forest_old, method = "near")

# Save results
writeRaster(cropland_2070_ssp245, 
            filename = paste0(output_dir, "cropland_proportion_1km_2070_ssp245.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  cropland_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(cropland_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(cropland_10km_2050_ssp245 <- terra::focal(cropland_1000_temp_2050_ssp245, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
cropland_10km_resampled_2050_ssp245 <- terra::resample(cropland_10km_2050_ssp245, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
cropland_10km_2050_ssp245 <- terra::project(x = cropland_10km_resampled_2050_ssp245,
                                            y = x)

# Before saving, make the name of the value correspond to the variable
names(cropland_10km_2050_ssp245) <- "cropland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
cropland_10km_2050_ssp245[is.na(cropland_10km_2050_ssp245)] <- 0
cropland_10km_2050_ssp245 <- terra::mask(x = cropland_10km_2050_ssp245, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(cropland_10km_2050_ssp245)

cropland_10km_2050_ssp245 <- resample(cropland_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(cropland_10km_2050_ssp245, 
            filename = paste0(output_dir, "cropland_10km_2050_ssp245.tif"), 
            overwrite = TRUE)


# barren

system.time(
  barren_temp_2070_ssp245 <- aggregate(landcover_2070_ssp245,
                                       Nratio, 
                                       # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                       fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(barren_temp_2070_ssp245 )
barren_temp_2070_ssp245

# Resample at correct resolution
barren_resampled_2070_ssp245 <- terra::resample(barren_temp_2070_ssp245, 
                                                x_lc, 
                                                method = "bilinear")

# Project raster
barren_2070_ssp245 <- terra::project(x = barren_resampled_2070_ssp245,
                                     y = x)

# Before saving, make the name of the value correspond to the variable
names(barren_2070_ssp245) <- "barren"
barren_2070_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
barren_2070_ssp245[is.na(barren_2070_ssp245)] <- 0
barren_2070_ssp245 <- terra::mask(x = barren_2070_ssp245, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(barren_2070_ssp245)
barren_2070_ssp245 <- resample(barren_2070_ssp245, forest_old, method = "near")

# Save results
writeRaster(barren_2070_ssp245, 
            filename = paste0(output_dir, "barren_proportion_1km_2070_ssp245.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  barren_1000_temp_2070_ssp126 <- terra::aggregate(landcover_2070_ssp126,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(barren_1000_temp_2070_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(barren_10km_2070_ssp126 <- terra::focal(barren_1000_temp_2070_ssp126, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells


# Resample at correct resolution
barren_10km_resampled_2070_ssp126 <- terra::resample(barren_10km_2070_ssp126, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
barren_10km_2070_ssp126 <- terra::project(x = barren_10km_resampled_2070_ssp126,
                                          y = x)

# Before saving, make the name of the value correspond to the variable
names(barren_10km_2070_ssp126) <- "barren_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
barren_10km_2070_ssp126[is.na(barren_10km_2070_ssp126)] <- 0
barren_10km_2070_ssp126 <- terra::mask(x = barren_10km_2070_ssp126, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(barren_10km_2070_ssp126)

barren_10km_2070_ssp126 <- resample(barren_10km_2070_ssp126, forest_old, method = "near")

# Save results
writeRaster(barren_10km_2070_ssp126, 
            filename = paste0(output_dir, "barren_10km_2070_ssp126.tif"), 
            overwrite = TRUE)



# built_up

system.time(
  built_up_temp_2070_ssp245 <- aggregate(landcover_2070_ssp245,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(built_up_temp_2070_ssp245 )
built_up_temp_2070_ssp245

# Resample at correct resolution
built_up_resampled_2070_ssp245 <- terra::resample(built_up_temp_2070_ssp245, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
built_up_2070_ssp245 <- terra::project(x = built_up_resampled_2070_ssp245,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(built_up_2070_ssp245) <- "built_up"
built_up_2070_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
built_up_2070_ssp245[is.na(built_up_2070_ssp245)] <- 0
built_up_2070_ssp245 <- terra::mask(x = built_up_2070_ssp245, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(built_up_2070_ssp245)
built_up_2070_ssp245 <- resample(built_up_2070_ssp245, forest_old, method = "near")

# Save results
writeRaster(built_up_2070_ssp245, 
            filename = paste0(output_dir, "built_up_proportion_1km_2070_ssp245.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  built_up_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(built_up_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(built_up_10km_2050_ssp126 <- terra::focal(built_up_1000_temp_2050_ssp126, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
built_up_10km_resampled_2050_ssp126 <- terra::resample(built_up_10km_2050_ssp126, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
built_up_10km_2050_ssp126 <- terra::project(x = built_up_10km_resampled_2050_ssp126,
                                            y = x)

# Before saving, make the name of the value correspond to the variable
names(built_up_10km_2050_ssp126) <- "built_up_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
built_up_10km_2050_ssp126[is.na(built_up_10km_2050_ssp126)] <- 0
built_up_10km_2050_ssp126 <- terra::mask(x = built_up_10km_2050_ssp126, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(built_up_10km_2050_ssp126)

built_up_10km_2050_ssp126 <- resample(built_up_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(built_up_10km_2050_ssp126, 
            filename = paste0(output_dir, "built_up_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# water

system.time(
  water_temp_2070_ssp245 <- aggregate(landcover_2070_ssp245,
                                      Nratio, 
                                      # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                      fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(water_temp_2070_ssp245 )
water_temp_2070_ssp245

# Resample at correct resolution
water_resampled_2070_ssp245 <- terra::resample(water_temp_2070_ssp245, 
                                               x_lc, 
                                               method = "bilinear")

# Project raster
water_2070_ssp245 <- terra::project(x = water_resampled_2070_ssp245,
                                    y = x)

# Before saving, make the name of the value correspond to the variable
names(water_2070_ssp245) <- "water"
water_2070_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
water_2070_ssp245[is.na(water_2070_ssp245)] <- 0
water_2070_ssp245 <- terra::mask(x = water_2070_ssp245, 
                                 mask = SECanadaVectTemp %>% 
                                   st_as_sf(.) %>%
                                   vect(.))

plot(water_2070_ssp245)
water_2070_ssp245 <- resample(water_2070_ssp245, forest_old, method = "near")

# Save results
writeRaster(water_2070_ssp245, 
            filename = paste0(output_dir, "water_proportion_1km_2070_ssp245.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  water_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                  Nratio_step, 
                                                  fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(water_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(water_10km_2050_ssp126 <- terra::focal(water_1000_temp_2050_ssp126, 
                                                   w = mat_10km,
                                                   fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
water_10km_resampled_2050_ssp126 <- terra::resample(water_10km_2050_ssp126, 
                                                    x_lc, 
                                                    method = "bilinear")

# Project raster
water_10km_2050_ssp126 <- terra::project(x = water_10km_resampled_2050_ssp126,
                                         y = x)

# Before saving, make the name of the value correspond to the variable
names(water_10km_2050_ssp126) <- "water_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
water_10km_2050_ssp126[is.na(water_10km_2050_ssp126)] <- 0
water_10km_2050_ssp126 <- terra::mask(x = water_10km_2050_ssp126, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(water_10km_2050_ssp126)

water_10km_2050_ssp126 <- resample(water_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(water_10km_2050_ssp126, 
            filename = paste0(output_dir, "water_10km_2050_ssp126.tif"), 
            overwrite = TRUE)





# 2070 ssp585

# Now, calculate the proportion of habitat per site for forest
system.time(
  forest_temp_2070_ssp585 <- aggregate(landcover_2070_ssp585,
                                       Nratio, 
                                       # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                       fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(forest_temp_2070_ssp585 )
forest_temp_2070_ssp585

# Resample at correct resolution
forest_resampled_2070_ssp585 <- terra::resample(forest_temp_2070_ssp585, 
                                                x_lc, 
                                                method = "bilinear")

# Project raster
forest_2070_ssp585 <- terra::project(x = forest_resampled_2070_ssp585,
                                     y = x)

# Before saving, make the name of the value correspond to the variable
names(forest_2070_ssp585) <- "forest"
forest_2070_ssp585

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
forest_2070_ssp585[is.na(forest_2070_ssp585)] <- 0
forest_2070_ssp585 <- terra::mask(x = forest_2070_ssp585, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(forest_2070_ssp585)

forest_2070_ssp585 <- resample(forest_2070_ssp585, forest_old, method = "near")

# Save results
writeRaster(forest_2070_ssp585, 
            filename = paste0(output_dir, "forest_proportion_1km_2070_ssp585.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  forest_1000_temp_2070_ssp245 <- terra::aggregate(landcover_2070_ssp245,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(forest_1000_temp_2070_ssp245)

system.time(forest_10km_2070_ssp245 <- terra::focal(forest_1000_temp_2070_ssp245, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121}))

# Resample at correct resolution
forest_10km_resampled_2070_ssp245 <- terra::resample(forest_10km_2070_ssp245, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
forest_10km_2070_ssp245 <- terra::project(x = forest_10km_resampled_2070_ssp245,
                                          y = x)

names(forest_10km_2070_ssp245) <- "forest_10km"

forest_10km_2070_ssp245[is.na(forest_10km_2070_ssp245)] <- 0
forest_10km_2070_ssp245 <- terra::mask(x = forest_10km_2070_ssp245, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(forest_10km_2070_ssp245)

forest_10km_2070_ssp245 <- resample(forest_10km_2070_ssp245, forest_old, method = "near")

writeRaster(forest_10km_2070_ssp245, 
            filename = paste0(output_dir, "forest_10km_2070_ssp245.tif"), 
            overwrite = TRUE)



# shrublands

system.time(
  shrublands_temp_2070_ssp585 <- aggregate(landcover_2070_ssp585,
                                           Nratio, 
                                           fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(shrublands_temp_2070_ssp585 )
shrublands_temp_2070_ssp585

shrublands_resampled_2070_ssp585 <- terra::resample(shrublands_temp_2070_ssp585, 
                                                    x_lc, 
                                                    method = "bilinear")

shrublands_2070_ssp585 <- terra::project(x = shrublands_resampled_2070_ssp585,
                                         y = x)

names(shrublands_2070_ssp585) <- "opforest"

shrublands_2070_ssp585[is.na(shrublands_2070_ssp585)] <- 0
shrublands_2070_ssp585 <- terra::mask(x = shrublands_2070_ssp585, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(shrublands_2070_ssp585)

shrublands_2070_ssp585 <- resample(shrublands_2070_ssp585, forest_old, method = "near")

writeRaster(shrublands_2070_ssp585, 
            filename = paste0(output_dir, "opforest_proportion_1km_2070_ssp585.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  shrublands_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                       Nratio_step, 
                                                       fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(shrublands_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(shrublands_10km_2050_ssp245 <- terra::focal(shrublands_1000_temp_2050_ssp245, 
                                                        w = mat_10km,
                                                        fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
shrublands_10km_resampled_2050_ssp245 <- terra::resample(shrublands_10km_2050_ssp245, 
                                                         x_lc, 
                                                         method = "bilinear")

# Project raster
shrublands_10km_2050_ssp245 <- terra::project(x = shrublands_10km_resampled_2050_ssp245,
                                              y = x)

# Before saving, make the name of the value correspond to the variable
names(shrublands_10km_2050_ssp245) <- "opforest_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
shrublands_10km_2050_ssp245[is.na(shrublands_10km_2050_ssp245)] <- 0
shrublands_10km_2050_ssp245 <- terra::mask(x = shrublands_10km_2050_ssp245, 
                                           mask = SECanadaVectTemp %>% 
                                             st_as_sf(.) %>%
                                             vect(.))

plot(shrublands_10km_2050_ssp245)

shrublands_10km_2050_ssp245 <- resample(shrublands_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(shrublands_10km_2050_ssp245, 
            filename = paste0(output_dir, "opforest_10km_2050_ssp245.tif"), 
            overwrite = TRUE)


# grassland

system.time(
  grassland_temp_2070_ssp585 <- aggregate(landcover_2070_ssp585,
                                          Nratio, 
                                          # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                          fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(grassland_temp_2070_ssp585 )
grassland_temp_2070_ssp585

# Resample at correct resolution
grassland_resampled_2070_ssp585 <- terra::resample(grassland_temp_2070_ssp585, 
                                                   x_lc, 
                                                   method = "bilinear")

# Project raster
grassland_2070_ssp585 <- terra::project(x = grassland_resampled_2070_ssp585,
                                        y = x)

# Before saving, make the name of the value correspond to the variable
names(grassland_2070_ssp585) <- "grassland"
grassland_2070_ssp585

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
grassland_2070_ssp585[is.na(grassland_2070_ssp585)] <- 0
grassland_2070_ssp585 <- terra::mask(x = grassland_2070_ssp585, 
                                     mask = SECanadaVectTemp %>% 
                                       st_as_sf(.) %>%
                                       vect(.))

plot(grassland_2070_ssp585)

grassland_2070_ssp585 <- resample(grassland_2070_ssp585, forest_old, method = "near")

# Save results
writeRaster(grassland_2070_ssp585, 
            filename = paste0(output_dir, "grassland_proportion_1km_2070_ssp585.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  grassland_1000_temp_2070_ssp245 <- terra::aggregate(landcover_2070_ssp245,
                                                      Nratio_step, 
                                                      fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(grassland_1000_temp_2070_ssp245)

system.time(grassland_10km_2070_ssp245 <- terra::focal(grassland_1000_temp_2070_ssp245, 
                                                       w = mat_10km,
                                                       fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121}))

# Resample at correct resolution
grassland_10km_resampled_2070_ssp245 <- terra::resample(grassland_10km_2070_ssp245, 
                                                        x_lc, 
                                                        method = "bilinear")

# Project raster
grassland_10km_2070_ssp245 <- terra::project(x = grassland_10km_resampled_2070_ssp245,
                                             y = x)

names(grassland_10km_2070_ssp245) <- "grassland_10km"

grassland_10km_2070_ssp245[is.na(grassland_10km_2070_ssp245)] <- 0
grassland_10km_2070_ssp245 <- terra::mask(x = grassland_10km_2070_ssp245, 
                                          mask = SECanadaVectTemp %>% 
                                            st_as_sf(.) %>%
                                            vect(.))

plot(grassland_10km_2070_ssp245)

grassland_10km_2070_ssp245 <- resample(grassland_10km_2070_ssp245, forest_old, method = "near")

writeRaster(grassland_10km_2070_ssp245, 
            filename = paste0(output_dir, "grassland_10km_2070_ssp245.tif"), 
            overwrite = TRUE)


# wetland

system.time(
  wetland_temp_2070_ssp585 <- aggregate(landcover_2070_ssp585,
                                        Nratio, 
                                        fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(wetland_temp_2070_ssp585 )
wetland_temp_2070_ssp585

wetland_resampled_2070_ssp585 <- terra::resample(wetland_temp_2070_ssp585, 
                                                 x_lc, 
                                                 method = "bilinear")

wetland_2070_ssp585 <- terra::project(x = wetland_resampled_2070_ssp585,
                                      y = x)

names(wetland_2070_ssp585) <- "wetland"

wetland_2070_ssp585[is.na(wetland_2070_ssp585)] <- 0
wetland_2070_ssp585 <- terra::mask(x = wetland_2070_ssp585, 
                                   mask = SECanadaVectTemp %>% 
                                     st_as_sf(.) %>%
                                     vect(.))

plot(wetland_2070_ssp585)
wetland_2070_ssp585 <- resample(wetland_2070_ssp585, forest_old, method = "near")

writeRaster(wetland_2070_ssp585, 
            filename = paste0(output_dir, "wetland_proportion_1km_2070_ssp585.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  wetland_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                    Nratio_step, 
                                                    fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(wetland_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(wetland_10km_2050_ssp245 <- terra::focal(wetland_1000_temp_2050_ssp245, 
                                                     w = mat_10km,
                                                     fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
wetland_10km_resampled_2050_ssp245 <- terra::resample(wetland_10km_2050_ssp245, 
                                                      x_lc, 
                                                      method = "bilinear")

# Project raster
wetland_10km_2050_ssp245 <- terra::project(x = wetland_10km_resampled_2050_ssp245,
                                           y = x)

# Before saving, make the name of the value correspond to the variable
names(wetland_10km_2050_ssp245) <- "wetland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
wetland_10km_2050_ssp245[is.na(wetland_10km_2050_ssp245)] <- 0
wetland_10km_2050_ssp245 <- terra::mask(x = wetland_10km_2050_ssp245, 
                                        mask = SECanadaVectTemp %>% 
                                          st_as_sf(.) %>%
                                          vect(.))

plot(wetland_10km_2050_ssp245)

wetland_10km_2050_ssp245 <- resample(wetland_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(wetland_10km_2050_ssp245, 
            filename = paste0(output_dir, "wetland_10km_2050_ssp245.tif"), 
            overwrite = TRUE)


# cropland

system.time(
  cropland_temp_2070_ssp585 <- aggregate(landcover_2070_ssp585,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(cropland_temp_2070_ssp585 )
cropland_temp_2070_ssp585

# Resample at correct resolution
cropland_resampled_2070_ssp585 <- terra::resample(cropland_temp_2070_ssp585, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
cropland_2070_ssp585 <- terra::project(x = cropland_resampled_2070_ssp585,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(cropland_2070_ssp585) <- "cropland"
cropland_2070_ssp585

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

cropland_2070_ssp585[is.na(cropland_2070_ssp585)] <- 0
cropland_2070_ssp585 <- terra::mask(x = cropland_2070_ssp585, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(cropland_2070_ssp585)
cropland_2070_ssp585 <- resample(cropland_2070_ssp585, forest_old, method = "near")

writeRaster(cropland_2070_ssp585, 
            filename = paste0(output_dir, "cropland_proportion_1km_2070_ssp585.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  cropland_1000_temp_2070_ssp245 <- terra::aggregate(landcover_2070_ssp245,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(cropland_1000_temp_2070_ssp245)

system.time(cropland_10km_2070_ssp245 <- terra::focal(cropland_1000_temp_2070_ssp245, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121}))

# Resample at correct resolution
cropland_10km_resampled_2070_ssp245 <- terra::resample(cropland_10km_2070_ssp245, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
cropland_10km_2070_ssp245 <- terra::project(x = cropland_10km_resampled_2070_ssp245,
                                            y = x)

names(cropland_10km_2070_ssp245) <- "cropland_10km"

cropland_10km_2070_ssp245[is.na(cropland_10km_2070_ssp245)] <- 0
cropland_10km_2070_ssp245 <- terra::mask(x = cropland_10km_2070_ssp245, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(cropland_10km_2070_ssp245)

cropland_10km_2070_ssp245 <- resample(cropland_10km_2070_ssp245, forest_old, method = "near")

writeRaster(cropland_10km_2070_ssp245, 
            filename = paste0(output_dir, "cropland_10km_2070_ssp245.tif"), 
            overwrite = TRUE)


# barren

system.time(
  barren_temp_2070_ssp585 <- aggregate(landcover_2070_ssp585,
                                       Nratio, 
                                       fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(barren_temp_2070_ssp585 )
barren_temp_2070_ssp585

barren_resampled_2070_ssp585 <- terra::resample(barren_temp_2070_ssp585, 
                                                x_lc, 
                                                method = "bilinear")

barren_2070_ssp585 <- terra::project(x = barren_resampled_2070_ssp585,
                                     y = x)

names(barren_2070_ssp585) <- "barren"

barren_2070_ssp585[is.na(barren_2070_ssp585)] <- 0
barren_2070_ssp585 <- terra::mask(x = barren_2070_ssp585, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(barren_2070_ssp585)
barren_2070_ssp585 <- resample(barren_2070_ssp585, forest_old, method = "near")

writeRaster(barren_2070_ssp585, 
            filename = paste0(output_dir, "barren_proportion_1km_2070_ssp585.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  barren_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(barren_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(barren_10km_2050_ssp126 <- terra::focal(barren_1000_temp_2050_ssp126, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells


# Resample at correct resolution
barren_10km_resampled_2050_ssp126 <- terra::resample(barren_10km_2050_ssp126, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
barren_10km_2050_ssp126 <- terra::project(x = barren_10km_resampled_2050_ssp126,
                                          y = x)

# Before saving, make the name of the value correspond to the variable
names(barren_10km_2050_ssp126) <- "barren_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
barren_10km_2050_ssp126[is.na(barren_10km_2050_ssp126)] <- 0
barren_10km_2050_ssp126 <- terra::mask(x = barren_10km_2050_ssp126, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(barren_10km_2050_ssp126)

barren_10km_2050_ssp126 <- resample(barren_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(barren_10km_2050_ssp126, 
            filename = paste0(output_dir, "barren_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# built_up

system.time(
  built_up_temp_2070_ssp585 <- aggregate(landcover_2070_ssp585,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(built_up_temp_2070_ssp585 )
built_up_temp_2070_ssp585

# Resample at correct resolution
built_up_resampled_2070_ssp585 <- terra::resample(built_up_temp_2070_ssp585, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
built_up_2070_ssp585 <- terra::project(x = built_up_resampled_2070_ssp585,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(built_up_2070_ssp585) <- "built_up"
built_up_2070_ssp585

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

built_up_2070_ssp585[is.na(built_up_2070_ssp585)] <- 0
built_up_2070_ssp585 <- terra::mask(x = built_up_2070_ssp585, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(built_up_2070_ssp585)
built_up_2070_ssp585 <- resample(built_up_2070_ssp585, forest_old, method = "near")

writeRaster(built_up_2070_ssp585, 
            filename = paste0(output_dir, "built_up_proportion_1km_2070_ssp585.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  built_up_1000_temp_2070_ssp126 <- terra::aggregate(landcover_2070_ssp126,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(built_up_1000_temp_2070_ssp126)

system.time(built_up_10km_2070_ssp126 <- terra::focal(built_up_1000_temp_2070_ssp126, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121}))

# Resample at correct resolution
built_up_10km_resampled_2070_ssp126 <- terra::resample(built_up_10km_2070_ssp126, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
built_up_10km_2070_ssp126 <- terra::project(x = built_up_10km_resampled_2070_ssp126,
                                            y = x)

names(built_up_10km_2070_ssp126) <- "built_up_10km"

built_up_10km_2070_ssp126[is.na(built_up_10km_2070_ssp126)] <- 0
built_up_10km_2070_ssp126 <- terra::mask(x = built_up_10km_2070_ssp126, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(built_up_10km_2070_ssp126)

built_up_10km_2070_ssp126 <- resample(built_up_10km_2070_ssp126, forest_old, method = "near")

writeRaster(built_up_10km_2070_ssp126, 
            filename = paste0(output_dir, "built_up_10km_2070_ssp126.tif"), 
            overwrite = TRUE)



# water

system.time(
  water_temp_2070_ssp585 <- aggregate(landcover_2070_ssp585,
                                      Nratio, 
                                      # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                      fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(water_temp_2070_ssp585 )
water_temp_2070_ssp585

# Resample at correct resolution
water_resampled_2070_ssp585 <- terra::resample(water_temp_2070_ssp585, 
                                               x_lc, 
                                               method = "bilinear")

# Project raster
water_2070_ssp585 <- terra::project(x = water_resampled_2070_ssp585,
                                    y = x)

# Before saving, make the name of the value correspond to the variable
names(water_2070_ssp585) <- "water"
water_2070_ssp585

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

water_2070_ssp585[is.na(water_2070_ssp585)] <- 0
water_2070_ssp585 <- terra::mask(x = water_2070_ssp585, 
                                 mask = SECanadaVectTemp %>% 
                                   st_as_sf(.) %>%
                                   vect(.))

plot(water_2070_ssp585)
water_2070_ssp585 <- resample(water_2070_ssp585, forest_old, method = "near")

writeRaster(water_2070_ssp585, 
            filename = paste0(output_dir, "water_proportion_1km_2070_ssp585.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  water_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                  Nratio_step, 
                                                  fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(water_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(water_10km_2050_ssp126 <- terra::focal(water_1000_temp_2050_ssp126, 
                                                   w = mat_10km,
                                                   fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
water_10km_resampled_2050_ssp126 <- terra::resample(water_10km_2050_ssp126, 
                                                    x_lc, 
                                                    method = "bilinear")

# Project raster
water_10km_2050_ssp126 <- terra::project(x = water_10km_resampled_2050_ssp126,
                                         y = x)

# Before saving, make the name of the value correspond to the variable
names(water_10km_2050_ssp126) <- "water_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
water_10km_2050_ssp126[is.na(water_10km_2050_ssp126)] <- 0
water_10km_2050_ssp126 <- terra::mask(x = water_10km_2050_ssp126, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(water_10km_2050_ssp126)

water_10km_2050_ssp126 <- resample(water_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(water_10km_2050_ssp126, 
            filename = paste0(output_dir, "water_10km_2050_ssp126.tif"), 
            overwrite = TRUE)





# -------------------2100-------------------------

# Now, calculate the proportion of habitat per site for forest
system.time(
  forest_temp_2100_ssp126 <- aggregate(landcover_2100_ssp126,
                                       Nratio, 
                                       # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                       fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(forest_temp_2100_ssp126 )
forest_temp_2100_ssp126

# Resample at correct resolution
forest_resampled_2100_ssp126 <- terra::resample(forest_temp_2100_ssp126, 
                                                x_lc, 
                                                method = "bilinear")

# Project raster
forest_2100_ssp126 <- terra::project(x = forest_resampled_2100_ssp126,
                                     y = x)

# Before saving, make the name of the value correspond to the variable
names(forest_2100_ssp126) <- "forest"
forest_2100_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
forest_2100_ssp126[is.na(forest_2100_ssp126)] <- 0
forest_2100_ssp126 <- terra::mask(x = forest_2100_ssp126, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(forest_2100_ssp126)

forest_2100_ssp126 <- resample(forest_2100_ssp126, forest_old, method = "near")

# Save results
writeRaster(forest_2100_ssp126, 
            filename = paste0(output_dir, "forest_proportion_1km_2100_ssp126.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  forest_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(forest_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(forest_10km_2050_ssp126 <- terra::focal(forest_1000_temp_2050_ssp126, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
forest_10km_resampled_2050_ssp126 <- terra::resample(forest_10km_2050_ssp126, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
forest_10km_2050_ssp126 <- terra::project(x = forest_10km_resampled_2050_ssp126,
                                          y = x)

# Before saving, make the name of the value correspond to the variable
names(forest_10km_2050_ssp126) <- "forest_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
forest_10km_2050_ssp126[is.na(forest_10km_2050_ssp126)] <- 0
forest_10km_2050_ssp126 <- terra::mask(x = forest_10km_2050_ssp126, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(forest_10km_2050_ssp126)

forest_10km_2050_ssp126 <- resample(forest_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(forest_10km_2050_ssp126, 
            filename = paste0(output_dir, "forest_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# shrublands

system.time(
  shrublands_temp_2100_ssp126 <- aggregate(landcover_2100_ssp126,
                                           Nratio, 
                                           # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                           fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(shrublands_temp_2100_ssp126 )
shrublands_temp_2100_ssp126

# Resample at correct resolution
shrublands_resampled_2100_ssp126 <- terra::resample(shrublands_temp_2100_ssp126, 
                                                    x_lc, 
                                                    method = "bilinear")

# Project raster
shrublands_2100_ssp126 <- terra::project(x = shrublands_resampled_2100_ssp126,
                                         y = x)

# Before saving, make the name of the value correspond to the variable
names(shrublands_2100_ssp126) <- "opforest"
shrublands_2100_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
shrublands_2100_ssp126[is.na(shrublands_2100_ssp126)] <- 0
shrublands_2100_ssp126 <- terra::mask(x = shrublands_2100_ssp126, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(shrublands_2100_ssp126)

shrublands_2100_ssp126 <- resample(shrublands_2100_ssp126, forest_old, method = "near")

# Save results
writeRaster(shrublands_2100_ssp126, 
            filename = paste0(output_dir, "opforest_proportion_1km_2100_ssp126.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  shrublands_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                       Nratio_step, 
                                                       fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(shrublands_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(shrublands_10km_2050_ssp126 <- terra::focal(shrublands_1000_temp_2050_ssp126, 
                                                        w = mat_10km,
                                                        fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
shrublands_10km_resampled_2050_ssp126 <- terra::resample(shrublands_10km_2050_ssp126, 
                                                         x_lc, 
                                                         method = "bilinear")

# Project raster
shrublands_10km_2050_ssp126 <- terra::project(x = shrublands_10km_resampled_2050_ssp126,
                                              y = x)

# Before saving, make the name of the value correspond to the variable
names(shrublands_10km_2050_ssp126) <- "opforest_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
shrublands_10km_2050_ssp126[is.na(shrublands_10km_2050_ssp126)] <- 0
shrublands_10km_2050_ssp126 <- terra::mask(x = shrublands_10km_2050_ssp126, 
                                           mask = SECanadaVectTemp %>% 
                                             st_as_sf(.) %>%
                                             vect(.))

plot(shrublands_10km_2050_ssp126)

shrublands_10km_2050_ssp126 <- resample(shrublands_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(shrublands_10km_2050_ssp126, 
            filename = paste0(output_dir, "opforest_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# grassland

system.time(
  grassland_temp_2100_ssp126 <- aggregate(landcover_2100_ssp126,
                                          Nratio, 
                                          # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                          fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(grassland_temp_2100_ssp126 )
grassland_temp_2100_ssp126

# Resample at correct resolution
grassland_resampled_2100_ssp126 <- terra::resample(grassland_temp_2100_ssp126, 
                                                   x_lc, 
                                                   method = "bilinear")

# Project raster
grassland_2100_ssp126 <- terra::project(x = grassland_resampled_2100_ssp126,
                                        y = x)

# Before saving, make the name of the value correspond to the variable
names(grassland_2100_ssp126) <- "grassland"
grassland_2100_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
grassland_2100_ssp126[is.na(grassland_2100_ssp126)] <- 0
grassland_2100_ssp126 <- terra::mask(x = grassland_2100_ssp126, 
                                     mask = SECanadaVectTemp %>% 
                                       st_as_sf(.) %>%
                                       vect(.))

plot(grassland_2100_ssp126)

grassland_2100_ssp126 <- resample(grassland_2100_ssp126, forest_old, method = "near")

# Save results
writeRaster(grassland_2100_ssp126, 
            filename = paste0(output_dir, "grassland_proportion_1km_2100_ssp126.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  grassland_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                      Nratio_step, 
                                                      fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(grassland_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(grassland_10km_2050_ssp126 <- terra::focal(grassland_1000_temp_2050_ssp126, 
                                                       w = mat_10km,
                                                       fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
grassland_10km_resampled_2050_ssp126 <- terra::resample(grassland_10km_2050_ssp126, 
                                                        x_lc, 
                                                        method = "bilinear")

# Project raster
grassland_10km_2050_ssp126 <- terra::project(x = grassland_10km_resampled_2050_ssp126,
                                             y = x)

# Before saving, make the name of the value correspond to the variable
names(grassland_10km_2050_ssp126) <- "grassland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
grassland_10km_2050_ssp126[is.na(grassland_10km_2050_ssp126)] <- 0
grassland_10km_2050_ssp126 <- terra::mask(x = grassland_10km_2050_ssp126, 
                                          mask = SECanadaVectTemp %>% 
                                            st_as_sf(.) %>%
                                            vect(.))

plot(grassland_10km_2050_ssp126)

grassland_10km_2050_ssp126 <- resample(grassland_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(grassland_10km_2050_ssp126, 
            filename = paste0(output_dir, "grassland_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# wetland

system.time(
  wetland_temp_2100_ssp126 <- aggregate(landcover_2100_ssp126,
                                        Nratio, 
                                        # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                        fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(wetland_temp_2100_ssp126 )
wetland_temp_2100_ssp126

# Resample at correct resolution
wetland_resampled_2100_ssp126 <- terra::resample(wetland_temp_2100_ssp126, 
                                                 x_lc, 
                                                 method = "bilinear")

# Project raster
wetland_2100_ssp126 <- terra::project(x = wetland_resampled_2100_ssp126,
                                      y = x)

# Before saving, make the name of the value correspond to the variable
names(wetland_2100_ssp126) <- "wetland"
wetland_2100_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
wetland_2100_ssp126[is.na(wetland_2100_ssp126)] <- 0
wetland_2100_ssp126 <- terra::mask(x = wetland_2100_ssp126, 
                                   mask = SECanadaVectTemp %>% 
                                     st_as_sf(.) %>%
                                     vect(.))

plot(wetland_2100_ssp126)
wetland_2100_ssp126 <- resample(wetland_2100_ssp126, forest_old, method = "near")

# Save results
writeRaster(wetland_2100_ssp126, 
            filename = paste0(output_dir, "wetland_proportion_1km_2100_ssp126.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  wetland_1000_temp_2070_ssp126 <- terra::aggregate(landcover_2070_ssp126,
                                                    Nratio_step, 
                                                    fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(wetland_1000_temp_2070_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(wetland_10km_2070_ssp126 <- terra::focal(wetland_1000_temp_2070_ssp126, 
                                                     w = mat_10km,
                                                     fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
wetland_10km_resampled_2070_ssp126 <- terra::resample(wetland_10km_2070_ssp126, 
                                                      x_lc, 
                                                      method = "bilinear")

# Project raster
wetland_10km_2070_ssp126 <- terra::project(x = wetland_10km_resampled_2070_ssp126,
                                           y = x)

# Before saving, make the name of the value correspond to the variable
names(wetland_10km_2070_ssp126) <- "wetland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
wetland_10km_2070_ssp126[is.na(wetland_10km_2070_ssp126)] <- 0
wetland_10km_2070_ssp126 <- terra::mask(x = wetland_10km_2070_ssp126, 
                                        mask = SECanadaVectTemp %>% 
                                          st_as_sf(.) %>%
                                          vect(.))

plot(wetland_10km_2070_ssp126)

wetland_10km_2070_ssp126 <- resample(wetland_10km_2070_ssp126, forest_old, method = "near")

# Save results
writeRaster(wetland_10km_2070_ssp126, 
            filename = paste0(output_dir, "wetland_10km_2070_ssp126.tif"), 
            overwrite = TRUE)




# cropland

system.time(
  cropland_temp_2100_ssp126 <- aggregate(landcover_2100_ssp126,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(cropland_temp_2100_ssp126 )
cropland_temp_2100_ssp126

# Resample at correct resolution
cropland_resampled_2100_ssp126 <- terra::resample(cropland_temp_2100_ssp126, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
cropland_2100_ssp126 <- terra::project(x = cropland_resampled_2100_ssp126,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(cropland_2100_ssp126) <- "cropland"
cropland_2100_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
cropland_2100_ssp126[is.na(cropland_2100_ssp126)] <- 0
cropland_2100_ssp126 <- terra::mask(x = cropland_2100_ssp126, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(cropland_2100_ssp126)
cropland_2100_ssp126 <- resample(cropland_2100_ssp126, forest_old, method = "near")

# Save results
writeRaster(cropland_2100_ssp126, 
            filename = paste0(output_dir, "cropland_proportion_1km_2100_ssp126.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  cropland_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(cropland_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(cropland_10km_2050_ssp126 <- terra::focal(cropland_1000_temp_2050_ssp126, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
cropland_10km_resampled_2050_ssp126 <- terra::resample(cropland_10km_2050_ssp126, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
cropland_10km_2050_ssp126 <- terra::project(x = cropland_10km_resampled_2050_ssp126,
                                            y = x)

# Before saving, make the name of the value correspond to the variable
names(cropland_10km_2050_ssp126) <- "cropland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
cropland_10km_2050_ssp126[is.na(cropland_10km_2050_ssp126)] <- 0
cropland_10km_2050_ssp126 <- terra::mask(x = cropland_10km_2050_ssp126, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(cropland_10km_2050_ssp126)

cropland_10km_2050_ssp126 <- resample(cropland_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(cropland_10km_2050_ssp126, 
            filename = paste0(output_dir, "cropland_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# barren

system.time(
  barren_temp_2100_ssp126 <- aggregate(landcover_2100_ssp126,
                                       Nratio, 
                                       # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                       fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(barren_temp_2100_ssp126 )
barren_temp_2100_ssp126

# Resample at correct resolution
barren_resampled_2100_ssp126 <- terra::resample(barren_temp_2100_ssp126, 
                                                x_lc, 
                                                method = "bilinear")

# Project raster
barren_2100_ssp126 <- terra::project(x = barren_resampled_2100_ssp126,
                                     y = x)

# Before saving, make the name of the value correspond to the variable
names(barren_2100_ssp126) <- "barren"
barren_2100_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
barren_2100_ssp126[is.na(barren_2100_ssp126)] <- 0
barren_2100_ssp126 <- terra::mask(x = barren_2100_ssp126, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(barren_2100_ssp126)
barren_2100_ssp126 <- resample(barren_2100_ssp126, forest_old, method = "near")

# Save results
writeRaster(barren_2100_ssp126, 
            filename = paste0(output_dir, "barren_proportion_1km_2100_ssp126.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  barren_1000_temp_2070_ssp126 <- terra::aggregate(landcover_2070_ssp126,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(barren_1000_temp_2070_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(barren_10km_2070_ssp126 <- terra::focal(barren_1000_temp_2070_ssp126, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells


# Resample at correct resolution
barren_10km_resampled_2070_ssp126 <- terra::resample(barren_10km_2070_ssp126, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
barren_10km_2070_ssp126 <- terra::project(x = barren_10km_resampled_2070_ssp126,
                                          y = x)

# Before saving, make the name of the value correspond to the variable
names(barren_10km_2070_ssp126) <- "barren_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
barren_10km_2070_ssp126[is.na(barren_10km_2070_ssp126)] <- 0
barren_10km_2070_ssp126 <- terra::mask(x = barren_10km_2070_ssp126, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(barren_10km_2070_ssp126)

barren_10km_2070_ssp126 <- resample(barren_10km_2070_ssp126, forest_old, method = "near")

# Save results
writeRaster(barren_10km_2070_ssp126, 
            filename = paste0(output_dir, "barren_10km_2070_ssp126.tif"), 
            overwrite = TRUE)



# built_up

system.time(
  built_up_temp_2100_ssp126 <- aggregate(landcover_2100_ssp126,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(built_up_temp_2100_ssp126 )
built_up_temp_2100_ssp126

# Resample at correct resolution
built_up_resampled_2100_ssp126 <- terra::resample(built_up_temp_2100_ssp126, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
built_up_2100_ssp126 <- terra::project(x = built_up_resampled_2100_ssp126,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(built_up_2100_ssp126) <- "built_up"
built_up_2100_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
built_up_2100_ssp126[is.na(built_up_2100_ssp126)] <- 0
built_up_2100_ssp126 <- terra::mask(x = built_up_2100_ssp126, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(built_up_2100_ssp126)
built_up_2100_ssp126 <- resample(built_up_2100_ssp126, forest_old, method = "near")

# Save results
writeRaster(built_up_2100_ssp126, 
            filename = paste0(output_dir, "built_up_proportion_1km_2100_ssp126.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  built_up_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(built_up_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(built_up_10km_2050_ssp126 <- terra::focal(built_up_1000_temp_2050_ssp126, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
built_up_10km_resampled_2050_ssp126 <- terra::resample(built_up_10km_2050_ssp126, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
built_up_10km_2050_ssp126 <- terra::project(x = built_up_10km_resampled_2050_ssp126,
                                            y = x)

# Before saving, make the name of the value correspond to the variable
names(built_up_10km_2050_ssp126) <- "built_up_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
built_up_10km_2050_ssp126[is.na(built_up_10km_2050_ssp126)] <- 0
built_up_10km_2050_ssp126 <- terra::mask(x = built_up_10km_2050_ssp126, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(built_up_10km_2050_ssp126)

built_up_10km_2050_ssp126 <- resample(built_up_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(built_up_10km_2050_ssp126, 
            filename = paste0(output_dir, "built_up_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# water

system.time(
  water_temp_2100_ssp126 <- aggregate(landcover_2100_ssp126,
                                      Nratio, 
                                      # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                      fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(water_temp_2100_ssp126 )
water_temp_2100_ssp126

# Resample at correct resolution
water_resampled_2100_ssp126 <- terra::resample(water_temp_2100_ssp126, 
                                               x_lc, 
                                               method = "bilinear")

# Project raster
water_2100_ssp126 <- terra::project(x = water_resampled_2100_ssp126,
                                    y = x)

# Before saving, make the name of the value correspond to the variable
names(water_2100_ssp126) <- "water"
water_2100_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
water_2100_ssp126[is.na(water_2100_ssp126)] <- 0
water_2100_ssp126 <- terra::mask(x = water_2100_ssp126, 
                                 mask = SECanadaVectTemp %>% 
                                   st_as_sf(.) %>%
                                   vect(.))

plot(water_2100_ssp126)
water_2100_ssp126 <- resample(water_2100_ssp126, forest_old, method = "near")

# Save results
writeRaster(water_2100_ssp126, 
            filename = paste0(output_dir, "water_proportion_1km_2100_ssp126.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  water_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                  Nratio_step, 
                                                  fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(water_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(water_10km_2050_ssp126 <- terra::focal(water_1000_temp_2050_ssp126, 
                                                   w = mat_10km,
                                                   fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
water_10km_resampled_2050_ssp126 <- terra::resample(water_10km_2050_ssp126, 
                                                    x_lc, 
                                                    method = "bilinear")

# Project raster
water_10km_2050_ssp126 <- terra::project(x = water_10km_resampled_2050_ssp126,
                                         y = x)

# Before saving, make the name of the value correspond to the variable
names(water_10km_2050_ssp126) <- "water_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
water_10km_2050_ssp126[is.na(water_10km_2050_ssp126)] <- 0
water_10km_2050_ssp126 <- terra::mask(x = water_10km_2050_ssp126, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(water_10km_2050_ssp126)

water_10km_2050_ssp126 <- resample(water_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(water_10km_2050_ssp126, 
            filename = paste0(output_dir, "water_10km_2050_ssp126.tif"), 
            overwrite = TRUE)




# 2100 ssp245

# Now, calculate the proportion of habitat per site for forest
system.time(
  forest_temp_2100_ssp245 <- aggregate(landcover_2100_ssp245,
                                       Nratio, 
                                       # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                       fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(forest_temp_2100_ssp245 )
forest_temp_2100_ssp245

# Resample at correct resolution
forest_resampled_2100_ssp245 <- terra::resample(forest_temp_2100_ssp245, 
                                                x_lc, 
                                                method = "bilinear")

# Project raster
forest_2100_ssp245 <- terra::project(x = forest_resampled_2100_ssp245,
                                     y = x)

# Before saving, make the name of the value correspond to the variable
names(forest_2100_ssp245) <- "forest"
forest_2100_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
forest_2100_ssp245[is.na(forest_2100_ssp245)] <- 0
forest_2100_ssp245 <- terra::mask(x = forest_2100_ssp245, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(forest_2100_ssp245)

forest_2100_ssp245 <- resample(forest_2100_ssp245, forest_old, method = "near")

# Save results
writeRaster(forest_2100_ssp245, 
            filename = paste0(output_dir, "forest_proportion_1km_2100_ssp245.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  forest_1000_temp_2100_ssp245 <- terra::aggregate(landcover_2100_ssp245,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(forest_1000_temp_2100_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(forest_10km_2100_ssp245 <- terra::focal(forest_1000_temp_2100_ssp245, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
forest_10km_resampled_2100_ssp245 <- terra::resample(forest_10km_2100_ssp245, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
forest_10km_2100_ssp245 <- terra::project(x = forest_10km_resampled_2100_ssp245,
                                          y = x)

# Before saving, make the name of the value correspond to the variable
names(forest_10km_2100_ssp245) <- "forest_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
forest_10km_2100_ssp245[is.na(forest_10km_2100_ssp245)] <- 0
forest_10km_2100_ssp245 <- terra::mask(x = forest_10km_2100_ssp245, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(forest_10km_2100_ssp245)

forest_10km_2100_ssp245 <- resample(forest_10km_2100_ssp245, forest_old, method = "near")

# Save results
writeRaster(forest_10km_2100_ssp245, 
            filename = paste0(output_dir, "forest_10km_2100_ssp245.tif"), 
            overwrite = TRUE)



# shrublands

system.time(
  shrublands_temp_2100_ssp245 <- aggregate(landcover_2100_ssp245,
                                           Nratio, 
                                           # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                           fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(shrublands_temp_2100_ssp245 )
shrublands_temp_2100_ssp245

# Resample at correct resolution
shrublands_resampled_2100_ssp245 <- terra::resample(shrublands_temp_2100_ssp245, 
                                                    x_lc, 
                                                    method = "bilinear")

# Project raster
shrublands_2100_ssp245 <- terra::project(x = shrublands_resampled_2100_ssp245,
                                         y = x)

# Before saving, make the name of the value correspond to the variable
names(shrublands_2100_ssp245) <- "opforest"
shrublands_2100_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
shrublands_2100_ssp245[is.na(shrublands_2100_ssp245)] <- 0
shrublands_2100_ssp245 <- terra::mask(x = shrublands_2100_ssp245, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(shrublands_2100_ssp245)

shrublands_2100_ssp245 <- resample(shrublands_2100_ssp245, forest_old, method = "near")

# Save results
writeRaster(shrublands_2100_ssp245, 
            filename = paste0(output_dir, "opforest_proportion_1km_2100_ssp245.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  shrublands_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                       Nratio_step, 
                                                       fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(shrublands_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(shrublands_10km_2050_ssp245 <- terra::focal(shrublands_1000_temp_2050_ssp245, 
                                                        w = mat_10km,
                                                        fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
shrublands_10km_resampled_2050_ssp245 <- terra::resample(shrublands_10km_2050_ssp245, 
                                                         x_lc, 
                                                         method = "bilinear")

# Project raster
shrublands_10km_2050_ssp245 <- terra::project(x = shrublands_10km_resampled_2050_ssp245,
                                              y = x)

# Before saving, make the name of the value correspond to the variable
names(shrublands_10km_2050_ssp245) <- "opforest_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
shrublands_10km_2050_ssp245[is.na(shrublands_10km_2050_ssp245)] <- 0
shrublands_10km_2050_ssp245 <- terra::mask(x = shrublands_10km_2050_ssp245, 
                                           mask = SECanadaVectTemp %>% 
                                             st_as_sf(.) %>%
                                             vect(.))

plot(shrublands_10km_2050_ssp245)

shrublands_10km_2050_ssp245 <- resample(shrublands_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(shrublands_10km_2050_ssp245, 
            filename = paste0(output_dir, "opforest_10km_2050_ssp245.tif"), 
            overwrite = TRUE)


# grassland

system.time(
  grassland_temp_2100_ssp245 <- aggregate(landcover_2100_ssp245,
                                          Nratio, 
                                          # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                          fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(grassland_temp_2100_ssp245 )
grassland_temp_2100_ssp245

# Resample at correct resolution
grassland_resampled_2100_ssp245 <- terra::resample(grassland_temp_2100_ssp245, 
                                                   x_lc, 
                                                   method = "bilinear")

# Project raster
grassland_2100_ssp245 <- terra::project(x = grassland_resampled_2100_ssp245,
                                        y = x)

# Before saving, make the name of the value correspond to the variable
names(grassland_2100_ssp245) <- "grassland"
grassland_2100_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
grassland_2100_ssp245[is.na(grassland_2100_ssp245)] <- 0
grassland_2100_ssp245 <- terra::mask(x = grassland_2100_ssp245, 
                                     mask = SECanadaVectTemp %>% 
                                       st_as_sf(.) %>%
                                       vect(.))

plot(grassland_2100_ssp245)

grassland_2100_ssp245 <- resample(grassland_2100_ssp245, forest_old, method = "near")

# Save results
writeRaster(grassland_2100_ssp245, 
            filename = paste0(output_dir, "grassland_proportion_1km_2100_ssp245.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  grassland_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                      Nratio_step, 
                                                      fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(grassland_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(grassland_10km_2050_ssp245 <- terra::focal(grassland_1000_temp_2050_ssp245, 
                                                       w = mat_10km,
                                                       fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
grassland_10km_resampled_2050_ssp245 <- terra::resample(grassland_10km_2050_ssp245, 
                                                        x_lc, 
                                                        method = "bilinear")

# Project raster
grassland_10km_2050_ssp245 <- terra::project(x = grassland_10km_resampled_2050_ssp245,
                                             y = x)

# Before saving, make the name of the value correspond to the variable
names(grassland_10km_2050_ssp245) <- "grassland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
grassland_10km_2050_ssp245[is.na(grassland_10km_2050_ssp245)] <- 0
grassland_10km_2050_ssp245 <- terra::mask(x = grassland_10km_2050_ssp245, 
                                          mask = SECanadaVectTemp %>% 
                                            st_as_sf(.) %>%
                                            vect(.))

plot(grassland_10km_2050_ssp245)

grassland_10km_2050_ssp245 <- resample(grassland_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(grassland_10km_2050_ssp245, 
            filename = paste0(output_dir, "grassland_10km_2050_ssp245.tif"), 
            overwrite = TRUE)


# wetland

system.time(
  wetland_temp_2100_ssp245 <- aggregate(landcover_2100_ssp245,
                                        Nratio, 
                                        # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                        fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(wetland_temp_2100_ssp245 )
wetland_temp_2100_ssp245

# Resample at correct resolution
wetland_resampled_2100_ssp245 <- terra::resample(wetland_temp_2100_ssp245, 
                                                 x_lc, 
                                                 method = "bilinear")

# Project raster
wetland_2100_ssp245 <- terra::project(x = wetland_resampled_2100_ssp245,
                                      y = x)

# Before saving, make the name of the value correspond to the variable
names(wetland_2100_ssp245) <- "wetland"
wetland_2100_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
wetland_2100_ssp245[is.na(wetland_2100_ssp245)] <- 0
wetland_2100_ssp245 <- terra::mask(x = wetland_2100_ssp245, 
                                   mask = SECanadaVectTemp %>% 
                                     st_as_sf(.) %>%
                                     vect(.))

plot(wetland_2100_ssp245)
wetland_2100_ssp245 <- resample(wetland_2100_ssp245, forest_old, method = "near")

# Save results
writeRaster(wetland_2100_ssp245, 
            filename = paste0(output_dir, "wetland_proportion_1km_2100_ssp245.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  wetland_1000_temp_2070_ssp245 <- terra::aggregate(landcover_2070_ssp245,
                                                    Nratio_step, 
                                                    fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(wetland_1000_temp_2070_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(wetland_10km_2070_ssp245 <- terra::focal(wetland_1000_temp_2070_ssp245, 
                                                     w = mat_10km,
                                                     fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
wetland_10km_resampled_2070_ssp245 <- terra::resample(wetland_10km_2070_ssp245, 
                                                      x_lc, 
                                                      method = "bilinear")

# Project raster
wetland_10km_2070_ssp245 <- terra::project(x = wetland_10km_resampled_2070_ssp245,
                                           y = x)

# Before saving, make the name of the value correspond to the variable
names(wetland_10km_2070_ssp245) <- "wetland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
wetland_10km_2070_ssp245[is.na(wetland_10km_2070_ssp245)] <- 0
wetland_10km_2070_ssp245 <- terra::mask(x = wetland_10km_2070_ssp245, 
                                        mask = SECanadaVectTemp %>% 
                                          st_as_sf(.) %>%
                                          vect(.))

plot(wetland_10km_2070_ssp245)

wetland_10km_2070_ssp245 <- resample(wetland_10km_2070_ssp245, forest_old, method = "near")

# Save results
writeRaster(wetland_10km_2070_ssp245, 
            filename = paste0(output_dir, "wetland_10km_2070_ssp245.tif"), 
            overwrite = TRUE)


# cropland

system.time(
  cropland_temp_2100_ssp245 <- aggregate(landcover_2100_ssp245,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(cropland_temp_2100_ssp245 )
cropland_temp_2100_ssp245

# Resample at correct resolution
cropland_resampled_2100_ssp245 <- terra::resample(cropland_temp_2100_ssp245, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
cropland_2100_ssp245 <- terra::project(x = cropland_resampled_2100_ssp245,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(cropland_2100_ssp245) <- "cropland"
cropland_2100_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
cropland_2100_ssp245[is.na(cropland_2100_ssp245)] <- 0
cropland_2100_ssp245 <- terra::mask(x = cropland_2100_ssp245, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(cropland_2100_ssp245)
cropland_2100_ssp245 <- resample(cropland_2100_ssp245, forest_old, method = "near")

# Save results
writeRaster(cropland_2100_ssp245, 
            filename = paste0(output_dir, "cropland_proportion_1km_2100_ssp245.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  cropland_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(cropland_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(cropland_10km_2050_ssp245 <- terra::focal(cropland_1000_temp_2050_ssp245, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
cropland_10km_resampled_2050_ssp245 <- terra::resample(cropland_10km_2050_ssp245, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
cropland_10km_2050_ssp245 <- terra::project(x = cropland_10km_resampled_2050_ssp245,
                                            y = x)

# Before saving, make the name of the value correspond to the variable
names(cropland_10km_2050_ssp245) <- "cropland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
cropland_10km_2050_ssp245[is.na(cropland_10km_2050_ssp245)] <- 0
cropland_10km_2050_ssp245 <- terra::mask(x = cropland_10km_2050_ssp245, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(cropland_10km_2050_ssp245)

cropland_10km_2050_ssp245 <- resample(cropland_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(cropland_10km_2050_ssp245, 
            filename = paste0(output_dir, "cropland_10km_2050_ssp245.tif"), 
            overwrite = TRUE)


# barren

system.time(
  barren_temp_2100_ssp245 <- aggregate(landcover_2100_ssp245,
                                       Nratio, 
                                       # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                       fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(barren_temp_2100_ssp245 )
barren_temp_2100_ssp245

# Resample at correct resolution
barren_resampled_2100_ssp245 <- terra::resample(barren_temp_2100_ssp245, 
                                                x_lc, 
                                                method = "bilinear")

# Project raster
barren_2100_ssp245 <- terra::project(x = barren_resampled_2100_ssp245,
                                     y = x)

# Before saving, make the name of the value correspond to the variable
names(barren_2100_ssp245) <- "barren"
barren_2100_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
barren_2100_ssp245[is.na(barren_2100_ssp245)] <- 0
barren_2100_ssp245 <- terra::mask(x = barren_2100_ssp245, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(barren_2100_ssp245)
barren_2100_ssp245 <- resample(barren_2100_ssp245, forest_old, method = "near")

# Save results
writeRaster(barren_2100_ssp245, 
            filename = paste0(output_dir, "barren_proportion_1km_2100_ssp245.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  barren_1000_temp_2070_ssp126 <- terra::aggregate(landcover_2070_ssp126,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(barren_1000_temp_2070_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(barren_10km_2070_ssp126 <- terra::focal(barren_1000_temp_2070_ssp126, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells


# Resample at correct resolution
barren_10km_resampled_2070_ssp126 <- terra::resample(barren_10km_2070_ssp126, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
barren_10km_2070_ssp126 <- terra::project(x = barren_10km_resampled_2070_ssp126,
                                          y = x)

# Before saving, make the name of the value correspond to the variable
names(barren_10km_2070_ssp126) <- "barren_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
barren_10km_2070_ssp126[is.na(barren_10km_2070_ssp126)] <- 0
barren_10km_2070_ssp126 <- terra::mask(x = barren_10km_2070_ssp126, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(barren_10km_2070_ssp126)

barren_10km_2070_ssp126 <- resample(barren_10km_2070_ssp126, forest_old, method = "near")

# Save results
writeRaster(barren_10km_2070_ssp126, 
            filename = paste0(output_dir, "barren_10km_2070_ssp126.tif"), 
            overwrite = TRUE)



# built_up

system.time(
  built_up_temp_2100_ssp245 <- aggregate(landcover_2100_ssp245,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(built_up_temp_2100_ssp245 )
built_up_temp_2100_ssp245

# Resample at correct resolution
built_up_resampled_2100_ssp245 <- terra::resample(built_up_temp_2100_ssp245, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
built_up_2100_ssp245 <- terra::project(x = built_up_resampled_2100_ssp245,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(built_up_2100_ssp245) <- "built_up"
built_up_2100_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
built_up_2100_ssp245[is.na(built_up_2100_ssp245)] <- 0
built_up_2100_ssp245 <- terra::mask(x = built_up_2100_ssp245, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(built_up_2100_ssp245)
built_up_2100_ssp245 <- resample(built_up_2100_ssp245, forest_old, method = "near")

# Save results
writeRaster(built_up_2100_ssp245, 
            filename = paste0(output_dir, "built_up_proportion_1km_2100_ssp245.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  built_up_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(built_up_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(built_up_10km_2050_ssp126 <- terra::focal(built_up_1000_temp_2050_ssp126, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
built_up_10km_resampled_2050_ssp126 <- terra::resample(built_up_10km_2050_ssp126, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
built_up_10km_2050_ssp126 <- terra::project(x = built_up_10km_resampled_2050_ssp126,
                                            y = x)

# Before saving, make the name of the value correspond to the variable
names(built_up_10km_2050_ssp126) <- "built_up_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
built_up_10km_2050_ssp126[is.na(built_up_10km_2050_ssp126)] <- 0
built_up_10km_2050_ssp126 <- terra::mask(x = built_up_10km_2050_ssp126, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(built_up_10km_2050_ssp126)

built_up_10km_2050_ssp126 <- resample(built_up_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(built_up_10km_2050_ssp126, 
            filename = paste0(output_dir, "built_up_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# water

system.time(
  water_temp_2100_ssp245 <- aggregate(landcover_2100_ssp245,
                                      Nratio, 
                                      # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                      fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(water_temp_2100_ssp245 )
water_temp_2100_ssp245

# Resample at correct resolution
water_resampled_2100_ssp245 <- terra::resample(water_temp_2100_ssp245, 
                                               x_lc, 
                                               method = "bilinear")

# Project raster
water_2100_ssp245 <- terra::project(x = water_resampled_2100_ssp245,
                                    y = x)

# Before saving, make the name of the value correspond to the variable
names(water_2100_ssp245) <- "water"
water_2100_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
water_2100_ssp245[is.na(water_2100_ssp245)] <- 0
water_2100_ssp245 <- terra::mask(x = water_2100_ssp245, 
                                 mask = SECanadaVectTemp %>% 
                                   st_as_sf(.) %>%
                                   vect(.))

plot(water_2100_ssp245)
water_2100_ssp245 <- resample(water_2100_ssp245, forest_old, method = "near")

# Save results
writeRaster(water_2100_ssp245, 
            filename = paste0(output_dir, "water_proportion_1km_2100_ssp245.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  water_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                  Nratio_step, 
                                                  fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(water_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(water_10km_2050_ssp126 <- terra::focal(water_1000_temp_2050_ssp126, 
                                                   w = mat_10km,
                                                   fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
water_10km_resampled_2050_ssp126 <- terra::resample(water_10km_2050_ssp126, 
                                                    x_lc, 
                                                    method = "bilinear")

# Project raster
water_10km_2050_ssp126 <- terra::project(x = water_10km_resampled_2050_ssp126,
                                         y = x)

# Before saving, make the name of the value correspond to the variable
names(water_10km_2050_ssp126) <- "water_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
water_10km_2050_ssp126[is.na(water_10km_2050_ssp126)] <- 0
water_10km_2050_ssp126 <- terra::mask(x = water_10km_2050_ssp126, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(water_10km_2050_ssp126)

water_10km_2050_ssp126 <- resample(water_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(water_10km_2050_ssp126, 
            filename = paste0(output_dir, "water_10km_2050_ssp126.tif"), 
            overwrite = TRUE)





# 2100 ssp585

# Now, calculate the proportion of habitat per site for forest
system.time(
  forest_temp_2100_ssp585 <- aggregate(landcover_2100_ssp585,
                                       Nratio, 
                                       # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                       fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(forest_temp_2100_ssp585 )
forest_temp_2100_ssp585

# Resample at correct resolution
forest_resampled_2100_ssp585 <- terra::resample(forest_temp_2100_ssp585, 
                                                x_lc, 
                                                method = "bilinear")

# Project raster
forest_2100_ssp585 <- terra::project(x = forest_resampled_2100_ssp585,
                                     y = x)

# Before saving, make the name of the value correspond to the variable
names(forest_2100_ssp585) <- "forest"
forest_2100_ssp585

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
forest_2100_ssp585[is.na(forest_2100_ssp585)] <- 0
forest_2100_ssp585 <- terra::mask(x = forest_2100_ssp585, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(forest_2100_ssp585)

forest_2100_ssp585 <- resample(forest_2100_ssp585, forest_old, method = "near")

# Save results
writeRaster(forest_2100_ssp585, 
            filename = paste0(output_dir, "forest_proportion_1km_2100_ssp585.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  forest_1000_temp_2100_ssp245 <- terra::aggregate(landcover_2100_ssp245,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(forest_1000_temp_2100_ssp245)

system.time(forest_10km_2100_ssp245 <- terra::focal(forest_1000_temp_2100_ssp245, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121}))

# Resample at correct resolution
forest_10km_resampled_2100_ssp245 <- terra::resample(forest_10km_2100_ssp245, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
forest_10km_2100_ssp245 <- terra::project(x = forest_10km_resampled_2100_ssp245,
                                          y = x)

names(forest_10km_2100_ssp245) <- "forest_10km"

forest_10km_2100_ssp245[is.na(forest_10km_2100_ssp245)] <- 0
forest_10km_2100_ssp245 <- terra::mask(x = forest_10km_2100_ssp245, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(forest_10km_2100_ssp245)

forest_10km_2100_ssp245 <- resample(forest_10km_2100_ssp245, forest_old, method = "near")

writeRaster(forest_10km_2100_ssp245, 
            filename = paste0(output_dir, "forest_10km_2100_ssp245.tif"), 
            overwrite = TRUE)



# shrublands

system.time(
  shrublands_temp_2100_ssp585 <- aggregate(landcover_2100_ssp585,
                                           Nratio, 
                                           fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(shrublands_temp_2100_ssp585 )
shrublands_temp_2100_ssp585

shrublands_resampled_2100_ssp585 <- terra::resample(shrublands_temp_2100_ssp585, 
                                                    x_lc, 
                                                    method = "bilinear")

shrublands_2100_ssp585 <- terra::project(x = shrublands_resampled_2100_ssp585,
                                         y = x)

names(shrublands_2100_ssp585) <- "opforest"

shrublands_2100_ssp585[is.na(shrublands_2100_ssp585)] <- 0
shrublands_2100_ssp585 <- terra::mask(x = shrublands_2100_ssp585, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(shrublands_2100_ssp585)

shrublands_2100_ssp585 <- resample(shrublands_2100_ssp585, forest_old, method = "near")

writeRaster(shrublands_2100_ssp585, 
            filename = paste0(output_dir, "opforest_proportion_1km_2100_ssp585.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  shrublands_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                       Nratio_step, 
                                                       fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(shrublands_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(shrublands_10km_2050_ssp245 <- terra::focal(shrublands_1000_temp_2050_ssp245, 
                                                        w = mat_10km,
                                                        fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
shrublands_10km_resampled_2050_ssp245 <- terra::resample(shrublands_10km_2050_ssp245, 
                                                         x_lc, 
                                                         method = "bilinear")

# Project raster
shrublands_10km_2050_ssp245 <- terra::project(x = shrublands_10km_resampled_2050_ssp245,
                                              y = x)

# Before saving, make the name of the value correspond to the variable
names(shrublands_10km_2050_ssp245) <- "opforest_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
shrublands_10km_2050_ssp245[is.na(shrublands_10km_2050_ssp245)] <- 0
shrublands_10km_2050_ssp245 <- terra::mask(x = shrublands_10km_2050_ssp245, 
                                           mask = SECanadaVectTemp %>% 
                                             st_as_sf(.) %>%
                                             vect(.))

plot(shrublands_10km_2050_ssp245)

shrublands_10km_2050_ssp245 <- resample(shrublands_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(shrublands_10km_2050_ssp245, 
            filename = paste0(output_dir, "opforest_10km_2050_ssp245.tif"), 
            overwrite = TRUE)


# grassland

system.time(
  grassland_temp_2100_ssp585 <- aggregate(landcover_2100_ssp585,
                                          Nratio, 
                                          # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                          fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(grassland_temp_2100_ssp585 )
grassland_temp_2100_ssp585

# Resample at correct resolution
grassland_resampled_2100_ssp585 <- terra::resample(grassland_temp_2100_ssp585, 
                                                   x_lc, 
                                                   method = "bilinear")

# Project raster
grassland_2100_ssp585 <- terra::project(x = grassland_resampled_2100_ssp585,
                                        y = x)

# Before saving, make the name of the value correspond to the variable
names(grassland_2100_ssp585) <- "grassland"
grassland_2100_ssp585

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
grassland_2100_ssp585[is.na(grassland_2100_ssp585)] <- 0
grassland_2100_ssp585 <- terra::mask(x = grassland_2100_ssp585, 
                                     mask = SECanadaVectTemp %>% 
                                       st_as_sf(.) %>%
                                       vect(.))

plot(grassland_2100_ssp585)

grassland_2100_ssp585 <- resample(grassland_2100_ssp585, forest_old, method = "near")

# Save results
writeRaster(grassland_2100_ssp585, 
            filename = paste0(output_dir, "grassland_proportion_1km_2100_ssp585.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  grassland_1000_temp_2100_ssp245 <- terra::aggregate(landcover_2100_ssp245,
                                                      Nratio_step, 
                                                      fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(grassland_1000_temp_2100_ssp245)

system.time(grassland_10km_2100_ssp245 <- terra::focal(grassland_1000_temp_2100_ssp245, 
                                                       w = mat_10km,
                                                       fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121}))

# Resample at correct resolution
grassland_10km_resampled_2100_ssp245 <- terra::resample(grassland_10km_2100_ssp245, 
                                                        x_lc, 
                                                        method = "bilinear")

# Project raster
grassland_10km_2100_ssp245 <- terra::project(x = grassland_10km_resampled_2100_ssp245,
                                             y = x)

names(grassland_10km_2100_ssp245) <- "grassland_10km"

grassland_10km_2100_ssp245[is.na(grassland_10km_2100_ssp245)] <- 0
grassland_10km_2100_ssp245 <- terra::mask(x = grassland_10km_2100_ssp245, 
                                          mask = SECanadaVectTemp %>% 
                                            st_as_sf(.) %>%
                                            vect(.))

plot(grassland_10km_2100_ssp245)

grassland_10km_2100_ssp245 <- resample(grassland_10km_2100_ssp245, forest_old, method = "near")

writeRaster(grassland_10km_2100_ssp245, 
            filename = paste0(output_dir, "grassland_10km_2100_ssp245.tif"), 
            overwrite = TRUE)


# wetland

system.time(
  wetland_temp_2100_ssp585 <- aggregate(landcover_2100_ssp585,
                                        Nratio, 
                                        fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(wetland_temp_2100_ssp585 )
wetland_temp_2100_ssp585

wetland_resampled_2100_ssp585 <- terra::resample(wetland_temp_2100_ssp585, 
                                                 x_lc, 
                                                 method = "bilinear")

wetland_2100_ssp585 <- terra::project(x = wetland_resampled_2100_ssp585,
                                      y = x)

names(wetland_2100_ssp585) <- "wetland"

wetland_2100_ssp585[is.na(wetland_2100_ssp585)] <- 0
wetland_2100_ssp585 <- terra::mask(x = wetland_2100_ssp585, 
                                   mask = SECanadaVectTemp %>% 
                                     st_as_sf(.) %>%
                                     vect(.))

plot(wetland_2100_ssp585)
wetland_2100_ssp585 <- resample(wetland_2100_ssp585, forest_old, method = "near")

writeRaster(wetland_2100_ssp585, 
            filename = paste0(output_dir, "wetland_proportion_1km_2100_ssp585.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  wetland_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                    Nratio_step, 
                                                    fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(wetland_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(wetland_10km_2050_ssp245 <- terra::focal(wetland_1000_temp_2050_ssp245, 
                                                     w = mat_10km,
                                                     fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
wetland_10km_resampled_2050_ssp245 <- terra::resample(wetland_10km_2050_ssp245, 
                                                      x_lc, 
                                                      method = "bilinear")

# Project raster
wetland_10km_2050_ssp245 <- terra::project(x = wetland_10km_resampled_2050_ssp245,
                                           y = x)

# Before saving, make the name of the value correspond to the variable
names(wetland_10km_2050_ssp245) <- "wetland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
wetland_10km_2050_ssp245[is.na(wetland_10km_2050_ssp245)] <- 0
wetland_10km_2050_ssp245 <- terra::mask(x = wetland_10km_2050_ssp245, 
                                        mask = SECanadaVectTemp %>% 
                                          st_as_sf(.) %>%
                                          vect(.))

plot(wetland_10km_2050_ssp245)

wetland_10km_2050_ssp245 <- resample(wetland_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(wetland_10km_2050_ssp245, 
            filename = paste0(output_dir, "wetland_10km_2050_ssp245.tif"), 
            overwrite = TRUE)


# cropland

system.time(
  cropland_temp_2100_ssp585 <- aggregate(landcover_2100_ssp585,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(cropland_temp_2100_ssp585 )
cropland_temp_2100_ssp585

# Resample at correct resolution
cropland_resampled_2100_ssp585 <- terra::resample(cropland_temp_2100_ssp585, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
cropland_2100_ssp585 <- terra::project(x = cropland_resampled_2100_ssp585,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(cropland_2100_ssp585) <- "cropland"
cropland_2100_ssp585

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

cropland_2100_ssp585[is.na(cropland_2100_ssp585)] <- 0
cropland_2100_ssp585 <- terra::mask(x = cropland_2100_ssp585, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(cropland_2100_ssp585)
cropland_2100_ssp585 <- resample(cropland_2100_ssp585, forest_old, method = "near")

writeRaster(cropland_2100_ssp585, 
            filename = paste0(output_dir, "cropland_proportion_1km_2100_ssp585.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  cropland_1000_temp_2100_ssp245 <- terra::aggregate(landcover_2100_ssp245,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(cropland_1000_temp_2100_ssp245)

system.time(cropland_10km_2100_ssp245 <- terra::focal(cropland_1000_temp_2100_ssp245, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121}))

# Resample at correct resolution
cropland_10km_resampled_2100_ssp245 <- terra::resample(cropland_10km_2100_ssp245, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
cropland_10km_2100_ssp245 <- terra::project(x = cropland_10km_resampled_2100_ssp245,
                                            y = x)

names(cropland_10km_2100_ssp245) <- "cropland_10km"

cropland_10km_2100_ssp245[is.na(cropland_10km_2100_ssp245)] <- 0
cropland_10km_2100_ssp245 <- terra::mask(x = cropland_10km_2100_ssp245, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(cropland_10km_2100_ssp245)

cropland_10km_2100_ssp245 <- resample(cropland_10km_2100_ssp245, forest_old, method = "near")

writeRaster(cropland_10km_2100_ssp245, 
            filename = paste0(output_dir, "cropland_10km_2100_ssp245.tif"), 
            overwrite = TRUE)


# barren

system.time(
  barren_temp_2100_ssp585 <- aggregate(landcover_2100_ssp585,
                                       Nratio, 
                                       fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(barren_temp_2100_ssp585 )
barren_temp_2100_ssp585

barren_resampled_2100_ssp585 <- terra::resample(barren_temp_2100_ssp585, 
                                                x_lc, 
                                                method = "bilinear")

barren_2100_ssp585 <- terra::project(x = barren_resampled_2100_ssp585,
                                     y = x)

names(barren_2100_ssp585) <- "barren"

barren_2100_ssp585[is.na(barren_2100_ssp585)] <- 0
barren_2100_ssp585 <- terra::mask(x = barren_2100_ssp585, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(barren_2100_ssp585)
barren_2100_ssp585 <- resample(barren_2100_ssp585, forest_old, method = "near")

writeRaster(barren_2100_ssp585, 
            filename = paste0(output_dir, "barren_proportion_1km_2100_ssp585.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  barren_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(barren_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(barren_10km_2050_ssp126 <- terra::focal(barren_1000_temp_2050_ssp126, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells


# Resample at correct resolution
barren_10km_resampled_2050_ssp126 <- terra::resample(barren_10km_2050_ssp126, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
barren_10km_2050_ssp126 <- terra::project(x = barren_10km_resampled_2050_ssp126,
                                          y = x)

# Before saving, make the name of the value correspond to the variable
names(barren_10km_2050_ssp126) <- "barren_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
barren_10km_2050_ssp126[is.na(barren_10km_2050_ssp126)] <- 0
barren_10km_2050_ssp126 <- terra::mask(x = barren_10km_2050_ssp126, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(barren_10km_2050_ssp126)

barren_10km_2050_ssp126 <- resample(barren_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(barren_10km_2050_ssp126, 
            filename = paste0(output_dir, "barren_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# built_up

system.time(
  built_up_temp_2100_ssp585 <- aggregate(landcover_2100_ssp585,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(built_up_temp_2100_ssp585 )
built_up_temp_2100_ssp585

# Resample at correct resolution
built_up_resampled_2100_ssp585 <- terra::resample(built_up_temp_2100_ssp585, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
built_up_2100_ssp585 <- terra::project(x = built_up_resampled_2100_ssp585,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(built_up_2100_ssp585) <- "built_up"
built_up_2100_ssp585

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

built_up_2100_ssp585[is.na(built_up_2100_ssp585)] <- 0
built_up_2100_ssp585 <- terra::mask(x = built_up_2100_ssp585, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(built_up_2100_ssp585)
built_up_2100_ssp585 <- resample(built_up_2100_ssp585, forest_old, method = "near")

writeRaster(built_up_2100_ssp585, 
            filename = paste0(output_dir, "built_up_proportion_1km_2100_ssp585.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  built_up_1000_temp_2100_ssp126 <- terra::aggregate(landcover_2100_ssp126,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(built_up_1000_temp_2100_ssp126)

system.time(built_up_10km_2100_ssp126 <- terra::focal(built_up_1000_temp_2100_ssp126, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121}))

# Resample at correct resolution
built_up_10km_resampled_2100_ssp126 <- terra::resample(built_up_10km_2100_ssp126, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
built_up_10km_2100_ssp126 <- terra::project(x = built_up_10km_resampled_2100_ssp126,
                                            y = x)

names(built_up_10km_2100_ssp126) <- "built_up_10km"

built_up_10km_2100_ssp126[is.na(built_up_10km_2100_ssp126)] <- 0
built_up_10km_2100_ssp126 <- terra::mask(x = built_up_10km_2100_ssp126, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(built_up_10km_2100_ssp126)

built_up_10km_2100_ssp126 <- resample(built_up_10km_2100_ssp126, forest_old, method = "near")

writeRaster(built_up_10km_2100_ssp126, 
            filename = paste0(output_dir, "built_up_10km_2100_ssp126.tif"), 
            overwrite = TRUE)



# water

system.time(
  water_temp_2100_ssp585 <- aggregate(landcover_2100_ssp585,
                                      Nratio, 
                                      # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                      fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(water_temp_2100_ssp585 )
water_temp_2100_ssp585

# Resample at correct resolution
water_resampled_2100_ssp585 <- terra::resample(water_temp_2100_ssp585, 
                                               x_lc, 
                                               method = "bilinear")

# Project raster
water_2100_ssp585 <- terra::project(x = water_resampled_2100_ssp585,
                                    y = x)

# Before saving, make the name of the value correspond to the variable
names(water_2100_ssp585) <- "water"
water_2100_ssp585

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

water_2100_ssp585[is.na(water_2100_ssp585)] <- 0
water_2100_ssp585 <- terra::mask(x = water_2100_ssp585, 
                                 mask = SECanadaVectTemp %>% 
                                   st_as_sf(.) %>%
                                   vect(.))

plot(water_2100_ssp585)
water_2100_ssp585 <- resample(water_2100_ssp585, forest_old, method = "near")

writeRaster(water_2100_ssp585, 
            filename = paste0(output_dir, "water_proportion_1km_2100_ssp585.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  water_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                  Nratio_step, 
                                                  fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(water_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(water_10km_2050_ssp126 <- terra::focal(water_1000_temp_2050_ssp126, 
                                                   w = mat_10km,
                                                   fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
water_10km_resampled_2050_ssp126 <- terra::resample(water_10km_2050_ssp126, 
                                                    x_lc, 
                                                    method = "bilinear")

# Project raster
water_10km_2050_ssp126 <- terra::project(x = water_10km_resampled_2050_ssp126,
                                         y = x)

# Before saving, make the name of the value correspond to the variable
names(water_10km_2050_ssp126) <- "water_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
water_10km_2050_ssp126[is.na(water_10km_2050_ssp126)] <- 0
water_10km_2050_ssp126 <- terra::mask(x = water_10km_2050_ssp126, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(water_10km_2050_ssp126)

water_10km_2050_ssp126 <- resample(water_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(water_10km_2050_ssp126, 
            filename = paste0(output_dir, "water_10km_2050_ssp126.tif"), 
            overwrite = TRUE)





# -------------------2030-------------------------

# Now, calculate the proportion of habitat per site for forest
system.time(
  forest_temp_2030_ssp126 <- aggregate(landcover_2030_ssp126,
                                       Nratio, 
                                       # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                       fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(forest_temp_2030_ssp126 )
forest_temp_2030_ssp126

# Resample at correct resolution
forest_resampled_2030_ssp126 <- terra::resample(forest_temp_2030_ssp126, 
                                                x_lc, 
                                                method = "bilinear")

# Project raster
forest_2030_ssp126 <- terra::project(x = forest_resampled_2030_ssp126,
                                     y = x)

# Before saving, make the name of the value correspond to the variable
names(forest_2030_ssp126) <- "forest"
forest_2030_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
forest_2030_ssp126[is.na(forest_2030_ssp126)] <- 0
forest_2030_ssp126 <- terra::mask(x = forest_2030_ssp126, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(forest_2030_ssp126)

forest_2030_ssp126 <- resample(forest_2030_ssp126, forest_old, method = "near")

# Save results
writeRaster(forest_2030_ssp126, 
            filename = paste0(output_dir, "forest_proportion_1km_2030_ssp126.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  forest_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(forest_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(forest_10km_2050_ssp126 <- terra::focal(forest_1000_temp_2050_ssp126, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
forest_10km_resampled_2050_ssp126 <- terra::resample(forest_10km_2050_ssp126, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
forest_10km_2050_ssp126 <- terra::project(x = forest_10km_resampled_2050_ssp126,
                                          y = x)

# Before saving, make the name of the value correspond to the variable
names(forest_10km_2050_ssp126) <- "forest_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
forest_10km_2050_ssp126[is.na(forest_10km_2050_ssp126)] <- 0
forest_10km_2050_ssp126 <- terra::mask(x = forest_10km_2050_ssp126, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(forest_10km_2050_ssp126)

forest_10km_2050_ssp126 <- resample(forest_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(forest_10km_2050_ssp126, 
            filename = paste0(output_dir, "forest_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# shrublands

system.time(
  shrublands_temp_2030_ssp126 <- aggregate(landcover_2030_ssp126,
                                           Nratio, 
                                           # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                           fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(shrublands_temp_2030_ssp126 )
shrublands_temp_2030_ssp126

# Resample at correct resolution
shrublands_resampled_2030_ssp126 <- terra::resample(shrublands_temp_2030_ssp126, 
                                                    x_lc, 
                                                    method = "bilinear")

# Project raster
shrublands_2030_ssp126 <- terra::project(x = shrublands_resampled_2030_ssp126,
                                         y = x)

# Before saving, make the name of the value correspond to the variable
names(shrublands_2030_ssp126) <- "opforest"
shrublands_2030_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
shrublands_2030_ssp126[is.na(shrublands_2030_ssp126)] <- 0
shrublands_2030_ssp126 <- terra::mask(x = shrublands_2030_ssp126, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(shrublands_2030_ssp126)

shrublands_2030_ssp126 <- resample(shrublands_2030_ssp126, forest_old, method = "near")

# Save results
writeRaster(shrublands_2030_ssp126, 
            filename = paste0(output_dir, "opforest_proportion_1km_2030_ssp126.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  shrublands_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                       Nratio_step, 
                                                       fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(shrublands_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(shrublands_10km_2050_ssp126 <- terra::focal(shrublands_1000_temp_2050_ssp126, 
                                                        w = mat_10km,
                                                        fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
shrublands_10km_resampled_2050_ssp126 <- terra::resample(shrublands_10km_2050_ssp126, 
                                                         x_lc, 
                                                         method = "bilinear")

# Project raster
shrublands_10km_2050_ssp126 <- terra::project(x = shrublands_10km_resampled_2050_ssp126,
                                              y = x)

# Before saving, make the name of the value correspond to the variable
names(shrublands_10km_2050_ssp126) <- "opforest_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
shrublands_10km_2050_ssp126[is.na(shrublands_10km_2050_ssp126)] <- 0
shrublands_10km_2050_ssp126 <- terra::mask(x = shrublands_10km_2050_ssp126, 
                                           mask = SECanadaVectTemp %>% 
                                             st_as_sf(.) %>%
                                             vect(.))

plot(shrublands_10km_2050_ssp126)

shrublands_10km_2050_ssp126 <- resample(shrublands_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(shrublands_10km_2050_ssp126, 
            filename = paste0(output_dir, "opforest_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# grassland

system.time(
  grassland_temp_2030_ssp126 <- aggregate(landcover_2030_ssp126,
                                          Nratio, 
                                          # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                          fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(grassland_temp_2030_ssp126 )
grassland_temp_2030_ssp126

# Resample at correct resolution
grassland_resampled_2030_ssp126 <- terra::resample(grassland_temp_2030_ssp126, 
                                                   x_lc, 
                                                   method = "bilinear")

# Project raster
grassland_2030_ssp126 <- terra::project(x = grassland_resampled_2030_ssp126,
                                        y = x)

# Before saving, make the name of the value correspond to the variable
names(grassland_2030_ssp126) <- "grassland"
grassland_2030_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
grassland_2030_ssp126[is.na(grassland_2030_ssp126)] <- 0
grassland_2030_ssp126 <- terra::mask(x = grassland_2030_ssp126, 
                                     mask = SECanadaVectTemp %>% 
                                       st_as_sf(.) %>%
                                       vect(.))

plot(grassland_2030_ssp126)

grassland_2030_ssp126 <- resample(grassland_2030_ssp126, forest_old, method = "near")

# Save results
writeRaster(grassland_2030_ssp126, 
            filename = paste0(output_dir, "grassland_proportion_1km_2030_ssp126.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  grassland_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                      Nratio_step, 
                                                      fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(grassland_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(grassland_10km_2050_ssp126 <- terra::focal(grassland_1000_temp_2050_ssp126, 
                                                       w = mat_10km,
                                                       fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
grassland_10km_resampled_2050_ssp126 <- terra::resample(grassland_10km_2050_ssp126, 
                                                        x_lc, 
                                                        method = "bilinear")

# Project raster
grassland_10km_2050_ssp126 <- terra::project(x = grassland_10km_resampled_2050_ssp126,
                                             y = x)

# Before saving, make the name of the value correspond to the variable
names(grassland_10km_2050_ssp126) <- "grassland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
grassland_10km_2050_ssp126[is.na(grassland_10km_2050_ssp126)] <- 0
grassland_10km_2050_ssp126 <- terra::mask(x = grassland_10km_2050_ssp126, 
                                          mask = SECanadaVectTemp %>% 
                                            st_as_sf(.) %>%
                                            vect(.))

plot(grassland_10km_2050_ssp126)

grassland_10km_2050_ssp126 <- resample(grassland_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(grassland_10km_2050_ssp126, 
            filename = paste0(output_dir, "grassland_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# wetland

system.time(
  wetland_temp_2030_ssp126 <- aggregate(landcover_2030_ssp126,
                                        Nratio, 
                                        # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                        fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(wetland_temp_2030_ssp126 )
wetland_temp_2030_ssp126

# Resample at correct resolution
wetland_resampled_2030_ssp126 <- terra::resample(wetland_temp_2030_ssp126, 
                                                 x_lc, 
                                                 method = "bilinear")

# Project raster
wetland_2030_ssp126 <- terra::project(x = wetland_resampled_2030_ssp126,
                                      y = x)

# Before saving, make the name of the value correspond to the variable
names(wetland_2030_ssp126) <- "wetland"
wetland_2030_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
wetland_2030_ssp126[is.na(wetland_2030_ssp126)] <- 0
wetland_2030_ssp126 <- terra::mask(x = wetland_2030_ssp126, 
                                   mask = SECanadaVectTemp %>% 
                                     st_as_sf(.) %>%
                                     vect(.))

plot(wetland_2030_ssp126)
wetland_2030_ssp126 <- resample(wetland_2030_ssp126, forest_old, method = "near")

# Save results
writeRaster(wetland_2030_ssp126, 
            filename = paste0(output_dir, "wetland_proportion_1km_2030_ssp126.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  wetland_1000_temp_2070_ssp126 <- terra::aggregate(landcover_2070_ssp126,
                                                    Nratio_step, 
                                                    fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(wetland_1000_temp_2070_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(wetland_10km_2070_ssp126 <- terra::focal(wetland_1000_temp_2070_ssp126, 
                                                     w = mat_10km,
                                                     fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
wetland_10km_resampled_2070_ssp126 <- terra::resample(wetland_10km_2070_ssp126, 
                                                      x_lc, 
                                                      method = "bilinear")

# Project raster
wetland_10km_2070_ssp126 <- terra::project(x = wetland_10km_resampled_2070_ssp126,
                                           y = x)

# Before saving, make the name of the value correspond to the variable
names(wetland_10km_2070_ssp126) <- "wetland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
wetland_10km_2070_ssp126[is.na(wetland_10km_2070_ssp126)] <- 0
wetland_10km_2070_ssp126 <- terra::mask(x = wetland_10km_2070_ssp126, 
                                        mask = SECanadaVectTemp %>% 
                                          st_as_sf(.) %>%
                                          vect(.))

plot(wetland_10km_2070_ssp126)

wetland_10km_2070_ssp126 <- resample(wetland_10km_2070_ssp126, forest_old, method = "near")

# Save results
writeRaster(wetland_10km_2070_ssp126, 
            filename = paste0(output_dir, "wetland_10km_2070_ssp126.tif"), 
            overwrite = TRUE)




# cropland

system.time(
  cropland_temp_2030_ssp126 <- aggregate(landcover_2030_ssp126,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(cropland_temp_2030_ssp126 )
cropland_temp_2030_ssp126

# Resample at correct resolution
cropland_resampled_2030_ssp126 <- terra::resample(cropland_temp_2030_ssp126, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
cropland_2030_ssp126 <- terra::project(x = cropland_resampled_2030_ssp126,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(cropland_2030_ssp126) <- "cropland"
cropland_2030_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
cropland_2030_ssp126[is.na(cropland_2030_ssp126)] <- 0
cropland_2030_ssp126 <- terra::mask(x = cropland_2030_ssp126, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(cropland_2030_ssp126)
cropland_2030_ssp126 <- resample(cropland_2030_ssp126, forest_old, method = "near")

# Save results
writeRaster(cropland_2030_ssp126, 
            filename = paste0(output_dir, "cropland_proportion_1km_2030_ssp126.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  cropland_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(cropland_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(cropland_10km_2050_ssp126 <- terra::focal(cropland_1000_temp_2050_ssp126, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
cropland_10km_resampled_2050_ssp126 <- terra::resample(cropland_10km_2050_ssp126, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
cropland_10km_2050_ssp126 <- terra::project(x = cropland_10km_resampled_2050_ssp126,
                                            y = x)

# Before saving, make the name of the value correspond to the variable
names(cropland_10km_2050_ssp126) <- "cropland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
cropland_10km_2050_ssp126[is.na(cropland_10km_2050_ssp126)] <- 0
cropland_10km_2050_ssp126 <- terra::mask(x = cropland_10km_2050_ssp126, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(cropland_10km_2050_ssp126)

cropland_10km_2050_ssp126 <- resample(cropland_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(cropland_10km_2050_ssp126, 
            filename = paste0(output_dir, "cropland_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# barren

system.time(
  barren_temp_2030_ssp126 <- aggregate(landcover_2030_ssp126,
                                       Nratio, 
                                       # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                       fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(barren_temp_2030_ssp126 )
barren_temp_2030_ssp126

# Resample at correct resolution
barren_resampled_2030_ssp126 <- terra::resample(barren_temp_2030_ssp126, 
                                                x_lc, 
                                                method = "bilinear")

# Project raster
barren_2030_ssp126 <- terra::project(x = barren_resampled_2030_ssp126,
                                     y = x)

# Before saving, make the name of the value correspond to the variable
names(barren_2030_ssp126) <- "barren"
barren_2030_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
barren_2030_ssp126[is.na(barren_2030_ssp126)] <- 0
barren_2030_ssp126 <- terra::mask(x = barren_2030_ssp126, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(barren_2030_ssp126)
barren_2030_ssp126 <- resample(barren_2030_ssp126, forest_old, method = "near")

# Save results
writeRaster(barren_2030_ssp126, 
            filename = paste0(output_dir, "barren_proportion_1km_2030_ssp126.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  barren_1000_temp_2070_ssp126 <- terra::aggregate(landcover_2070_ssp126,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(barren_1000_temp_2070_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(barren_10km_2070_ssp126 <- terra::focal(barren_1000_temp_2070_ssp126, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells


# Resample at correct resolution
barren_10km_resampled_2070_ssp126 <- terra::resample(barren_10km_2070_ssp126, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
barren_10km_2070_ssp126 <- terra::project(x = barren_10km_resampled_2070_ssp126,
                                          y = x)

# Before saving, make the name of the value correspond to the variable
names(barren_10km_2070_ssp126) <- "barren_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
barren_10km_2070_ssp126[is.na(barren_10km_2070_ssp126)] <- 0
barren_10km_2070_ssp126 <- terra::mask(x = barren_10km_2070_ssp126, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(barren_10km_2070_ssp126)

barren_10km_2070_ssp126 <- resample(barren_10km_2070_ssp126, forest_old, method = "near")

# Save results
writeRaster(barren_10km_2070_ssp126, 
            filename = paste0(output_dir, "barren_10km_2070_ssp126.tif"), 
            overwrite = TRUE)



# built_up

system.time(
  built_up_temp_2030_ssp126 <- aggregate(landcover_2030_ssp126,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(built_up_temp_2030_ssp126 )
built_up_temp_2030_ssp126

# Resample at correct resolution
built_up_resampled_2030_ssp126 <- terra::resample(built_up_temp_2030_ssp126, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
built_up_2030_ssp126 <- terra::project(x = built_up_resampled_2030_ssp126,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(built_up_2030_ssp126) <- "built_up"
built_up_2030_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
built_up_2030_ssp126[is.na(built_up_2030_ssp126)] <- 0
built_up_2030_ssp126 <- terra::mask(x = built_up_2030_ssp126, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(built_up_2030_ssp126)
built_up_2030_ssp126 <- resample(built_up_2030_ssp126, forest_old, method = "near")

# Save results
writeRaster(built_up_2030_ssp126, 
            filename = paste0(output_dir, "built_up_proportion_1km_2030_ssp126.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  built_up_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(built_up_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(built_up_10km_2050_ssp126 <- terra::focal(built_up_1000_temp_2050_ssp126, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
built_up_10km_resampled_2050_ssp126 <- terra::resample(built_up_10km_2050_ssp126, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
built_up_10km_2050_ssp126 <- terra::project(x = built_up_10km_resampled_2050_ssp126,
                                            y = x)

# Before saving, make the name of the value correspond to the variable
names(built_up_10km_2050_ssp126) <- "built_up_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
built_up_10km_2050_ssp126[is.na(built_up_10km_2050_ssp126)] <- 0
built_up_10km_2050_ssp126 <- terra::mask(x = built_up_10km_2050_ssp126, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(built_up_10km_2050_ssp126)

built_up_10km_2050_ssp126 <- resample(built_up_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(built_up_10km_2050_ssp126, 
            filename = paste0(output_dir, "built_up_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# water

system.time(
  water_temp_2030_ssp126 <- aggregate(landcover_2030_ssp126,
                                      Nratio, 
                                      # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                      fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(water_temp_2030_ssp126 )
water_temp_2030_ssp126

# Resample at correct resolution
water_resampled_2030_ssp126 <- terra::resample(water_temp_2030_ssp126, 
                                               x_lc, 
                                               method = "bilinear")

# Project raster
water_2030_ssp126 <- terra::project(x = water_resampled_2030_ssp126,
                                    y = x)

# Before saving, make the name of the value correspond to the variable
names(water_2030_ssp126) <- "water"
water_2030_ssp126

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
water_2030_ssp126[is.na(water_2030_ssp126)] <- 0
water_2030_ssp126 <- terra::mask(x = water_2030_ssp126, 
                                 mask = SECanadaVectTemp %>% 
                                   st_as_sf(.) %>%
                                   vect(.))

plot(water_2030_ssp126)
water_2030_ssp126 <- resample(water_2030_ssp126, forest_old, method = "near")

# Save results
writeRaster(water_2030_ssp126, 
            filename = paste0(output_dir, "water_proportion_1km_2030_ssp126.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  water_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                  Nratio_step, 
                                                  fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(water_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(water_10km_2050_ssp126 <- terra::focal(water_1000_temp_2050_ssp126, 
                                                   w = mat_10km,
                                                   fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
water_10km_resampled_2050_ssp126 <- terra::resample(water_10km_2050_ssp126, 
                                                    x_lc, 
                                                    method = "bilinear")

# Project raster
water_10km_2050_ssp126 <- terra::project(x = water_10km_resampled_2050_ssp126,
                                         y = x)

# Before saving, make the name of the value correspond to the variable
names(water_10km_2050_ssp126) <- "water_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
water_10km_2050_ssp126[is.na(water_10km_2050_ssp126)] <- 0
water_10km_2050_ssp126 <- terra::mask(x = water_10km_2050_ssp126, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(water_10km_2050_ssp126)

water_10km_2050_ssp126 <- resample(water_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(water_10km_2050_ssp126, 
            filename = paste0(output_dir, "water_10km_2050_ssp126.tif"), 
            overwrite = TRUE)




# 2100 ssp245

# Now, calculate the proportion of habitat per site for forest
system.time(
  forest_temp_2030_ssp245 <- aggregate(landcover_2030_ssp245,
                                       Nratio, 
                                       # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                       fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(forest_temp_2030_ssp245 )
forest_temp_2030_ssp245

# Resample at correct resolution
forest_resampled_2030_ssp245 <- terra::resample(forest_temp_2030_ssp245, 
                                                x_lc, 
                                                method = "bilinear")

# Project raster
forest_2030_ssp245 <- terra::project(x = forest_resampled_2030_ssp245,
                                     y = x)

# Before saving, make the name of the value correspond to the variable
names(forest_2030_ssp245) <- "forest"
forest_2030_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
forest_2030_ssp245[is.na(forest_2030_ssp245)] <- 0
forest_2030_ssp245 <- terra::mask(x = forest_2030_ssp245, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(forest_2030_ssp245)

forest_2030_ssp245 <- resample(forest_2030_ssp245, forest_old, method = "near")

# Save results
writeRaster(forest_2030_ssp245, 
            filename = paste0(output_dir, "forest_proportion_1km_2030_ssp245.tif"), 
            overwrite = TRUE)



##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  forest_1000_temp_2030_ssp245 <- terra::aggregate(landcover_2030_ssp245,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(forest_1000_temp_2030_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(forest_10km_2030_ssp245 <- terra::focal(forest_1000_temp_2030_ssp245, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
forest_10km_resampled_2030_ssp245 <- terra::resample(forest_10km_2030_ssp245, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
forest_10km_2030_ssp245 <- terra::project(x = forest_10km_resampled_2030_ssp245,
                                          y = x)

# Before saving, make the name of the value correspond to the variable
names(forest_10km_2030_ssp245) <- "forest_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
forest_10km_2030_ssp245[is.na(forest_10km_2030_ssp245)] <- 0
forest_10km_2030_ssp245 <- terra::mask(x = forest_10km_2030_ssp245, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(forest_10km_2030_ssp245)

forest_10km_2030_ssp245 <- resample(forest_10km_2030_ssp245, forest_old, method = "near")

# Save results
writeRaster(forest_10km_2030_ssp245, 
            filename = paste0(output_dir, "forest_10km_2030_ssp245.tif"), 
            overwrite = TRUE)



# shrublands

system.time(
  shrublands_temp_2030_ssp245 <- aggregate(landcover_2030_ssp245,
                                           Nratio, 
                                           # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                           fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(shrublands_temp_2030_ssp245 )
shrublands_temp_2030_ssp245

# Resample at correct resolution
shrublands_resampled_2030_ssp245 <- terra::resample(shrublands_temp_2030_ssp245, 
                                                    x_lc, 
                                                    method = "bilinear")

# Project raster
shrublands_2030_ssp245 <- terra::project(x = shrublands_resampled_2030_ssp245,
                                         y = x)

# Before saving, make the name of the value correspond to the variable
names(shrublands_2030_ssp245) <- "opforest"
shrublands_2030_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
shrublands_2030_ssp245[is.na(shrublands_2030_ssp245)] <- 0
shrublands_2030_ssp245 <- terra::mask(x = shrublands_2030_ssp245, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(shrublands_2030_ssp245)

shrublands_2030_ssp245 <- resample(shrublands_2030_ssp245, forest_old, method = "near")

# Save results
writeRaster(shrublands_2030_ssp245, 
            filename = paste0(output_dir, "opforest_proportion_1km_2030_ssp245.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  shrublands_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                       Nratio_step, 
                                                       fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(shrublands_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(shrublands_10km_2050_ssp245 <- terra::focal(shrublands_1000_temp_2050_ssp245, 
                                                        w = mat_10km,
                                                        fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
shrublands_10km_resampled_2050_ssp245 <- terra::resample(shrublands_10km_2050_ssp245, 
                                                         x_lc, 
                                                         method = "bilinear")

# Project raster
shrublands_10km_2050_ssp245 <- terra::project(x = shrublands_10km_resampled_2050_ssp245,
                                              y = x)

# Before saving, make the name of the value correspond to the variable
names(shrublands_10km_2050_ssp245) <- "opforest_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
shrublands_10km_2050_ssp245[is.na(shrublands_10km_2050_ssp245)] <- 0
shrublands_10km_2050_ssp245 <- terra::mask(x = shrublands_10km_2050_ssp245, 
                                           mask = SECanadaVectTemp %>% 
                                             st_as_sf(.) %>%
                                             vect(.))

plot(shrublands_10km_2050_ssp245)

shrublands_10km_2050_ssp245 <- resample(shrublands_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(shrublands_10km_2050_ssp245, 
            filename = paste0(output_dir, "opforest_10km_2050_ssp245.tif"), 
            overwrite = TRUE)


# grassland

system.time(
  grassland_temp_2030_ssp245 <- aggregate(landcover_2030_ssp245,
                                          Nratio, 
                                          # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                          fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(grassland_temp_2030_ssp245 )
grassland_temp_2030_ssp245

# Resample at correct resolution
grassland_resampled_2030_ssp245 <- terra::resample(grassland_temp_2030_ssp245, 
                                                   x_lc, 
                                                   method = "bilinear")

# Project raster
grassland_2030_ssp245 <- terra::project(x = grassland_resampled_2030_ssp245,
                                        y = x)

# Before saving, make the name of the value correspond to the variable
names(grassland_2030_ssp245) <- "grassland"
grassland_2030_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
grassland_2030_ssp245[is.na(grassland_2030_ssp245)] <- 0
grassland_2030_ssp245 <- terra::mask(x = grassland_2030_ssp245, 
                                     mask = SECanadaVectTemp %>% 
                                       st_as_sf(.) %>%
                                       vect(.))

plot(grassland_2030_ssp245)

grassland_2030_ssp245 <- resample(grassland_2030_ssp245, forest_old, method = "near")

# Save results
writeRaster(grassland_2030_ssp245, 
            filename = paste0(output_dir, "grassland_proportion_1km_2030_ssp245.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  grassland_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                      Nratio_step, 
                                                      fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(grassland_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(grassland_10km_2050_ssp245 <- terra::focal(grassland_1000_temp_2050_ssp245, 
                                                       w = mat_10km,
                                                       fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
grassland_10km_resampled_2050_ssp245 <- terra::resample(grassland_10km_2050_ssp245, 
                                                        x_lc, 
                                                        method = "bilinear")

# Project raster
grassland_10km_2050_ssp245 <- terra::project(x = grassland_10km_resampled_2050_ssp245,
                                             y = x)

# Before saving, make the name of the value correspond to the variable
names(grassland_10km_2050_ssp245) <- "grassland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
grassland_10km_2050_ssp245[is.na(grassland_10km_2050_ssp245)] <- 0
grassland_10km_2050_ssp245 <- terra::mask(x = grassland_10km_2050_ssp245, 
                                          mask = SECanadaVectTemp %>% 
                                            st_as_sf(.) %>%
                                            vect(.))

plot(grassland_10km_2050_ssp245)

grassland_10km_2050_ssp245 <- resample(grassland_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(grassland_10km_2050_ssp245, 
            filename = paste0(output_dir, "grassland_10km_2050_ssp245.tif"), 
            overwrite = TRUE)


# wetland

system.time(
  wetland_temp_2030_ssp245 <- aggregate(landcover_2030_ssp245,
                                        Nratio, 
                                        # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                        fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(wetland_temp_2030_ssp245 )
wetland_temp_2030_ssp245

# Resample at correct resolution
wetland_resampled_2030_ssp245 <- terra::resample(wetland_temp_2030_ssp245, 
                                                 x_lc, 
                                                 method = "bilinear")

# Project raster
wetland_2030_ssp245 <- terra::project(x = wetland_resampled_2030_ssp245,
                                      y = x)

# Before saving, make the name of the value correspond to the variable
names(wetland_2030_ssp245) <- "wetland"
wetland_2030_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
wetland_2030_ssp245[is.na(wetland_2030_ssp245)] <- 0
wetland_2030_ssp245 <- terra::mask(x = wetland_2030_ssp245, 
                                   mask = SECanadaVectTemp %>% 
                                     st_as_sf(.) %>%
                                     vect(.))

plot(wetland_2030_ssp245)
wetland_2030_ssp245 <- resample(wetland_2030_ssp245, forest_old, method = "near")

# Save results
writeRaster(wetland_2030_ssp245, 
            filename = paste0(output_dir, "wetland_proportion_1km_2030_ssp245.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  wetland_1000_temp_2070_ssp245 <- terra::aggregate(landcover_2070_ssp245,
                                                    Nratio_step, 
                                                    fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(wetland_1000_temp_2070_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(wetland_10km_2070_ssp245 <- terra::focal(wetland_1000_temp_2070_ssp245, 
                                                     w = mat_10km,
                                                     fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
wetland_10km_resampled_2070_ssp245 <- terra::resample(wetland_10km_2070_ssp245, 
                                                      x_lc, 
                                                      method = "bilinear")

# Project raster
wetland_10km_2070_ssp245 <- terra::project(x = wetland_10km_resampled_2070_ssp245,
                                           y = x)

# Before saving, make the name of the value correspond to the variable
names(wetland_10km_2070_ssp245) <- "wetland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
wetland_10km_2070_ssp245[is.na(wetland_10km_2070_ssp245)] <- 0
wetland_10km_2070_ssp245 <- terra::mask(x = wetland_10km_2070_ssp245, 
                                        mask = SECanadaVectTemp %>% 
                                          st_as_sf(.) %>%
                                          vect(.))

plot(wetland_10km_2070_ssp245)

wetland_10km_2070_ssp245 <- resample(wetland_10km_2070_ssp245, forest_old, method = "near")

# Save results
writeRaster(wetland_10km_2070_ssp245, 
            filename = paste0(output_dir, "wetland_10km_2070_ssp245.tif"), 
            overwrite = TRUE)


# cropland

system.time(
  cropland_temp_2030_ssp245 <- aggregate(landcover_2030_ssp245,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(cropland_temp_2030_ssp245 )
cropland_temp_2030_ssp245

# Resample at correct resolution
cropland_resampled_2030_ssp245 <- terra::resample(cropland_temp_2030_ssp245, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
cropland_2030_ssp245 <- terra::project(x = cropland_resampled_2030_ssp245,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(cropland_2030_ssp245) <- "cropland"
cropland_2030_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
cropland_2030_ssp245[is.na(cropland_2030_ssp245)] <- 0
cropland_2030_ssp245 <- terra::mask(x = cropland_2030_ssp245, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(cropland_2030_ssp245)
cropland_2030_ssp245 <- resample(cropland_2030_ssp245, forest_old, method = "near")

# Save results
writeRaster(cropland_2030_ssp245, 
            filename = paste0(output_dir, "cropland_proportion_1km_2030_ssp245.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  cropland_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(cropland_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(cropland_10km_2050_ssp245 <- terra::focal(cropland_1000_temp_2050_ssp245, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
cropland_10km_resampled_2050_ssp245 <- terra::resample(cropland_10km_2050_ssp245, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
cropland_10km_2050_ssp245 <- terra::project(x = cropland_10km_resampled_2050_ssp245,
                                            y = x)

# Before saving, make the name of the value correspond to the variable
names(cropland_10km_2050_ssp245) <- "cropland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
cropland_10km_2050_ssp245[is.na(cropland_10km_2050_ssp245)] <- 0
cropland_10km_2050_ssp245 <- terra::mask(x = cropland_10km_2050_ssp245, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(cropland_10km_2050_ssp245)

cropland_10km_2050_ssp245 <- resample(cropland_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(cropland_10km_2050_ssp245, 
            filename = paste0(output_dir, "cropland_10km_2050_ssp245.tif"), 
            overwrite = TRUE)


# barren

system.time(
  barren_temp_2030_ssp245 <- aggregate(landcover_2030_ssp245,
                                       Nratio, 
                                       # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                       fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(barren_temp_2030_ssp245 )
barren_temp_2030_ssp245

# Resample at correct resolution
barren_resampled_2030_ssp245 <- terra::resample(barren_temp_2030_ssp245, 
                                                x_lc, 
                                                method = "bilinear")

# Project raster
barren_2030_ssp245 <- terra::project(x = barren_resampled_2030_ssp245,
                                     y = x)

# Before saving, make the name of the value correspond to the variable
names(barren_2030_ssp245) <- "barren"
barren_2030_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
barren_2030_ssp245[is.na(barren_2030_ssp245)] <- 0
barren_2030_ssp245 <- terra::mask(x = barren_2030_ssp245, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(barren_2030_ssp245)
barren_2030_ssp245 <- resample(barren_2030_ssp245, forest_old, method = "near")

# Save results
writeRaster(barren_2030_ssp245, 
            filename = paste0(output_dir, "barren_proportion_1km_2030_ssp245.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  barren_1000_temp_2070_ssp126 <- terra::aggregate(landcover_2070_ssp126,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(barren_1000_temp_2070_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(barren_10km_2070_ssp126 <- terra::focal(barren_1000_temp_2070_ssp126, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells


# Resample at correct resolution
barren_10km_resampled_2070_ssp126 <- terra::resample(barren_10km_2070_ssp126, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
barren_10km_2070_ssp126 <- terra::project(x = barren_10km_resampled_2070_ssp126,
                                          y = x)

# Before saving, make the name of the value correspond to the variable
names(barren_10km_2070_ssp126) <- "barren_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
barren_10km_2070_ssp126[is.na(barren_10km_2070_ssp126)] <- 0
barren_10km_2070_ssp126 <- terra::mask(x = barren_10km_2070_ssp126, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(barren_10km_2070_ssp126)

barren_10km_2070_ssp126 <- resample(barren_10km_2070_ssp126, forest_old, method = "near")

# Save results
writeRaster(barren_10km_2070_ssp126, 
            filename = paste0(output_dir, "barren_10km_2070_ssp126.tif"), 
            overwrite = TRUE)



# built_up

system.time(
  built_up_temp_2030_ssp245 <- aggregate(landcover_2030_ssp245,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(built_up_temp_2030_ssp245 )
built_up_temp_2030_ssp245

# Resample at correct resolution
built_up_resampled_2030_ssp245 <- terra::resample(built_up_temp_2030_ssp245, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
built_up_2030_ssp245 <- terra::project(x = built_up_resampled_2030_ssp245,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(built_up_2030_ssp245) <- "built_up"
built_up_2030_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
built_up_2030_ssp245[is.na(built_up_2030_ssp245)] <- 0
built_up_2030_ssp245 <- terra::mask(x = built_up_2030_ssp245, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(built_up_2030_ssp245)
built_up_2030_ssp245 <- resample(built_up_2030_ssp245, forest_old, method = "near")

# Save results
writeRaster(built_up_2030_ssp245, 
            filename = paste0(output_dir, "built_up_proportion_1km_2030_ssp245.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  built_up_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(built_up_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(built_up_10km_2050_ssp126 <- terra::focal(built_up_1000_temp_2050_ssp126, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
built_up_10km_resampled_2050_ssp126 <- terra::resample(built_up_10km_2050_ssp126, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
built_up_10km_2050_ssp126 <- terra::project(x = built_up_10km_resampled_2050_ssp126,
                                            y = x)

# Before saving, make the name of the value correspond to the variable
names(built_up_10km_2050_ssp126) <- "built_up_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
built_up_10km_2050_ssp126[is.na(built_up_10km_2050_ssp126)] <- 0
built_up_10km_2050_ssp126 <- terra::mask(x = built_up_10km_2050_ssp126, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(built_up_10km_2050_ssp126)

built_up_10km_2050_ssp126 <- resample(built_up_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(built_up_10km_2050_ssp126, 
            filename = paste0(output_dir, "built_up_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# water

system.time(
  water_temp_2030_ssp245 <- aggregate(landcover_2030_ssp245,
                                      Nratio, 
                                      # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                      fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(water_temp_2030_ssp245 )
water_temp_2030_ssp245

# Resample at correct resolution
water_resampled_2030_ssp245 <- terra::resample(water_temp_2030_ssp245, 
                                               x_lc, 
                                               method = "bilinear")

# Project raster
water_2030_ssp245 <- terra::project(x = water_resampled_2030_ssp245,
                                    y = x)

# Before saving, make the name of the value correspond to the variable
names(water_2030_ssp245) <- "water"
water_2030_ssp245

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
water_2030_ssp245[is.na(water_2030_ssp245)] <- 0
water_2030_ssp245 <- terra::mask(x = water_2030_ssp245, 
                                 mask = SECanadaVectTemp %>% 
                                   st_as_sf(.) %>%
                                   vect(.))

plot(water_2030_ssp245)
water_2030_ssp245 <- resample(water_2030_ssp245, forest_old, method = "near")

# Save results
writeRaster(water_2030_ssp245, 
            filename = paste0(output_dir, "water_proportion_1km_2030_ssp245.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  water_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                  Nratio_step, 
                                                  fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(water_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(water_10km_2050_ssp126 <- terra::focal(water_1000_temp_2050_ssp126, 
                                                   w = mat_10km,
                                                   fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
water_10km_resampled_2050_ssp126 <- terra::resample(water_10km_2050_ssp126, 
                                                    x_lc, 
                                                    method = "bilinear")

# Project raster
water_10km_2050_ssp126 <- terra::project(x = water_10km_resampled_2050_ssp126,
                                         y = x)

# Before saving, make the name of the value correspond to the variable
names(water_10km_2050_ssp126) <- "water_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
water_10km_2050_ssp126[is.na(water_10km_2050_ssp126)] <- 0
water_10km_2050_ssp126 <- terra::mask(x = water_10km_2050_ssp126, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(water_10km_2050_ssp126)

water_10km_2050_ssp126 <- resample(water_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(water_10km_2050_ssp126, 
            filename = paste0(output_dir, "water_10km_2050_ssp126.tif"), 
            overwrite = TRUE)





# 2100 ssp585

# Now, calculate the proportion of habitat per site for forest
system.time(
  forest_temp_2030_ssp585 <- aggregate(landcover_2030_ssp585,
                                       Nratio, 
                                       # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                       fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(forest_temp_2030_ssp585 )
forest_temp_2030_ssp585

# Resample at correct resolution
forest_resampled_2030_ssp585 <- terra::resample(forest_temp_2030_ssp585, 
                                                x_lc, 
                                                method = "bilinear")

# Project raster
forest_2030_ssp585 <- terra::project(x = forest_resampled_2030_ssp585,
                                     y = x)

# Before saving, make the name of the value correspond to the variable
names(forest_2030_ssp585) <- "forest"
forest_2030_ssp585

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
forest_2030_ssp585[is.na(forest_2030_ssp585)] <- 0
forest_2030_ssp585 <- terra::mask(x = forest_2030_ssp585, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(forest_2030_ssp585)

forest_2030_ssp585 <- resample(forest_2030_ssp585, forest_old, method = "near")

# Save results
writeRaster(forest_2030_ssp585, 
            filename = paste0(output_dir, "forest_proportion_1km_2030_ssp585.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  forest_1000_temp_2030_ssp245 <- terra::aggregate(landcover_2030_ssp245,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==2, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(forest_1000_temp_2030_ssp245)

system.time(forest_10km_2030_ssp245 <- terra::focal(forest_1000_temp_2030_ssp245, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121}))

# Resample at correct resolution
forest_10km_resampled_2030_ssp245 <- terra::resample(forest_10km_2030_ssp245, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
forest_10km_2030_ssp245 <- terra::project(x = forest_10km_resampled_2030_ssp245,
                                          y = x)

names(forest_10km_2030_ssp245) <- "forest_10km"

forest_10km_2030_ssp245[is.na(forest_10km_2030_ssp245)] <- 0
forest_10km_2030_ssp245 <- terra::mask(x = forest_10km_2030_ssp245, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(forest_10km_2030_ssp245)

forest_10km_2030_ssp245 <- resample(forest_10km_2030_ssp245, forest_old, method = "near")

writeRaster(forest_10km_2030_ssp245, 
            filename = paste0(output_dir, "forest_10km_2030_ssp245.tif"), 
            overwrite = TRUE)



# shrublands

system.time(
  shrublands_temp_2030_ssp585 <- aggregate(landcover_2030_ssp585,
                                           Nratio, 
                                           fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(shrublands_temp_2030_ssp585 )
shrublands_temp_2030_ssp585

shrublands_resampled_2030_ssp585 <- terra::resample(shrublands_temp_2030_ssp585, 
                                                    x_lc, 
                                                    method = "bilinear")

shrublands_2030_ssp585 <- terra::project(x = shrublands_resampled_2030_ssp585,
                                         y = x)

names(shrublands_2030_ssp585) <- "opforest"

shrublands_2030_ssp585[is.na(shrublands_2030_ssp585)] <- 0
shrublands_2030_ssp585 <- terra::mask(x = shrublands_2030_ssp585, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(shrublands_2030_ssp585)

shrublands_2030_ssp585 <- resample(shrublands_2030_ssp585, forest_old, method = "near")

writeRaster(shrublands_2030_ssp585, 
            filename = paste0(output_dir, "opforest_proportion_1km_2030_ssp585.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  shrublands_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                       Nratio_step, 
                                                       fun = function(x, na.rm=T) {(sum(x==8, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(shrublands_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(shrublands_10km_2050_ssp245 <- terra::focal(shrublands_1000_temp_2050_ssp245, 
                                                        w = mat_10km,
                                                        fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
shrublands_10km_resampled_2050_ssp245 <- terra::resample(shrublands_10km_2050_ssp245, 
                                                         x_lc, 
                                                         method = "bilinear")

# Project raster
shrublands_10km_2050_ssp245 <- terra::project(x = shrublands_10km_resampled_2050_ssp245,
                                              y = x)

# Before saving, make the name of the value correspond to the variable
names(shrublands_10km_2050_ssp245) <- "opforest_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
shrublands_10km_2050_ssp245[is.na(shrublands_10km_2050_ssp245)] <- 0
shrublands_10km_2050_ssp245 <- terra::mask(x = shrublands_10km_2050_ssp245, 
                                           mask = SECanadaVectTemp %>% 
                                             st_as_sf(.) %>%
                                             vect(.))

plot(shrublands_10km_2050_ssp245)

shrublands_10km_2050_ssp245 <- resample(shrublands_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(shrublands_10km_2050_ssp245, 
            filename = paste0(output_dir, "opforest_10km_2050_ssp245.tif"), 
            overwrite = TRUE)


# grassland

system.time(
  grassland_temp_2030_ssp585 <- aggregate(landcover_2030_ssp585,
                                          Nratio, 
                                          # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                          fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(grassland_temp_2030_ssp585 )
grassland_temp_2030_ssp585

# Resample at correct resolution
grassland_resampled_2030_ssp585 <- terra::resample(grassland_temp_2030_ssp585, 
                                                   x_lc, 
                                                   method = "bilinear")

# Project raster
grassland_2030_ssp585 <- terra::project(x = grassland_resampled_2030_ssp585,
                                        y = x)

# Before saving, make the name of the value correspond to the variable
names(grassland_2030_ssp585) <- "grassland"
grassland_2030_ssp585

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
grassland_2030_ssp585[is.na(grassland_2030_ssp585)] <- 0
grassland_2030_ssp585 <- terra::mask(x = grassland_2030_ssp585, 
                                     mask = SECanadaVectTemp %>% 
                                       st_as_sf(.) %>%
                                       vect(.))

plot(grassland_2030_ssp585)

grassland_2030_ssp585 <- resample(grassland_2030_ssp585, forest_old, method = "near")

# Save results
writeRaster(grassland_2030_ssp585, 
            filename = paste0(output_dir, "grassland_proportion_1km_2030_ssp585.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  grassland_1000_temp_2030_ssp245 <- terra::aggregate(landcover_2030_ssp245,
                                                      Nratio_step, 
                                                      fun = function(x, na.rm=T) {(sum(x==3, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(grassland_1000_temp_2030_ssp245)

system.time(grassland_10km_2030_ssp245 <- terra::focal(grassland_1000_temp_2030_ssp245, 
                                                       w = mat_10km,
                                                       fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121}))

# Resample at correct resolution
grassland_10km_resampled_2030_ssp245 <- terra::resample(grassland_10km_2030_ssp245, 
                                                        x_lc, 
                                                        method = "bilinear")

# Project raster
grassland_10km_2030_ssp245 <- terra::project(x = grassland_10km_resampled_2030_ssp245,
                                             y = x)

names(grassland_10km_2030_ssp245) <- "grassland_10km"

grassland_10km_2030_ssp245[is.na(grassland_10km_2030_ssp245)] <- 0
grassland_10km_2030_ssp245 <- terra::mask(x = grassland_10km_2030_ssp245, 
                                          mask = SECanadaVectTemp %>% 
                                            st_as_sf(.) %>%
                                            vect(.))

plot(grassland_10km_2030_ssp245)

grassland_10km_2030_ssp245 <- resample(grassland_10km_2030_ssp245, forest_old, method = "near")

writeRaster(grassland_10km_2030_ssp245, 
            filename = paste0(output_dir, "grassland_10km_2030_ssp245.tif"), 
            overwrite = TRUE)


# wetland

system.time(
  wetland_temp_2030_ssp585 <- aggregate(landcover_2030_ssp585,
                                        Nratio, 
                                        fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(wetland_temp_2030_ssp585 )
wetland_temp_2030_ssp585

wetland_resampled_2030_ssp585 <- terra::resample(wetland_temp_2030_ssp585, 
                                                 x_lc, 
                                                 method = "bilinear")

wetland_2030_ssp585 <- terra::project(x = wetland_resampled_2030_ssp585,
                                      y = x)

names(wetland_2030_ssp585) <- "wetland"

wetland_2030_ssp585[is.na(wetland_2030_ssp585)] <- 0
wetland_2030_ssp585 <- terra::mask(x = wetland_2030_ssp585, 
                                   mask = SECanadaVectTemp %>% 
                                     st_as_sf(.) %>%
                                     vect(.))

plot(wetland_2030_ssp585)
wetland_2030_ssp585 <- resample(wetland_2030_ssp585, forest_old, method = "near")

writeRaster(wetland_2030_ssp585, 
            filename = paste0(output_dir, "wetland_proportion_1km_2030_ssp585.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  wetland_1000_temp_2050_ssp245 <- terra::aggregate(landcover_2050_ssp245,
                                                    Nratio_step, 
                                                    fun = function(x, na.rm=T) {(sum(x==7, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(wetland_1000_temp_2050_ssp245)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(wetland_10km_2050_ssp245 <- terra::focal(wetland_1000_temp_2050_ssp245, 
                                                     w = mat_10km,
                                                     fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
wetland_10km_resampled_2050_ssp245 <- terra::resample(wetland_10km_2050_ssp245, 
                                                      x_lc, 
                                                      method = "bilinear")

# Project raster
wetland_10km_2050_ssp245 <- terra::project(x = wetland_10km_resampled_2050_ssp245,
                                           y = x)

# Before saving, make the name of the value correspond to the variable
names(wetland_10km_2050_ssp245) <- "wetland_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
wetland_10km_2050_ssp245[is.na(wetland_10km_2050_ssp245)] <- 0
wetland_10km_2050_ssp245 <- terra::mask(x = wetland_10km_2050_ssp245, 
                                        mask = SECanadaVectTemp %>% 
                                          st_as_sf(.) %>%
                                          vect(.))

plot(wetland_10km_2050_ssp245)

wetland_10km_2050_ssp245 <- resample(wetland_10km_2050_ssp245, forest_old, method = "near")

# Save results
writeRaster(wetland_10km_2050_ssp245, 
            filename = paste0(output_dir, "wetland_10km_2050_ssp245.tif"), 
            overwrite = TRUE)


# cropland

system.time(
  cropland_temp_2030_ssp585 <- aggregate(landcover_2030_ssp585,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(cropland_temp_2030_ssp585 )
cropland_temp_2030_ssp585

# Resample at correct resolution
cropland_resampled_2030_ssp585 <- terra::resample(cropland_temp_2030_ssp585, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
cropland_2030_ssp585 <- terra::project(x = cropland_resampled_2030_ssp585,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(cropland_2030_ssp585) <- "cropland"
cropland_2030_ssp585

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

cropland_2030_ssp585[is.na(cropland_2030_ssp585)] <- 0
cropland_2030_ssp585 <- terra::mask(x = cropland_2030_ssp585, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(cropland_2030_ssp585)
cropland_2030_ssp585 <- resample(cropland_2030_ssp585, forest_old, method = "near")

writeRaster(cropland_2030_ssp585, 
            filename = paste0(output_dir, "cropland_proportion_1km_2030_ssp585.tif"), 
            overwrite = TRUE)


##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  cropland_1000_temp_2030_ssp245 <- terra::aggregate(landcover_2030_ssp245,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==1, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(cropland_1000_temp_2030_ssp245)

system.time(cropland_10km_2030_ssp245 <- terra::focal(cropland_1000_temp_2030_ssp245, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121}))

# Resample at correct resolution
cropland_10km_resampled_2030_ssp245 <- terra::resample(cropland_10km_2030_ssp245, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
cropland_10km_2030_ssp245 <- terra::project(x = cropland_10km_resampled_2030_ssp245,
                                            y = x)

names(cropland_10km_2030_ssp245) <- "cropland_10km"

cropland_10km_2030_ssp245[is.na(cropland_10km_2030_ssp245)] <- 0
cropland_10km_2030_ssp245 <- terra::mask(x = cropland_10km_2030_ssp245, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(cropland_10km_2030_ssp245)

cropland_10km_2030_ssp245 <- resample(cropland_10km_2030_ssp245, forest_old, method = "near")

writeRaster(cropland_10km_2030_ssp245, 
            filename = paste0(output_dir, "cropland_10km_2030_ssp245.tif"), 
            overwrite = TRUE)


# barren

system.time(
  barren_temp_2030_ssp585 <- aggregate(landcover_2030_ssp585,
                                       Nratio, 
                                       fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(barren_temp_2030_ssp585 )
barren_temp_2030_ssp585

barren_resampled_2030_ssp585 <- terra::resample(barren_temp_2030_ssp585, 
                                                x_lc, 
                                                method = "bilinear")

barren_2030_ssp585 <- terra::project(x = barren_resampled_2030_ssp585,
                                     y = x)

names(barren_2030_ssp585) <- "barren"

barren_2030_ssp585[is.na(barren_2030_ssp585)] <- 0
barren_2030_ssp585 <- terra::mask(x = barren_2030_ssp585, 
                                  mask = SECanadaVectTemp %>% 
                                    st_as_sf(.) %>%
                                    vect(.))

plot(barren_2030_ssp585)
barren_2030_ssp585 <- resample(barren_2030_ssp585, forest_old, method = "near")

writeRaster(barren_2030_ssp585, 
            filename = paste0(output_dir, "barren_proportion_1km_2030_ssp585.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  barren_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                   Nratio_step, 
                                                   fun = function(x, na.rm=T) {(sum(x==4, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(barren_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(barren_10km_2050_ssp126 <- terra::focal(barren_1000_temp_2050_ssp126, 
                                                    w = mat_10km,
                                                    fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells


# Resample at correct resolution
barren_10km_resampled_2050_ssp126 <- terra::resample(barren_10km_2050_ssp126, 
                                                     x_lc, 
                                                     method = "bilinear")

# Project raster
barren_10km_2050_ssp126 <- terra::project(x = barren_10km_resampled_2050_ssp126,
                                          y = x)

# Before saving, make the name of the value correspond to the variable
names(barren_10km_2050_ssp126) <- "barren_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
barren_10km_2050_ssp126[is.na(barren_10km_2050_ssp126)] <- 0
barren_10km_2050_ssp126 <- terra::mask(x = barren_10km_2050_ssp126, 
                                       mask = SECanadaVectTemp %>% 
                                         st_as_sf(.) %>%
                                         vect(.))

plot(barren_10km_2050_ssp126)

barren_10km_2050_ssp126 <- resample(barren_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(barren_10km_2050_ssp126, 
            filename = paste0(output_dir, "barren_10km_2050_ssp126.tif"), 
            overwrite = TRUE)



# built_up

system.time(
  built_up_temp_2030_ssp585 <- aggregate(landcover_2030_ssp585,
                                         Nratio, 
                                         # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                         fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(built_up_temp_2030_ssp585 )
built_up_temp_2030_ssp585

# Resample at correct resolution
built_up_resampled_2030_ssp585 <- terra::resample(built_up_temp_2030_ssp585, 
                                                  x_lc, 
                                                  method = "bilinear")

# Project raster
built_up_2030_ssp585 <- terra::project(x = built_up_resampled_2030_ssp585,
                                       y = x)

# Before saving, make the name of the value correspond to the variable
names(built_up_2030_ssp585) <- "built_up"
built_up_2030_ssp585

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

built_up_2030_ssp585[is.na(built_up_2030_ssp585)] <- 0
built_up_2030_ssp585 <- terra::mask(x = built_up_2030_ssp585, 
                                    mask = SECanadaVectTemp %>% 
                                      st_as_sf(.) %>%
                                      vect(.))

plot(built_up_2030_ssp585)
built_up_2030_ssp585 <- resample(built_up_2030_ssp585, forest_old, method = "near")

writeRaster(built_up_2030_ssp585, 
            filename = paste0(output_dir, "built_up_proportion_1km_2030_ssp585.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  built_up_1000_temp_2030_ssp126 <- terra::aggregate(landcover_2030_ssp126,
                                                     Nratio_step, 
                                                     fun = function(x, na.rm=T) {(sum(x==5, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(built_up_1000_temp_2030_ssp126)

system.time(built_up_10km_2030_ssp126 <- terra::focal(built_up_1000_temp_2030_ssp126, 
                                                      w = mat_10km,
                                                      fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121}))

# Resample at correct resolution
built_up_10km_resampled_2030_ssp126 <- terra::resample(built_up_10km_2030_ssp126, 
                                                       x_lc, 
                                                       method = "bilinear")

# Project raster
built_up_10km_2030_ssp126 <- terra::project(x = built_up_10km_resampled_2030_ssp126,
                                            y = x)

names(built_up_10km_2030_ssp126) <- "built_up_10km"

built_up_10km_2030_ssp126[is.na(built_up_10km_2030_ssp126)] <- 0
built_up_10km_2030_ssp126 <- terra::mask(x = built_up_10km_2030_ssp126, 
                                         mask = SECanadaVectTemp %>% 
                                           st_as_sf(.) %>%
                                           vect(.))

plot(built_up_10km_2030_ssp126)

built_up_10km_2030_ssp126 <- resample(built_up_10km_2030_ssp126, forest_old, method = "near")

writeRaster(built_up_10km_2030_ssp126, 
            filename = paste0(output_dir, "built_up_10km_2030_ssp126.tif"), 
            overwrite = TRUE)



# water

system.time(
  water_temp_2030_ssp585 <- aggregate(landcover_2030_ssp585,
                                      Nratio, 
                                      # Function sums the number of annual cropland cells x cell area (10 x 10) / area of environmental layer raster cells (1000 x 1000)
                                      fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)})
)

plot(water_temp_2030_ssp585 )
water_temp_2030_ssp585

# Resample at correct resolution
water_resampled_2030_ssp585 <- terra::resample(water_temp_2030_ssp585, 
                                               x_lc, 
                                               method = "bilinear")

# Project raster
water_2030_ssp585 <- terra::project(x = water_resampled_2030_ssp585,
                                    y = x)

# Before saving, make the name of the value correspond to the variable
names(water_2030_ssp585) <- "water"
water_2030_ssp585

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

water_2030_ssp585[is.na(water_2030_ssp585)] <- 0
water_2030_ssp585 <- terra::mask(x = water_2030_ssp585, 
                                 mask = SECanadaVectTemp %>% 
                                   st_as_sf(.) %>%
                                   vect(.))

plot(water_2030_ssp585)
water_2030_ssp585 <- resample(water_2030_ssp585, forest_old, method = "near")

writeRaster(water_2030_ssp585, 
            filename = paste0(output_dir, "water_proportion_1km_2030_ssp585.tif"), 
            overwrite = TRUE)

##### 3. Calculate proportion of habitat within 10 km -----------------------------

system.time(
  water_1000_temp_2050_ssp126 <- terra::aggregate(landcover_2050_ssp126,
                                                  Nratio_step, 
                                                  fun = function(x, na.rm=T) {(sum(x==6, na.rm = na.rm))*30*30/(1000*1000)}) 
)

plot(water_1000_temp_2050_ssp126)

# Find area of grassland within 10000/1000 = 10 cells  = fact 10
# Takes Dell laptop core i5 ~ 1 seconds for entire landscape
system.time(water_10km_2050_ssp126 <- terra::focal(water_1000_temp_2050_ssp126, 
                                                   w = mat_10km,
                                                   fun = function(x, na.rm = T){sum(x, na.rm = na.rm)/121})) # 11 x 11 matrix = 121 cells



# Resample at correct resolution
water_10km_resampled_2050_ssp126 <- terra::resample(water_10km_2050_ssp126, 
                                                    x_lc, 
                                                    method = "bilinear")

# Project raster
water_10km_2050_ssp126 <- terra::project(x = water_10km_resampled_2050_ssp126,
                                         y = x)

# Before saving, make the name of the value correspond to the variable
names(water_10km_2050_ssp126) <- "water_10km"

#Need to mask landcover resampled rasters (so that 0 within mask, NA outside)

# Make NA values 0, then re-mask by lc_outline
water_10km_2050_ssp126[is.na(water_10km_2050_ssp126)] <- 0
water_10km_2050_ssp126 <- terra::mask(x = water_10km_2050_ssp126, 
                                      mask = SECanadaVectTemp %>% 
                                        st_as_sf(.) %>%
                                        vect(.))

plot(water_10km_2050_ssp126)

water_10km_2050_ssp126 <- resample(water_10km_2050_ssp126, forest_old, method = "near")

# Save results
writeRaster(water_10km_2050_ssp126, 
            filename = paste0(output_dir, "water_10km_2050_ssp126.tif"), 
            overwrite = TRUE)










