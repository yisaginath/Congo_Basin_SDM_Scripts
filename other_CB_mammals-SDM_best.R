## Script name: SDM model fitting pipeline using test data sets for Southeastern Canada
##
## Purpose of script: To build a pipeline for modeling large amounts of species occurrence data, using parallel clusters
##
## Source: Yisa Ginath Yuh, adapted from scripts produced for the prairie region by James Paterson
## Email: gyisa@uottawa.ca

##--------------------------------------------------------------------------------------

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



##--------------------------------------------------------------------------------------

# The MaxEnt model fitting requires java, and it is very imperative ti allocate a maximum memory space with java, so as to reduce memory issues when running java operations (e.g., rjava).
# Ensure to set java heap space based on your computer RAM.
# Here, I am setting my Java heap space to 8GB. users can modify to different GB limits
options(java.parameters = '-Xmx16g') 

#par(mfrow = c(1, 1))


###-------------- Loading, cleaning, and projecting data-----------------------

# 1. I start my analysis by loading and stacking my spatial environmental variables using a function

# Load rasters for environmental data

MAT <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/MAT_current_99.tif")
MAT
MAP <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAP_filtered_5_95/MAP_current_99.tif")
MAP

MAT_2030_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2030_ssp126_filt5_99.tif")
MAT_2030_ssp126
MAP_2030_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAP_filtered_5_95/prec_2030_ssp126_filt5_99.tif")
MAP_2030_ssp126

MAT_2030_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2030_ssp245_filt5_99.tif")
MAT_2030_ssp245
MAP_2030_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAP_filtered_5_95/prec_2030_ssp245_filt5_99.tif")
MAP_2030_ssp245

MAT_2030_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2030_ssp585_filt5_99.tif")
MAT_2030_ssp585
MAP_2030_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAP_filtered_5_95/prec_2030_ssp585_filt5_99.tif")
MAP_2030_ssp585

MAT_2050_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2050_ssp126_filt5_99.tif")
MAT_2050_ssp126
MAP_2050_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAP_filtered_5_95/prec_2050_ssp126_filt5_99.tif")
MAP_2050_ssp126

MAT_2050_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2050_ssp245_filt5_99.tif")
MAT_2050_ssp245
MAP_2050_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAP_filtered_5_95/prec_2050_ssp245_filt5_99.tif")
MAP_2050_ssp245

MAT_2050_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2050_ssp585_filt5_99.tif")
MAT_2050_ssp585
MAP_2050_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAP_filtered_5_95/prec_2050_ssp585_filt5_99.tif")
MAP_2050_ssp585

MAT_2070_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2070_ssp126_filt5_99.tif")
MAT_2070_ssp126
MAP_2070_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAP_filtered_5_95/prec_2070_ssp126_filt5_99.tif")
MAP_2070_ssp126

MAT_2070_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2070_ssp245_filt5_99.tif")
MAT_2070_ssp245
MAP_2070_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAP_filtered_5_95/prec_2070_ssp245_filt5_99.tif")
MAP_2070_ssp245

MAT_2070_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2070_ssp585_filt5_99.tif")
MAT_2070_ssp585
MAP_2070_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAP_filtered_5_95/prec_2070_ssp585_filt5_99.tif")
MAP_2070_ssp585

MAT_2100_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2100_ssp126_filt5_99.tif")
MAT_2100_ssp126
MAP_2100_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAP_filtered_5_95/prec_2100_ssp126_filt5_99.tif")
MAP_2100_ssp126

MAT_2100_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2100_ssp245_filt5_99.tif")
MAT_2100_ssp245
MAP_2100_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAP_filtered_5_95/prec_2100_ssp245_filt5_99.tif")
MAP_2100_ssp245

MAT_2100_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAT_filtered_5_95/temp_2100_ssp585_filt5_99.tif")
MAT_2100_ssp585
MAP_2100_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors/MAP_filtered_5_95/prec_2100_ssp585_filt5_99.tif")
MAP_2100_ssp585

elevation <- raster("F:/WWF_data/SDMs_LC_climate/predictors/elevation.tif")
slope <- raster("F:/WWF_data/SDMs_LC_climate/predictors/slope.tif")

forest <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/forest_proportion_1km.tif")
cropland <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/croplands_proportion_1km.tif")
urban <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/built_up_proportion_1km.tif")
grassland <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/grassland_proportion_1km.tif")
barren <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/barren_lands_proportion_1km.tif")
water <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/water_proportion_1km.tif")
oforest <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/opforest_proportion_1km.tif")
wetland <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/wetlands_proportion_1km.tif")

forest_2030_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/forest_proportion_1km_2030_ssp126.tif")
cropland_2030_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/cropland_proportion_1km_2030_ssp126.tif")
urban_2030_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/built_up_proportion_1km_2030_ssp126.tif")
grassland_2030_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/grassland_proportion_1km_2030_ssp126.tif")
barren_2030_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/barren_proportion_1km_2030_ssp126.tif")
water_2030_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/water_proportion_1km_2030_ssp126.tif")
oforest_2030_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/opforest_proportion_1km_2030_ssp126.tif")
wetland_2030_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/wetland_proportion_1km_2030_ssp126.tif")

forest_2030_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/forest_proportion_1km_2030_ssp245.tif")
cropland_2030_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/cropland_proportion_1km_2030_ssp245.tif")
urban_2030_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/built_up_proportion_1km_2030_ssp245.tif")
grassland_2030_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/grassland_proportion_1km_2030_ssp245.tif")
barren_2030_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/barren_proportion_1km_2030_ssp245.tif")
water_2030_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/water_proportion_1km_2030_ssp245.tif")
oforest_2030_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/opforest_proportion_1km_2030_ssp245.tif")
wetland_2030_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/wetland_proportion_1km_2030_ssp245.tif")

forest_2030_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/forest_proportion_1km_2030_ssp585.tif")
cropland_2030_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/cropland_proportion_1km_2030_ssp585.tif")
urban_2030_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/built_up_proportion_1km_2030_ssp585.tif")
grassland_2030_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/grassland_proportion_1km_2030_ssp585.tif")
barren_2030_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/barren_proportion_1km_2030_ssp585.tif")
water_2030_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/water_proportion_1km_2030_ssp585.tif")
oforest_2030_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/opforest_proportion_1km_2030_ssp585.tif")
wetland_2030_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/wetland_proportion_1km_2030_ssp585.tif")

forest_2050_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/forest_proportion_1km_2050_ssp126.tif")
cropland_2050_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/cropland_proportion_1km_2050_ssp126.tif")
urban_2050_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/built_up_proportion_1km_2050_ssp126.tif")
grassland_2050_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/grassland_proportion_1km_2050_ssp126.tif")
barren_2050_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/barren_proportion_1km_2050_ssp126.tif")
water_2050_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/water_proportion_1km_2050_ssp126.tif")
oforest_2050_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/opforest_proportion_1km_2050_ssp126.tif")
wetland_2050_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/wetland_proportion_1km_2050_ssp126.tif")

forest_2050_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/forest_proportion_1km_2050_ssp245.tif")
cropland_2050_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/cropland_proportion_1km_2050_ssp245.tif")
urban_2050_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/built_up_proportion_1km_2050_ssp245.tif")
grassland_2050_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/grassland_proportion_1km_2050_ssp245.tif")
barren_2050_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/barren_proportion_1km_2050_ssp245.tif")
water_2050_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/water_proportion_1km_2050_ssp245.tif")
oforest_2050_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/opforest_proportion_1km_2050_ssp245.tif")
wetland_2050_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/wetland_proportion_1km_2050_ssp245.tif")

forest_2050_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/forest_proportion_1km_2050_ssp585.tif")
cropland_2050_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/cropland_proportion_1km_2050_ssp585.tif")
urban_2050_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/built_up_proportion_1km_2050_ssp585.tif")
grassland_2050_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/grassland_proportion_1km_2050_ssp585.tif")
barren_2050_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/barren_proportion_1km_2050_ssp585.tif")
water_2050_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/water_proportion_1km_2050_ssp585.tif")
oforest_2050_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/opforest_proportion_1km_2050_ssp585.tif")
wetland_2050_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/wetland_proportion_1km_2050_ssp585.tif")

forest_2070_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/forest_proportion_1km_2070_ssp126.tif")
cropland_2070_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/cropland_proportion_1km_2070_ssp126.tif")
urban_2070_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/built_up_proportion_1km_2070_ssp126.tif")
grassland_2070_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/grassland_proportion_1km_2070_ssp126.tif")
barren_2070_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/barren_proportion_1km_2070_ssp126.tif")
water_2070_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/water_proportion_1km_2070_ssp126.tif")
oforest_2070_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/opforest_proportion_1km_2070_ssp126.tif")
wetland_2070_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/wetland_proportion_1km_2070_ssp126.tif")

forest_2070_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/forest_proportion_1km_2070_ssp245.tif")
cropland_2070_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/cropland_proportion_1km_2070_ssp245.tif")
urban_2070_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/built_up_proportion_1km_2070_ssp245.tif")
grassland_2070_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/grassland_proportion_1km_2070_ssp245.tif")
barren_2070_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/barren_proportion_1km_2070_ssp245.tif")
water_2070_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/water_proportion_1km_2070_ssp245.tif")
oforest_2070_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/opforest_proportion_1km_2070_ssp245.tif")
wetland_2070_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/wetland_proportion_1km_2070_ssp245.tif")

forest_2070_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/forest_proportion_1km_2070_ssp585.tif")
cropland_2070_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/cropland_proportion_1km_2070_ssp585.tif")
urban_2070_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/built_up_proportion_1km_2070_ssp585.tif")
grassland_2070_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/grassland_proportion_1km_2070_ssp585.tif")
barren_2070_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/barren_proportion_1km_2070_ssp585.tif")
water_2070_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/water_proportion_1km_2070_ssp585.tif")
oforest_2070_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/opforest_proportion_1km_2070_ssp585.tif")
wetland_2070_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/wetland_proportion_1km_2070_ssp585.tif")

forest_2100_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/forest_proportion_1km_2100_ssp126.tif")
cropland_2100_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/cropland_proportion_1km_2100_ssp126.tif")
urban_2100_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/built_up_proportion_1km_2100_ssp126.tif")
grassland_2100_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/grassland_proportion_1km_2100_ssp126.tif")
barren_2100_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/barren_proportion_1km_2100_ssp126.tif")
water_2100_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/water_proportion_1km_2100_ssp126.tif")
oforest_2100_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/opforest_proportion_1km_2100_ssp126.tif")
wetland_2100_ssp126 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/wetland_proportion_1km_2100_ssp126.tif")

forest_2100_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/forest_proportion_1km_2100_ssp245.tif")
cropland_2100_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/cropland_proportion_1km_2100_ssp245.tif")
urban_2100_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/built_up_proportion_1km_2100_ssp245.tif")
grassland_2100_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/grassland_proportion_1km_2100_ssp245.tif")
barren_2100_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/barren_proportion_1km_2100_ssp245.tif")
water_2100_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/water_proportion_1km_2100_ssp245.tif")
oforest_2100_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/opforest_proportion_1km_2100_ssp245.tif")
wetland_2100_ssp245 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/wetland_proportion_1km_2100_ssp245.tif")

forest_2100_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/forest_proportion_1km_2100_ssp585.tif")
cropland_2100_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/cropland_proportion_1km_2100_ssp585.tif")
urban_2100_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/built_up_proportion_1km_2100_ssp585.tif")
grassland_2100_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/grassland_proportion_1km_2100_ssp585.tif")
barren_2100_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/barren_proportion_1km_2100_ssp585.tif")
water_2100_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/water_proportion_1km_2100_ssp585.tif")
oforest_2100_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/opforest_proportion_1km_2100_ssp585.tif")
wetland_2100_ssp585 <- raster("F:/WWF_data/SDMs_LC_climate/predictors1/wetland_proportion_1km_2100_ssp585.tif")


# start with current predictions

# Stack all environmental rasters (raster stack)
#environmental_var_stack <- raster::stack(MAT, MAP, forest, cropland, urban,
#grassland, barren, water, elevation, slope, 
#oforest, wetland)
#environmental_var_stack

#plot(environmental_var_stack)


#names(environmental_var_stack) <- c("temp", "prec", "forest", "cropland", "urban", 
#"grassland", "barren", "water", "elevation", "slope",
#"oforest", "wetland")



# Stack all environmental rasters (raster stack)
environmental_var_stack <- raster::stack(MAT, MAP, forest, cropland, urban,
                                         grassland, barren, water, elevation, slope, 
                                         oforest, wetland)
environmental_var_stack

plot(environmental_var_stack)


names(environmental_var_stack) <- c("temp", "prec", "forest", "cropland", "urban", 
                                    "grassland", "barren", "water", "elevation", "slope",
                                    "oforest", "wetland")


# Read study area 
shapefile_path <- "F:/WWF_data/climate_species_analysis/defined_study_area/study_area_fit.shp"
studyarea <- st_read(shapefile_path)  # Path to your study area shapefile

study_area <- studyarea %>%
  # Transform the CRS of the ppr_region to match that of environmental variables(environmental_var_stack)
  st_transform(crs = crs(environmental_var_stack)) %>%
  # Remove the Z dimension, if present
  st_zm() %>%
  # Convert sf (simple feature) to sp (spatial) object
  as_Spatial()

plot(MAT)
plot(study_area, add = TRUE, border = "red", lwd = 2)


##3. Second step is to load species list and observations 

# I start by loading the csv file that contains the list of all species for the prairie region, filter and modify the data, and adds columns for model performance and environmental variables
modelPerformanceAllSpecies <- read.csv("F:/WWF_data/SDMs_LC_climate/other_mammal_summary.csv") %>%
  filter(n_CB > 20) %>% # this code filters and keep rows where the n_SECanada column has  20 observation records or more. 20 records is the minimum threshold required for fitting models for the SE Canada region
  #dplyr::select(-X) %>%
  # the below code adds to two columns with variables mean_auc and mean_threshold, initialized with a NA value in inorder to store future model performance values
  mutate(mean_auc = NA,
         mean_threshold = NA) %>%
  # add blank columns for mean species values of each environmental variable, and filling them with NA values
  cbind(., t(rep(NA,length(names(environmental_var_stack)))))

dim(modelPerformanceAllSpecies)  # checks the number of rows and columns in the species list
head(modelPerformanceAllSpecies)# checks some few rows to see how the data looks like with the columns added
#View(modelPerformanceAllSpecies)

#add environmental variable names to data frame
# the code below retrieves and adds new columns to the modelPerformanceAllSpecies file, with names of each environmental variable in the raster stack, starting from the 8th column to the last column (ncol(modelPerformanceAllSpecies))
names(modelPerformanceAllSpecies)[8:ncol(modelPerformanceAllSpecies)] <- names(environmental_var_stack)

#Now reprint the modelPerformanceAllSpecies data to check if the environmental variables have been successfully added 
head(modelPerformanceAllSpecies)
#View(modelPerformanceAllSpecies)

# Load species observation data 
#mammal_observations <- read.csv("F:/WWF_data/SDMs_LC_climate/mammal_observations_cleaned.csv")
#ape_observations <- read.csv("F:/WWF_data/SDMs_LC_climate/apes.csv")
mammal_observations <- read.csv("F:/WWF_data/SDMs_LC_climate/other_mammal_observations.csv")

# Combine all observations
species_observations <- rbind(mammal_observations) 
#species_observations <- rbind(ape_observations, elephant_observations) 
head(species_observations)

# Species observations are still in dataframe format, hence, a need to convert the data frame to an sf object
species_observations_sf <- st_as_sf(
  species_observations,
  coords = c("longitude", "latitude"),
  crs = 4326,  # WGS84 coordinate reference system
  remove = FALSE  # Keeps the original longitude and latitude columns
)

# Check the structure
print(species_observations_sf)

# Reproject the sf object to the CRS of forest (e.g., a raster layer)
species_observations_transformed <- st_transform(species_observations_sf, crs = st_crs(environmental_var_stack))

# Remove rows where "species" column is NA
species_observations_cleaned <- species_observations_transformed[!is.na(species_observations_transformed$species), ]

# Check the result
head(species_observations_cleaned)
# Plot observation points and visualize on climate maps
plot(environmental_var_stack[[1]], main = "Reprojected & Masked Bio9 Layer")
# Add the species points
plot(species_observations_cleaned, 
     add = TRUE, 
     col = "red", 
     pch = 16)


# Step 2: Remove Z/M dimensions if present (if needed for conversion to Spatial)
species_observations_reproj <- st_zm(species_observations_cleaned)

# Get the extent of environmental_var_stack as an sf polygon
env_extent <- as(extent(environmental_var_stack), "SpatialPolygons")
crs(env_extent) <- crs(environmental_var_stack)
env_extent_sf <- st_as_sf(env_extent)

# Clip points that fall within extent
species_observations_clipped <- st_intersection(species_observations_reproj, env_extent_sf)

# Plot
plot(environmental_var_stack[[1]], main = "Clipped Species Observations ")
plot(st_geometry(species_observations_clipped), 
     add = TRUE, 
     col = "red", 
     pch = 16)


### Use loop to create raster for each taxonomic group.
# In this section, I will start by defining a group vector, by assigning the taxonomic groups that will be iterated in a loop
# Print unique names in the "group" column
unique_groups <- unique(species_observations_clipped$group)# get group names from observation data
print(unique_groups)

group_vector <- c("mammals")
#group_vector <- c("apes", "elephants")
group_vector
# The second stage is to Store target group Sampling raster in a list (named by taxon)
# Here, I will first create an empty list to store raster objects, that will be created as kernel density estimates for each taxonomic group
observations_kde_list <- list()

names(environmental_var_stack)


# Next, is to define resolution and extent based on environmental_mask. This will ensure that all raster layers that will be generated, will follow thesame resolution and spatial extents as those of the stacked raster layers created at the early stage (e.g., forest
# first, create an environmental mask for one of the stacked raster layer (here i use forest), which will be used in setting the desired resolution and extent for all other raster objects as i move forward
environmental_mask <- terra::rast(environmental_var_stack$forest)
# second, I create a target resolution to be used for all raster layers as I proceed, by extracting the resolution of the masked raster layer (forest), using the terra package
target_resolution <- terra::res(environmental_mask)
# third, I also create a target extent that will be used for all raster layers to be developed, by extracting the extent of the masked raster
target_extent <- terra::ext(environmental_mask)

environmental_mask
plot(environmental_mask)
target_resolution
target_extent

for(g in group_vector){ # this function defines a loop over each element in the group_vector defined above (birds, mammals, amphibians and reptiles), where data specific to each group, g, will be processed independently in each iteration in the loop
  # after defining the loop function, the next thing is to ensure that each species observation is filtered to include only records for the group, g, with these records converted to a spatial format for further processing
  group_g_observations <- species_observations_clipped %>%
    filter(group == g) %>%
    slice_sample(n = 100000)  # for each group with filtered records, I randomly sample about 100,000 observations for producing KDE. 100,000 observations seem to be a manageable threshold record for each group in every KDE operation
  
  # Check if there are any observations to process
  if (nrow(group_g_observations) == 0) {
    message("No observations for group: ", g)
    next  # Skip to the next group if no data
  }
  summary(group_g_observations)
  
  # next, for each group observation g, convert the coordinates to simple feature feature (sf) objects using the sf package, so it becomes compatible with spatial feature functions in R
  group_g_observations_sf <- st_as_sf(group_g_observations, coords = c("longitude", "latitude"), crs = 4326) # here, I have converted the coordinates to EPSG:4326, the standard World Geodetic coordinate system 
  
  # the next thing is to project the group_g_observations_sf data to match that of the stacked environmental layers
  group_g_observations_sf <- st_transform(group_g_observations_sf, st_crs(environmental_var_stack))
  
  # I will now calculate KDE (kernel density estimates) for each group observation g, by masking the calculations (raster data) to the target resolution defined in the earlier face of the codes, using the resolution of one of the stacked predictors (forest). I am using the spatialEco package to do this
  kde_result <- spatialEco::sp.kde(
    x = group_g_observations_sf,
    res = target_resolution,   # Set KDE resolution to match environmental_mask
    standardize = TRUE #normalizes the density values to make them comparable across groups.
  )
  # Check the class of the KDE result and resample if needed
  # here, there is a need to verify is the KDE results produced are SpatRaster objects, considering that the package used now is the terra package, which has replaced the previos Rgeos package that has depreciated
  # the code therefore ensures that, If kde_result is not a SpatRaster object, then, the stop() function is executed to halt the operation and display the message "KDE result is not a SpatRaster." This ensures that only valid SpatRaster objects are processed in the next steps.
  if (!inherits(kde_result, "SpatRaster")) {
    stop("KDE result is not a SpatRaster.")
  }
  
  # After calculating the KDE for each species observation, g, there is a need ensure that the results fall within the geographic boundaries of the target extent (forest), define in the earlier stage of this script
  kde_result <- terra::crop(kde_result, environmental_mask)  # Crop to target extent using the crop function of the terra package
  kde_result <- terra::resample(kde_result, environmental_mask, method = "bilinear")  # although the target resolution has been specified in the KDE calculations, it is always a good approach to resample to target resolution again after coping the target extent. Also, the bilinear interpolation in the raster resampling, ensures that new pixel values are calculated based on the weighted average of surrounding pixels, yielding a smoother result
  
  # To ensure that there is no mismatch in spatial extents and resolutions between the calculated KDEs and masked or stacked environmental layers, there is a need to mask the KDE result with the stacked or masked environmental layer (forest)
  observations_kde_list[[g]] <- terra::mask(kde_result, environmental_mask)
}


# Plot KDE results for each group
par(mfrow = c(1, 1))  # Arrange plots in a 2x2 grid

for (g in names(observations_kde_list)) {
  plot(observations_kde_list[[g]], main = paste("KDE for", g))
}


# Set species common name: here, I extract the common name of each species in each row i (within the modelPerformanceAllSpecies csv file), and store it in a new variable called scientific_name_i, for easy reference in the loop iterations
for (i in 1:nrow(modelPerformanceAllSpecies)) {
  # Extract key species attributes
  group_i <- modelPerformanceAllSpecies$group[i]  
  genus_i <- modelPerformanceAllSpecies$genus[i]
  species_i <- modelPerformanceAllSpecies$species[i]
  scientific_name_i <- modelPerformanceAllSpecies$scientific_name[i]
  common_name_i <- modelPerformanceAllSpecies$scientific_name[i]
  
  message("\nProcessing species: ", scientific_name_i, " (", genus_i, " ", species_i, ") in group: ", group_i)
  
  # Ensure KDE raster exists for this group
  if (!group_i %in% names(observations_kde_list)) {
    message("No KDE raster available for group: ", group_i, " - Skipping species: ", scientific_name_i)
    next
  }
  observations_kde_group_i <- observations_kde_list[[group_i]]
  
  # Filter species observations
  species_i_sp <- species_observations_clipped %>%
    filter(scientific_name == scientific_name_i) %>%
    slice_sample(n = 1, by = square_id)  # Avoids spatial autocorrelation issues
  
  if (nrow(species_i_sp) == 0) {
    message("No valid occurrences found for species: ", scientific_name_i, " - Skipping.")
    next
  }
  
  # Convert species occurrence data to spatial format
  species_i_sp <- st_as_sf(species_i_sp, coords = c("longitude", "latitude"), crs = 4326)
  
  # Ensure study_area is an sf object and matches CRS
  study_area <- st_as_sf(study_area)
  
  if (st_crs(species_i_sp) != st_crs(study_area)) {
    message("Transforming CRS of study area to match species observations.")
    study_area <- st_transform(study_area, st_crs(species_i_sp))
  }
  
  # Print key elements for debugging (optional)
  print(observations_kde_group_i)
  
  # Intersect species points with the study area
  species_i_intersects_SE <- st_intersects(species_i_sp, study_area, sparse = FALSE)
  
  # Filter species points by intersection (keep only points inside the study area)
  species_i_sp <- species_i_sp[which(rowSums(species_i_intersects_SE) > 0), ]
  
  # Check if there are any remaining points after filtering
  if (nrow(species_i_sp) == 0) {
    message("No valid intersection found for species: ", scientific_name_i, " - Skipping.")
    modelPerformanceAllSpecies$n_MWCASC[i] <- 0  # Assign zero if no observations remain
    next
  }
  
  # Transform the CRS of the filtered points to match the environmental variable stack
  species_i_sp <- st_transform(species_i_sp, st_crs(environmental_var_stack))
  
  # Extract unique observation points (latitude and longitude), removing duplicate coordinates
  obs_species_i <- unique(st_coordinates(species_i_sp))
  
  # Convert the matrix of coordinates into a data frame
  obs_species_i <- data.frame(longitude = obs_species_i[, 1],
                              latitude = obs_species_i[, 2])
  
  # Check the first few records (optional for debugging)
  head(obs_species_i)
  
  # Save sample size: store the count of unique observation points in modelPerformanceAllSpecies
  modelPerformanceAllSpecies$n_MWCASC[i] <- nrow(obs_species_i)
  
  # Optional: Print progress update
  message("Processed ", nrow(obs_species_i), " unique occurrences for species: ", common_name_i)
  
  # Define output folder and create it if it doesn't exist
  output_dir_range <- "F:/WWF_data/SDMs_LC_climate/species_range_other_mammals"
  dir.create(output_dir_range, recursive = TRUE, showWarnings = FALSE)
  
  # Only model species with >= 20 observations
  if(modelPerformanceAllSpecies$n_MWCASC[i] >= 20){# here, i first of all check and select the number of unique observations filtered for the prairie region (n_prairie) in each row of the modelPerformanceAllSpecies data, with records  >= 20; 20 is the minimum threshold)
    # I now assign the obs_species_i data frame (which contains the unique coordinates in each grid id) into a new variable "species_i_ppr_sp", assign lon and lat coordinates to make it a spatial object, and finally project the variable to a similar CRS with the stacked environmental variables
    species_i_sp <- obs_species_i
    coordinates(species_i_sp) <- c("longitude", "latitude")
    species_i_sp@proj4string <- raster::crs(environmental_var_stack)
    
    # Generate a 50 km buffer around each observation point
    species_i_buffers <- spTransform(species_i_sp, CRS = raster::crs(environmental_var_stack)) %>%
      raster::buffer(width = 50000)  # 5 km buffer around each observation
    
    # Convert the buffer to an sf object for easier handling
    species_i_buffers_sf <- st_as_sf(species_i_buffers)
    
    # Dissolve and merge all buffers
    species_i_buffers_dissolved <- species_i_buffers_sf %>%
      st_union()  # Dissolve all buffers into a single polygon
    
    # Define species name and file path
    species_names <- modelPerformanceAllSpecies$species[i]
    shapefile_path <- file.path(output_dir_range, paste0(gsub(" ", "_", species_names), "_range.shp"))
    
    # Save the shapefile
    st_write(species_i_buffers_dissolved, shapefile_path, delete_layer = TRUE)
    
    # Plot the dissolved buffer (clipped to the study area) and the study area
    #par(mfrow = c(1, 1))  # Set up a single panel for plotting
    #plot(st_geometry(species_i_buffers_dissolved), main = "Clipped Dissolved 5 km Buffer for Fowler's Toad Observations", col = "lightblue")
    #plot(st_geometry(clipped_study_area_proj), add = TRUE, border = "black")  # Overlay study area
    
    
    ggplot() +
      geom_sf(data = study_area, fill = "gray95", color = "black") +
      geom_sf(data = st_as_sf(species_i_buffers_dissolved), color = "darkgreen", size = 2, alpha = 0.7) +
      theme_minimal() +
      labs(title = "Species Observations",
           subtitle = "Species Observations within the Study Area",
           caption = "Species observation points have been transformed to match CRS")
    
    
    # Now Calculate species range size
    species_i_rangesize <- species_i_buffers_dissolved %>% 
      st_intersection(study_area) %>%  # Intersects the species buffer with the clipped study area to remove parts outside
      st_area() %>%  # Calculates the total area of the intersected buffers
      as.numeric() %>%  # Converts the calculated area to numeric for ease of further analysis
      sum()  # Sums up areas of multiple polygons to get the total range size
    species_i_rangesize
    
    # Estimate study area size
    study_area_size <- st_area(study_area) %>% as.numeric()
    study_area_size 
    
    # Ensure the buffer has the same CRS as the environmental variable stack
    species_i_buffers_dissolved <- st_transform(species_i_buffers_dissolved, crs = st_crs(environmental_var_stack))
    
    # Convert the dissolved buffer to an sp object
    species_i_buffers_dissolved_sp <- as(species_i_buffers_dissolved, "Spatial")
    
    # Now apply the crop and mask functions
    if(species_i_rangesize >= 0.9 * 2.018284e+12){ # Use full study area if range size is >= 90% of study area
      environmental_var_stack_i <- environmental_var_stack  # Use the entire study area
    } else {
      environmental_var_stack_i <- environmental_var_stack %>%
        raster::crop(., species_i_buffers_dissolved_sp) %>%  # Crop environmental data to species' range
        raster::mask(., species_i_buffers_dissolved_sp)  # Mask environmental data to species' range
    }
    
    
    # Plot to visualize the environmental variable stack and buffer
    #plot(environmental_var_stack_i$temp)
    #plot(species_i_buffers_dissolved, add = TRUE, lty = 5)  # Overlay species buffer on top of the environmental data
    
    # Use the full study area, regardless of the species' range size
    #environmental_var_stack_i <- environmental_var_stack  # Use the entire study area
    
    # Convert species_i_sp to an sf object if it is a SpatialPointsDataFrame
    species_i_sf <- st_as_sf(species_i_sp)
    
    # Next, is to extract values of environmental_var_stack_i for all obs_species_i
    modelPerformanceAllSpecies[i,8:ncol(modelPerformanceAllSpecies)] <- raster::extract(environmental_var_stack_i,
                                                                                        species_i_sf) %>%
      # first, this code ensures that the raster values of each environmental variable (within the stacked raster) is extracted for every given location of a species, within the species observation variable, "species_i_ppr_sp".
      # through this approach, a data structure is created for each observation point, where each column represents each environmental variable, and each row represents each observation 
      as_tibble(.) %>% # here, the data structure is converted to a tibble, a modern data frame structure in R that facilitates subsequent operations
      summarize_all(., mean, na.rm = TRUE) # this operation ensures that the mean of each environmental variable is calculated across observations for the said variable, ignoring all NA values
    # In the definition,modelPerformanceAllSpecies[i, 8:ncol(modelPerformanceAllSpecies)] <- ..., the calculated means areassigned to columns 8 onward for each row, i, of the modelPerformanceAllSpecies data frame. The results of these columns are required for evaluating model performance or habitat suitability for species in subsequent analysis
    
    modelPerformanceAllSpecies[i,8:ncol(modelPerformanceAllSpecies)]
    
    # Because the environmental_var_stack_i variable created above is still working with the raster package, there is need to the data to a SpatRaster in order to avoid model fitting and evaluation errors
    environmental_var_stack_i <- terra::rast(environmental_var_stack_i)
    # after converting to a SpatRaster, we need to ensure that our species KDE calculated in th earlier stage of this analysis matches the extent and resolution of our newly created environmental_var_stack_ic SpatRaster object
    # It should be noted that KDE raster captures observation biases
    observationbias_species_i <- observations_kde_group_i %>%
      terra::crop(environmental_var_stack_i) %>%
      terra::mask(environmental_var_stack_i)
    observationbias_species_i
    
    
    # Check if the observation bias raster has any values
    plot(observationbias_species_i)
    
    # If it's empty or not valid, skip further processing for this species
    if (all(is.na(values(observationbias_species_i)))) {
      message("Observation bias is empty for species: ", scientific_name_i, " - Skipping further analysis")
    } else {
      # Plot the observation bias for species
      plot(observationbias_species_i)
    }
  }
  
  
  # Next, is to generate background points based on species range size pixels
  # However, I will first, calculate the number of pixels in the species range, using the global() function from the terra package 
  species_i_rangepixels <- global(((observationbias_species_i[[1]] * 0) + 1), 'sum', na.rm = TRUE)[1, 1]
  # Here, the observationbias_species_i[[1]] * 0 creates a raster with the same structure as observationbias_species_i but filled with zeros
  # + 1 then turns all pixel values to 1, representing each pixel as a unit.
  #global(..., 'sum', na.rm = TRUE) then sums all these pixel values, providing the count of pixels that cover the species' range
  # the [1, 1] extracts this count from the result, storing it in species_i_rangepixels.
  species_i_rangepixels
  
  # Now, determine background points based on species range size pixels
  if(species_i_rangepixels < 100000){
    # Use 20% for small ranges i.e., assign backg_n as 20% of species_i_rangepixels
    backg_n <- round(species_i_rangepixels*0.2, 0)
  }else if(species_i_rangepixels >= 100000 & species_i_rangepixels < 250000){
    # Use 10% for medium ranges i.e., assign backg_n as 10% of species_i_rangepixels
    backg_n <- round(species_i_rangepixels*0.1, 0)
  }else if(species_i_rangepixels >= 250000){
    # Use 5% for big ranges
    backg_n <- round(species_i_rangepixels*0.05, 0) #rounds the calculated value to the nearest integer for easier indexing
  }
  
  backg_n
  
  
  # Sample background points, using bias specific to a target group
  ext <- ext(observationbias_species_i)
  # Generate background points using terra::spatSample()
  backg <- terra::spatSample(observationbias_species_i, 
                             size = backg_n, method = "random", 
                             na.rm = TRUE, xy = TRUE)
  backg
  
  colnames(backg)
  
  backg <- backg[, 1:2]  # Select only the first two columns
  colnames(backg) <- c("lon", "lat")
  
  # Make one sf for background
  backg_sf <- backg %>%
    as.data.frame(.) %>%
    st_as_sf(.,
             coords = c("lon", "lat"),
             crs = raster::crs(environmental_var_stack_i)) %>%
    mutate(p = 0) %>%
    cbind(., st_coordinates(.))
  
  head(backg_sf)
  
  # Make sf object with presence and background points
  species_i_presbackg_sf <- species_i_sf %>%
    st_as_sf(., crs = raster::crs(environmental_var_stack_i)) %>%
    mutate(p = 1) %>%
    cbind(., st_coordinates(.)) %>%
    rbind(backg_sf)
  
  head(species_i_presbackg_sf)
  tail(species_i_presbackg_sf)
  
  # Make sure all points have values for each environmental variable
  species_i_pb_extract <- raster::extract(environmental_var_stack_i,
                                          species_i_presbackg_sf) %>%
    as.data.frame(.)
  
  head(species_i_pb_extract)
  #View(species_i_pb_extract)
  
  names(environmental_var_stack)
  
  # Omit NA row names
  species_i_omit_rownames <- row.names(species_i_pb_extract[ is.na(species_i_pb_extract$temp)|
                                                               is.na(species_i_pb_extract$prec)| 
                                                               is.na(species_i_pb_extract$forest)| 
                                                               is.na(species_i_pb_extract$cropland)| 
                                                               is.na(species_i_pb_extract$urban)| 
                                                               is.na(species_i_pb_extract$grassland)| 
                                                               is.na(species_i_pb_extract$barren)| 
                                                               is.na(species_i_pb_extract$water)| 
                                                               is.na(species_i_pb_extract$elevation)| 
                                                               is.na(species_i_pb_extract$slope)| 
                                                               is.na(species_i_pb_extract$oforest)| 
                                                               is.na(species_i_pb_extract$wetland),]) 
  
  
  # species_i_omit_rownames <- row.names(species_i_pb_extract[
  # is.na(species_i_pb_extract$temp),]) 
  
  
  # Alternative approach to omitting row names
  #species_i_omit_rownames <- rownames(species_i_pb_extract[rowSums(is.na(species_i_pb_extract[
  #c("forest", "water", "wetlands", "annual.croplands", "barren.lands",
  #"nativegrass", "perennial.croplands", "roads", "settlements",
  #"tmax", "tmin", "prec")])) > 0, ])
  
  species_i_omit_rownames
  summary(species_i_omit_rownames)
  
  
  # Define species presence/background data
  species_i_presbackg_sf <- species_i_presbackg_sf %>%
    filter(!(row.names(.) %in% species_i_omit_rownames)) # Remove rows with NA values
  species_i_presbackg_sf
  
  # Run spatial blocking to prepare data for cross-validation
  sb <- spatialBlock(speciesData = species_i_presbackg_sf %>%
                       filter(!(row.names(.) %in% species_i_omit_rownames)),
                     species = "p",  # Presence column (1 for presence, 0 for background)
                     rasterLayer = environmental_var_stack_i,
                     selection = "systematic",
                     rows = 10,
                     cols = 10,  # Define the number of rows and columns in the grid
                     k = 5,  # Number of folds for cross-validation
                     biomod2Format = TRUE)  # Format compatible with biomod2 package
  
  # Add fold ID to dataset
  species_i_presbackg_sf <- species_i_presbackg_sf %>%
    filter(!(row.names(.) %in% species_i_omit_rownames)) %>%
    mutate(fold = sb$foldID)
  
  # Ensure the fold assignments are properly distributed
  table(species_i_presbackg_sf$fold)  # Check the distribution of folds
  
  # Filter out rows with missing values
  species_i_pb_extract <- species_i_pb_extract %>%
    filter(!(row.names(.) %in% species_i_omit_rownames))
  
  # Initialize evaluation metrics
  species_i_eval_summary <- data.frame(fpr = c(), tpr = c(), fold = c())
  species_i_maxent_models <- list()  # List to store Maxent models
  species_i_px <- list()  # List to store habitat suitability predictions
  AUCs_i <- vector(mode = "numeric", length = 5)  # Store AUCs for each fold
  
  # Identify folds with no observations (e.g. large range, but small clustered points like Great Plains Toad)
  species_i_foldvector <- species_i_presbackg_sf %>%
    st_drop_geometry(.) %>%
    filter(p == 1) %>%
    group_by(fold) %>%
    summarize(total = n()) %>%
    pull(fold)
  
  # Loop through folds for cross-validation
  for (k in 1:5) {
    # Split data into training and testing sets based on fold
    trainSet <- which(sb$foldID != k)
    testSet <- which(sb$foldID == k)
    
    # Check if both training and testing sets have data
    if (length(trainSet) == 0 || length(testSet) == 0) {
      cat("No observations in fold", k, ", skipping this fold.\n")
      next  # Skip this fold if no observations are available
    }
    
    
    # Check column names in the species_i_presbackg_sf to verify the existence of the 'ID' column
    cat("Column names in species_i_presbackg_sf:", colnames(species_i_presbackg_sf), "\n")
    
    # Remove the 'ID' column from the environmental and presence-background data
    # If 'ID' column exists in species_i_presbackg_sf, it will be removed
    if("ID" %in% colnames(species_i_presbackg_sf)) {
      species_i_presbackg_sf_no_ID <- species_i_presbackg_sf %>%
        st_drop_geometry() %>%
        dplyr::select(-ID)  # Remove 'ID' column from the spatial data
    } else {
      cat("'ID' column not found in species_i_presbackg_sf\n")
      species_i_presbackg_sf_no_ID <- species_i_presbackg_sf  # Keep the data as is if no 'ID' column
    }
    
    # Remove the 'ID' column from the environmental data (assuming it's named 'ID' in species_i_pb_extract)
    species_i_pb_extract_no_ID <- species_i_pb_extract[, !colnames(species_i_pb_extract) %in% "ID"]
    species_i_pb_extract_no_ID
    
    # Subset the training and testing data
    trainData <- species_i_presbackg_sf[trainSet, ]
    testData <- species_i_presbackg_sf[testSet, ]
    trainEnv <-  species_i_pb_extract_no_ID[trainSet, ]
    testEnv <-  species_i_pb_extract_no_ID[testSet, ]
    
    # Now, fit the MaxEnt model using linear, quadratic, and hinge features
    
    # Fit Maxent model
    species_i_maxent_model <- maxent(
      x = trainEnv,  # Environmental variables for the training set
      p = trainData$p,  # Presence data for the training set
      a = trainData$background,  # Background data for the training set
      args = c("outputformat=cloglog", "betamultiplier=1", "maximumiterations=1000")  # Maxent model arguments
    )
    
    species_i_maxent_model
    
    # Store the fitted Maxent model
    species_i_maxent_models[[k]] <- species_i_maxent_model 
    
    # Get variable importance from the fitted Maxent model
    # Define output directory for variable importance (if not already defined)
    variable_importance_dir <- file.path("F:/WWF_data/SDMs_LC_climate/results/other_mammals/current/variable_importance")
    dir.create(variable_importance_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Extract variable importance for the species
    if (!is.null(species_i_maxent_model)) {
      
      # Extract variable contribution and permutation importance
      variable_importance <- species_i_maxent_model@results
      
      # Optionally: keep only relevant variable importance rows
      var_imp_rows <- grep("contribution$|permutation.importance$", rownames(variable_importance), value = TRUE)
      variable_importance_filtered <- variable_importance[var_imp_rows, , drop = FALSE]
      
      # Print for debugging
      cat("Variable importance for", scientific_name_i, ":\n")
      print(variable_importance_filtered)
      
      # Save as CSV with species name
      variable_importance_path <- file.path(variable_importance_dir, paste0("variable_importance_", scientific_name_i, ".csv"))
      write.csv(variable_importance_filtered, file = variable_importance_path, row.names = TRUE)
      
      cat("Variable importance saved for", scientific_name_i, "\n")
      
    } else {
      cat("No model found for", scientific_name_i, "— skipping variable importance export.\n")
    }
    
    
    # Predict habitat suitability for the test data
    species_i_px[[k]] <- predict(environmental_var_stack_i, 
                                 species_i_maxent_models[[k]], 
                                 ext = ext, 
                                 na.rm = TRUE,
                                 progress = '') # suppresses the display of the progress bar during model prediction (an empty string means no progress bar will be shown).
    
    plot( species_i_px[[k]], main = paste("Final Predicted climatic Habitat Suitability for Species", scientific_name_i))
    
    
    # Define the directory where the prediction files will be saved
    output_dir <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/current"
    
    # Ensure the directory exists
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)  # Create the directory if it doesn't exist
    }
    
    for (k in seq_along(species_i_px)) {
      if (!is.null(species_i_px[[k]])) {
        # Define the file path for each fold
        file_path <- paste0(output_dir, "habitat_suitability_species_", scientific_name_i, "_fold_", k, ".tif")
        
        # Save the raster as a TIFF file
        terra::writeRaster(species_i_px[[k]], filename = file_path, overwrite = TRUE)
        
        # Print confirmation message
        cat("Saved:", file_path, "\n")
      } else {
        cat("No valid prediction for fold", k, "\n")
      }
    }
    
    # Evualuate model
    
    species_predictions_avg <- list()
    species_i_eval <- list()  # Initialize list to store evaluation results
    species_i_eval_summary <- data.frame(
      fpr = numeric(),
      tpr = numeric(),
      mean_tpr = numeric(),
      sd_tpr = numeric(),
      se_tpr = numeric(),
      fold = integer()
    )
    
    # Initialize a vector to store AUC values
    auc_values <- numeric()
    
    
    if(k %in% species_i_foldvector){
      
      # Save evaluation for each fold
      species_i_eval[[k]] <- evaluate(p = species_i_presbackg_sf[testSet,] %>%
                                        st_drop_geometry(.) %>%
                                        filter(p == 1) %>%
                                        dplyr::select(-p, -fold),
                                      # p = presence, a = absence
                                      a = species_i_presbackg_sf[testSet,] %>%
                                        st_drop_geometry(.) %>%
                                        filter(p == 0) %>%
                                        dplyr::select(-p,-fold), 
                                      # model = maxent model
                                      model = species_i_maxent_models[[k]],
                                      # x = environmental variable raster stack
                                      x = environmental_var_stack_i)
      
      # Ensure the evaluation object is valid before proceeding
      if (!is.null(species_i_eval[[k]])) {
        
        # Extract FPR, TPR from model evaluation
        eval_df <- data.frame(
          fpr = species_i_eval[[k]]@FPR,
          tpr = species_i_eval[[k]]@TPR,  # Ensure TPR is included
          mean_tpr = mean(species_i_eval[[k]]@TPR, na.rm = TRUE),  # Compute mean TPR
          sd_tpr = sd(species_i_eval[[k]]@TPR, na.rm = TRUE),      # Compute SD of TPR
          se_tpr = sd(species_i_eval[[k]]@TPR, na.rm = TRUE) / sqrt(length(species_i_eval[[k]]@TPR)),  # Compute SE of TPR
          fold = k
        )
        
        # Step 1: Remove any existing NA rows in species_i_eval_summary
        species_i_eval_summary <- species_i_eval_summary %>%
          filter(!is.na(tpr))
        
        # Step 2: Ensure column names match
        eval_df <- eval_df %>%
          dplyr::select(all_of(colnames(species_i_eval_summary)))  # Keep column order consistent
        
        # Step 3: Append without introducing new NA values
        species_i_eval_summary <- bind_rows(species_i_eval_summary, eval_df)
        
        # Extract AUC value for the current fold and append it to the auc_values vector
        auc_values <- c(auc_values, species_i_eval[[k]]@auc)
        
        # Print number of test observations
        num_test_obs <- species_i_presbackg_sf[testSet, ] %>%
          st_drop_geometry() %>%
          filter(p == 1) %>%
          dplyr::select(-p, -fold) %>%
          nrow()
        
        print(paste("Number of test observations in fold", k, "is", num_test_obs))
        
      } else {
        cat(" Evaluation object is NULL for fold", k, "- Skipping.\n")
      }
    }
    
    
    # Calculate the average AUC for all folds (if auc_values contains any valid values)
    if (length(auc_values) > 0) {
      avg_auc <- mean(auc_values, na.rm = TRUE)
      cat("Average AUC across all folds for", scientific_name_i, ":", avg_auc, "\n")
      
      # Update the mean_auc column in modelPerformanceAllSpecies using the species name
      row_index <- which(modelPerformanceAllSpecies$scientific_name == scientific_name_i)
      
      if (length(row_index) == 1) {
        modelPerformanceAllSpecies$mean_auc[row_index] <- avg_auc
        cat("Average AUC saved for", scientific_name_i, "\n")
      } else {
        cat("Warning: Could not uniquely identify row for species", scientific_name_i, "\n")
      }
    } else {
      cat("No valid AUC values found for species", scientific_name_i, "\n")
    }
    
    
    # Step 1: Keep only non-NULL raster predictions
    valid_indices <- which(!sapply(species_i_px, is.null))
    species_i_px_valid <- species_i_px[valid_indices]
    
    # Step 2: Calculate AUC weights based on the average AUC across all folds
    # Adjust AUC weights using the average AUC value calculated earlier (avg_auc)
    auc_weights_valid <- (avg_auc - 0.5)^2  # Use avg_auc to compute weights
    auc_weights_valid <- rep(auc_weights_valid, length(valid_indices))  # Replicate for each valid prediction
    
    # Step 3: Stack the valid SpatRaster predictions
    species_i_px_stack <- rast(species_i_px_valid)
    
    # Check if there is only one valid prediction layer
    if (nlyr(species_i_px_stack) == 1) {
      species_i_px_avg <- species_i_px_stack
      cat("Only one valid prediction layer — using it as the final habitat suitability surface.\n")
    } else {
      # Calculate the weighted average habitat suitability surface
      species_i_px_avg <- app(species_i_px_stack, fun = function(x) {
        if (all(is.na(x))) {
          return(NA)
        } else {
          return(weighted.mean(x, w = auc_weights_valid, na.rm = TRUE))
        }
      })
      cat("Weighted average prediction surface computed for", scientific_name_i, "\n")
    }
    
    plot(species_i_px_avg, main = paste("Average Habitat Suitability -", scientific_name_i))
    
    
    #species_predictions_avg <- list()
    species_predictions_avg[[scientific_name_i]] <- species_i_px_avg
    names(species_predictions_avg)
    
    
    # Define output path for the average suitability raster
    output_dir1 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/current/average"
    dir.create(output_dir1, recursive = TRUE, showWarnings = FALSE)
    avg_suitability_path <- file.path(output_dir1, paste0("weighted_avg_habitat_suitability_", scientific_name_i, ".tif"))
    
    # Save the weighted average raster (SpatRaster object)
    if (exists("species_i_px_avg")) {
      
      # Save to disk
      terra::writeRaster(species_i_px_avg, filename = avg_suitability_path, overwrite = TRUE)
      cat("Saved weighted average habitat suitability surface for", scientific_name_i, "\n")
      
      # Store in named list
      species_predictions_avg[[scientific_name_i]] <- species_i_px_avg
      
    } else {
      cat("species_i_px_avg not found — skipping raster save for", scientific_name_i, "\n")
    }
    
    
    # --------------Obtaining optimal threshold---------------------------
    
    # ---------- Step 1: Extract thresholds (spec_sens) from each fold safely ----------
    fold_thresholds <- sapply(species_i_eval, function(eval_obj) {
      tryCatch({
        if (!is.null(eval_obj)) {
          dismo::threshold(eval_obj, 'spec_sens')
        } else {
          NA
        }
      }, error = function(e) NA)
    })
    
    # ---------- Step 2: Calculate valid weights ----------
    auc_weights <- (AUCs_i - 0.5)^2
    auc_weights[is.na(auc_weights)] <- 0  # zero out missing AUCs
    
    valid_folds <- which(!is.na(fold_thresholds) & auc_weights > 0)
    cat("Folds contributing to threshold calculation for", scientific_name_i, ":", paste(valid_folds, collapse = ", "), "\n")
    
    valid_thresholds <- fold_thresholds[valid_folds]
    valid_weights <- auc_weights[valid_folds]
    
    # ---------- Step 3: Compute the final threshold ----------
    if (length(valid_thresholds) > 0 && sum(valid_weights, na.rm = TRUE) > 0) {
      species_i_threshold <- weighted.mean(valid_thresholds, w = valid_weights, na.rm = TRUE)
    } else if (length(valid_thresholds) > 0) {
      species_i_threshold <- mean(valid_thresholds, na.rm = TRUE)
      cat("All weights were zero — used unweighted mean instead.\n")
    } else {
      species_i_threshold <- NA
      cat("No valid thresholds found — returning NA.\n")
    }
    
    cat("Final average threshold (spec_sens) for", scientific_name_i, ":", species_i_threshold, "\n")
    
    # ---------- Step 4: Save threshold to performance table ----------
    modelPerformanceAllSpecies$mean_threshold[modelPerformanceAllSpecies$scientific_name == scientific_name_i] <- species_i_threshold
    
    # Define file paths
    species_dir <- file.path("F:/WWF_data/SDMs_LC_climate/results/other_mammals/current/binary", gsub(" ", "_", scientific_name_i))
    dir.create(file.path(species_dir, "binary_mammals"), showWarnings = FALSE, recursive = TRUE)
    
    write.csv(modelPerformanceAllSpecies,
              file = file.path(species_dir, paste0(scientific_name_i, "_performance.csv")),
              row.names = FALSE)
    cat("Model performance table saved as CSV for", scientific_name_i, "\n")
    
    # ---------- Step 5: Create and Save Binary Suitability Raster ----------
    if (!is.na(species_i_threshold)) {
      species_i_binary <- species_i_px_avg > species_i_threshold
      
      # plot
      plot(species_i_binary, main = paste("Binary Suitability for", scientific_name_i))
      
      binary_path <- file.path(species_dir, "binary_mammals", paste0(scientific_name_i, "_mean_pixel_binary.tif"))
      writeRaster(
        species_i_binary,
        filename = binary_path,
        overwrite = TRUE
      )
      
      cat("Binary habitat raster saved successfully for", scientific_name_i, "\n")
    } else {
      cat("Binary raster not created — threshold was NA for", scientific_name_i, "\n")
    }
    
    
  }}





# projecting from 2030 ssp126


# projecting from 2030 ssp126


# A. predicting 2030 alone under full study area to obtain exsitu refugia

environmental_var_stack <- raster::stack(MAT_2030_ssp126, MAP_2030_ssp126,
                                         forest_2030_ssp126, cropland_2030_ssp126,
                                         urban_2030_ssp126, grassland_2030_ssp126,
                                         barren_2030_ssp126, water_2030_ssp126, 
                                         elevation, slope, oforest_2030_ssp126,
                                         wetland_2030_ssp126)


names(environmental_var_stack) <- c("temp", "prec", "forest", "cropland", "urban", 
                                    "grassland", "barren", "water", "elevation", 
                                    "slope", "oforest", "wetland")


# Read study area 
shapefile_path <- "F:/WWF_data/climate_species_analysis/defined_study_area/study_area_fit.shp"
studyarea <- st_read(shapefile_path)  # Path to your study area shapefile

study_area <- studyarea %>%
  # Transform the CRS of the ppr_region to match that of environmental variables(environmental_var_stack)
  st_transform(crs = crs(environmental_var_stack)) %>%
  # Remove the Z dimension, if present
  st_zm() %>%
  # Convert sf (simple feature) to sp (spatial) object
  as_Spatial()

plot(MAT)
plot(study_area, add = TRUE, border = "red", lwd = 2)


##3. Second step is to load species list and observations 

# I start by loading the csv file that contains the list of all species for the prairie region, filter and modify the data, and adds columns for model performance and environmental variables
modelPerformanceAllSpecies <- read.csv("F:/WWF_data/SDMs_LC_climate/other_mammal_summary.csv") %>%
  filter(n_CB > 20) %>% # this code filters and keep rows where the n_SECanada column has  20 observation records or more. 20 records is the minimum threshold required for fitting models for the SE Canada region
  #dplyr::select(-X) %>%
  # the below code adds to two columns with variables mean_auc and mean_threshold, initialized with a NA value in inorder to store future model performance values
  mutate(mean_auc = NA,
         mean_threshold = NA) %>%
  # add blank columns for mean species values of each environmental variable, and filling them with NA values
  cbind(., t(rep(NA,length(names(environmental_var_stack)))))

dim(modelPerformanceAllSpecies)  # checks the number of rows and columns in the species list
head(modelPerformanceAllSpecies)# checks some few rows to see how the data looks like with the columns added
#View(modelPerformanceAllSpecies)

#add environmental variable names to data frame
# the code below retrieves and adds new columns to the modelPerformanceAllSpecies file, with names of each environmental variable in the raster stack, starting from the 8th column to the last column (ncol(modelPerformanceAllSpecies))
names(modelPerformanceAllSpecies)[8:ncol(modelPerformanceAllSpecies)] <- names(environmental_var_stack)

#Now reprint the modelPerformanceAllSpecies data to check if the environmental variables have been successfully added 
head(modelPerformanceAllSpecies)
#View(modelPerformanceAllSpecies)

# Load species observation data 
#mammal_observations <- read.csv("F:/WWF_data/SDMs_LC_climate/mammal_observations_cleaned.csv")
#ape_observations <- read.csv("F:/WWF_data/SDMs_LC_climate/apes.csv")
mammal_observations <- read.csv("F:/WWF_data/SDMs_LC_climate/other_mammal_observations.csv")

# Combine all observations
#species_observations <- rbind(mammal_observations) 
species_observations <- rbind(ape_observations, elephant_observations) 
head(species_observations)

# Species observations are still in dataframe format, hence, a need to convert the data frame to an sf object
species_observations_sf <- st_as_sf(
  species_observations,
  coords = c("longitude", "latitude"),
  crs = 4326,  # WGS84 coordinate reference system
  remove = FALSE  # Keeps the original longitude and latitude columns
)

# Check the structure
print(species_observations_sf)

# Reproject the sf object to the CRS of forest (e.g., a raster layer)
species_observations_transformed <- st_transform(species_observations_sf, crs = st_crs(environmental_var_stack))

# Remove rows where "species" column is NA
species_observations_cleaned <- species_observations_transformed[!is.na(species_observations_transformed$species), ]

# Check the result
head(species_observations_cleaned)
# Plot observation points and visualize on climate maps
plot(environmental_var_stack[[1]], main = "Reprojected & Masked Bio9 Layer")
# Add the species points
plot(species_observations_cleaned, 
     add = TRUE, 
     col = "red", 
     pch = 16)


# Step 2: Remove Z/M dimensions if present (if needed for conversion to Spatial)
species_observations_reproj <- st_zm(species_observations_cleaned)

# Get the extent of environmental_var_stack as an sf polygon
env_extent <- as(extent(environmental_var_stack), "SpatialPolygons")
crs(env_extent) <- crs(environmental_var_stack)
env_extent_sf <- st_as_sf(env_extent)

# Clip points that fall within extent
species_observations_clipped <- st_intersection(species_observations_reproj, env_extent_sf)

# Plot
plot(environmental_var_stack[[1]], main = "Clipped Species Observations ")
plot(st_geometry(species_observations_clipped), 
     add = TRUE, 
     col = "red", 
     pch = 16)


### Use loop to create raster for each taxonomic group.
# In this section, I will start by defining a group vector, by assigning the taxonomic groups that will be iterated in a loop
# Print unique names in the "group" column
unique_groups <- unique(species_observations_clipped$group)# get group names from observation data
print(unique_groups)

group_vector <- c("mammals")
#group_vector <- c("apes", "elephants")
group_vector
# The second stage is to Store target group Sampling raster in a list (named by taxon)
# Here, I will first create an empty list to store raster objects, that will be created as kernel density estimates for each taxonomic group
observations_kde_list <- list()

names(environmental_var_stack)


# Next, is to define resolution and extent based on environmental_mask. This will ensure that all raster layers that will be generated, will follow thesame resolution and spatial extents as those of the stacked raster layers created at the early stage (e.g., forest
# first, create an environmental mask for one of the stacked raster layer (here i use forest), which will be used in setting the desired resolution and extent for all other raster objects as i move forward
environmental_mask <- terra::rast(environmental_var_stack$forest)
# second, I create a target resolution to be used for all raster layers as I proceed, by extracting the resolution of the masked raster layer (forest), using the terra package
target_resolution <- terra::res(environmental_mask)
# third, I also create a target extent that will be used for all raster layers to be developed, by extracting the extent of the masked raster
target_extent <- terra::ext(environmental_mask)

environmental_mask
plot(environmental_mask)
target_resolution
target_extent

for(g in group_vector){ # this function defines a loop over each element in the group_vector defined above (birds, mammals, amphibians and reptiles), where data specific to each group, g, will be processed independently in each iteration in the loop
  # after defining the loop function, the next thing is to ensure that each species observation is filtered to include only records for the group, g, with these records converted to a spatial format for further processing
  group_g_observations <- species_observations_clipped %>%
    filter(group == g) %>%
    slice_sample(n = 100000)  # for each group with filtered records, I randomly sample about 100,000 observations for producing KDE. 100,000 observations seem to be a manageable threshold record for each group in every KDE operation
  
  # Check if there are any observations to process
  if (nrow(group_g_observations) == 0) {
    message("No observations for group: ", g)
    next  # Skip to the next group if no data
  }
  summary(group_g_observations)
  
  # next, for each group observation g, convert the coordinates to simple feature feature (sf) objects using the sf package, so it becomes compatible with spatial feature functions in R
  group_g_observations_sf <- st_as_sf(group_g_observations, coords = c("longitude", "latitude"), crs = 4326) # here, I have converted the coordinates to EPSG:4326, the standard World Geodetic coordinate system 
  
  # the next thing is to project the group_g_observations_sf data to match that of the stacked environmental layers
  group_g_observations_sf <- st_transform(group_g_observations_sf, st_crs(environmental_var_stack))
  
  # I will now calculate KDE (kernel density estimates) for each group observation g, by masking the calculations (raster data) to the target resolution defined in the earlier face of the codes, using the resolution of one of the stacked predictors (forest). I am using the spatialEco package to do this
  kde_result <- spatialEco::sp.kde(
    x = group_g_observations_sf,
    res = target_resolution,   # Set KDE resolution to match environmental_mask
    standardize = TRUE #normalizes the density values to make them comparable across groups.
  )
  # Check the class of the KDE result and resample if needed
  # here, there is a need to verify is the KDE results produced are SpatRaster objects, considering that the package used now is the terra package, which has replaced the previos Rgeos package that has depreciated
  # the code therefore ensures that, If kde_result is not a SpatRaster object, then, the stop() function is executed to halt the operation and display the message "KDE result is not a SpatRaster." This ensures that only valid SpatRaster objects are processed in the next steps.
  if (!inherits(kde_result, "SpatRaster")) {
    stop("KDE result is not a SpatRaster.")
  }
  
  # After calculating the KDE for each species observation, g, there is a need ensure that the results fall within the geographic boundaries of the target extent (forest), define in the earlier stage of this script
  kde_result <- terra::crop(kde_result, environmental_mask)  # Crop to target extent using the crop function of the terra package
  kde_result <- terra::resample(kde_result, environmental_mask, method = "bilinear")  # although the target resolution has been specified in the KDE calculations, it is always a good approach to resample to target resolution again after coping the target extent. Also, the bilinear interpolation in the raster resampling, ensures that new pixel values are calculated based on the weighted average of surrounding pixels, yielding a smoother result
  
  # To ensure that there is no mismatch in spatial extents and resolutions between the calculated KDEs and masked or stacked environmental layers, there is a need to mask the KDE result with the stacked or masked environmental layer (forest)
  observations_kde_list[[g]] <- terra::mask(kde_result, environmental_mask)
}


# Plot KDE results for each group
par(mfrow = c(1, 1))  # Arrange plots in a 2x2 grid

for (g in names(observations_kde_list)) {
  plot(observations_kde_list[[g]], main = paste("KDE for", g))
}


# Set species common name: here, I extract the common name of each species in each row i (within the modelPerformanceAllSpecies csv file), and store it in a new variable called scientific_name_i, for easy reference in the loop iterations
for (i in 1:nrow(modelPerformanceAllSpecies)) {
  # Extract key species attributes
  group_i <- modelPerformanceAllSpecies$group[i]  
  genus_i <- modelPerformanceAllSpecies$genus[i]
  species_i <- modelPerformanceAllSpecies$species[i]
  scientific_name_i <- modelPerformanceAllSpecies$scientific_name[i]
  common_name_i <- modelPerformanceAllSpecies$scientific_name[i]
  
  message("\nProcessing species: ", scientific_name_i, " (", genus_i, " ", species_i, ") in group: ", group_i)
  
  # Ensure KDE raster exists for this group
  if (!group_i %in% names(observations_kde_list)) {
    message("No KDE raster available for group: ", group_i, " - Skipping species: ", scientific_name_i)
    next
  }
  observations_kde_group_i <- observations_kde_list[[group_i]]
  
  # Filter species observations
  species_i_sp <- species_observations_clipped %>%
    filter(scientific_name == scientific_name_i) %>%
    slice_sample(n = 1, by = square_id)  # Avoids spatial autocorrelation issues
  
  if (nrow(species_i_sp) == 0) {
    message("No valid occurrences found for species: ", scientific_name_i, " - Skipping.")
    next
  }
  
  # Convert species occurrence data to spatial format
  species_i_sp <- st_as_sf(species_i_sp, coords = c("longitude", "latitude"), crs = 4326)
  
  # Ensure study_area is an sf object and matches CRS
  study_area <- st_as_sf(study_area)
  
  if (st_crs(species_i_sp) != st_crs(study_area)) {
    message("Transforming CRS of study area to match species observations.")
    study_area <- st_transform(study_area, st_crs(species_i_sp))
  }
  
  # Print key elements for debugging (optional)
  print(observations_kde_group_i)
  
  # Intersect species points with the study area
  species_i_intersects_SE <- st_intersects(species_i_sp, study_area, sparse = FALSE)
  
  # Filter species points by intersection (keep only points inside the study area)
  species_i_sp <- species_i_sp[which(rowSums(species_i_intersects_SE) > 0), ]
  
  # Check if there are any remaining points after filtering
  if (nrow(species_i_sp) == 0) {
    message("No valid intersection found for species: ", scientific_name_i, " - Skipping.")
    modelPerformanceAllSpecies$n_MWCASC[i] <- 0  # Assign zero if no observations remain
    next
  }
  
  # Transform the CRS of the filtered points to match the environmental variable stack
  species_i_sp <- st_transform(species_i_sp, st_crs(environmental_var_stack))
  
  # Extract unique observation points (latitude and longitude), removing duplicate coordinates
  obs_species_i <- unique(st_coordinates(species_i_sp))
  
  # Convert the matrix of coordinates into a data frame
  obs_species_i <- data.frame(longitude = obs_species_i[, 1],
                              latitude = obs_species_i[, 2])
  
  # Check the first few records (optional for debugging)
  head(obs_species_i)
  
  # Save sample size: store the count of unique observation points in modelPerformanceAllSpecies
  modelPerformanceAllSpecies$n_MWCASC[i] <- nrow(obs_species_i)
  
  # Optional: Print progress update
  message("Processed ", nrow(obs_species_i), " unique occurrences for species: ", common_name_i)
  
  # Define output folder and create it if it doesn't exist
  #output_dir_range <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"
  #dir.create(output_dir_range, recursive = TRUE, showWarnings = FALSE)
  
  # Only model species with >= 20 observations
  if(modelPerformanceAllSpecies$n_MWCASC[i] >= 20){# here, i first of all check and select the number of unique observations filtered for the prairie region (n_prairie) in each row of the modelPerformanceAllSpecies data, with records  >= 20; 20 is the minimum threshold)
    # I now assign the obs_species_i data frame (which contains the unique coordinates in each grid id) into a new variable "species_i_ppr_sp", assign lon and lat coordinates to make it a spatial object, and finally project the variable to a similar CRS with the stacked environmental variables
    species_i_sp <- obs_species_i
    coordinates(species_i_sp) <- c("longitude", "latitude")
    species_i_sp@proj4string <- raster::crs(environmental_var_stack)
    
    # Generate a 50 km buffer around each observation point
    species_i_buffers <- spTransform(species_i_sp, CRS = raster::crs(environmental_var_stack)) %>%
      raster::buffer(width = 50000)  # 5 km buffer around each observation
    
    # Convert the buffer to an sf object for easier handling
    species_i_buffers_sf <- st_as_sf(species_i_buffers)
    
    # Dissolve and merge all buffers
    species_i_buffers_dissolved <- species_i_buffers_sf %>%
      st_union()  # Dissolve all buffers into a single polygon
    
    # Define species name and file path
    species_names <- modelPerformanceAllSpecies$species[i]
    shapefile_path <- file.path(output_dir_range, paste0(gsub(" ", "_", species_names), "_range.shp"))
    
    # Save the shapefile
    st_write(species_i_buffers_dissolved, shapefile_path, delete_layer = TRUE)
    
    # Plot the dissolved buffer (clipped to the study area) and the study area
    #par(mfrow = c(1, 1))  # Set up a single panel for plotting
    #plot(st_geometry(species_i_buffers_dissolved), main = "Clipped Dissolved 5 km Buffer for Fowler's Toad Observations", col = "lightblue")
    #plot(st_geometry(clipped_study_area_proj), add = TRUE, border = "black")  # Overlay study area
    
    
    ggplot() +
      geom_sf(data = study_area, fill = "gray95", color = "black") +
      geom_sf(data = st_as_sf(species_i_buffers_dissolved), color = "darkgreen", size = 2, alpha = 0.7) +
      theme_minimal() +
      labs(title = "Species Observations",
           subtitle = "Species Observations within the Study Area",
           caption = "Species observation points have been transformed to match CRS")
    
    
    # Now Calculate species range size
    species_i_rangesize <- species_i_buffers_dissolved %>% 
      st_intersection(study_area) %>%  # Intersects the species buffer with the clipped study area to remove parts outside
      st_area() %>%  # Calculates the total area of the intersected buffers
      as.numeric() %>%  # Converts the calculated area to numeric for ease of further analysis
      sum()  # Sums up areas of multiple polygons to get the total range size
    species_i_rangesize
    
    # Estimate study area size
    study_area_size <- st_area(study_area) %>% as.numeric()
    study_area_size 
    
    # Ensure the buffer has the same CRS as the environmental variable stack
    species_i_buffers_dissolved <- st_transform(species_i_buffers_dissolved, crs = st_crs(environmental_var_stack))
    
    # Convert the dissolved buffer to an sp object
    species_i_buffers_dissolved_sp <- as(species_i_buffers_dissolved, "Spatial")
    
    # Now apply the crop and mask functions
    if(species_i_rangesize >= 0.9 * 2.018284e+12){ # Use full study area if range size is >= 90% of study area
      environmental_var_stack_i <- environmental_var_stack  # Use the entire study area
    } else {
      environmental_var_stack_i <- environmental_var_stack %>%
        raster::crop(., species_i_buffers_dissolved_sp) %>%  # Crop environmental data to species' range
        raster::mask(., species_i_buffers_dissolved_sp)  # Mask environmental data to species' range
    }
    
    
    # Plot to visualize the environmental variable stack and buffer
    #plot(environmental_var_stack_i$temp)
    #plot(species_i_buffers_dissolved, add = TRUE, lty = 5)  # Overlay species buffer on top of the environmental data
    
    # Use the full study area, regardless of the species' range size
    environmental_var_stack_i <- environmental_var_stack  # Use the entire study area
    
    # Convert species_i_sp to an sf object if it is a SpatialPointsDataFrame
    species_i_sf <- st_as_sf(species_i_sp)
    
    # Next, is to extract values of environmental_var_stack_i for all obs_species_i
    modelPerformanceAllSpecies[i,8:ncol(modelPerformanceAllSpecies)] <- raster::extract(environmental_var_stack_i,
                                                                                        species_i_sf) %>%
      # first, this code ensures that the raster values of each environmental variable (within the stacked raster) is extracted for every given location of a species, within the species observation variable, "species_i_ppr_sp".
      # through this approach, a data structure is created for each observation point, where each column represents each environmental variable, and each row represents each observation 
      as_tibble(.) %>% # here, the data structure is converted to a tibble, a modern data frame structure in R that facilitates subsequent operations
      summarize_all(., mean, na.rm = TRUE) # this operation ensures that the mean of each environmental variable is calculated across observations for the said variable, ignoring all NA values
    # In the definition,modelPerformanceAllSpecies[i, 8:ncol(modelPerformanceAllSpecies)] <- ..., the calculated means areassigned to columns 8 onward for each row, i, of the modelPerformanceAllSpecies data frame. The results of these columns are required for evaluating model performance or habitat suitability for species in subsequent analysis
    
    modelPerformanceAllSpecies[i,8:ncol(modelPerformanceAllSpecies)]
    
    # Because the environmental_var_stack_i variable created above is still working with the raster package, there is need to the data to a SpatRaster in order to avoid model fitting and evaluation errors
    environmental_var_stack_i <- terra::rast(environmental_var_stack_i)
    # after converting to a SpatRaster, we need to ensure that our species KDE calculated in th earlier stage of this analysis matches the extent and resolution of our newly created environmental_var_stack_ic SpatRaster object
    # It should be noted that KDE raster captures observation biases
    observationbias_species_i <- observations_kde_group_i %>%
      terra::crop(environmental_var_stack_i) %>%
      terra::mask(environmental_var_stack_i)
    observationbias_species_i
    
    
    # Check if the observation bias raster has any values
    plot(observationbias_species_i)
    
    # If it's empty or not valid, skip further processing for this species
    if (all(is.na(values(observationbias_species_i)))) {
      message("Observation bias is empty for species: ", scientific_name_i, " - Skipping further analysis")
    } else {
      # Plot the observation bias for species
      plot(observationbias_species_i)
    }
  }
  
  
  # Next, is to generate background points based on species range size pixels
  # However, I will first, calculate the number of pixels in the species range, using the global() function from the terra package 
  species_i_rangepixels <- global(((observationbias_species_i[[1]] * 0) + 1), 'sum', na.rm = TRUE)[1, 1]
  # Here, the observationbias_species_i[[1]] * 0 creates a raster with the same structure as observationbias_species_i but filled with zeros
  # + 1 then turns all pixel values to 1, representing each pixel as a unit.
  #global(..., 'sum', na.rm = TRUE) then sums all these pixel values, providing the count of pixels that cover the species' range
  # the [1, 1] extracts this count from the result, storing it in species_i_rangepixels.
  species_i_rangepixels
  
  # Now, determine background points based on species range size pixels
  if(species_i_rangepixels < 100000){
    # Use 20% for small ranges i.e., assign backg_n as 20% of species_i_rangepixels
    backg_n <- round(species_i_rangepixels*0.2, 0)
  }else if(species_i_rangepixels >= 100000 & species_i_rangepixels < 250000){
    # Use 10% for medium ranges i.e., assign backg_n as 10% of species_i_rangepixels
    backg_n <- round(species_i_rangepixels*0.1, 0)
  }else if(species_i_rangepixels >= 250000){
    # Use 5% for big ranges
    backg_n <- round(species_i_rangepixels*0.05, 0) #rounds the calculated value to the nearest integer for easier indexing
  }
  
  backg_n
  
  
  # Sample background points, using bias specific to a target group
  ext <- ext(observationbias_species_i)
  # Generate background points using terra::spatSample()
  backg <- terra::spatSample(observationbias_species_i, 
                             size = backg_n, method = "random", 
                             na.rm = TRUE, xy = TRUE)
  backg
  
  colnames(backg)
  
  backg <- backg[, 1:2]  # Select only the first two columns
  colnames(backg) <- c("lon", "lat")
  
  # Make one sf for background
  backg_sf <- backg %>%
    as.data.frame(.) %>%
    st_as_sf(.,
             coords = c("lon", "lat"),
             crs = raster::crs(environmental_var_stack_i)) %>%
    mutate(p = 0) %>%
    cbind(., st_coordinates(.))
  
  head(backg_sf)
  
  # Make sf object with presence and background points
  species_i_presbackg_sf <- species_i_sf %>%
    st_as_sf(., crs = raster::crs(environmental_var_stack_i)) %>%
    mutate(p = 1) %>%
    cbind(., st_coordinates(.)) %>%
    rbind(backg_sf)
  
  head(species_i_presbackg_sf)
  tail(species_i_presbackg_sf)
  
  # Make sure all points have values for each environmental variable
  species_i_pb_extract <- raster::extract(environmental_var_stack_i,
                                          species_i_presbackg_sf) %>%
    as.data.frame(.)
  
  head(species_i_pb_extract)
  #View(species_i_pb_extract)
  
  names(environmental_var_stack)
  
  # Omit NA row names
  species_i_omit_rownames <- row.names(species_i_pb_extract[ is.na(species_i_pb_extract$temp)|
                                                               is.na(species_i_pb_extract$prec)| 
                                                               is.na(species_i_pb_extract$forest)| 
                                                               is.na(species_i_pb_extract$cropland)| 
                                                               is.na(species_i_pb_extract$urban)| 
                                                               is.na(species_i_pb_extract$grassland)| 
                                                               is.na(species_i_pb_extract$barren)| 
                                                               is.na(species_i_pb_extract$water)| 
                                                               is.na(species_i_pb_extract$elevation)| 
                                                               is.na(species_i_pb_extract$slope)| 
                                                               is.na(species_i_pb_extract$oforest)| 
                                                               is.na(species_i_pb_extract$wetland),]) 
  
  
  # species_i_omit_rownames <- row.names(species_i_pb_extract[
  # is.na(species_i_pb_extract$temp),]) 
  
  
  # Alternative approach to omitting row names
  #species_i_omit_rownames <- rownames(species_i_pb_extract[rowSums(is.na(species_i_pb_extract[
  #c("forest", "water", "wetlands", "annual.croplands", "barren.lands",
  #"nativegrass", "perennial.croplands", "roads", "settlements",
  #"tmax", "tmin", "prec")])) > 0, ])
  
  species_i_omit_rownames
  summary(species_i_omit_rownames)
  
  
  # Define species presence/background data
  species_i_presbackg_sf <- species_i_presbackg_sf %>%
    filter(!(row.names(.) %in% species_i_omit_rownames)) # Remove rows with NA values
  species_i_presbackg_sf
  
  # Run spatial blocking to prepare data for cross-validation
  sb <- spatialBlock(speciesData = species_i_presbackg_sf %>%
                       filter(!(row.names(.) %in% species_i_omit_rownames)),
                     species = "p",  # Presence column (1 for presence, 0 for background)
                     rasterLayer = environmental_var_stack_i,
                     selection = "systematic",
                     rows = 10,
                     cols = 10,  # Define the number of rows and columns in the grid
                     k = 5,  # Number of folds for cross-validation
                     biomod2Format = TRUE)  # Format compatible with biomod2 package
  
  # Add fold ID to dataset
  species_i_presbackg_sf <- species_i_presbackg_sf %>%
    filter(!(row.names(.) %in% species_i_omit_rownames)) %>%
    mutate(fold = sb$foldID)
  
  # Ensure the fold assignments are properly distributed
  table(species_i_presbackg_sf$fold)  # Check the distribution of folds
  
  # Filter out rows with missing values
  species_i_pb_extract <- species_i_pb_extract %>%
    filter(!(row.names(.) %in% species_i_omit_rownames))
  
  # Initialize evaluation metrics
  species_i_eval_summary <- data.frame(fpr = c(), tpr = c(), fold = c())
  species_i_maxent_models <- list()  # List to store Maxent models
  species_i_px <- list()  # List to store habitat suitability predictions
  AUCs_i <- vector(mode = "numeric", length = 5)  # Store AUCs for each fold
  
  # Identify folds with no observations (e.g. large range, but small clustered points like Great Plains Toad)
  species_i_foldvector <- species_i_presbackg_sf %>%
    st_drop_geometry(.) %>%
    filter(p == 1) %>%
    group_by(fold) %>%
    summarize(total = n()) %>%
    pull(fold)
  
  # Loop through folds for cross-validation
  for (k in 1:5) {
    # Split data into training and testing sets based on fold
    trainSet <- which(sb$foldID != k)
    testSet <- which(sb$foldID == k)
    
    # Check if both training and testing sets have data
    if (length(trainSet) == 0 || length(testSet) == 0) {
      cat("No observations in fold", k, ", skipping this fold.\n")
      next  # Skip this fold if no observations are available
    }
    
    
    # Check column names in the species_i_presbackg_sf to verify the existence of the 'ID' column
    cat("Column names in species_i_presbackg_sf:", colnames(species_i_presbackg_sf), "\n")
    
    # Remove the 'ID' column from the environmental and presence-background data
    # If 'ID' column exists in species_i_presbackg_sf, it will be removed
    if("ID" %in% colnames(species_i_presbackg_sf)) {
      species_i_presbackg_sf_no_ID <- species_i_presbackg_sf %>%
        st_drop_geometry() %>%
        dplyr::select(-ID)  # Remove 'ID' column from the spatial data
    } else {
      cat("'ID' column not found in species_i_presbackg_sf\n")
      species_i_presbackg_sf_no_ID <- species_i_presbackg_sf  # Keep the data as is if no 'ID' column
    }
    
    # Remove the 'ID' column from the environmental data (assuming it's named 'ID' in species_i_pb_extract)
    species_i_pb_extract_no_ID <- species_i_pb_extract[, !colnames(species_i_pb_extract) %in% "ID"]
    species_i_pb_extract_no_ID
    
    # Subset the training and testing data
    trainData <- species_i_presbackg_sf[trainSet, ]
    testData <- species_i_presbackg_sf[testSet, ]
    trainEnv <-  species_i_pb_extract_no_ID[trainSet, ]
    testEnv <-  species_i_pb_extract_no_ID[testSet, ]
    
    # Now, fit the MaxEnt model using linear, quadratic, and hinge features
    
    # Fit Maxent model
    species_i_maxent_model <- maxent(
      x = trainEnv,  # Environmental variables for the training set
      p = trainData$p,  # Presence data for the training set
      a = trainData$background,  # Background data for the training set
      args = c("outputformat=cloglog", "betamultiplier=1", "maximumiterations=1000")  # Maxent model arguments
    )
    
    species_i_maxent_model
    
    # Store the fitted Maxent model
    species_i_maxent_models[[k]] <- species_i_maxent_model 
    
    # Get variable importance from the fitted Maxent model
    # Define output directory for variable importance (if not already defined)
    variable_importance_dir <- file.path("F:/WWF_data/SDMs_LC_climate/results/other_mammals/from_2030/ssp126/variable_importance")
    dir.create(variable_importance_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Extract variable importance for the species
    if (!is.null(species_i_maxent_model)) {
      
      # Extract variable contribution and permutation importance
      variable_importance <- species_i_maxent_model@results
      
      # Optionally: keep only relevant variable importance rows
      var_imp_rows <- grep("contribution$|permutation.importance$", rownames(variable_importance), value = TRUE)
      variable_importance_filtered <- variable_importance[var_imp_rows, , drop = FALSE]
      
      # Print for debugging
      cat("Variable importance for", scientific_name_i, ":\n")
      print(variable_importance_filtered)
      
      # Save as CSV with species name
      variable_importance_path <- file.path(variable_importance_dir, paste0("variable_importance_", scientific_name_i, ".csv"))
      write.csv(variable_importance_filtered, file = variable_importance_path, row.names = TRUE)
      
      cat("Variable importance saved for", scientific_name_i, "\n")
      
    } else {
      cat("No model found for", scientific_name_i, "— skipping variable importance export.\n")
    }
    
    
    # Predict habitat suitability for the test data
    species_i_px[[k]] <- predict(environmental_var_stack_i, 
                                 species_i_maxent_models[[k]], 
                                 ext = ext, 
                                 na.rm = TRUE,
                                 progress = '') # suppresses the display of the progress bar during model prediction (an empty string means no progress bar will be shown).
    
    plot( species_i_px[[k]], main = paste("Final Predicted climatic Habitat Suitability for Species", scientific_name_i))
    
    
    # Define the directory where the prediction files will be saved
    output_dir <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/from_2030/ssp126"
    
    # Ensure the directory exists
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)  # Create the directory if it doesn't exist
    }
    
    for (k in seq_along(species_i_px)) {
      if (!is.null(species_i_px[[k]])) {
        # Define the file path for each fold
        file_path <- paste0(output_dir, "habitat_suitability_species_", scientific_name_i, "_fold_", k, ".tif")
        
        # Save the raster as a TIFF file
        terra::writeRaster(species_i_px[[k]], filename = file_path, overwrite = TRUE)
        
        # Print confirmation message
        cat("Saved:", file_path, "\n")
      } else {
        cat("No valid prediction for fold", k, "\n")
      }
    }
    
    # Evualuate model
    
    species_predictions_avg <- list()
    species_i_eval <- list()  # Initialize list to store evaluation results
    species_i_eval_summary <- data.frame(
      fpr = numeric(),
      tpr = numeric(),
      mean_tpr = numeric(),
      sd_tpr = numeric(),
      se_tpr = numeric(),
      fold = integer()
    )
    
    # Initialize a vector to store AUC values
    auc_values <- numeric()
    
    
    if(k %in% species_i_foldvector){
      
      # Save evaluation for each fold
      species_i_eval[[k]] <- evaluate(p = species_i_presbackg_sf[testSet,] %>%
                                        st_drop_geometry(.) %>%
                                        filter(p == 1) %>%
                                        dplyr::select(-p, -fold),
                                      # p = presence, a = absence
                                      a = species_i_presbackg_sf[testSet,] %>%
                                        st_drop_geometry(.) %>%
                                        filter(p == 0) %>%
                                        dplyr::select(-p,-fold), 
                                      # model = maxent model
                                      model = species_i_maxent_models[[k]],
                                      # x = environmental variable raster stack
                                      x = environmental_var_stack_i)
      
      # Ensure the evaluation object is valid before proceeding
      if (!is.null(species_i_eval[[k]])) {
        
        # Extract FPR, TPR from model evaluation
        eval_df <- data.frame(
          fpr = species_i_eval[[k]]@FPR,
          tpr = species_i_eval[[k]]@TPR,  # Ensure TPR is included
          mean_tpr = mean(species_i_eval[[k]]@TPR, na.rm = TRUE),  # Compute mean TPR
          sd_tpr = sd(species_i_eval[[k]]@TPR, na.rm = TRUE),      # Compute SD of TPR
          se_tpr = sd(species_i_eval[[k]]@TPR, na.rm = TRUE) / sqrt(length(species_i_eval[[k]]@TPR)),  # Compute SE of TPR
          fold = k
        )
        
        # Step 1: Remove any existing NA rows in species_i_eval_summary
        species_i_eval_summary <- species_i_eval_summary %>%
          filter(!is.na(tpr))
        
        # Step 2: Ensure column names match
        eval_df <- eval_df %>%
          dplyr::select(all_of(colnames(species_i_eval_summary)))  # Keep column order consistent
        
        # Step 3: Append without introducing new NA values
        species_i_eval_summary <- bind_rows(species_i_eval_summary, eval_df)
        
        # Extract AUC value for the current fold and append it to the auc_values vector
        auc_values <- c(auc_values, species_i_eval[[k]]@auc)
        
        # Print number of test observations
        num_test_obs <- species_i_presbackg_sf[testSet, ] %>%
          st_drop_geometry() %>%
          filter(p == 1) %>%
          dplyr::select(-p, -fold) %>%
          nrow()
        
        print(paste("Number of test observations in fold", k, "is", num_test_obs))
        
      } else {
        cat(" Evaluation object is NULL for fold", k, "- Skipping.\n")
      }
    }
    
    
    # Calculate the average AUC for all folds (if auc_values contains any valid values)
    if (length(auc_values) > 0) {
      avg_auc <- mean(auc_values, na.rm = TRUE)
      cat("Average AUC across all folds for", scientific_name_i, ":", avg_auc, "\n")
      
      # Update the mean_auc column in modelPerformanceAllSpecies using the species name
      row_index <- which(modelPerformanceAllSpecies$scientific_name == scientific_name_i)
      
      if (length(row_index) == 1) {
        modelPerformanceAllSpecies$mean_auc[row_index] <- avg_auc
        cat("Average AUC saved for", scientific_name_i, "\n")
      } else {
        cat("Warning: Could not uniquely identify row for species", scientific_name_i, "\n")
      }
    } else {
      cat("No valid AUC values found for species", scientific_name_i, "\n")
    }
    
    
    # Step 1: Keep only non-NULL raster predictions
    valid_indices <- which(!sapply(species_i_px, is.null))
    species_i_px_valid <- species_i_px[valid_indices]
    
    # Step 2: Calculate AUC weights based on the average AUC across all folds
    # Adjust AUC weights using the average AUC value calculated earlier (avg_auc)
    auc_weights_valid <- (avg_auc - 0.5)^2  # Use avg_auc to compute weights
    auc_weights_valid <- rep(auc_weights_valid, length(valid_indices))  # Replicate for each valid prediction
    
    # Step 3: Stack the valid SpatRaster predictions
    species_i_px_stack <- rast(species_i_px_valid)
    
    # Check if there is only one valid prediction layer
    if (nlyr(species_i_px_stack) == 1) {
      species_i_px_avg <- species_i_px_stack
      cat("Only one valid prediction layer — using it as the final habitat suitability surface.\n")
    } else {
      # Calculate the weighted average habitat suitability surface
      species_i_px_avg <- app(species_i_px_stack, fun = function(x) {
        if (all(is.na(x))) {
          return(NA)
        } else {
          return(weighted.mean(x, w = auc_weights_valid, na.rm = TRUE))
        }
      })
      cat("Weighted average prediction surface computed for", scientific_name_i, "\n")
    }
    
    plot(species_i_px_avg, main = paste("Average Habitat Suitability -", scientific_name_i))
    
    
    #species_predictions_avg <- list()
    species_predictions_avg[[scientific_name_i]] <- species_i_px_avg
    names(species_predictions_avg)
    
    
    # Define output path for the average suitability raster
    output_dir1 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/from_2030/sspp126/average"
    dir.create(output_dir1, recursive = TRUE, showWarnings = FALSE)
    avg_suitability_path <- file.path(output_dir1, paste0("weighted_avg_habitat_suitability_", scientific_name_i, ".tif"))
    
    # Save the weighted average raster (SpatRaster object)
    if (exists("species_i_px_avg")) {
      
      # Save to disk
      terra::writeRaster(species_i_px_avg, filename = avg_suitability_path, overwrite = TRUE)
      cat("Saved weighted average habitat suitability surface for", scientific_name_i, "\n")
      
      # Store in named list
      species_predictions_avg[[scientific_name_i]] <- species_i_px_avg
      
    } else {
      cat("species_i_px_avg not found — skipping raster save for", scientific_name_i, "\n")
    }
    
    
    # --------------Obtaining optimal threshold---------------------------
    
    # ---------- Step 1: Extract thresholds (spec_sens) from each fold safely ----------
    fold_thresholds <- sapply(species_i_eval, function(eval_obj) {
      tryCatch({
        if (!is.null(eval_obj)) {
          dismo::threshold(eval_obj, 'spec_sens')
        } else {
          NA
        }
      }, error = function(e) NA)
    })
    
    # ---------- Step 2: Calculate valid weights ----------
    auc_weights <- (AUCs_i - 0.5)^2
    auc_weights[is.na(auc_weights)] <- 0  # zero out missing AUCs
    
    valid_folds <- which(!is.na(fold_thresholds) & auc_weights > 0)
    cat("Folds contributing to threshold calculation for", scientific_name_i, ":", paste(valid_folds, collapse = ", "), "\n")
    
    valid_thresholds <- fold_thresholds[valid_folds]
    valid_weights <- auc_weights[valid_folds]
    
    # ---------- Step 3: Compute the final threshold ----------
    if (length(valid_thresholds) > 0 && sum(valid_weights, na.rm = TRUE) > 0) {
      species_i_threshold <- weighted.mean(valid_thresholds, w = valid_weights, na.rm = TRUE)
    } else if (length(valid_thresholds) > 0) {
      species_i_threshold <- mean(valid_thresholds, na.rm = TRUE)
      cat("All weights were zero — used unweighted mean instead.\n")
    } else {
      species_i_threshold <- NA
      cat("No valid thresholds found — returning NA.\n")
    }
    
    cat("Final average threshold (spec_sens) for", scientific_name_i, ":", species_i_threshold, "\n")
    
    # ---------- Step 4: Save threshold to performance table ----------
    modelPerformanceAllSpecies$mean_threshold[modelPerformanceAllSpecies$scientific_name == scientific_name_i] <- species_i_threshold
    
    # Define file paths
    species_dir <- file.path("F:/WWF_data/SDMs_LC_climate/results/other_mammals/from_2030/sspp126/binary", gsub(" ", "_", scientific_name_i))
    dir.create(file.path(species_dir, "binary_mammals"), showWarnings = FALSE, recursive = TRUE)
    
    write.csv(modelPerformanceAllSpecies,
              file = file.path(species_dir, paste0(scientific_name_i, "_performance.csv")),
              row.names = FALSE)
    cat("Model performance table saved as CSV for", scientific_name_i, "\n")
    
    # ---------- Step 5: Create and Save Binary Suitability Raster ----------
    if (!is.na(species_i_threshold)) {
      species_i_binary <- species_i_px_avg > species_i_threshold
      
      # plot
      plot(species_i_binary, main = paste("Binary Suitability for", scientific_name_i))
      
      binary_path <- file.path(species_dir, "binary_mammals", paste0(scientific_name_i, "_mean_pixel_binary.tif"))
      writeRaster(
        species_i_binary,
        filename = binary_path,
        overwrite = TRUE
      )
      
      cat("Binary habitat raster saved successfully for", scientific_name_i, "\n")
    } else {
      cat("Binary raster not created — threshold was NA for", scientific_name_i, "\n")
    }
    
  }}



# B projecting from  2030 to 2100

environmental_var_stack <- raster::stack(MAT_2030_ssp126, MAP_2030_ssp126,
                                         forest_2030_ssp126, cropland_2030_ssp126,
                                         urban_2030_ssp126, grassland_2030_ssp126,
                                         barren_2030_ssp126, water_2030_ssp126, 
                                         elevation, slope, oforest_2030_ssp126,
                                         wetland_2030_ssp126)


names(environmental_var_stack) <- c("temp", "prec", "forest", "cropland", "urban", 
                                    "grassland", "barren", "water", "elevation", 
                                    "slope", "oforest", "wetland")


# Read study area 
shapefile_path <- "F:/WWF_data/climate_species_analysis/defined_study_area/study_area_fit.shp"
studyarea <- st_read(shapefile_path)  # Path to your study area shapefile

study_area <- studyarea %>%
  # Transform the CRS of the ppr_region to match that of environmental variables(environmental_var_stack)
  st_transform(crs = crs(environmental_var_stack)) %>%
  # Remove the Z dimension, if present
  st_zm() %>%
  # Convert sf (simple feature) to sp (spatial) object
  as_Spatial()

plot(MAT)
plot(study_area, add = TRUE, border = "red", lwd = 2)


##3. Second step is to load species list and observations 

# I start by loading the csv file that contains the list of all species for the prairie region, filter and modify the data, and adds columns for model performance and environmental variables
modelPerformanceAllSpecies <- read.csv("F:/WWF_data/SDMs_LC_climate/other_mammal_summary.csv") %>%
  filter(n_CB > 20) %>% # this code filters and keep rows where the n_SECanada column has  20 observation records or more. 20 records is the minimum threshold required for fitting models for the SE Canada region
  #dplyr::select(-X) %>%
  # the below code adds to two columns with variables mean_auc and mean_threshold, initialized with a NA value in inorder to store future model performance values
  mutate(mean_auc = NA,
         mean_threshold = NA) %>%
  # add blank columns for mean species values of each environmental variable, and filling them with NA values
  cbind(., t(rep(NA,length(names(environmental_var_stack)))))

dim(modelPerformanceAllSpecies)  # checks the number of rows and columns in the species list
head(modelPerformanceAllSpecies)# checks some few rows to see how the data looks like with the columns added
#View(modelPerformanceAllSpecies)

#add environmental variable names to data frame
# the code below retrieves and adds new columns to the modelPerformanceAllSpecies file, with names of each environmental variable in the raster stack, starting from the 8th column to the last column (ncol(modelPerformanceAllSpecies))
names(modelPerformanceAllSpecies)[8:ncol(modelPerformanceAllSpecies)] <- names(environmental_var_stack)

#Now reprint the modelPerformanceAllSpecies data to check if the environmental variables have been successfully added 
head(modelPerformanceAllSpecies)
#View(modelPerformanceAllSpecies)

# Load species observation data 
#mammal_observations <- read.csv("F:/WWF_data/SDMs_LC_climate/mammal_observations_cleaned.csv")
#ape_observations <- read.csv("F:/WWF_data/SDMs_LC_climate/apes.csv")
mammal_observations <- read.csv("F:/WWF_data/SDMs_LC_climate/other_mammal_observations.csv")

# Combine all observations
species_observations <- rbind(mammal_observations) 
#species_observations <- rbind(ape_observations, elephant_observations) 
head(species_observations)

# Species observations are still in dataframe format, hence, a need to convert the data frame to an sf object
species_observations_sf <- st_as_sf(
  species_observations,
  coords = c("longitude", "latitude"),
  crs = 4326,  # WGS84 coordinate reference system
  remove = FALSE  # Keeps the original longitude and latitude columns
)

# Check the structure
print(species_observations_sf)

# Reproject the sf object to the CRS of forest (e.g., a raster layer)
species_observations_transformed <- st_transform(species_observations_sf, crs = st_crs(environmental_var_stack))

# Remove rows where "species" column is NA
species_observations_cleaned <- species_observations_transformed[!is.na(species_observations_transformed$species), ]

# Check the result
head(species_observations_cleaned)
# Plot observation points and visualize on climate maps
plot(environmental_var_stack[[1]], main = "Reprojected & Masked Bio9 Layer")
# Add the species points
plot(species_observations_cleaned, 
     add = TRUE, 
     col = "red", 
     pch = 16)


# Step 2: Remove Z/M dimensions if present (if needed for conversion to Spatial)
species_observations_reproj <- st_zm(species_observations_cleaned)

# Get the extent of environmental_var_stack as an sf polygon
env_extent <- as(extent(environmental_var_stack), "SpatialPolygons")
crs(env_extent) <- crs(environmental_var_stack)
env_extent_sf <- st_as_sf(env_extent)

# Clip points that fall within extent
species_observations_clipped <- st_intersection(species_observations_reproj, env_extent_sf)

# Plot
plot(environmental_var_stack[[1]], main = "Clipped Species Observations ")
plot(st_geometry(species_observations_clipped), 
     add = TRUE, 
     col = "red", 
     pch = 16)


### Use loop to create raster for each taxonomic group.
# In this section, I will start by defining a group vector, by assigning the taxonomic groups that will be iterated in a loop
# Print unique names in the "group" column
unique_groups <- unique(species_observations_clipped$group)# get group names from observation data
print(unique_groups)

group_vector <- c("mammals")
#group_vector <- c("apes", "elephants")
#group_vector
# The second stage is to Store target group Sampling raster in a list (named by taxon)
# Here, I will first create an empty list to store raster objects, that will be created as kernel density estimates for each taxonomic group
observations_kde_list <- list()

names(environmental_var_stack)


# Next, is to define resolution and extent based on environmental_mask. This will ensure that all raster layers that will be generated, will follow thesame resolution and spatial extents as those of the stacked raster layers created at the early stage (e.g., forest
# first, create an environmental mask for one of the stacked raster layer (here i use forest), which will be used in setting the desired resolution and extent for all other raster objects as i move forward
environmental_mask <- terra::rast(environmental_var_stack$forest)
# second, I create a target resolution to be used for all raster layers as I proceed, by extracting the resolution of the masked raster layer (forest), using the terra package
target_resolution <- terra::res(environmental_mask)
# third, I also create a target extent that will be used for all raster layers to be developed, by extracting the extent of the masked raster
target_extent <- terra::ext(environmental_mask)

environmental_mask
plot(environmental_mask)
target_resolution
target_extent

for(g in group_vector){ # this function defines a loop over each element in the group_vector defined above (birds, mammals, amphibians and reptiles), where data specific to each group, g, will be processed independently in each iteration in the loop
  # after defining the loop function, the next thing is to ensure that each species observation is filtered to include only records for the group, g, with these records converted to a spatial format for further processing
  group_g_observations <- species_observations_clipped %>%
    filter(group == g) %>%
    slice_sample(n = 100000)  # for each group with filtered records, I randomly sample about 100,000 observations for producing KDE. 100,000 observations seem to be a manageable threshold record for each group in every KDE operation
  
  # Check if there are any observations to process
  if (nrow(group_g_observations) == 0) {
    message("No observations for group: ", g)
    next  # Skip to the next group if no data
  }
  summary(group_g_observations)
  
  # next, for each group observation g, convert the coordinates to simple feature feature (sf) objects using the sf package, so it becomes compatible with spatial feature functions in R
  group_g_observations_sf <- st_as_sf(group_g_observations, coords = c("longitude", "latitude"), crs = 4326) # here, I have converted the coordinates to EPSG:4326, the standard World Geodetic coordinate system 
  
  # the next thing is to project the group_g_observations_sf data to match that of the stacked environmental layers
  group_g_observations_sf <- st_transform(group_g_observations_sf, st_crs(environmental_var_stack))
  
  # I will now calculate KDE (kernel density estimates) for each group observation g, by masking the calculations (raster data) to the target resolution defined in the earlier face of the codes, using the resolution of one of the stacked predictors (forest). I am using the spatialEco package to do this
  kde_result <- spatialEco::sp.kde(
    x = group_g_observations_sf,
    res = target_resolution,   # Set KDE resolution to match environmental_mask
    standardize = TRUE #normalizes the density values to make them comparable across groups.
  )
  # Check the class of the KDE result and resample if needed
  # here, there is a need to verify is the KDE results produced are SpatRaster objects, considering that the package used now is the terra package, which has replaced the previos Rgeos package that has depreciated
  # the code therefore ensures that, If kde_result is not a SpatRaster object, then, the stop() function is executed to halt the operation and display the message "KDE result is not a SpatRaster." This ensures that only valid SpatRaster objects are processed in the next steps.
  if (!inherits(kde_result, "SpatRaster")) {
    stop("KDE result is not a SpatRaster.")
  }
  
  # After calculating the KDE for each species observation, g, there is a need ensure that the results fall within the geographic boundaries of the target extent (forest), define in the earlier stage of this script
  kde_result <- terra::crop(kde_result, environmental_mask)  # Crop to target extent using the crop function of the terra package
  kde_result <- terra::resample(kde_result, environmental_mask, method = "bilinear")  # although the target resolution has been specified in the KDE calculations, it is always a good approach to resample to target resolution again after coping the target extent. Also, the bilinear interpolation in the raster resampling, ensures that new pixel values are calculated based on the weighted average of surrounding pixels, yielding a smoother result
  
  # To ensure that there is no mismatch in spatial extents and resolutions between the calculated KDEs and masked or stacked environmental layers, there is a need to mask the KDE result with the stacked or masked environmental layer (forest)
  observations_kde_list[[g]] <- terra::mask(kde_result, environmental_mask)
}


# Plot KDE results for each group
par(mfrow = c(1, 1))  # Arrange plots in a 2x2 grid

for (g in names(observations_kde_list)) {
  plot(observations_kde_list[[g]], main = paste("KDE for", g))
}


# Set species common name: here, I extract the common name of each species in each row i (within the modelPerformanceAllSpecies csv file), and store it in a new variable called scientific_name_i, for easy reference in the loop iterations
for (i in 1:nrow(modelPerformanceAllSpecies)) {
  # Extract key species attributes
  group_i <- modelPerformanceAllSpecies$group[i]  
  genus_i <- modelPerformanceAllSpecies$genus[i]
  species_i <- modelPerformanceAllSpecies$species[i]
  scientific_name_i <- modelPerformanceAllSpecies$scientific_name[i]
  common_name_i <- modelPerformanceAllSpecies$scientific_name[i]
  
  message("\nProcessing species: ", scientific_name_i, " (", genus_i, " ", species_i, ") in group: ", group_i)
  
  # Ensure KDE raster exists for this group
  if (!group_i %in% names(observations_kde_list)) {
    message("No KDE raster available for group: ", group_i, " - Skipping species: ", scientific_name_i)
    next
  }
  observations_kde_group_i <- observations_kde_list[[group_i]]
  
  # Filter species observations
  species_i_sp <- species_observations_clipped %>%
    filter(scientific_name == scientific_name_i) %>%
    slice_sample(n = 1, by = square_id)  # Avoids spatial autocorrelation issues
  
  if (nrow(species_i_sp) == 0) {
    message("No valid occurrences found for species: ", scientific_name_i, " - Skipping.")
    next
  }
  
  # Convert species occurrence data to spatial format
  species_i_sp <- st_as_sf(species_i_sp, coords = c("longitude", "latitude"), crs = 4326)
  
  # Ensure study_area is an sf object and matches CRS
  study_area <- st_as_sf(study_area)
  
  if (st_crs(species_i_sp) != st_crs(study_area)) {
    message("Transforming CRS of study area to match species observations.")
    study_area <- st_transform(study_area, st_crs(species_i_sp))
  }
  
  # Print key elements for debugging (optional)
  print(observations_kde_group_i)
  
  # Intersect species points with the study area
  species_i_intersects_SE <- st_intersects(species_i_sp, study_area, sparse = FALSE)
  
  # Filter species points by intersection (keep only points inside the study area)
  species_i_sp <- species_i_sp[which(rowSums(species_i_intersects_SE) > 0), ]
  
  # Check if there are any remaining points after filtering
  if (nrow(species_i_sp) == 0) {
    message("No valid intersection found for species: ", scientific_name_i, " - Skipping.")
    modelPerformanceAllSpecies$n_MWCASC[i] <- 0  # Assign zero if no observations remain
    next
  }
  
  # Transform the CRS of the filtered points to match the environmental variable stack
  species_i_sp <- st_transform(species_i_sp, st_crs(environmental_var_stack))
  
  # Extract unique observation points (latitude and longitude), removing duplicate coordinates
  obs_species_i <- unique(st_coordinates(species_i_sp))
  
  # Convert the matrix of coordinates into a data frame
  obs_species_i <- data.frame(longitude = obs_species_i[, 1],
                              latitude = obs_species_i[, 2])
  
  # Check the first few records (optional for debugging)
  head(obs_species_i)
  
  # Save sample size: store the count of unique observation points in modelPerformanceAllSpecies
  modelPerformanceAllSpecies$n_MWCASC[i] <- nrow(obs_species_i)
  
  # Optional: Print progress update
  message("Processed ", nrow(obs_species_i), " unique occurrences for species: ", common_name_i)
  
  # Define output folder and create it if it doesn't exist
  #output_dir_range <- "F:/WWF_data/SDMs_LC_climate/species_range_ape_elephant"
  #dir.create(output_dir_range, recursive = TRUE, showWarnings = FALSE)
  
  # Only model species with >= 20 observations
  if(modelPerformanceAllSpecies$n_MWCASC[i] >= 20){# here, i first of all check and select the number of unique observations filtered for the prairie region (n_prairie) in each row of the modelPerformanceAllSpecies data, with records  >= 20; 20 is the minimum threshold)
    # I now assign the obs_species_i data frame (which contains the unique coordinates in each grid id) into a new variable "species_i_ppr_sp", assign lon and lat coordinates to make it a spatial object, and finally project the variable to a similar CRS with the stacked environmental variables
    species_i_sp <- obs_species_i
    coordinates(species_i_sp) <- c("longitude", "latitude")
    species_i_sp@proj4string <- raster::crs(environmental_var_stack)
    
    # Generate a 50 km buffer around each observation point
    species_i_buffers <- spTransform(species_i_sp, CRS = raster::crs(environmental_var_stack)) %>%
      raster::buffer(width = 50000)  # 5 km buffer around each observation
    
    # Convert the buffer to an sf object for easier handling
    species_i_buffers_sf <- st_as_sf(species_i_buffers)
    
    # Dissolve and merge all buffers
    species_i_buffers_dissolved <- species_i_buffers_sf %>%
      st_union()  # Dissolve all buffers into a single polygon
    
    # Define species name and file path
    species_names <- modelPerformanceAllSpecies$species[i]
    shapefile_path <- file.path(output_dir_range, paste0(gsub(" ", "_", species_names), "_range.shp"))
    
    # Save the shapefile
    # st_write(species_i_buffers_dissolved, shapefile_path, delete_layer = TRUE)
    
    # Plot the dissolved buffer (clipped to the study area) and the study area
    #par(mfrow = c(1, 1))  # Set up a single panel for plotting
    #plot(st_geometry(species_i_buffers_dissolved), main = "Clipped Dissolved 5 km Buffer for Fowler's Toad Observations", col = "lightblue")
    #plot(st_geometry(clipped_study_area_proj), add = TRUE, border = "black")  # Overlay study area
    
    
    ggplot() +
      geom_sf(data = study_area, fill = "gray95", color = "black") +
      geom_sf(data = st_as_sf(species_i_buffers_dissolved), color = "darkgreen", size = 2, alpha = 0.7) +
      theme_minimal() +
      labs(title = "Species Observations",
           subtitle = "Species Observations within the Study Area",
           caption = "Species observation points have been transformed to match CRS")
    
    
    # Now Calculate species range size
    species_i_rangesize <- species_i_buffers_dissolved %>% 
      st_intersection(study_area) %>%  # Intersects the species buffer with the clipped study area to remove parts outside
      st_area() %>%  # Calculates the total area of the intersected buffers
      as.numeric() %>%  # Converts the calculated area to numeric for ease of further analysis
      sum()  # Sums up areas of multiple polygons to get the total range size
    species_i_rangesize
    
    # Estimate study area size
    study_area_size <- st_area(study_area) %>% as.numeric()
    study_area_size 
    
    # Ensure the buffer has the same CRS as the environmental variable stack
    species_i_buffers_dissolved <- st_transform(species_i_buffers_dissolved, crs = st_crs(environmental_var_stack))
    
    # Convert the dissolved buffer to an sp object
    species_i_buffers_dissolved_sp <- as(species_i_buffers_dissolved, "Spatial")
    
    # Now apply the crop and mask functions
    if(species_i_rangesize >= 0.9 * 2.018284e+12){ # Use full study area if range size is >= 90% of study area
      environmental_var_stack_i <- environmental_var_stack  # Use the entire study area
    } else {
      environmental_var_stack_i <- environmental_var_stack %>%
        raster::crop(., species_i_buffers_dissolved_sp) %>%  # Crop environmental data to species' range
        raster::mask(., species_i_buffers_dissolved_sp)  # Mask environmental data to species' range
    }
    
    
    # Plot to visualize the environmental variable stack and buffer
    #plot(environmental_var_stack_i$temp)
    #plot(species_i_buffers_dissolved, add = TRUE, lty = 5)  # Overlay species buffer on top of the environmental data
    
    # Use the full study area, regardless of the species' range size
    #environmental_var_stack_i <- environmental_var_stack  # Use the entire study area
    
    # Convert species_i_sp to an sf object if it is a SpatialPointsDataFrame
    species_i_sf <- st_as_sf(species_i_sp)
    
    # Next, is to extract values of environmental_var_stack_i for all obs_species_i
    modelPerformanceAllSpecies[i,8:ncol(modelPerformanceAllSpecies)] <- raster::extract(environmental_var_stack_i,
                                                                                        species_i_sf) %>%
      # first, this code ensures that the raster values of each environmental variable (within the stacked raster) is extracted for every given location of a species, within the species observation variable, "species_i_ppr_sp".
      # through this approach, a data structure is created for each observation point, where each column represents each environmental variable, and each row represents each observation 
      as_tibble(.) %>% # here, the data structure is converted to a tibble, a modern data frame structure in R that facilitates subsequent operations
      summarize_all(., mean, na.rm = TRUE) # this operation ensures that the mean of each environmental variable is calculated across observations for the said variable, ignoring all NA values
    # In the definition,modelPerformanceAllSpecies[i, 8:ncol(modelPerformanceAllSpecies)] <- ..., the calculated means areassigned to columns 8 onward for each row, i, of the modelPerformanceAllSpecies data frame. The results of these columns are required for evaluating model performance or habitat suitability for species in subsequent analysis
    
    modelPerformanceAllSpecies[i,8:ncol(modelPerformanceAllSpecies)]
    
    # Because the environmental_var_stack_i variable created above is still working with the raster package, there is need to the data to a SpatRaster in order to avoid model fitting and evaluation errors
    environmental_var_stack_i <- terra::rast(environmental_var_stack_i)
    # after converting to a SpatRaster, we need to ensure that our species KDE calculated in th earlier stage of this analysis matches the extent and resolution of our newly created environmental_var_stack_ic SpatRaster object
    # It should be noted that KDE raster captures observation biases
    observationbias_species_i <- observations_kde_group_i %>%
      terra::crop(environmental_var_stack_i) %>%
      terra::mask(environmental_var_stack_i)
    observationbias_species_i
    
    
    # Check if the observation bias raster has any values
    plot(observationbias_species_i)
    
    # If it's empty or not valid, skip further processing for this species
    if (all(is.na(values(observationbias_species_i)))) {
      message("Observation bias is empty for species: ", scientific_name_i, " - Skipping further analysis")
    } else {
      # Plot the observation bias for species
      plot(observationbias_species_i)
    }
  }
  
  
  # Next, is to generate background points based on species range size pixels
  # However, I will first, calculate the number of pixels in the species range, using the global() function from the terra package 
  species_i_rangepixels <- global(((observationbias_species_i[[1]] * 0) + 1), 'sum', na.rm = TRUE)[1, 1]
  # Here, the observationbias_species_i[[1]] * 0 creates a raster with the same structure as observationbias_species_i but filled with zeros
  # + 1 then turns all pixel values to 1, representing each pixel as a unit.
  #global(..., 'sum', na.rm = TRUE) then sums all these pixel values, providing the count of pixels that cover the species' range
  # the [1, 1] extracts this count from the result, storing it in species_i_rangepixels.
  species_i_rangepixels
  
  # Now, determine background points based on species range size pixels
  if(species_i_rangepixels < 100000){
    # Use 20% for small ranges i.e., assign backg_n as 20% of species_i_rangepixels
    backg_n <- round(species_i_rangepixels*0.2, 0)
  }else if(species_i_rangepixels >= 100000 & species_i_rangepixels < 250000){
    # Use 10% for medium ranges i.e., assign backg_n as 10% of species_i_rangepixels
    backg_n <- round(species_i_rangepixels*0.1, 0)
  }else if(species_i_rangepixels >= 250000){
    # Use 5% for big ranges
    backg_n <- round(species_i_rangepixels*0.05, 0) #rounds the calculated value to the nearest integer for easier indexing
  }
  
  backg_n
  
  
  # Sample background points, using bias specific to a target group
  ext <- ext(observationbias_species_i)
  # Generate background points using terra::spatSample()
  backg <- terra::spatSample(observationbias_species_i, 
                             size = backg_n, method = "random", 
                             na.rm = TRUE, xy = TRUE)
  backg
  
  colnames(backg)
  
  backg <- backg[, 1:2]  # Select only the first two columns
  colnames(backg) <- c("lon", "lat")
  
  # Make one sf for background
  backg_sf <- backg %>%
    as.data.frame(.) %>%
    st_as_sf(.,
             coords = c("lon", "lat"),
             crs = raster::crs(environmental_var_stack_i)) %>%
    mutate(p = 0) %>%
    cbind(., st_coordinates(.))
  
  head(backg_sf)
  
  # Make sf object with presence and background points
  species_i_presbackg_sf <- species_i_sf %>%
    st_as_sf(., crs = raster::crs(environmental_var_stack_i)) %>%
    mutate(p = 1) %>%
    cbind(., st_coordinates(.)) %>%
    rbind(backg_sf)
  
  head(species_i_presbackg_sf)
  tail(species_i_presbackg_sf)
  
  # Make sure all points have values for each environmental variable
  species_i_pb_extract <- raster::extract(environmental_var_stack_i,
                                          species_i_presbackg_sf) %>%
    as.data.frame(.)
  
  head(species_i_pb_extract)
  #View(species_i_pb_extract)
  
  names(environmental_var_stack)
  
  # Omit NA row names
  species_i_omit_rownames <- row.names(species_i_pb_extract[ is.na(species_i_pb_extract$temp)|
                                                               is.na(species_i_pb_extract$prec)| 
                                                               is.na(species_i_pb_extract$forest)| 
                                                               is.na(species_i_pb_extract$cropland)| 
                                                               is.na(species_i_pb_extract$urban)| 
                                                               is.na(species_i_pb_extract$grassland)| 
                                                               is.na(species_i_pb_extract$barren)| 
                                                               is.na(species_i_pb_extract$water)| 
                                                               is.na(species_i_pb_extract$elevation)| 
                                                               is.na(species_i_pb_extract$slope)| 
                                                               is.na(species_i_pb_extract$oforest)| 
                                                               is.na(species_i_pb_extract$wetland),]) 
  
  
  # species_i_omit_rownames <- row.names(species_i_pb_extract[
  # is.na(species_i_pb_extract$temp),]) 
  
  
  # Alternative approach to omitting row names
  #species_i_omit_rownames <- rownames(species_i_pb_extract[rowSums(is.na(species_i_pb_extract[
  #c("forest", "water", "wetlands", "annual.croplands", "barren.lands",
  #"nativegrass", "perennial.croplands", "roads", "settlements",
  #"tmax", "tmin", "prec")])) > 0, ])
  
  species_i_omit_rownames
  summary(species_i_omit_rownames)
  
  
  # Define species presence/background data
  species_i_presbackg_sf <- species_i_presbackg_sf %>%
    filter(!(row.names(.) %in% species_i_omit_rownames)) # Remove rows with NA values
  species_i_presbackg_sf
  
  # Run spatial blocking to prepare data for cross-validation
  sb <- spatialBlock(speciesData = species_i_presbackg_sf %>%
                       filter(!(row.names(.) %in% species_i_omit_rownames)),
                     species = "p",  # Presence column (1 for presence, 0 for background)
                     rasterLayer = environmental_var_stack_i,
                     selection = "systematic",
                     rows = 10,
                     cols = 10,  # Define the number of rows and columns in the grid
                     k = 5,  # Number of folds for cross-validation
                     biomod2Format = TRUE)  # Format compatible with biomod2 package
  
  # Add fold ID to dataset
  species_i_presbackg_sf <- species_i_presbackg_sf %>%
    filter(!(row.names(.) %in% species_i_omit_rownames)) %>%
    mutate(fold = sb$foldID)
  
  # Ensure the fold assignments are properly distributed
  table(species_i_presbackg_sf$fold)  # Check the distribution of folds
  
  # Filter out rows with missing values
  species_i_pb_extract <- species_i_pb_extract %>%
    filter(!(row.names(.) %in% species_i_omit_rownames))
  
  # Initialize evaluation metrics
  species_i_eval_summary <- data.frame(fpr = c(), tpr = c(), fold = c())
  species_i_maxent_models <- list()  # List to store Maxent models
  species_i_px <- list()  # List to store habitat suitability predictions
  AUCs_i <- vector(mode = "numeric", length = 5)  # Store AUCs for each fold
  
  # Identify folds with no observations (e.g. large range, but small clustered points like Great Plains Toad)
  species_i_foldvector <- species_i_presbackg_sf %>%
    st_drop_geometry(.) %>%
    filter(p == 1) %>%
    group_by(fold) %>%
    summarize(total = n()) %>%
    pull(fold)
  
  # Loop through folds for cross-validation
  for (k in 1:5) {
    # Split data into training and testing sets based on fold
    trainSet <- which(sb$foldID != k)
    testSet <- which(sb$foldID == k)
    
    # Check if both training and testing sets have data
    if (length(trainSet) == 0 || length(testSet) == 0) {
      cat("No observations in fold", k, ", skipping this fold.\n")
      next  # Skip this fold if no observations are available
    }
    
    
    # Check column names in the species_i_presbackg_sf to verify the existence of the 'ID' column
    cat("Column names in species_i_presbackg_sf:", colnames(species_i_presbackg_sf), "\n")
    
    # Remove the 'ID' column from the environmental and presence-background data
    # If 'ID' column exists in species_i_presbackg_sf, it will be removed
    if("ID" %in% colnames(species_i_presbackg_sf)) {
      species_i_presbackg_sf_no_ID <- species_i_presbackg_sf %>%
        st_drop_geometry() %>%
        dplyr::select(-ID)  # Remove 'ID' column from the spatial data
    } else {
      cat("'ID' column not found in species_i_presbackg_sf\n")
      species_i_presbackg_sf_no_ID <- species_i_presbackg_sf  # Keep the data as is if no 'ID' column
    }
    
    # Remove the 'ID' column from the environmental data (assuming it's named 'ID' in species_i_pb_extract)
    species_i_pb_extract_no_ID <- species_i_pb_extract[, !colnames(species_i_pb_extract) %in% "ID"]
    species_i_pb_extract_no_ID
    
    # Subset the training and testing data
    trainData <- species_i_presbackg_sf[trainSet, ]
    testData <- species_i_presbackg_sf[testSet, ]
    trainEnv <-  species_i_pb_extract_no_ID[trainSet, ]
    testEnv <-  species_i_pb_extract_no_ID[testSet, ]
    
    # Now, fit the MaxEnt model using linear, quadratic, and hinge features
    
    # Fit Maxent model
    species_i_maxent_model <- maxent(
      x = trainEnv,  # Environmental variables for the training set
      p = trainData$p,  # Presence data for the training set
      a = trainData$background,  # Background data for the training set
      args = c("outputformat=cloglog", "betamultiplier=1", "maximumiterations=1000")  # Maxent model arguments
    )
    
    species_i_maxent_model
    
    # Store the fitted Maxent model
    species_i_maxent_models[[k]] <- species_i_maxent_model 
    
    # Get variable importance from the fitted Maxent model
    # Define output directory for variable importance (if not already defined)
    variable_importance_dir <- file.path("F:/WWF_data/SDMs_LC_climate/results/other_mammals/from_2030/range_full/ssp126/variable_importance")
    dir.create(variable_importance_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Extract variable importance for the species
    if (!is.null(species_i_maxent_model)) {
      
      # Extract variable contribution and permutation importance
      variable_importance <- species_i_maxent_model@results
      
      # Optionally: keep only relevant variable importance rows
      var_imp_rows <- grep("contribution$|permutation.importance$", rownames(variable_importance), value = TRUE)
      variable_importance_filtered <- variable_importance[var_imp_rows, , drop = FALSE]
      
      # Print for debugging
      cat("Variable importance for", scientific_name_i, ":\n")
      print(variable_importance_filtered)
      
      # Save as CSV with species name
      variable_importance_path <- file.path(variable_importance_dir, paste0("variable_importance_", scientific_name_i, ".csv"))
      write.csv(variable_importance_filtered, file = variable_importance_path, row.names = TRUE)
      
      cat("Variable importance saved for", scientific_name_i, "\n")
      
    } else {
      cat("No model found for", scientific_name_i, "— skipping variable importance export.\n")
    }
    
    
    # Predict habitat suitability for the test data
    species_i_px[[k]] <- predict(environmental_var_stack_i, 
                                 species_i_maxent_models[[k]], 
                                 ext = ext, 
                                 na.rm = TRUE,
                                 progress = '') # suppresses the display of the progress bar during model prediction (an empty string means no progress bar will be shown).
    
    plot( species_i_px[[k]], main = paste("Final Predicted climatic Habitat Suitability for Species", scientific_name_i))
    
    
    # Define the directory where the prediction files will be saved
    output_dir <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/from_2030/range_full/ssp126"
    
    # Ensure the directory exists
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)  # Create the directory if it doesn't exist
    }
    
    for (k in seq_along(species_i_px)) {
      if (!is.null(species_i_px[[k]])) {
        # Define the file path for each fold
        file_path <- paste0(output_dir, "habitat_suitability_species_", scientific_name_i, "_fold_", k, ".tif")
        
        # Save the raster as a TIFF file
        terra::writeRaster(species_i_px[[k]], filename = file_path, overwrite = TRUE)
        
        # Print confirmation message
        cat("Saved:", file_path, "\n")
      } else {
        cat("No valid prediction for fold", k, "\n")
      }
    }
    
    # Evualuate model
    
    species_predictions_avg <- list()
    species_i_eval <- list()  # Initialize list to store evaluation results
    species_i_eval_summary <- data.frame(
      fpr = numeric(),
      tpr = numeric(),
      mean_tpr = numeric(),
      sd_tpr = numeric(),
      se_tpr = numeric(),
      fold = integer()
    )
    
    # Initialize a vector to store AUC values
    auc_values <- numeric()
    
    
    if(k %in% species_i_foldvector){
      
      # Save evaluation for each fold
      species_i_eval[[k]] <- evaluate(p = species_i_presbackg_sf[testSet,] %>%
                                        st_drop_geometry(.) %>%
                                        filter(p == 1) %>%
                                        dplyr::select(-p, -fold),
                                      # p = presence, a = absence
                                      a = species_i_presbackg_sf[testSet,] %>%
                                        st_drop_geometry(.) %>%
                                        filter(p == 0) %>%
                                        dplyr::select(-p,-fold), 
                                      # model = maxent model
                                      model = species_i_maxent_models[[k]],
                                      # x = environmental variable raster stack
                                      x = environmental_var_stack_i)
      
      # Ensure the evaluation object is valid before proceeding
      if (!is.null(species_i_eval[[k]])) {
        
        # Extract FPR, TPR from model evaluation
        eval_df <- data.frame(
          fpr = species_i_eval[[k]]@FPR,
          tpr = species_i_eval[[k]]@TPR,  # Ensure TPR is included
          mean_tpr = mean(species_i_eval[[k]]@TPR, na.rm = TRUE),  # Compute mean TPR
          sd_tpr = sd(species_i_eval[[k]]@TPR, na.rm = TRUE),      # Compute SD of TPR
          se_tpr = sd(species_i_eval[[k]]@TPR, na.rm = TRUE) / sqrt(length(species_i_eval[[k]]@TPR)),  # Compute SE of TPR
          fold = k
        )
        
        # Step 1: Remove any existing NA rows in species_i_eval_summary
        species_i_eval_summary <- species_i_eval_summary %>%
          filter(!is.na(tpr))
        
        # Step 2: Ensure column names match
        eval_df <- eval_df %>%
          dplyr::select(all_of(colnames(species_i_eval_summary)))  # Keep column order consistent
        
        # Step 3: Append without introducing new NA values
        species_i_eval_summary <- bind_rows(species_i_eval_summary, eval_df)
        
        # Extract AUC value for the current fold and append it to the auc_values vector
        auc_values <- c(auc_values, species_i_eval[[k]]@auc)
        
        # Print number of test observations
        num_test_obs <- species_i_presbackg_sf[testSet, ] %>%
          st_drop_geometry() %>%
          filter(p == 1) %>%
          dplyr::select(-p, -fold) %>%
          nrow()
        
        print(paste("Number of test observations in fold", k, "is", num_test_obs))
        
      } else {
        cat(" Evaluation object is NULL for fold", k, "- Skipping.\n")
      }
    }
    
    
    # Calculate the average AUC for all folds (if auc_values contains any valid values)
    if (length(auc_values) > 0) {
      avg_auc <- mean(auc_values, na.rm = TRUE)
      cat("Average AUC across all folds for", scientific_name_i, ":", avg_auc, "\n")
      
      # Update the mean_auc column in modelPerformanceAllSpecies using the species name
      row_index <- which(modelPerformanceAllSpecies$scientific_name == scientific_name_i)
      
      if (length(row_index) == 1) {
        modelPerformanceAllSpecies$mean_auc[row_index] <- avg_auc
        cat("Average AUC saved for", scientific_name_i, "\n")
      } else {
        cat("Warning: Could not uniquely identify row for species", scientific_name_i, "\n")
      }
    } else {
      cat("No valid AUC values found for species", scientific_name_i, "\n")
    }
    
    
    # Step 1: Keep only non-NULL raster predictions
    valid_indices <- which(!sapply(species_i_px, is.null))
    species_i_px_valid <- species_i_px[valid_indices]
    
    # Step 2: Calculate AUC weights based on the average AUC across all folds
    # Adjust AUC weights using the average AUC value calculated earlier (avg_auc)
    auc_weights_valid <- (avg_auc - 0.5)^2  # Use avg_auc to compute weights
    auc_weights_valid <- rep(auc_weights_valid, length(valid_indices))  # Replicate for each valid prediction
    
    # Step 3: Stack the valid SpatRaster predictions
    species_i_px_stack <- rast(species_i_px_valid)
    
    # Check if there is only one valid prediction layer
    if (nlyr(species_i_px_stack) == 1) {
      species_i_px_avg <- species_i_px_stack
      cat("Only one valid prediction layer — using it as the final habitat suitability surface.\n")
    } else {
      # Calculate the weighted average habitat suitability surface
      species_i_px_avg <- app(species_i_px_stack, fun = function(x) {
        if (all(is.na(x))) {
          return(NA)
        } else {
          return(weighted.mean(x, w = auc_weights_valid, na.rm = TRUE))
        }
      })
      cat("Weighted average prediction surface computed for", scientific_name_i, "\n")
    }
    
    plot(species_i_px_avg, main = paste("Average Habitat Suitability -", scientific_name_i))
    
    
    #species_predictions_avg <- list()
    species_predictions_avg[[scientific_name_i]] <- species_i_px_avg
    names(species_predictions_avg)
    
    
    # Define output path for the average suitability raster
    output_dir1 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/from_2030/range_full/ssp126/average"
    dir.create(output_dir1, recursive = TRUE, showWarnings = FALSE)
    avg_suitability_path <- file.path(output_dir1, paste0("weighted_avg_habitat_suitability_", scientific_name_i, ".tif"))
    
    # Save the weighted average raster (SpatRaster object)
    if (exists("species_i_px_avg")) {
      
      # Save to disk
      terra::writeRaster(species_i_px_avg, filename = avg_suitability_path, overwrite = TRUE)
      cat("Saved weighted average habitat suitability surface for", scientific_name_i, "\n")
      
      # Store in named list
      species_predictions_avg[[scientific_name_i]] <- species_i_px_avg
      
    } else {
      cat("species_i_px_avg not found — skipping raster save for", scientific_name_i, "\n")
    }
    
    
    # --------------Obtaining optimal threshold---------------------------
    
    # ---------- Step 1: Extract thresholds (spec_sens) from each fold safely ----------
    fold_thresholds <- sapply(species_i_eval, function(eval_obj) {
      tryCatch({
        if (!is.null(eval_obj)) {
          dismo::threshold(eval_obj, 'spec_sens')
        } else {
          NA
        }
      }, error = function(e) NA)
    })
    
    # ---------- Step 2: Calculate valid weights ----------
    auc_weights <- (AUCs_i - 0.5)^2
    auc_weights[is.na(auc_weights)] <- 0  # zero out missing AUCs
    
    valid_folds <- which(!is.na(fold_thresholds) & auc_weights > 0)
    cat("Folds contributing to threshold calculation for", scientific_name_i, ":", paste(valid_folds, collapse = ", "), "\n")
    
    valid_thresholds <- fold_thresholds[valid_folds]
    valid_weights <- auc_weights[valid_folds]
    
    # ---------- Step 3: Compute the final threshold ----------
    if (length(valid_thresholds) > 0 && sum(valid_weights, na.rm = TRUE) > 0) {
      species_i_threshold <- weighted.mean(valid_thresholds, w = valid_weights, na.rm = TRUE)
    } else if (length(valid_thresholds) > 0) {
      species_i_threshold <- mean(valid_thresholds, na.rm = TRUE)
      cat("All weights were zero — used unweighted mean instead.\n")
    } else {
      species_i_threshold <- NA
      cat("No valid thresholds found — returning NA.\n")
    }
    
    cat("Final average threshold (spec_sens) for", scientific_name_i, ":", species_i_threshold, "\n")
    
    # ---------- Step 4: Save threshold to performance table ----------
    modelPerformanceAllSpecies$mean_threshold[modelPerformanceAllSpecies$scientific_name == scientific_name_i] <- species_i_threshold
    
    # Define file paths
    species_dir <- file.path("F:/WWF_data/SDMs_LC_climate/results/other_mammals/from_2030/range_full/ssp126/binary", gsub(" ", "_", scientific_name_i))
    dir.create(file.path(species_dir, "binary_mammals"), showWarnings = FALSE, recursive = TRUE)
    
    write.csv(modelPerformanceAllSpecies,
              file = file.path(species_dir, paste0(scientific_name_i, "_performance.csv")),
              row.names = FALSE)
    cat("Model performance table saved as CSV for", scientific_name_i, "\n")
    
    # ---------- Step 5: Create and Save Binary Suitability Raster ----------
    if (!is.na(species_i_threshold)) {
      species_i_binary <- species_i_px_avg > species_i_threshold
      
      # plot
      plot(species_i_binary, main = paste("Binary Suitability for", scientific_name_i))
      
      binary_path <- file.path(species_dir, "binary_mammals", paste0(scientific_name_i, "_mean_pixel_binary.tif"))
      writeRaster(
        species_i_binary,
        filename = binary_path,
        overwrite = TRUE
      )
      
      cat("Binary habitat raster saved successfully for", scientific_name_i, "\n")
    } else {
      cat("Binary raster not created — threshold was NA for", scientific_name_i, "\n")
    }
    
    
    
    
    # ---------------------Conduct future projections------------------- 
    
    
    # ---------2021-2040-ssp245-------------
    
    environmental_var_stack_2030_ssp245 <- raster::stack(MAT_2030_ssp245, MAP_2030_ssp245,
                                                         forest_2030_ssp245, cropland_2030_ssp245,
                                                         urban_2030_ssp245, grassland_2030_ssp245,
                                                         barren_2030_ssp245, water_2030_ssp245,
                                                         elevation, slope, oforest_2030_ssp245,
                                                         wetland_2030_ssp245)
    environmental_var_stack_2030_ssp245
    
    plot(environmental_var_stack_2030_ssp245)
    
    
    names(environmental_var_stack_2030_ssp245) <- c("temp", "prec", "forest", "cropland", "urban", 
                                                    "grassland", "barren", "water", "elevation", 
                                                    "slope", "oforest", "wetland")
    
    
    environmental_var_stack_2030_ssp245 <- terra::rast(environmental_var_stack_2030_ssp245)
    
    species_2030s_Access_ssp245_i_px <- list()
    
    # Predict habitat suitability for the test data
    species_2030s_Access_ssp245_i_px[[k]] <- predict(environmental_var_stack_2030_ssp245, 
                                                     species_i_maxent_models[[k]], 
                                                     ext = ext, 
                                                     na.rm = TRUE,
                                                     progress = '')
    
    plot(species_2030s_Access_ssp245_i_px[[k]], main = paste("Predicted climatic Habitat Suitability 2030 Access ssp245", scientific_name_i))
    
    
    # Define the directory where the prediction files will be saved
    output_dir22 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/from_2030/range_full/ssp245"
    
    # Ensure the directory exists
    if (!dir.exists(output_dir22)) {
      dir.create(output_dir22, recursive = TRUE)
    }
    
    for (k in seq_along(species_2030s_Access_ssp245_i_px)) {
      if (!is.null(species_2030s_Access_ssp245_i_px[[k]])) {
        file_path22 <- paste0(output_dir22, "habitat_suitability_species_2030s_ssp245_", scientific_name_i, "_fold_", k, ".tif")
        
        terra::writeRaster(species_2030s_Access_ssp245_i_px[[k]], filename = file_path22, overwrite = TRUE)
        
        cat("Saved:", file_path22, "\n")
      } else {
        cat("No valid prediction for fold", k, "\n")
      }
    }
    
    
    # Step 1: Keep only non-NULL raster predictions
    valid_indices11 <- which(!sapply(species_2030s_Access_ssp245_i_px, is.null))
    species_i_px_valid11 <- species_2030s_Access_ssp245_i_px[valid_indices11]
    
    # Step 2: Calculate AUC weights based on the average AUC across all folds
    auc_weights_valid11 <- (avg_auc - 0.5)^2
    auc_weights_valid11 <- rep(auc_weights_valid11, length(valid_indices11))
    
    # Step 3: Stack the valid SpatRaster predictions
    species_2030s_Access_ssp245_i_px_stack <- rast(species_i_px_valid11)
    
    # Check if there is only one valid prediction layer
    if (nlyr(species_2030s_Access_ssp245_i_px_stack) == 1) {
      species_2030s_Access_ssp245_i_px_avg <- species_2030s_Access_ssp245_i_px_stack
      cat("Only one valid prediction layer — using it as the final habitat suitability surface.\n")
    } else {
      species_2030s_Access_ssp245_i_px_avg <- app(species_2030s_Access_ssp245_i_px_stack, fun = function(x) {
        if (all(is.na(x))) {
          return(NA)
        } else {
          return(weighted.mean(x, w = auc_weights_valid11, na.rm = TRUE))
        }
      })
      cat("Weighted average prediction surface computed for", scientific_name_i, "\n")
    }
    
    plot(species_2030s_Access_ssp245_i_px_avg, main = paste("Average Habitat Suitability 2030s Access ssp245", scientific_name_i))
    
    
    species_2030s_Access_ssp245_predictions_avg <- list()
    species_2030s_Access_ssp245_predictions_avg[[scientific_name_i]] <- species_2030s_Access_ssp245_i_px_avg
    names(species_2030s_Access_ssp245_predictions_avg)
    
    
    # Define output path for the average suitability raster
    output_dir33 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/from_2030/range_full/ssp245/average"
    dir.create(output_dir33, recursive = TRUE, showWarnings = FALSE)
    avg_suitability_path33 <- file.path(output_dir33, paste0("weighted_avg_habitat_suitability_2030s_ssp245_", scientific_name_i, ".tif"))
    
    # Save the weighted average raster (SpatRaster object)
    if (exists("species_2030s_Access_ssp245_i_px_avg")) {
      
      terra::writeRaster(species_2030s_Access_ssp245_i_px_avg, filename = avg_suitability_path33, overwrite = TRUE)
      cat("Saved weighted average habitat suitability 2030s Access ssp245 for", scientific_name_i, "\n")
      
      species_2030s_Access_ssp245_predictions_avg[[scientific_name_i]] <- species_2030s_Access_ssp245_i_px_avg
      
    } else {
      cat("species_2030s_Access_ssp245_i_px_avg not found — skipping raster save for", scientific_name_i, "\n")
    }
    
    
    # ---------- Step 5: Create and Save Binary Suitability Raster ----------
    if (!is.na(species_i_threshold)) {
      species_i_binary_2030s_Access_ssp245 <- species_2030s_Access_ssp245_i_px_avg > species_i_threshold
      
      plot(species_i_binary_2030s_Access_ssp245, main = paste("Binary Suitability 2030s Access ssp245 for", scientific_name_i))
      
      species_dir11 <- file.path("F:/WWF_data/SDMs_LC_climate/results/other_mammals/from_2030/range_full/ssp245", gsub(" ", "_", scientific_name_i))
      dir.create(file.path(species_dir11, "binary"), showWarnings = FALSE, recursive = TRUE)
      binary11 <- file.path(species_dir11, "binary", paste0(scientific_name_i, "_mean_pixel_binary_2030s_ssp245.tif"))
      writeRaster(
        species_i_binary_2030s_Access_ssp245,
        filename = binary11,
        overwrite = TRUE
      )
      
      cat("Binary habitat raster saved successfully for", scientific_name_i, "\n")
    } else {
      cat("Binary raster not created — threshold was NA for", scientific_name_i, "\n")
    }
    
    
    # ---------2021-2040-ssp585-------------
    
    # Stack all environmental rasters (raster stack)
    environmental_var_stack_2030_ssp585 <- raster::stack(MAT_2030_ssp585, MAP_2030_ssp585, forest_2030_ssp585,
                                                         cropland_2030_ssp585, urban_2030_ssp585,
                                                         grassland_2030_ssp585, barren_2030_ssp585,
                                                         water_2030_ssp585, elevation, 
                                                         slope, oforest_2030_ssp585, wetland_2030_ssp585)
    environmental_var_stack_2030_ssp585
    
    plot(environmental_var_stack_2030_ssp585)
    
    
    names(environmental_var_stack_2030_ssp585) <- c("temp", "prec", "forest", "cropland", "urban", 
                                                    "grassland", "barren", "water", 
                                                    "elevation", "slope", "oforest", "wetland")
    
    
    environmental_var_stack_2030_ssp585 <- terra::rast(environmental_var_stack_2030_ssp585)
    
    species_2030s_Access_ssp585_i_px <- list()
    
    # Predict habitat suitability for the test data
    species_2030s_Access_ssp585_i_px[[k]] <- predict(environmental_var_stack_2030_ssp585, 
                                                     species_i_maxent_models[[k]], 
                                                     ext = ext, 
                                                     na.rm = TRUE,
                                                     progress = '')
    
    plot(species_2030s_Access_ssp585_i_px[[k]], main = paste("Predicted climatic Habitat Suitability 2030 Access ssp585", scientific_name_i))
    
    # Define the directory where the prediction files will be saved
    output_dir222 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/from_2030/range_full/ssp585"
    
    # Ensure the directory exists
    if (!dir.exists(output_dir222)) {
      dir.create(output_dir222, recursive = TRUE)
    }
    
    for (k in seq_along(species_2030s_Access_ssp585_i_px)) {
      if (!is.null(species_2030s_Access_ssp585_i_px[[k]])) {
        file_path222 <- paste0(output_dir222, "habitat_suitability_species_2030s_ssp585_", scientific_name_i, "_fold_", k, ".tif")
        
        terra::writeRaster(species_2030s_Access_ssp585_i_px[[k]], filename = file_path222, overwrite = TRUE)
        
        cat("Saved:", file_path222, "\n")
      } else {
        cat("No valid prediction for fold", k, "\n")
      }
    }
    
    
    # Step 1: Keep only non-NULL raster predictions
    valid_indices111 <- which(!sapply(species_2030s_Access_ssp585_i_px, is.null))
    species_i_px_valid111 <- species_2030s_Access_ssp585_i_px[valid_indices111]
    
    # Step 2: Calculate AUC weights
    auc_weights_valid111 <- (avg_auc - 0.5)^2
    auc_weights_valid111 <- rep(auc_weights_valid111, length(valid_indices111))
    
    # Step 3: Stack the valid SpatRaster predictions
    species_2030s_Access_ssp585_i_px_stack <- rast(species_i_px_valid111)
    
    # Check if there is only one valid prediction layer
    if (nlyr(species_2030s_Access_ssp585_i_px_stack) == 1) {
      species_2030s_Access_ssp585_i_px_avg <- species_2030s_Access_ssp585_i_px_stack
      cat("Only one valid prediction layer — using it as the final habitat suitability surface.\n")
    } else {
      species_2030s_Access_ssp585_i_px_avg <- app(species_2030s_Access_ssp585_i_px_stack, fun = function(x) {
        if (all(is.na(x))) {
          return(NA)
        } else {
          return(weighted.mean(x, w = auc_weights_valid111, na.rm = TRUE))
        }
      })
      cat("Weighted average prediction surface computed for", scientific_name_i, "\n")
    }
    
    plot(species_2030s_Access_ssp585_i_px_avg, main = paste("Average Habitat Suitability 2030s Access ssp585", scientific_name_i))
    
    
    species_2030s_Access_ssp585_predictions_avg <- list()
    species_2030s_Access_ssp585_predictions_avg[[scientific_name_i]] <- species_2030s_Access_ssp585_i_px_avg
    names(species_2030s_Access_ssp585_predictions_avg)
    
    # Define output path for the average suitability raster
    output_dir333 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/from_2030/range_full/ssp585/average"
    dir.create(output_dir333, recursive = TRUE, showWarnings = FALSE)
    avg_suitability_path333 <- file.path(output_dir333, paste0("weighted_avg_habitat_suitability_2030s_ssp585_", scientific_name_i, ".tif"))
    
    # Save the weighted average raster
    if (exists("species_2030s_Access_ssp585_i_px_avg")) {
      
      terra::writeRaster(species_2030s_Access_ssp585_i_px_avg, filename = avg_suitability_path333, overwrite = TRUE)
      cat("Saved weighted average habitat suitability 2030s ssp585 for", scientific_name_i, "\n")
      
      species_2030s_Access_ssp585_predictions_avg[[scientific_name_i]] <- species_2030s_Access_ssp585_i_px_avg
      
    } else {
      cat("species_2030s_Access_ssp585_i_px_avg not found — skipping raster save for", scientific_name_i, "\n")
    }
    
    
    # ---------- Step 5: Create and Save Binary Suitability Raster ----------
    if (!is.na(species_i_threshold)) {
      species_i_binary_2030s_Access_ssp585 <- species_2030s_Access_ssp585_i_px_avg > species_i_threshold
      
      plot(species_i_binary_2030s_Access_ssp585, main = paste("Binary Suitability 2030s Access ssp585 for", scientific_name_i))
      
      species_dir111 <- file.path("F:/WWF_data/SDMs_LC_climate/results/other_mammals/from_2030/range_full/ssp585", gsub(" ", "_", scientific_name_i))
      dir.create(file.path(species_dir111, "binary"), showWarnings = FALSE, recursive = TRUE)
      binary111 <- file.path(species_dir111, "binary", paste0(scientific_name_i, "_mean_pixel_binary_2030s_ssp585.tif"))
      writeRaster(
        species_i_binary_2030s_Access_ssp585,
        filename = binary111,
        overwrite = TRUE
      )
      
      cat("Binary habitat raster saved successfully for", scientific_name_i, "\n")
    } else {
      cat("Binary raster not created — threshold was NA for", scientific_name_i, "\n")
    }
    
    
    
    # A. ---------2041-2060-------------
    
    #------------2041-2060_ssp126--------------
    
    # Stack all environmental rasters (raster stack)
    # Stack all environmental rasters (raster stack)
    environmental_var_stack_2050_ssp126 <- raster::stack(MAT_2050_ssp126, MAP_2050_ssp126,
                                                         forest_2050_ssp126, cropland_2050_ssp126,
                                                         urban_2050_ssp126, grassland_2050_ssp126,
                                                         barren_2050_ssp126, water_2050_ssp126,
                                                         elevation, slope, oforest_2050_ssp126,
                                                         wetland_2050_ssp126)
    environmental_var_stack_2050_ssp126
    
    plot(environmental_var_stack_2050_ssp126)
    
    
    names(environmental_var_stack_2050_ssp126) <- c("temp", "prec", "forest", "cropland", "urban", 
                                                    "grassland", "barren", "water", 
                                                    "elevation", "slope", "oforest", "wetland")
    
    
    
    environmental_var_stack_2050_ssp126 <- terra::rast(environmental_var_stack_2050_ssp126)
    
    species_2050s_Access_ssp126_i_px <- list()
    
    # Predict habitat suitability for the test data
    species_2050s_Access_ssp126_i_px[[k]] <- predict(environmental_var_stack_2050_ssp126, 
                                                     species_i_maxent_models[[k]], 
                                                     ext = ext, 
                                                     na.rm = TRUE,
                                                     progress = '') # suppresses the display of the progress bar during model prediction (an empty string means no progress bar will be shown).
    
    plot(species_2050s_Access_ssp126_i_px[[k]], main = paste("Predicted climatic Habitat Suitability 2020 Access ssp126", scientific_name_i))
    
    
    # Define the directory where the prediction files will be saved
    output_dir2 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/2050/range_full/ssp126"
    
    # Ensure the directory exists
    if (!dir.exists(output_dir2)) {
      dir.create(output_dir2, recursive = TRUE)  # Create the directory if it doesn't exist
    }
    
    for (k in seq_along(species_2050s_Access_ssp126_i_px)) {
      if (!is.null(species_2050s_Access_ssp126_i_px[[k]])) {
        # Define the file path for each fold
        file_path2 <- paste0(output_dir2, "habitat_suitability_species_2050s_ssp126_", scientific_name_i, "_fold_", k, ".tif")
        
        # Save the raster as a TIFF file
        terra::writeRaster(species_2050s_Access_ssp126_i_px[[k]], filename = file_path2, overwrite = TRUE)
        
        # Print confirmation message
        cat("Saved:", file_path2, "\n")
      } else {
        cat("No valid prediction for fold", k, "\n")
      }
    }
    
    
    # Step 1: Keep only non-NULL raster predictions
    valid_indices1 <- which(!sapply(species_2050s_Access_ssp126_i_px, is.null))
    species_i_px_valid1 <- species_2050s_Access_ssp126_i_px[valid_indices1]
    
    # Step 2: Calculate AUC weights based on the average AUC across all folds
    # Adjust AUC weights using the average AUC value calculated earlier (avg_auc)
    auc_weights_valid1 <- (avg_auc - 0.5)^2  # Use avg_auc to compute weights
    auc_weights_valid1 <- rep(auc_weights_valid1, length(valid_indices1))  # Replicate for each valid prediction
    
    # Step 3: Stack the valid SpatRaster predictions
    species_2050s_Access_ssp126_i_px_stack <- rast(species_i_px_valid1)
    
    # Check if there is only one valid prediction layer
    if (nlyr(species_2050s_Access_ssp126_i_px_stack) == 1) {
      species_2050s_Access_ssp126_i_px_avg <- species_2050s_Access_ssp126_i_px_stack
      cat("Only one valid prediction layer — using it as the final habitat suitability surface.\n")
    } else {
      # Calculate the weighted average habitat suitability surface
      species_2050s_Access_ssp126_i_px_avg <- app(species_2050s_Access_ssp126_i_px_stack, fun = function(x) {
        if (all(is.na(x))) {
          return(NA)
        } else {
          return(weighted.mean(x, w = auc_weights_valid1, na.rm = TRUE))
        }
      })
      cat("Weighted average prediction surface computed for", scientific_name_i, "\n")
    }
    
    plot(species_2050s_Access_ssp126_i_px_avg, main = paste("Average Habitat Suitability 2020s ssp126", scientific_name_i))
    
    
    species_2050s_Access_ssp126_predictions_avg <- list()
    species_2050s_Access_ssp126_predictions_avg[[scientific_name_i]] <- species_2050s_Access_ssp126_i_px_avg
    names(species_2050s_Access_ssp126_predictions_avg)
    
    
    # Define output path for the average suitability raster
    output_dir3 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/2050/range_full/ssp126/average"
    dir.create(output_dir3, recursive = TRUE, showWarnings = FALSE)
    avg_suitability_path3 <- file.path(output_dir3, paste0("weighted_avg_habitat_suitability_2050s_ssp126_", scientific_name_i, ".tif"))
    
    # Save the weighted average raster (SpatRaster object)
    if (exists("species_2050s_Access_ssp126_i_px_avg")) {
      
      # Save to disk
      terra::writeRaster(species_2050s_Access_ssp126_i_px_avg, filename = avg_suitability_path3, overwrite = TRUE)
      cat("Saved weighted average habitat suitability 2050s Access ssp126 for", scientific_name_i, "\n")
      
      # Store in named list
      species_2050s_Access_ssp126_predictions_avg[[scientific_name_i]] <- species_2050s_Access_ssp126_i_px_avg
      
    } else {
      cat("species_2050s_Access_ssp126_i_px_avg not found — skipping raster save for", scientific_name_i, "\n")
    }
    
    
    # ---------- Step 5: Create and Save Binary Suitability Raster ----------
    if (!is.na(species_i_threshold)) {
      species_i_binary_2050s_Access_ssp126 <- species_2050s_Access_ssp126_i_px_avg > species_i_threshold
      
      # plot
      plot(species_i_binary_2050s_Access_ssp126, main = paste("Binary Suitability 2050s ssp126 for", scientific_name_i))
      
      species_dir1 <- file.path("F:/WWF_data/SDMs_LC_climate/results/other_mammals/2050/range_full/ssp126", gsub(" ", "_", scientific_name_i))
      dir.create(file.path(species_dir1, "binary"), showWarnings = FALSE, recursive = TRUE)
      binary1 <- file.path(species_dir1, "binary", paste0(scientific_name_i, "_mean_pixel_binary_2050s_ssp126.tif"))
      writeRaster(
        species_i_binary_2050s_Access_ssp126,
        filename = binary1,
        overwrite = TRUE
      )
      
      cat("Binary habitat raster saved successfully for", scientific_name_i, "\n")
    } else {
      cat("Binary raster not created — threshold was NA for", scientific_name_i, "\n")
    }
    
    
    
    # ---------2021-2040-ssp245-------------
    
    environmental_var_stack_2050_ssp245 <- raster::stack(MAT_2050_ssp245, MAP_2050_ssp245,
                                                         forest_2050_ssp245, cropland_2050_ssp245,
                                                         urban_2050_ssp245, grassland_2050_ssp245,
                                                         barren_2050_ssp245, water_2050_ssp245,
                                                         elevation, slope, oforest_2050_ssp245,
                                                         wetland_2050_ssp245)
    environmental_var_stack_2050_ssp245
    
    plot(environmental_var_stack_2050_ssp245)
    
    
    names(environmental_var_stack_2050_ssp245) <- c("temp", "prec", "forest", "cropland", "urban", 
                                                    "grassland", "barren", "water", 
                                                    "elevation", "slope", "oforest", "wetland")
    
    
    environmental_var_stack_2050_ssp245 <- terra::rast(environmental_var_stack_2050_ssp245)
    
    species_2050s_Access_ssp245_i_px <- list()
    
    # Predict habitat suitability for the test data
    species_2050s_Access_ssp245_i_px[[k]] <- predict(environmental_var_stack_2050_ssp245, 
                                                     species_i_maxent_models[[k]], 
                                                     ext = ext, 
                                                     na.rm = TRUE,
                                                     progress = '') # suppresses the display of the progress bar during model prediction (an empty string means no progress bar will be shown).
    
    plot(species_2050s_Access_ssp245_i_px[[k]], main = paste("Predicted climatic Habitat Suitability 2050 Access ssp245", scientific_name_i))
    
    
    # Define the directory where the prediction files will be saved
    output_dir22 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/2050/range_full/ssp245"
    
    # Ensure the directory exists
    if (!dir.exists(output_dir22)) {
      dir.create(output_dir22, recursive = TRUE)  # Create the directory if it doesn't exist
    }
    
    for (k in seq_along(species_2050s_Access_ssp245_i_px)) {
      if (!is.null(species_2050s_Access_ssp245_i_px[[k]])) {
        # Define the file path for each fold
        file_path22 <- paste0(output_dir22, "habitat_suitability_species_2050s_ssp245_", scientific_name_i, "_fold_", k, ".tif")
        
        # Save the raster as a TIFF file
        terra::writeRaster(species_2050s_Access_ssp245_i_px[[k]], filename = file_path22, overwrite = TRUE)
        
        # Print confirmation message
        cat("Saved:", file_path22, "\n")
      } else {
        cat("No valid prediction for fold", k, "\n")
      }
    }
    
    
    # Step 1: Keep only non-NULL raster predictions
    valid_indices11 <- which(!sapply(species_2050s_Access_ssp245_i_px, is.null))
    species_i_px_valid11 <- species_2050s_Access_ssp245_i_px[valid_indices11]
    
    # Step 2: Calculate AUC weights based on the average AUC across all folds
    # Adjust AUC weights using the average AUC value calculated earlier (avg_auc)
    auc_weights_valid11 <- (avg_auc - 0.5)^2  # Use avg_auc to compute weights
    auc_weights_valid11 <- rep(auc_weights_valid11, length(valid_indices11))  # Replicate for each valid prediction
    
    # Step 3: Stack the valid SpatRaster predictions
    species_2050s_Access_ssp245_i_px_stack <- rast(species_i_px_valid11)
    
    # Check if there is only one valid prediction layer
    if (nlyr(species_2050s_Access_ssp245_i_px_stack) == 1) {
      species_2050s_Access_ssp245_i_px_avg <- species_2050s_Access_ssp245_i_px_stack
      cat("Only one valid prediction layer — using it as the final habitat suitability surface.\n")
    } else {
      # Calculate the weighted average habitat suitability surface
      species_2050s_Access_ssp245_i_px_avg <- app(species_2050s_Access_ssp245_i_px_stack, fun = function(x) {
        if (all(is.na(x))) {
          return(NA)
        } else {
          return(weighted.mean(x, w = auc_weights_valid11, na.rm = TRUE))
        }
      })
      cat("Weighted average prediction surface computed for", scientific_name_i, "\n")
    }
    
    plot(species_2050s_Access_ssp245_i_px_avg, main = paste("Average Habitat Suitability 2050s Access ssp245", scientific_name_i))
    
    
    species_2050s_Access_ssp245_predictions_avg <- list()
    species_2050s_Access_ssp245_predictions_avg[[scientific_name_i]] <- species_2050s_Access_ssp245_i_px_avg
    names(species_2050s_Access_ssp245_predictions_avg)
    
    
    # Define output path for the average suitability raster
    output_dir33 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/2050/range_full/ssp245/average"
    dir.create(output_dir33, recursive = TRUE, showWarnings = FALSE)
    avg_suitability_path33 <- file.path(output_dir33, paste0("weighted_avg_habitat_suitability_2050s_ssp245_", scientific_name_i, ".tif"))
    
    # Save the weighted average raster (SpatRaster object)
    if (exists("species_2050s_Access_ssp245_i_px_avg")) {
      
      # Save to disk
      terra::writeRaster(species_2050s_Access_ssp245_i_px_avg, filename = avg_suitability_path33, overwrite = TRUE)
      cat("Saved weighted average habitat suitability 2050s Access ssp245 for", scientific_name_i, "\n")
      
      # Store in named list
      species_2050s_Access_ssp245_predictions_avg[[scientific_name_i]] <- species_2050s_Access_ssp245_i_px_avg
      
    } else {
      cat("species_2050s_Access_ssp245_i_px_avg not found — skipping raster save for", scientific_name_i, "\n")
    }
    
    
    # ---------- Step 5: Create and Save Binary Suitability Raster ----------
    if (!is.na(species_i_threshold)) {
      species_i_binary_2050s_Access_ssp245 <- species_2050s_Access_ssp245_i_px_avg > species_i_threshold
      
      # plot
      plot(species_i_binary_2050s_Access_ssp245, main = paste("Binary Suitability 2050s Access ssp245 for", scientific_name_i))
      
      species_dir11 <- file.path("F:/WWF_data/SDMs_LC_climate/results/other_mammals/2050/range_full/ssp245", gsub(" ", "_", scientific_name_i))
      dir.create(file.path(species_dir11, "binary"), showWarnings = FALSE, recursive = TRUE)
      binary11 <- file.path(species_dir11, "binary", paste0(scientific_name_i, "_mean_pixel_binary_2050s_ssp245.tif"))
      writeRaster(
        species_i_binary_2050s_Access_ssp245,
        filename = binary11,
        overwrite = TRUE
      )
      
      cat("Binary habitat raster saved successfully for", scientific_name_i, "\n")
    } else {
      cat("Binary raster not created — threshold was NA for", scientific_name_i, "\n")
    }
    
    
    
    # ---------2041-2060-ssp585-------------
    
    # Stack all environmental rasters (raster stack)
    environmental_var_stack_2050_ssp585 <- raster::stack(MAT_2050_ssp585, MAP_2050_ssp585, forest_2050_ssp585,
                                                         cropland_2050_ssp585, urban_2050_ssp585,
                                                         grassland_2050_ssp585, barren_2050_ssp585,
                                                         water_2050_ssp585, elevation, slope,
                                                         oforest_2050_ssp585, wetland_2050_ssp585)
    environmental_var_stack_2050_ssp585
    
    plot(environmental_var_stack_2050_ssp585)
    
    
    names(environmental_var_stack_2050_ssp585) <- c("temp", "prec", "forest", "cropland", "urban", 
                                                    "grassland", "barren", "water", 
                                                    "elevation", "slope", "oforest", "wetland")
    
    
    environmental_var_stack_2050_ssp585 <- terra::rast(environmental_var_stack_2050_ssp585)
    
    species_2050s_Access_ssp585_i_px <- list()
    
    # Predict habitat suitability for the test data
    species_2050s_Access_ssp585_i_px[[k]] <- predict(environmental_var_stack_2050_ssp585, 
                                                     species_i_maxent_models[[k]], 
                                                     ext = ext, 
                                                     na.rm = TRUE,
                                                     progress = '') # suppresses the display of the progress bar during model prediction (an empty string means no progress bar will be shown).
    
    plot(species_2050s_Access_ssp585_i_px[[k]], main = paste("Predicted climatic Habitat Suitability 2050 Access ssp585", scientific_name_i))
    
    # Define the directory where the prediction files will be saved
    output_dir222 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/2050/range_full/ssp585"
    
    # Ensure the directory exists
    if (!dir.exists(output_dir222)) {
      dir.create(output_dir222, recursive = TRUE)  # Create the directory if it doesn't exist
    }
    
    for (k in seq_along(species_2050s_Access_ssp585_i_px)) {
      if (!is.null(species_2050s_Access_ssp585_i_px[[k]])) {
        # Define the file path for each fold
        file_path222 <- paste0(output_dir222, "habitat_suitability_species_2050s_ssp585_", scientific_name_i, "_fold_", k, ".tif")
        
        # Save the raster as a TIFF file
        terra::writeRaster(species_2050s_Access_ssp585_i_px[[k]], filename = file_path222, overwrite = TRUE)
        
        # Print confirmation message
        cat("Saved:", file_path222, "\n")
      } else {
        cat("No valid prediction for fold", k, "\n")
      }
    }
    
    
    # Step 1: Keep only non-NULL raster predictions
    valid_indices111 <- which(!sapply(species_2050s_Access_ssp585_i_px, is.null))
    species_i_px_valid111 <- species_2050s_Access_ssp585_i_px[valid_indices111]
    
    # Step 2: Calculate AUC weights based on the average AUC across all folds
    # Adjust AUC weights using the average AUC value calculated earlier (avg_auc)
    auc_weights_valid111 <- (avg_auc - 0.5)^2  # Use avg_auc to compute weights
    auc_weights_valid111 <- rep(auc_weights_valid111, length(valid_indices111))  # Replicate for each valid prediction
    
    # Step 3: Stack the valid SpatRaster predictions
    species_2050s_Access_ssp585_i_px_stack <- rast(species_i_px_valid111)
    
    # Check if there is only one valid prediction layer
    if (nlyr(species_2050s_Access_ssp585_i_px_stack) == 1) {
      species_2050s_Access_ssp585_i_px_avg <- species_2050s_Access_ssp585_i_px_stack
      cat("Only one valid prediction layer — using it as the final habitat suitability surface.\n")
    } else {
      # Calculate the weighted average habitat suitability surface
      species_2050s_Access_ssp585_i_px_avg <- app(species_2050s_Access_ssp585_i_px_stack, fun = function(x) {
        if (all(is.na(x))) {
          return(NA)
        } else {
          return(weighted.mean(x, w = auc_weights_valid111, na.rm = TRUE))
        }
      })
      cat("Weighted average prediction surface computed for", scientific_name_i, "\n")
    }
    
    plot(species_2050s_Access_ssp585_i_px_avg, main = paste("Average Habitat Suitability 2050s Access ssp585", scientific_name_i))
    
    
    species_2050s_Access_ssp585_predictions_avg <- list()
    species_2050s_Access_ssp585_predictions_avg[[scientific_name_i]] <- species_2050s_Access_ssp585_i_px_avg
    names(species_2050s_Access_ssp585_predictions_avg)
    
    # Define output path for the average suitability raster
    output_dir333 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/2050/range_full/ssp585/average"
    dir.create(output_dir333, recursive = TRUE, showWarnings = FALSE)
    avg_suitability_path333 <- file.path(output_dir333, paste0("weighted_avg_habitat_suitability_2050s_ssp585_", scientific_name_i, ".tif"))
    
    # Save the weighted average raster (SpatRaster object)
    if (exists("species_2050s_Access_ssp585_i_px_avg")) {
      
      # Save to disk
      terra::writeRaster(species_2050s_Access_ssp585_i_px_avg, filename = avg_suitability_path333, overwrite = TRUE)
      cat("Saved weighted average habitat suitability 2050s ssp585 for", scientific_name_i, "\n")
      
      # Store in named list
      species_2050s_Access_ssp585_predictions_avg[[scientific_name_i]] <- species_2050s_Access_ssp585_i_px_avg
      
    } else {
      cat("species_2050s_Access_ssp585_i_px_avg not found — skipping raster save for", scientific_name_i, "\n")
    }
    
    
    # ---------- Step 5: Create and Save Binary Suitability Raster ----------
    if (!is.na(species_i_threshold)) {
      species_i_binary_2050s_Access_ssp585 <- species_2050s_Access_ssp585_i_px_avg > species_i_threshold
      
      # plot
      plot(species_i_binary_2050s_Access_ssp585, main = paste("Binary Suitability 2050s Access ssp585 for", scientific_name_i))
      
      species_dir111 <- file.path("F:/WWF_data/SDMs_LC_climate/results/other_mammals/2050/range_full/ssp585", gsub(" ", "_", scientific_name_i))
      dir.create(file.path(species_dir111, "binary"), showWarnings = FALSE, recursive = TRUE)
      binary111 <- file.path(species_dir111, "binary", paste0(scientific_name_i, "_mean_pixel_binary_2050s_ssp585.tif"))
      writeRaster(
        species_i_binary_2050s_Access_ssp585,
        filename = binary111,
        overwrite = TRUE
      )
      
      cat("Binary habitat raster saved successfully for", scientific_name_i, "\n")
    } else {
      cat("Binary raster not created — threshold was NA for", scientific_name_i, "\n")
    }
    
    
    #------------------------------2061 - 2080-----------------------------
    # ssp126
    
    # Stack all environmental rasters (raster stack)
    # Stack all environmental rasters (raster stack)
    environmental_var_stack_2070_ssp126 <- raster::stack(MAT_2070_ssp126, MAP_2070_ssp126,
                                                         forest_2070_ssp126, cropland_2070_ssp126,
                                                         urban_2070_ssp126, grassland_2070_ssp126,
                                                         barren_2070_ssp126, water_2070_ssp126,
                                                         elevation, slope, oforest_2070_ssp126,
                                                         wetland_2070_ssp126)
    environmental_var_stack_2070_ssp126
    
    plot(environmental_var_stack_2070_ssp126)
    
    
    names(environmental_var_stack_2070_ssp126) <- c("temp", "prec", "forest", "cropland", "urban", 
                                                    "grassland", "barren", "water", 
                                                    "elevation", "slope", "oforest", "wetland")
    
    
    
    environmental_var_stack_2070_ssp126 <- terra::rast(environmental_var_stack_2070_ssp126)
    
    species_2070s_Access_ssp126_i_px <- list()
    
    # Predict habitat suitability for the test data
    species_2070s_Access_ssp126_i_px[[k]] <- predict(environmental_var_stack_2070_ssp126, 
                                                     species_i_maxent_models[[k]], 
                                                     ext = ext, 
                                                     na.rm = TRUE,
                                                     progress = '')
    
    plot(species_2070s_Access_ssp126_i_px[[k]], main = paste("Predicted climatic Habitat Suitability 2020 Access ssp126", scientific_name_i))
    
    
    # Define the directory where the prediction files will be saved
    output_dir2 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/2070/range_full/ssp126"
    
    # Ensure the directory exists
    if (!dir.exists(output_dir2)) {
      dir.create(output_dir2, recursive = TRUE)
    }
    
    for (k in seq_along(species_2070s_Access_ssp126_i_px)) {
      if (!is.null(species_2070s_Access_ssp126_i_px[[k]])) {
        file_path2 <- paste0(output_dir2, "habitat_suitability_species_2070s_ssp126_", scientific_name_i, "_fold_", k, ".tif")
        
        terra::writeRaster(species_2070s_Access_ssp126_i_px[[k]], filename = file_path2, overwrite = TRUE)
        
        cat("Saved:", file_path2, "\n")
      } else {
        cat("No valid prediction for fold", k, "\n")
      }
    }
    
    
    # Step 1: Keep only non-NULL raster predictions
    valid_indices1 <- which(!sapply(species_2070s_Access_ssp126_i_px, is.null))
    species_i_px_valid1 <- species_2070s_Access_ssp126_i_px[valid_indices1]
    
    # Step 2: Calculate AUC weights based on the average AUC across all folds
    auc_weights_valid1 <- (avg_auc - 0.5)^2
    auc_weights_valid1 <- rep(auc_weights_valid1, length(valid_indices1))
    
    # Step 3: Stack the valid SpatRaster predictions
    species_2070s_Access_ssp126_i_px_stack <- rast(species_i_px_valid1)
    
    # Check if there is only one valid prediction layer
    if (nlyr(species_2070s_Access_ssp126_i_px_stack) == 1) {
      species_2070s_Access_ssp126_i_px_avg <- species_2070s_Access_ssp126_i_px_stack
      cat("Only one valid prediction layer — using it as the final habitat suitability surface.\n")
    } else {
      species_2070s_Access_ssp126_i_px_avg <- app(species_2070s_Access_ssp126_i_px_stack, fun = function(x) {
        if (all(is.na(x))) {
          return(NA)
        } else {
          return(weighted.mean(x, w = auc_weights_valid1, na.rm = TRUE))
        }
      })
      cat("Weighted average prediction surface computed for", scientific_name_i, "\n")
    }
    
    plot(species_2070s_Access_ssp126_i_px_avg, main = paste("Average Habitat Suitability 2020s ssp126", scientific_name_i))
    
    
    species_2070s_Access_ssp126_predictions_avg <- list()
    species_2070s_Access_ssp126_predictions_avg[[scientific_name_i]] <- species_2070s_Access_ssp126_i_px_avg
    names(species_2070s_Access_ssp126_predictions_avg)
    
    
    # Define output path for the average suitability raster
    output_dir3 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/2070/range_full/ssp126/average"
    dir.create(output_dir3, recursive = TRUE, showWarnings = FALSE)
    avg_suitability_path3 <- file.path(output_dir3, paste0("weighted_avg_habitat_suitability_2070s_ssp126_", scientific_name_i, ".tif"))
    
    # Save the weighted average raster (SpatRaster object)
    if (exists("species_2070s_Access_ssp126_i_px_avg")) {
      
      terra::writeRaster(species_2070s_Access_ssp126_i_px_avg, filename = avg_suitability_path3, overwrite = TRUE)
      cat("Saved weighted average habitat suitability 2070s Access ssp126 for", scientific_name_i, "\n")
      
      species_2070s_Access_ssp126_predictions_avg[[scientific_name_i]] <- species_2070s_Access_ssp126_i_px_avg
      
    } else {
      cat("species_2070s_Access_ssp126_i_px_avg not found — skipping raster save for", scientific_name_i, "\n")
    }
    
    
    # ---------- Step 5: Create and Save Binary Suitability Raster ----------
    if (!is.na(species_i_threshold)) {
      species_i_binary_2070s_Access_ssp126 <- species_2070s_Access_ssp126_i_px_avg > species_i_threshold
      
      plot(species_i_binary_2070s_Access_ssp126, main = paste("Binary Suitability 2070s ssp126 for", scientific_name_i))
      
      species_dir1 <- file.path("F:/WWF_data/SDMs_LC_climate/results/other_mammals/2070/range_full/ssp126", gsub(" ", "_", scientific_name_i))
      dir.create(file.path(species_dir1, "binary"), showWarnings = FALSE, recursive = TRUE)
      binary1 <- file.path(species_dir1, "binary", paste0(scientific_name_i, "_mean_pixel_binary_2070s_ssp126.tif"))
      writeRaster(
        species_i_binary_2070s_Access_ssp126,
        filename = binary1,
        overwrite = TRUE
      )
      
      cat("Binary habitat raster saved successfully for", scientific_name_i, "\n")
    } else {
      cat("Binary raster not created — threshold was NA for", scientific_name_i, "\n")
    }
    
    
    #--------------ssp245
    
    environmental_var_stack_2070_ssp245 <- raster::stack(MAT_2070_ssp245, MAP_2070_ssp245,
                                                         forest_2070_ssp245, cropland_2070_ssp245,
                                                         urban_2070_ssp245, grassland_2070_ssp245,
                                                         barren_2070_ssp245, water_2070_ssp245,
                                                         elevation, slope, oforest_2070_ssp245,
                                                         wetland_2070_ssp245)
    environmental_var_stack_2070_ssp245
    
    plot(environmental_var_stack_2070_ssp245)
    
    
    names(environmental_var_stack_2070_ssp245) <- c("temp", "prec", "forest", "cropland", "urban", 
                                                    "grassland", "barren", "water", 
                                                    "elevation", "slope", "oforest", "wetland")
    
    
    environmental_var_stack_2070_ssp245 <- terra::rast(environmental_var_stack_2070_ssp245)
    
    species_2070s_Access_ssp245_i_px <- list()
    
    # Predict habitat suitability for the test data
    species_2070s_Access_ssp245_i_px[[k]] <- predict(environmental_var_stack_2070_ssp245, 
                                                     species_i_maxent_models[[k]], 
                                                     ext = ext, 
                                                     na.rm = TRUE,
                                                     progress = '')
    
    plot(species_2070s_Access_ssp245_i_px[[k]], main = paste("Predicted climatic Habitat Suitability 2070 Access ssp245", scientific_name_i))
    
    
    # Define the directory where the prediction files will be saved
    output_dir22 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/2070/range_full/ssp245"
    
    # Ensure the directory exists
    if (!dir.exists(output_dir22)) {
      dir.create(output_dir22, recursive = TRUE)
    }
    
    for (k in seq_along(species_2070s_Access_ssp245_i_px)) {
      if (!is.null(species_2070s_Access_ssp245_i_px[[k]])) {
        file_path22 <- paste0(output_dir22, "habitat_suitability_species_2070s_ssp245_", scientific_name_i, "_fold_", k, ".tif")
        
        terra::writeRaster(species_2070s_Access_ssp245_i_px[[k]], filename = file_path22, overwrite = TRUE)
        
        cat("Saved:", file_path22, "\n")
      } else {
        cat("No valid prediction for fold", k, "\n")
      }
    }
    
    
    # Step 1: Keep only non-NULL raster predictions
    valid_indices11 <- which(!sapply(species_2070s_Access_ssp245_i_px, is.null))
    species_i_px_valid11 <- species_2070s_Access_ssp245_i_px[valid_indices11]
    
    # Step 2: Calculate AUC weights based on the average AUC across all folds
    auc_weights_valid11 <- (avg_auc - 0.5)^2
    auc_weights_valid11 <- rep(auc_weights_valid11, length(valid_indices11))
    
    # Step 3: Stack the valid SpatRaster predictions
    species_2070s_Access_ssp245_i_px_stack <- rast(species_i_px_valid11)
    
    # Check if there is only one valid prediction layer
    if (nlyr(species_2070s_Access_ssp245_i_px_stack) == 1) {
      species_2070s_Access_ssp245_i_px_avg <- species_2070s_Access_ssp245_i_px_stack
      cat("Only one valid prediction layer — using it as the final habitat suitability surface.\n")
    } else {
      species_2070s_Access_ssp245_i_px_avg <- app(species_2070s_Access_ssp245_i_px_stack, fun = function(x) {
        if (all(is.na(x))) {
          return(NA)
        } else {
          return(weighted.mean(x, w = auc_weights_valid11, na.rm = TRUE))
        }
      })
      cat("Weighted average prediction surface computed for", scientific_name_i, "\n")
    }
    
    plot(species_2070s_Access_ssp245_i_px_avg, main = paste("Average Habitat Suitability 2070s Access ssp245", scientific_name_i))
    
    
    species_2070s_Access_ssp245_predictions_avg <- list()
    species_2070s_Access_ssp245_predictions_avg[[scientific_name_i]] <- species_2070s_Access_ssp245_i_px_avg
    names(species_2070s_Access_ssp245_predictions_avg)
    
    
    # Define output path for the average suitability raster
    output_dir33 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/2070/range_full/ssp245/average"
    dir.create(output_dir33, recursive = TRUE, showWarnings = FALSE)
    avg_suitability_path33 <- file.path(output_dir33, paste0("weighted_avg_habitat_suitability_2070s_ssp245_", scientific_name_i, ".tif"))
    
    # Save the weighted average raster (SpatRaster object)
    if (exists("species_2070s_Access_ssp245_i_px_avg")) {
      
      terra::writeRaster(species_2070s_Access_ssp245_i_px_avg, filename = avg_suitability_path33, overwrite = TRUE)
      cat("Saved weighted average habitat suitability 2070s Access ssp245 for", scientific_name_i, "\n")
      
      species_2070s_Access_ssp245_predictions_avg[[scientific_name_i]] <- species_2070s_Access_ssp245_i_px_avg
      
    } else {
      cat("species_2070s_Access_ssp245_i_px_avg not found — skipping raster save for", scientific_name_i, "\n")
    }
    
    
    # ---------- Step 5: Create and Save Binary Suitability Raster ----------
    if (!is.na(species_i_threshold)) {
      species_i_binary_2070s_Access_ssp245 <- species_2070s_Access_ssp245_i_px_avg > species_i_threshold
      
      plot(species_i_binary_2070s_Access_ssp245, main = paste("Binary Suitability 2070s Access ssp245 for", scientific_name_i))
      
      species_dir11 <- file.path("F:/WWF_data/SDMs_LC_climate/results/other_mammals/2070/range_full/ssp245", gsub(" ", "_", scientific_name_i))
      dir.create(file.path(species_dir11, "binary"), showWarnings = FALSE, recursive = TRUE)
      binary11 <- file.path(species_dir11, "binary", paste0(scientific_name_i, "_mean_pixel_binary_2070s_ssp245.tif"))
      writeRaster(
        species_i_binary_2070s_Access_ssp245,
        filename = binary11,
        overwrite = TRUE
      )
      
      cat("Binary habitat raster saved successfully for", scientific_name_i, "\n")
    } else {
      cat("Binary raster not created — threshold was NA for", scientific_name_i, "\n")
    }
    
    
    # ssp585 ------------------------
    
    # Stack all environmental rasters (raster stack)
    environmental_var_stack_2070_ssp585 <- raster::stack(MAT_2070_ssp585, MAP_2070_ssp585, forest_2070_ssp585,
                                                         cropland_2070_ssp585, urban_2070_ssp585,
                                                         grassland_2070_ssp585, barren_2070_ssp585,
                                                         water_2070_ssp585, elevation, slope,
                                                         oforest_2070_ssp585, wetland_2070_ssp585)
    environmental_var_stack_2070_ssp585
    
    plot(environmental_var_stack_2070_ssp585)
    
    
    names(environmental_var_stack_2070_ssp585) <- c("temp", "prec", "forest", "cropland", "urban", 
                                                    "grassland", "barren", "water", 
                                                    "elevation", "slope", "oforest", "wetland")
    
    
    environmental_var_stack_2070_ssp585 <- terra::rast(environmental_var_stack_2070_ssp585)
    
    species_2070s_Access_ssp585_i_px <- list()
    
    # Predict habitat suitability for the test data
    species_2070s_Access_ssp585_i_px[[k]] <- predict(environmental_var_stack_2070_ssp585, 
                                                     species_i_maxent_models[[k]], 
                                                     ext = ext, 
                                                     na.rm = TRUE,
                                                     progress = '')
    
    plot(species_2070s_Access_ssp585_i_px[[k]], main = paste("Predicted climatic Habitat Suitability 2070 Access ssp585", scientific_name_i))
    
    # Define the directory where the prediction files will be saved
    output_dir222 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/2070/range_full/ssp585"
    
    # Ensure the directory exists
    if (!dir.exists(output_dir222)) {
      dir.create(output_dir222, recursive = TRUE)
    }
    
    for (k in seq_along(species_2070s_Access_ssp585_i_px)) {
      if (!is.null(species_2070s_Access_ssp585_i_px[[k]])) {
        file_path222 <- paste0(output_dir222, "habitat_suitability_species_2070s_ssp585_", scientific_name_i, "_fold_", k, ".tif")
        
        terra::writeRaster(species_2070s_Access_ssp585_i_px[[k]], filename = file_path222, overwrite = TRUE)
        
        cat("Saved:", file_path222, "\n")
      } else {
        cat("No valid prediction for fold", k, "\n")
      }
    }
    
    
    # Step 1: Keep only non-NULL raster predictions
    valid_indices111 <- which(!sapply(species_2070s_Access_ssp585_i_px, is.null))
    species_i_px_valid111 <- species_2070s_Access_ssp585_i_px[valid_indices111]
    
    # Step 2: Calculate AUC weights based on the average AUC across all folds
    auc_weights_valid111 <- (avg_auc - 0.5)^2
    auc_weights_valid111 <- rep(auc_weights_valid111, length(valid_indices111))
    
    # Step 3: Stack the valid SpatRaster predictions
    species_2070s_Access_ssp585_i_px_stack <- rast(species_i_px_valid111)
    
    # Check if there is only one valid prediction layer
    if (nlyr(species_2070s_Access_ssp585_i_px_stack) == 1) {
      species_2070s_Access_ssp585_i_px_avg <- species_2070s_Access_ssp585_i_px_stack
      cat("Only one valid prediction layer — using it as the final habitat suitability surface.\n")
    } else {
      species_2070s_Access_ssp585_i_px_avg <- app(species_2070s_Access_ssp585_i_px_stack, fun = function(x) {
        if (all(is.na(x))) {
          return(NA)
        } else {
          return(weighted.mean(x, w = auc_weights_valid111, na.rm = TRUE))
        }
      })
      cat("Weighted average prediction surface computed for", scientific_name_i, "\n")
    }
    
    plot(species_2070s_Access_ssp585_i_px_avg, main = paste("Average Habitat Suitability 2070s Access ssp585", scientific_name_i))
    
    
    species_2070s_Access_ssp585_predictions_avg <- list()
    species_2070s_Access_ssp585_predictions_avg[[scientific_name_i]] <- species_2070s_Access_ssp585_i_px_avg
    names(species_2070s_Access_ssp585_predictions_avg)
    
    # Define output path for the average suitability raster
    output_dir333 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/2070/range_full/ssp585/average"
    dir.create(output_dir333, recursive = TRUE, showWarnings = FALSE)
    avg_suitability_path333 <- file.path(output_dir333, paste0("weighted_avg_habitat_suitability_2070s_ssp585_", scientific_name_i, ".tif"))
    
    # Save the weighted average raster (SpatRaster object)
    if (exists("species_2070s_Access_ssp585_i_px_avg")) {
      
      terra::writeRaster(species_2070s_Access_ssp585_i_px_avg, filename = avg_suitability_path333, overwrite = TRUE)
      cat("Saved weighted average habitat suitability 2070s ssp585 for", scientific_name_i, "\n")
      
      species_2070s_Access_ssp585_predictions_avg[[scientific_name_i]] <- species_2070s_Access_ssp585_i_px_avg
      
    } else {
      cat("species_2070s_Access_ssp585_i_px_avg not found — skipping raster save for", scientific_name_i, "\n")
    }
    
    
    # ---------- Step 5: Create and Save Binary Suitability Raster ----------
    if (!is.na(species_i_threshold)) {
      species_i_binary_2070s_Access_ssp585 <- species_2070s_Access_ssp585_i_px_avg > species_i_threshold
      
      plot(species_i_binary_2070s_Access_ssp585, main = paste("Binary Suitability 2070s Access ssp585 for", scientific_name_i))
      
      species_dir111 <- file.path("F:/WWF_data/SDMs_LC_climate/results/other_mammals/2070/range_full/ssp585", gsub(" ", "_", scientific_name_i))
      dir.create(file.path(species_dir111, "binary"), showWarnings = FALSE, recursive = TRUE)
      binary111 <- file.path(species_dir111, "binary", paste0(scientific_name_i, "_mean_pixel_binary_2070s_ssp585.tif"))
      writeRaster(
        species_i_binary_2070s_Access_ssp585,
        filename = binary111,
        overwrite = TRUE
      )
      
      cat("Binary habitat raster saved successfully for", scientific_name_i, "\n")
    } else {
      cat("Binary raster not created — threshold was NA for", scientific_name_i, "\n")
    }
    
    
    
    #------------2071-2100_ssp126--------------
    
    # load future envitonmental layers
    
    # Stack all environmental rasters (raster stack)
    environmental_var_stack_2100_ssp126 <- raster::stack(MAT_2100_ssp126, MAP_2100_ssp126,
                                                         forest_2100_ssp126, cropland_2100_ssp126,
                                                         urban_2100_ssp126, grassland_2100_ssp126,
                                                         barren_2100_ssp126, water_2100_ssp126,
                                                         elevation, slope, oforest_2100_ssp126,
                                                         wetland_2100_ssp126)
    environmental_var_stack_2100_ssp126
    
    plot(environmental_var_stack_2100_ssp126)
    
    
    names(environmental_var_stack_2100_ssp126) <- c("temp", "prec", "forest", "cropland", "urban", 
                                                    "grassland", "barren", "water", 
                                                    "elevation", "slope", "oforest", "wetland")
    
    
    environmental_var_stack_2100_ssp126 <- terra::rast(environmental_var_stack_2100_ssp126)
    
    species_2100s_Access_ssp126_i_px <- list()
    
    # Predict habitat suitability for the test data
    species_2100s_Access_ssp126_i_px[[k]] <- predict(environmental_var_stack_2100_ssp126, 
                                                     species_i_maxent_models[[k]], 
                                                     ext = ext, 
                                                     na.rm = TRUE,
                                                     progress = '')
    
    plot(species_2100s_Access_ssp126_i_px[[k]], main = paste("Predicted climatic Habitat Suitability 2020 Access ssp126", scientific_name_i))
    
    # Define the directory where the prediction files will be saved
    output_dir23 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/2100/range_full/ssp126"
    
    # Ensure the directory exists
    if (!dir.exists(output_dir23)) {
      dir.create(output_dir23, recursive = TRUE)
    }
    
    for (k in seq_along(species_2100s_Access_ssp126_i_px)) {
      if (!is.null(species_2100s_Access_ssp126_i_px[[k]])) {
        file_path23 <- paste0(output_dir23, "habitat_suitability_species_2100s_ssp126_", scientific_name_i, "_fold_", k, ".tif")
        
        terra::writeRaster(species_2100s_Access_ssp126_i_px[[k]], filename = file_path23, overwrite = TRUE)
        
        cat("Saved:", file_path23, "\n")
      } else {
        cat("No valid prediction for fold", k, "\n")
      }
    }
    
    
    # Step 1: Keep only non-NULL raster predictions
    valid_indices12 <- which(!sapply(species_2100s_Access_ssp126_i_px, is.null))
    species_i_px_valid12 <- species_2100s_Access_ssp126_i_px[valid_indices12]
    
    # Step 2: Calculate AUC weights
    auc_weights_valid12 <- (avg_auc - 0.5)^2
    auc_weights_valid12 <- rep(auc_weights_valid12, length(valid_indices12))
    
    # Step 3: Stack the valid SpatRaster predictions
    species_2100s_Access_ssp126_i_px_stack <- rast(species_i_px_valid12)
    
    # Check if there is only one valid prediction layer
    if (nlyr(species_2100s_Access_ssp126_i_px_stack) == 1) {
      species_2100s_Access_ssp126_i_px_avg <- species_2100s_Access_ssp126_i_px_stack
      cat("Only one valid prediction layer — using it as the final habitat suitability surface.\n")
    } else {
      species_2100s_Access_ssp126_i_px_avg <- app(species_2100s_Access_ssp126_i_px_stack, fun = function(x) {
        if (all(is.na(x))) {
          return(NA)
        } else {
          return(weighted.mean(x, w = auc_weights_valid12, na.rm = TRUE))
        }
      })
      cat("Weighted average prediction surface computed for", scientific_name_i, "\n")
    }
    
    plot(species_2100s_Access_ssp126_i_px_avg, main = paste("Average Habitat Suitability 2100 ssp126", scientific_name_i))
    
    
    species_2100s_Access_ssp126_predictions_avg <- list()
    species_2100s_Access_ssp126_predictions_avg[[scientific_name_i]] <- species_2100s_Access_ssp126_i_px_avg
    names(species_2100s_Access_ssp126_predictions_avg)
    
    
    # Define output path for the average suitability raster
    output_dir32 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/2100/range_full/ssp126/average"
    dir.create(output_dir32, recursive = TRUE, showWarnings = FALSE)
    avg_suitability_path32 <- file.path(output_dir32, paste0("weighted_avg_habitat_suitability_2100s_ssp126_", scientific_name_i, ".tif"))
    
    # Save the weighted average raster
    if (exists("species_2100s_Access_ssp126_i_px_avg")) {
      
      terra::writeRaster(species_2100s_Access_ssp126_i_px_avg, filename = avg_suitability_path32, overwrite = TRUE)
      cat("Saved weighted average habitat suitability 2100 ssp126 for", scientific_name_i, "\n")
      
      species_2100s_Access_ssp126_predictions_avg[[scientific_name_i]] <- species_2100s_Access_ssp126_i_px_avg
      
    } else {
      cat("species_2100s_Access_ssp126_i_px_avg not found — skipping raster save for", scientific_name_i, "\n")
    }
    
    
    # ---------- Step 5: Create and Save Binary Suitability Raster ----------
    if (!is.na(species_i_threshold)) {
      species_i_binary_2100s_Access_ssp126 <- species_2100s_Access_ssp126_i_px_avg > species_i_threshold
      
      plot(species_i_binary_2100s_Access_ssp126, main = paste("Binary Suitability 2100s Access ssp126 for", scientific_name_i))
      
      species_dir12 <- file.path("F:/WWF_data/SDMs_LC_climate/results/other_mammals/2100/range_full/ssp126", gsub(" ", "_", scientific_name_i))
      dir.create(file.path(species_dir12, "binary"), showWarnings = FALSE, recursive = TRUE)
      binary12 <- file.path(species_dir12, "binary", paste0(scientific_name_i, "_mean_pixel_binary_2100s_ssp126.tif"))
      writeRaster(
        species_i_binary_2100s_Access_ssp126,
        filename = binary12,
        overwrite = TRUE
      )
      
      cat("Binary habitat raster saved successfully for", scientific_name_i, "\n")
    } else {
      cat("Binary raster not created — threshold was NA for", scientific_name_i, "\n")
    }
    
    
    # ---------2081-2100-ssp245-------------
    
    # Stack all environmental rasters (raster stack)
    environmental_var_stack_2100_ssp245 <- raster::stack(MAT_2100_ssp245, MAP_2100_ssp245,
                                                         forest_2100_ssp245, cropland_2100_ssp245,
                                                         urban_2100_ssp245, grassland_2100_ssp245,
                                                         barren_2100_ssp245, water_2100_ssp245,
                                                         elevation, slope, oforest_2100_ssp245,
                                                         wetland_2100_ssp245)
    environmental_var_stack_2100_ssp245
    
    plot(environmental_var_stack_2100_ssp245)
    
    names(environmental_var_stack_2100_ssp245) <- c("temp", "prec", "forest", "cropland", "urban", 
                                                    "grassland", "barren", "water", 
                                                    "elevation", "slope", "oforest", "wetland")
    
    environmental_var_stack_2100_ssp245 <- terra::rast(environmental_var_stack_2100_ssp245)
    
    species_2100s_Access_ssp245_i_px <- list()
    
    # Predict habitat suitability for the test data
    species_2100s_Access_ssp245_i_px[[k]] <- predict(environmental_var_stack_2100_ssp245, 
                                                     species_i_maxent_models[[k]], 
                                                     ext = ext, 
                                                     na.rm = TRUE,
                                                     progress = '')
    
    plot(species_2100s_Access_ssp245_i_px[[k]], main = paste("Predicted climatic Habitat Suitability 2050 Access ssp245", scientific_name_i))
    
    # Define the directory where the prediction files will be saved
    output_dir223 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/2100/range_full/ssp245"
    
    # Ensure the directory exists
    if (!dir.exists(output_dir223)) {
      dir.create(output_dir223, recursive = TRUE)
    }
    
    for (k in seq_along(species_2100s_Access_ssp245_i_px)) {
      if (!is.null(species_2100s_Access_ssp245_i_px[[k]])) {
        file_path223 <- paste0(output_dir223, "habitat_suitability_species_2100s_ssp245_", scientific_name_i, "_fold_", k, ".tif")
        
        terra::writeRaster(species_2100s_Access_ssp245_i_px[[k]], filename = file_path223, overwrite = TRUE)
        
        cat("Saved:", file_path223, "\n")
      } else {
        cat("No valid prediction for fold", k, "\n")
      }
    }
    
    
    # Step 1: Keep only non-NULL raster predictions
    valid_indices112 <- which(!sapply(species_2100s_Access_ssp245_i_px, is.null))
    species_i_px_valid112 <- species_2100s_Access_ssp245_i_px[valid_indices112]
    
    # Step 2: Calculate AUC weights
    auc_weights_valid112 <- (avg_auc - 0.5)^2
    auc_weights_valid112 <- rep(auc_weights_valid112, length(valid_indices112))
    
    # Step 3: Stack the valid SpatRaster predictions
    species_2100s_Access_ssp245_i_px_stack <- rast(species_i_px_valid112)
    
    # Check if there is only one valid prediction layer
    if (nlyr(species_2100s_Access_ssp245_i_px_stack) == 1) {
      species_2100s_Access_ssp245_i_px_avg <- species_2100s_Access_ssp245_i_px_stack
      cat("Only one valid prediction layer — using it as the final habitat suitability surface.\n")
    } else {
      species_2100s_Access_ssp245_i_px_avg <- app(species_2100s_Access_ssp245_i_px_stack, fun = function(x) {
        if (all(is.na(x))) {
          return(NA)
        } else {
          return(weighted.mean(x, w = auc_weights_valid112, na.rm = TRUE))
        }
      })
      cat("Weighted average prediction surface computed for", scientific_name_i, "\n")
    }
    
    plot(species_2100s_Access_ssp245_i_px_avg, main = paste("Average Habitat Suitability 2050sssp245", scientific_name_i))
    
    
    species_2100s_Access_ssp245_predictions_avg <- list()
    species_2100s_Access_ssp245_predictions_avg[[scientific_name_i]] <- species_2100s_Access_ssp245_i_px_avg
    names(species_2100s_Access_ssp245_predictions_avg)
    
    
    # Define output path for the average suitability raster
    output_dir331 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/2100/range_full/ssp245/average"
    dir.create(output_dir331, recursive = TRUE, showWarnings = FALSE)
    avg_suitability_path331 <- file.path(output_dir331, paste0("weighted_avg_habitat_suitability_2100s_ssp245_", scientific_name_i, ".tif"))
    
    # Save the weighted average raster
    if (exists("species_2100s_Access_ssp245_i_px_avg")) {
      
      terra::writeRaster(species_2100s_Access_ssp245_i_px_avg, filename = avg_suitability_path331, overwrite = TRUE)
      cat("Saved weighted average habitat suitability 2080s ssp245 for", scientific_name_i, "\n")
      
      species_2100s_Access_ssp245_predictions_avg[[scientific_name_i]] <- species_2100s_Access_ssp245_i_px_avg
      
    } else {
      cat("species_2100s_Access_ssp245_i_px_avg not found — skipping raster save for", scientific_name_i, "\n")
    }
    
    
    # ---------- Step 5: Create and Save Binary Suitability Raster ----------
    if (!is.na(species_i_threshold)) {
      species_i_binary_2100s_Access_ssp245 <- species_2100s_Access_ssp245_i_px_avg > species_i_threshold
      
      plot(species_i_binary_2100s_Access_ssp245, main = paste("Binary Suitability 2080s ssp245 for", scientific_name_i))
      
      species_dir113 <- file.path("F:/WWF_data/SDMs_LC_climate/results/other_mammals/2100/range_full/ssp245", gsub(" ", "_", scientific_name_i))
      dir.create(file.path(species_dir113, "binary"), showWarnings = FALSE, recursive = TRUE)
      binary113 <- file.path(species_dir113, "binary", paste0(scientific_name_i, "_mean_pixel_binary_2100s_ssp245.tif"))
      writeRaster(
        species_i_binary_2100s_Access_ssp245,
        filename = binary113,
        overwrite = TRUE
      )
      
      cat("Binary habitat raster saved successfully for", scientific_name_i, "\n")
    } else {
      cat("Binary raster not created — threshold was NA for", scientific_name_i, "\n")
    }
    
    
    # ---------2081-2100-ssp585-------------
    
    # Stack all environmental rasters (raster stack)
    environmental_var_stack_2100_ssp585 <- raster::stack(MAT_2100_ssp585, MAP_2100_ssp585,
                                                         forest_2100_ssp585, cropland_2100_ssp585,
                                                         urban_2100_ssp585, grassland_2100_ssp585,
                                                         barren_2100_ssp585, water_2100_ssp585,
                                                         elevation, slope, oforest_2100_ssp585,
                                                         wetland_2100_ssp585)
    environmental_var_stack_2100_ssp585
    
    plot(environmental_var_stack_2100_ssp585)
    
    
    names(environmental_var_stack_2100_ssp585) <- c("temp", "prec", "forest", "cropland", "urban", 
                                                    "grassland", "barren", "water", 
                                                    "elevation", "slope", "oforest", "wetland")
    
    
    environmental_var_stack_2100_ssp585 <- terra::rast(environmental_var_stack_2100_ssp585)
    
    species_2100s_Access_ssp585_i_px <- list()
    
    # Predict habitat suitability for the test data
    species_2100s_Access_ssp585_i_px[[k]] <- predict(environmental_var_stack_2100_ssp585, 
                                                     species_i_maxent_models[[k]], 
                                                     ext = ext, 
                                                     na.rm = TRUE,
                                                     progress = '') # suppresses the display of the progress bar during model prediction (an empty string means no progress bar will be shown).
    
    plot(species_2100s_Access_ssp585_i_px[[k]], main = paste("Predicted climatic Habitat Suitability 2100 Access ssp585", scientific_name_i))
    
    # Define the directory where the prediction files will be saved
    output_dir2223 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/2100/range_full/ssp585"
    
    # Ensure the directory exists
    if (!dir.exists(output_dir2223)) {
      dir.create(output_dir2223, recursive = TRUE)  # Create the directory if it doesn't exist
    }
    
    for (k in seq_along(species_2100s_Access_ssp585_i_px)) {
      if (!is.null(species_2100s_Access_ssp585_i_px[[k]])) {
        # Define the file path for each fold
        file_path2223 <- paste0(output_dir2223, "habitat_suitability_species_2100s_ssp585_", scientific_name_i, "_fold_", k, ".tif")
        
        # Save the raster as a TIFF file
        terra::writeRaster(species_2100s_Access_ssp585_i_px[[k]], filename = file_path2223, overwrite = TRUE)
        
        # Print confirmation message
        cat("Saved:", file_path2223, "\n")
      } else {
        cat("No valid prediction for fold", k, "\n")
      }
    }
    
    
    # Step 1: Keep only non-NULL raster predictions
    valid_indices1112 <- which(!sapply(species_2100s_Access_ssp585_i_px, is.null))
    species_i_px_valid1112 <- species_2100s_Access_ssp585_i_px[valid_indices1112]
    
    # Step 2: Calculate AUC weights based on the average AUC across all folds
    # Adjust AUC weights using the average AUC value calculated earlier (avg_auc)
    auc_weights_valid1112 <- (avg_auc - 0.5)^2  # Use avg_auc to compute weights
    auc_weights_valid1112 <- rep(auc_weights_valid1112, length(valid_indices1112))  # Replicate for each valid prediction
    
    # Step 3: Stack the valid SpatRaster predictions
    species_2100s_Access_ssp585_i_px_stack <- rast(species_i_px_valid1112)
    
    # Check if there is only one valid prediction layer
    if (nlyr(species_2100s_Access_ssp585_i_px_stack) == 1) {
      species_2100s_Access_ssp585_i_px_avg <- species_2100s_Access_ssp585_i_px_stack
      cat("Only one valid prediction layer — using it as the final habitat suitability surface.\n")
    } else {
      # Calculate the weighted average habitat suitability surface
      species_2100s_Access_ssp585_i_px_avg <- app(species_2100s_Access_ssp585_i_px_stack, fun = function(x) {
        if (all(is.na(x))) {
          return(NA)
        } else {
          return(weighted.mean(x, w = auc_weights_valid1112, na.rm = TRUE))
        }
      })
      cat("Weighted average prediction surface computed for", scientific_name_i, "\n")
    }
    
    plot(species_2100s_Access_ssp585_i_px_avg, main = paste("Average Habitat Suitability 2100s ssp585", scientific_name_i))
    
    
    species_2100s_Access_ssp585_predictions_avg <- list()
    species_2100s_Access_ssp585_predictions_avg[[scientific_name_i]] <- species_2100s_Access_ssp585_i_px_avg
    names(species_2100s_Access_ssp585_predictions_avg)
    
    
    # Define output path for the average suitability raster
    output_dir3331 <- "F:/WWF_data/SDMs_LC_climate/results/other_mammals/2100/range_full/ssp585/average"
    dir.create(output_dir3331, recursive = TRUE, showWarnings = FALSE)
    avg_suitability_path3331 <- file.path(output_dir3331, paste0("weighted_avg_habitat_suitability_2100s_ssp585_", scientific_name_i, ".tif"))
    
    # Save the weighted average raster (SpatRaster object)
    if (exists("species_2100s_Access_ssp585_i_px_avg")) {
      
      # Save to disk
      terra::writeRaster(species_2100s_Access_ssp585_i_px_avg, filename = avg_suitability_path3331, overwrite = TRUE)
      cat("Saved weighted average habitat suitability 2100s ssp585 for", scientific_name_i, "\n")
      
      # Store in named list
      species_2100s_Access_ssp585_predictions_avg[[scientific_name_i]] <- species_2100s_Access_ssp585_i_px_avg
      
    } else {
      cat("species_2100s_Access_ssp585_i_px_avg not found — skipping raster save for", scientific_name_i, "\n")
    }
    
    
    # ---------- Step 5: Create and Save Binary Suitability Raster ----------
    if (!is.na(species_i_threshold)) {
      species_i_binary_2100s_Access_ssp585 <- species_2100s_Access_ssp585_i_px_avg > species_i_threshold
      
      # plot
      plot(species_i_binary_2100s_Access_ssp585, main = paste("Binary Suitability 2100s Access ssp585 for", scientific_name_i))
      
      species_dir1112 <- file.path("F:/WWF_data/SDMs_LC_climate/results/other_mammals/2100/range_full/ssp585", gsub(" ", "_", scientific_name_i))
      dir.create(file.path(species_dir1112, "binary"), showWarnings = FALSE, recursive = TRUE)
      binary1112 <- file.path(species_dir1112, "binary", paste0(scientific_name_i, "_mean_pixel_binary_2100s_ssp585.tif"))
      writeRaster(
        species_i_binary_2100s_Access_ssp585,
        filename = binary1112,
        overwrite = TRUE
      )
      
      cat("Binary habitat raster saved successfully for", scientific_name_i, "\n")
    } else {
      cat("Binary raster not created — threshold was NA for", scientific_name_i, "\n")
    }
    
    
  }}

