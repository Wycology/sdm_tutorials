# -------------------------------------------------------------------------
# Species Distribution Modelling (SDM) for three Grewia species in Africa
# Author: Dr. agr. Wyclife Agumba Oluoch
# YouTube: https://www.youtube.com/@wycology
# LinkedIn: https://www.linkedin.com/in/wyclife-oluoch-3924341b9/
# Website: https://sites.google.com/view/agumbaoluoch/
# -------------------------------------------------------------------------
# This script:
#  1. Defines Africa as the region of interest
#  2. Downloads and prepares environmental predictor variables
#  3. Removes highly correlated predictors using VIF
#  4. Downloads and cleans species occurrence records
#  5. Builds a multi-species SDM using RF, SVM, and MaxEnt
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Load required libraries
# -------------------------------------------------------------------------

library(sdm)           # Version 1.2.67 ==> Species distribution modelling framework
library(usdm)          # Version 2.1.7 ==> Multicollinearity diagnostics (VIF)
library(dplyr)         # Version 1.1.4 ==> Data manipulation
library(tidyr)         # Version 1.3.2 ==> Data tidying
library(geodata)       # Version 0.6.6 ==> Climate and elevation data
library(rnaturalearth) # Version 1.1.0 ==> Administrative boundaries

# -------------------------------------------------------------------------
# Define region of interest (Africa)
# -------------------------------------------------------------------------

# Download African country boundaries and dissolve into a single polygon
africa <- aggregate(vect(ne_countries(continent = "Africa")))
plot(africa)
# -------------------------------------------------------------------------
# Download and prepare environmental predictor variables
# -------------------------------------------------------------------------

# Bioclimatic variables (WorldClim v2)
bioc <- worldclim_global(var = "bio", res = 10, path = tempdir())
names(bioc) <- paste0("bio", 1:19)

# Elevation
elev <- elevation_global(res = 10, path = tempdir())
names(elev) <- "elevation"

# Terrain derivatives
slope  <- terrain(x = elev, "slope")
aspect <- terrain(x = elev, "aspect")

# Combine all predictors into a single SpatRaster
preds <- c(bioc, elev, slope, aspect)

# Crop predictors to Africa
preds_africa <- crop(x = preds, y = africa, mask = TRUE)

# -------------------------------------------------------------------------
# Remove highly correlated predictors
# -------------------------------------------------------------------------

# Identify predictors with pairwise correlation > 0.9
v <- vifcor(x = preds_africa, th = 0.9)

# Exclude correlated variables
preds_used <- exclude(x = preds_africa, vif = v)

# -------------------------------------------------------------------------
# Download species occurrence records (from GBIF)
# -------------------------------------------------------------------------

# First species

mollis <- sp_occurrence(
  genus   = "Grewia",
  species = "mollis",
  ext     = africa,
  geo     = TRUE,
  ntries  = 5
)

# Second species

tenax <- sp_occurrence(
  genus   = "Grewia",
  species = "tenax",
  ext     = africa,
  geo     = TRUE,
  ntries  = 5
)

# Third species

villosa <- sp_occurrence(
  genus   = "Grewia",
  species = "villosa",
  ext     = africa,
  geo     = TRUE,
  ntries  = 5
)

# -------------------------------------------------------------------------
# Clean and standardize occurrence data
# -------------------------------------------------------------------------
# - Keep only reliable observation types
# - Remove duplicates and missing coordinates
# - Add species label

mollis <- mollis %>%
  filter(basisOfRecord %in% c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")) %>%
  dplyr::select(lon, lat) %>%
  distinct() %>%
  drop_na() %>%
  mutate(species = "mollis")

tenax <- tenax %>%
  filter(basisOfRecord %in% c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")) %>%
  dplyr::select(lon, lat) %>%
  distinct() %>%
  drop_na() %>%
  mutate(species = "tenax")

villosa <- villosa %>%
  filter(basisOfRecord %in% c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN")) %>%
  dplyr::select(lon, lat) %>%
  distinct() %>%
  drop_na() %>%
  mutate(species = "villosa")

# -------------------------------------------------------------------------
# Combine all species into a single data.frame
# -------------------------------------------------------------------------

all_species <- bind_rows(mollis, tenax, villosa)
head(all_species)
# Columns: lon, lat, species

# -------------------------------------------------------------------------
# Convert occurrence data to a spatial object
# -------------------------------------------------------------------------

sp_vect <- vect(x = all_species,
                geom = c("lon", "lat"),
                crs = "EPSG:4326")

sp_vect <- crop(x = sp_vect, y = africa)

set.seed(248) # This is to enable reproducibility
bg <- background(x = preds_used,
                 n = 1000,
                 method = "gRandom")

bg_vect <- vect(bg, geom = c("x", "y"), crs = "EPSG:4326")

# -------------------------------------------------------------------------
# Visual inspection of species occurrences
# -------------------------------------------------------------------------

plot(africa)
plot(bg_vect, col = "gray90", add = TRUE)
plot(sp_vect[sp_vect$species == "mollis", ], col = "red", add = TRUE)
plot(sp_vect[sp_vect$species == "tenax", ], col = "green", add = TRUE)
plot(sp_vect[sp_vect$species == "villosa", ], col = "blue", add = TRUE)

# -------------------------------------------------------------------------
# Build SDM data object
# -------------------------------------------------------------------------

d <- sdmData(
  formula    = species ~ .,
  train      = sp_vect,
  predictors = preds_used,
  bg         = bg          # Number of background points
)

d

View(as.data.frame(d)) # Check how the species are combined in the d object

# -------------------------------------------------------------------------
# Create multi-species model formula
# -------------------------------------------------------------------------
# Response: mollis + tenax + villosa
# Predictors: all retained environmental variables

formula_all_sp <- reformulate(termlabels = names(preds_used),
                              response   = paste(unique(sp_vect$species), collapse = "+"))

formula_all_sp # Just the response as a function of the predictor variables

# -------------------------------------------------------------------------
# Fit SDM models
# -------------------------------------------------------------------------
# Algorithms:
#  - Random Forest (rf)
#  - Support Vector Machine (svm)
#  - MaxEnt
#
# Model evaluation:
#  - Subsampling replication
#  - 30% test data
#  - 10 repetitions
#  - Parallel execution

m <- sdm(
  formula = formula_all_sp,
  data    = d,
  methods = c("rf", "svm", "maxent"),
  replication = "sub",
  test.p = 30,
  n = 10, # Check cores with parallel::detectCores() and adjust for your case 
  parallelSetting = list(ncore  = 10, method = "parallel")
)

m

# Create a models folder to store the model

if (!dir.exists("models")) {
  dir.create("models", recursive = TRUE)
}

# Write the model to disk
write.sdm(m, "models/multi_sp.sdm") 

# Read the model into R session
m <- read.sdm("models/multi_sp.sdm")

m

# Next we will check the model and use it accordingly---------------------------