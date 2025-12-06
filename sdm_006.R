###############################################################
# Script: 06_sdm_future_prediction.R
#
# Purpose:
#   Demonstrate how to safely use a species distribution model
#   (SDM) trained on current climate predictors to make future
#   climate predictions **without getting errors** caused by:
#      ✔ layer name mismatches
#      ✔ missing predictors
#      ✔ inconsistent predictor order
#
# Key Teaching Points:
#   - WorldClim and CMIP6 rasters often have different layer names.
#   - SDM models require predictors to have EXACTLY the same names.
#   - After VIF selection, the SAME predictor set must be used.
#   - Solution: rename and align predictors BEFORE model training.
#
# Workflow:
#   1. Load current climate predictors
#   2. Standardize layer names
#   3. Fit the SDM model
#   4. Load future climate data
#   5. Apply identical names and predictor selection
#   6. Predict future suitability (no errors!)
#
# Author: Wyclife Agumba Oluoch
# Date: 2025-12-06
###############################################################

library(sdm)            # Version 1.2.67
library(geodata)        # Version 0.6.6
library(tidyverse)      # Version 2.0.0
library(usdm)           # Version 2.1.7
library(rnaturalearth)  # Version 1.1.0
library(terra)          # Version 1.8.87

# ---------------------------------------------------------------------------
# 1. Region of Interest: Africa
# ---------------------------------------------------------------------------
roi <- aggregate(vect(rnaturalearth::ne_countries(continent = "Africa")))

# ---------------------------------------------------------------------------
# 2. Occurrence Data (Salvadora persica)
# ---------------------------------------------------------------------------
sp <- sp_occurrence(
  genus   = "Salvadora",
  species = "persica",
  ext     = roi,
  geo     = TRUE
)

sp_clean <- sp %>%
  select(lon, lat) %>%
  drop_na() %>%
  distinct()

sp_clean$species <- 1
sp_pts <- vect(sp_clean, geom = c("lon", "lat"), crs = "EPSG:4326")

# Crop to ROI
sp_pts <- crop(sp_pts, roi)

# ---------------------------------------------------------------------------
# 3. CURRENT Predictors (WorldClim BIO, Elevation, Slope)
# ---------------------------------------------------------------------------

# --- Climate ---
clim <- worldclim_global(var = "bio", res = 10, path = tempdir())
clim_afr <- crop(clim, roi, mask = TRUE)

# Rename to simple / consistent scheme
names(clim_afr) <- paste0("bio", 1:19)

# --- Topography ---
elev <- elevation_global(res = 10, path = tempdir())
names(elev) <- "elevation"

elev_afr  <- crop(elev, roi, mask = TRUE)
slope_afr <- terrain(elev_afr, "slope")

# Combine
preds_current <- c(clim_afr, elev_afr, slope_afr)

# ---------------------------------------------------------------------------
# 4. Remove Multicollinearity (VIF)
# ---------------------------------------------------------------------------
# Store the names to be used again later!
v <- vifcor(preds_current, th = 0.9)
preds_current_used <- exclude(preds_current, v)

# ---------------------------------------------------------------------------
# 5. Model Training
# ---------------------------------------------------------------------------
d <- sdmData(
  species ~ .,
  train      = sp_pts,
  predictors = preds_current_used,
  bg         = 1000
)

m <- sdm(
  species ~ .,
  data        = d,
  method      = "rf",
  replication = "sub",
  test.p      = 30,
  n           = 3
)

# Predict CURRENT suitability
current_pred <- predict(m, preds_current_used, mean = TRUE)
plot(current_pred)

# ---------------------------------------------------------------------------
# 6. FUTURE Predictors (CMIP6)
# ---------------------------------------------------------------------------
fut <- cmip6_world(
  model = "HadGEM3-GC31-LL",
  ssp   = "126",
  time  = "2021-2040",
  var   = "bioc",
  res   = 10,
  path  = tempdir()
)

# IMPORTANT:
# CMIP6 bioclim variables come with different names.
# We MUST rename them to MATCH the current predictors.
names(fut) <- paste0("bio", 1:19)

# Crop & mask to Africa
fut_afr <- crop(fut, roi, mask = TRUE)

# Rebuild future predictor stack with topography
fut_preds_full <- c(fut_afr, elev_afr, slope_afr)

# ---------------------------------------------------------------------------
# 7. Select *exactly the same predictors* as the current model
# ---------------------------------------------------------------------------

# Filter future predictions to include only layers selected after VIF
fut_preds_used <- exclude(fut_preds_full, v) # You could use exclude as in the video

# Check consistency...this can help you check if names match!
stopifnot(identical(names(fut_preds_used), names(preds_current_used)))

# ---------------------------------------------------------------------------
# 8. Future Prediction (NO name mismatch errors!)
# ---------------------------------------------------------------------------
future_pred <- predict(m, fut_preds_used, mean = TRUE)

plot(future_pred)

# Optional: if you want to see the error we are avoiding here
future_uncertainty <- predict(m, fut) # Error because of name mismatch

# Error in .generateWLP(x = object, newdata = newdata, w = id, species = species,  : 
#                        the newdata does not contain some or all of the predictor variables required by the model...!

# Cite data sources and packages you use in your script, happy coding!

###############################################################
# End of Script
###############################################################