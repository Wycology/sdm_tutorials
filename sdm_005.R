#devtools::install_github("babaknaimi/sdm")
library(sdm)        # version 1.2.60 
library(geodata)    # version 0.6.2
library(tidyverse)  # version 2.0.0

roi <- gadm(country = "Namibia", level = 0, path = tempdir())
sp <- sp_occurrence(genus = "Salvadora", 
                    species = "persica", 
                    ext = roi,
                    geo = TRUE,
                    removeZeros = TRUE)

sp_clean <- sp |> 
  select(lon, lat) |> 
  drop_na() |> 
  unique() 

sp_vect <- vect(sp_clean, 
                geom = c("lon", "lat"), 
                crs = "epsg:4326")
sp_crop <- crop(sp_vect, roi)

preds <- worldclim_country(country = "Namibia",
                           var = "bio",
                           path = tempdir())

preds_mask <- mask(preds, roi)

plot(preds_mask[[1]])

v <- usdm::vifcor(preds_mask)

preds_used <- usdm::exclude(preds_mask, v)

sp_crop$species <- 1

bg <- background(x = preds_used, 
                 n = 1000, 
                 method = "eDist", 
                 sp = sp_crop)

d <- sdmData(formula = species ~., 
             train = sp_crop, 
             predictors = preds_used, 
             bg = bg)

m <- sdm(formula = species ~., 
         data = d, 
         methods = "maxent",
         replication = "cv",
         cv.folds = 4)

p1 <- predict(object = m, 
              newdata = preds_used, 
              mean = TRUE)

rcurve(m)
roc(m)
plot(getVarImp(m))
gui(m)

mapview::mapview(p1) + mapview::mapview(sp_crop)

# END 