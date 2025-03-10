# https://www.youtube.com/watch?v=83dMS3bcjJM&t=787s
#----------Script for ENM 2020 online course

# In this demonstration, we are going to demonstrate the sdm (and usdm) package
# to fit species distribution models for "Acinonyx jubatus"
# and predict/project the potential distribution in the current and future times
# and measure the changes in the potential distribution (range shift) due to climate change


# install.packages("sdm")
# or
# devtools::install_github("babaknaimi/sdm")

# setwd

library(sdm)
# installAll() # only first time after installing the sdm package

# library(dismo)
library(dplyr)
library(tidyr)
library(mapview)
library(mapedit)
library(geodata)
library(usdm)


sp_occurrence(genus = "Acinonyx", 
              species = "jubatus", download = F)

sp <- sp_occurrence("Acinonyx", "jubatus", download = T)

class(sp)
dim(sp)
table(sp$basisOfRecord)

sp <- sp |> 
  filter(basisOfRecord %in% c("HUMAN_OBSERVATION", 
                              "OBSERVATION", 
                              "PRESERVED_SPECIMEN"))

nrow(sp)

spg <- sp |> dplyr::select(lon, lat)
head(spg)
spg$species <- 1
spg <- spg |> drop_na()
nrow(spg)
#----------------
class(spg)
spg <- vect(spg, geom = c("lon", "lat"), crs = 'epsg:4326')
class(spg)
####################################
# download the bioclim data

# bio <- raster::getData("worldclim", var = "bio", res = 10)
bio <- worldclim_global(var = 'bio', res = 10, path = tempdir())
bio
names(bio)
names(bio) <- paste0("bio", 1:19)


plot(bio[[1]])
points(spg)
plot(spg, add = TRUE)

# e <- drawExtent()
e <- drawFeatures()

spg <- crop(spg, e)

# points(spg, col = "red")
plot(spg, col = "red", add = T)

bioc <- crop(bio, e, mask = TRUE)

plot(bioc[[1]])

#----------------

library(usdm)

vif(bioc)

ex <- raster::extract(bioc, spg)
ex <- terra::extract(bioc, spg)

head(ex)

v <- vifstep(bioc)

# vifcor()
bioc <- exclude(bioc, v)

#-----------------------

d <- sdmData(species ~., spg, 
             predictors = bioc, 
             bg = list(method = "gRandom", n = 1000))
d

getmethodNames()

m <- sdm(species ~., d, methods = c("glm", "brt", "rf", "fda"),
         replication = c("sub", "boot"), test.p = 30, n = 3,
         parallelSetting = list(cores = 10, method = 'parallel' ))

m
# m@models$species$rf$`13`@object

gui(m)

p1 <- predict(m, bioc)
p1
names(p1)
plot(p1[[c(1, 7, 13, 23)]])

en1 <- ensemble(m, bio, filename = 'en.img', 
                setting = list(method = "weighted", 
                               stat = "tss", opt = 2)) # id...

en1 <- ensemble(m, p1, filename = 'en2.img', 
                setting = list(method = "weighted", 
                               stat = "tss", opt = 2)) # id...

plot(en1)
#########################
biof <- raster::getData("CMIP5", var = "bio", res = 10,
                        rcp = 85, model = BC, year = 70)
biof <- geodata::cmip6_world(model = "HadGEM3-GC31-LL", 
                             ssp = "585", 
                             time = "2061-2080", 
                             var = "bioc", 
                             res = 10,
                             path = tempdir())

biof

plot(biof[[1]])

names(biof)
names(biof) <- names(bio)

en2 <- ensemble(m, biof, filename = 'enf2.img', 
                setting = list(method = "weighted", stat = "tss", opt = 2)) # id...
plot(en2)
cl <-  colorRampPalette(c("#3E49BB", "#3498DB", "yellow", "orange", "red", "darkred"))
plot(en1, col = cl(200))
plot(en2, col = cl(200))
#---------------------------------

#mapview(stack(en1, en2), col.regions = cl(200)) + spg
mapview(en1, col.regions = cl(200)) + spg

#############################
ch <- crop(en2, e, mask = T) - en1

cl2 <- colorRampPalette(c("red", "orange", "yellow", "gray", "green", "blue"))
plot(ch, col = cl2(200))

#-----------------------------
df <- as.data.frame(d)
head(df)
#df <- data.frame(species = df$species, coordinates(d))
df <- data.frame(species = df$species, coords(d))

head(df)
xy <- as.matrix(df[, c("x", "y")])
head(xy)
p <- terra::extract(en1, xy)
head(p)
nrow(df)
# length(p)
p <- p$ensemble_weighted
length(p)

ev <- evaluates(df$species, p)
#------------------------------
ev@statistics
th <- ev@threshold_based$threshold[2]

# pa1 <- raster(en1)
pa1 <- en1

pa1[] <- ifelse(en1[] >= th, 1, 0)
plot(pa1)

# pa2 <- raster(en2)

pa2 <- en2

pa2[] <- ifelse(en2[] >= th, 1, 0)
plot(pa2)

chp <- crop(pa2, e, mask = T) - pa1

plot(chp)
plot(chp, col = c("red", "gray", "blue"))


# New function ------------------------------------------------------------

simplified <- sdm::pa(x = en1, 
                      y = m, 
                      id = 'ensemble', 
                      opt = 2)
plot(simplified)
plot(pa1)

##################################
rcurve(m, n = "bio9")
rcurve(m, id = 1:6)
rcurve(m, id = 7:12)
getVarImp(m)
getVarImp(m, id = 1)
plot(getVarImp(m))
plot(getVarImp(m, method = 'rf'))

en1_c <- crop(en1, e, mask = T)

niche(raster::stack(bioc), raster(en1_c), 
      n = c('bio3', 'bio18'), 
      col = cl(200))

aoa_p <- aoa(x = exclude(biof, v), 
             d = d)

# additionals
# background
# pa
# aoa
