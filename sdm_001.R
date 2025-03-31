# install.packages("sdm")
# devtools::install_github("babaknaimi/sdm")
# remotes::install_github("babaknaimi/sdm")

library(sdm) # version 1.2.56

species <- vect(system.file('external/species.shp', 
                            package = 'sdm'))

preds <- rast(list.files(system.file('external', package = 'sdm'), 
           pattern = 'asc',
           full.names = T))

d <- sdmData(Occurrence ~., train = species, predictors = preds)

m <- sdm(Occurrence ~., data = d, methods = c('rf', 'svm'),
         replication = 'sub', test.p = 30, n = 10)

write.sdm(m, 'my_sdm.sdm')
m <- read.sdm('my_sdm.sdm')

p <- predict(m, preds, mean = TRUE)
p
writeRaster(p, "my_raster.tif")
plot(p)

rcurve(m)
roc(m)
gui(m)
# END