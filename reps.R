library(ENMTools)

monticola <- iberolacerta.clade$species$monticola

# Bootstrap
reps <- list()

for(i in 1:10){
  thisrep <- monticola
  npres <- nrow(thisrep$presence.points)
  bootrows <- sample(1:npres, npres, replace = TRUE)
  thisrep$presence.points <- thisrep$presence.points[bootrows,]
  thisname <- paste0("rep", i)
  reps[[thisname]] <- enmtools.gam(thisrep, euro.worldclim, test.prop = 0.3)
}

bootstack <- reps[[1]]$suitability

for(i in 2:10){
  bootstack <- addLayer(bootstack, reps[[i]]$suitability)
}

names(bootstack) <- names(reps)

meanboot <- calc(bootstack, fun = mean, na.rm = T)
plot(meanboot)

sdboot <- raster::calc(bootstack, fun = sd, na.rm = T)
plot(sdboot)




# Or if you just wanted to do repeated subsampling for test data with no bootstrap
reps <- list()

for(i in 1:10){
  thisname <- paste0("rep", i)
  reps[[thisname]] <- enmtools.gam(monticola, euro.worldclim, test.prop = 0.3)
}

repstack <- reps[[1]]$suitability

for(i in 2:10){
  repstack <- addLayer(repstack, reps[[i]]$suitability)
}

names(repstack) <- names(reps)

meanreps <- raster::calc(repstack, fun = mean, na.rm = T)
plot(meanreps)

sdreps <- raster::calc(repstack, fun = sd, na.rm = T)
plot(sdreps)

