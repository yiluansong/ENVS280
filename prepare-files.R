library(raster)
library(rgdal)

install.packages("devtools")
devtools::install_github("cbrown5/vocc")
library(vocc)

setwd("<insert path to project folder>")

calcvelocityXY <- function(grad, slope, y_dist = 111.325) {
  slope$w <- y_dist * cos(CircStats::rad(slope$y))
  grd <- data.frame(NSold = grad$NS, WEold = grad$WE)
  grd$NS <- ifelse(is.na(grd$NSold) == TRUE, 0, grd$NSold)
  grd$WE <- ifelse(is.na(grd$WEold) == TRUE, 0, grd$WEold)
  grd$NAsort <- ifelse(abs(grd$NS) + abs(grd$WE) == 0, NA, 1)
  grd$Grad <- grd$NAsort * sqrt((grd$WE^2) + (grd$NS^2))
  grd$NS <- grd$NAsort * grd$NS
  grd$WE <- grd$NAsort * grd$WE
  velocity <- data.frame(x = slope$x, y = slope$y, temporal_trend = slope$slope, spatial_gradient = grd$Grad, NSgrad = grad$NS, WEgrad = grad$WE, angle = grad$angle, w = slope$w, icell = grad$icell)
  velocity$velocity <- with(velocity, temporal_trend / spatial_gradient)
  velocity$anglenew <- ifelse(velocity$temporal_trend < 0, velocity$angle + 180, velocity$angle)
  velocity$velocityX <- -velocity$velocity * (velocity$WEgrad / sqrt(velocity$WEgrad^2 + velocity$NSgrad^2))
  velocity$velocityY <- -velocity$velocity * (velocity$NSgrad / sqrt(velocity$WEgrad^2 + velocity$NSgrad^2))
  return(velocity)
}

file <- c("data/TMP_World.nc")
TMP <- brick(file)

slopedat <- calcslope(TMP, divisor = 1)

allyears <- rep(1, nlayers(TMP))
mnTMP <- stackApply(TMP, indices = allyears, fun = mean)
spatx <- spatialgrad(mnTMP, y_dist = c(111.325, 111.325), y_diff = 0.5)

velodf <- calcvelocityXY(spatx, slopedat)

rtrendC <- rgradC <- rvoccC <- rangleC <- rvoccCX <- rvoccCY <- raster(TMP)
rgradC[spatx$icell] <- sqrt((spatx$NS)^2 + (spatx$WE)^2)
rtrendC[slopedat$icell] <- slopedat$slope
rvoccC[velodf$icell] <- abs(velodf$velocity)
rangleC[velodf$icell] <- velodf$anglenew
rvoccCX[velodf$icell] <- velodf$velocityX
rvoccCY[velodf$icell] <- velodf$velocityY

writeRaster(rvoccC, "data/Vc.nc", overwrite = TRUE, format = "CDF", varname = "Vc", varunit = "km/year", longname = "Velocity of MAT", xname = "lon", yname = "lat", zname = "time")
plot(rvoccC)

file <- c("data/SOS_World_0.5deg_QA_Smooth.nc")
SOS <- brick(file)
plot(SOS[[1]])

slopedat <- calcslope(SOS, divisor = 1)

allyears <- rep(1, nlayers(SOS))
mnSOS <- stackApply(SOS, indices = allyears, fun = mean)
spatx <- spatialgrad(mnSOS, y_dist = c(111.325, 111.325), y_diff = 0.5)

velodf <- calcvelocityXY(spatx, slopedat)

rtrendP <- rgradP <- rvoccP <- rangleP <- rvoccPX <- rvoccPY <- raster(SOS)
rgradP[spatx$icell] <- sqrt((spatx$NS)^2 + (spatx$WE)^2)
rtrendP[slopedat$icell] <- slopedat$slope
rvoccP[velodf$icell] <- abs(velodf$velocity)
rangleP[velodf$icell] <- velodf$anglenew
rvoccPX[velodf$icell] <- velodf$velocityX
rvoccPY[velodf$icell] <- velodf$velocityY

writeRaster(rvoccP, "data/Vp.nc", overwrite = TRUE, format = "CDF", varname = "Vp", varunit = "km/year", longname = "Velocity of SOS", xname = "lon", yname = "lat", zname = "time")

projection <- (rvoccCX * rvoccPX + rvoccCY * rvoccPY) / rvoccC
plot(projection)

writeRaster(projection, "data/projection.nc", overwrite = TRUE, format = "CDF", varname = "projection", varunit = "km/year", longname = "Projection of SOS on MAT", xname = "lon", yname = "lat", zname = "time")

mismatch <- rvoccC - projection

writeRaster(mismatch, "data/mismatch.nc", overwrite = TRUE, format = "CDF", varname = "mismatch", varunit = "km/year", longname = "mismatch between SOS and MAT", xname = "lon", yname = "lat", zname = "time")
