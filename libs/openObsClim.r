openObsClim <- function(file, r_eg = raster("outputs/biomes.nc")) {
    r = brick(file)
    if (nlayers(r) == 1) r = r[[1]] else {
        if (grepl('conPhase', file)) r = r[[2]]
        else r = mean(r)
    }
    r = raster::resample(r, r_eg)
    return(r)
}
