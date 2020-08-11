
make_obs_phaseConc <- function(file, justPhase = FALSE) {
    if (justPhase) fname = 'phase' else fname = 'conPhase'
    temp_file = paste0('temp/', filename.noPath(file), fname, '.nc')
    #if (file.exists(temp_file)) return(temp_file)
    dat = brick(file)
    if (grepl('Tas', file)) dat = dat + 273.15
    dat = PolarConcentrationAndPhase(dat, justPhase = justPhase, phase_units = 'months')
    dat = writeRaster(dat, file = temp_file, overwrite = TRUE)
    return(temp_file)    
}

make_obs_climateology <- function(file) {
    temp_file = paste0('temp/', filename.noPath(file), '-claimaitology', '.nc')
    if (file.exists(temp_file)) return(temp_file)

    dat = convert2Climatology(brick(file))
    dat = writeRaster(dat, file = temp_file, overwrite = TRUE)
    return(temp_file)
}
    
