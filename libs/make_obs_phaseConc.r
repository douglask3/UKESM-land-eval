
make_obs_phaseConc <- function(file, justPhase = FALSE) {
    if (justPhase) fname = 'phase' else fname = 'conPhase'
    temp_file = paste0('temp/', filename.noPath(file), fname, '.nc')
    if (file.exists(temp_file)) return(temp_file)
    
    dat = PolarConcentrationAndPhase(brick(file), justPhase = justPhase, phase_units = 'months')
    dat = writeRaster(dat, file = temp_file, overwrite = TRUE)
    return(temp_file)    
}
