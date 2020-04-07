source("cfg.r")

years = list(2000:2006, 2001:2013)

file_ids = c(precip = 'Tot_PRECIP_RATE-00024', SW = 'TotalDownSW',
             tas = 'SURFACE_TEMP-00024.nc')

out_dir = 'outputs/clim/'
makeDir(out_dir)
run <- function(name, id, job) {
    run4years <- function(year) {
        file = mod_files[grepl(id, mod_files) & grepl(job, mod_files)]
        dat = brick(file)
        dat = getYrsFromLayers(dat, year, NULL)
        clim = convert2Climatology(dat)   
        pc = PolarConcentrationAndPhase(clim, phase_units = "months")
        dat = getYrsFromLayers(dat, year)
        MAD = mean(dat)
    
        fname_out = paste0(out_dir, '/', name , '/')
        makeDir(fname_out)
        fname_out = paste0(fname_out, c('mean_annual', 'climatology',
                                        'phase', 'concentration', 'annual'),
                           '-', min(year), '_', max(year), '-', job, '.nc')
        writeRaster.Standard(MAD    , fname_out[1])
        writeRaster.Standard(clim   , fname_out[2])
        writeRaster.Standard(pc[[1]], fname_out[3])
        writeRaster.Standard(pc[[2]], fname_out[4])
        writeRaster.Standard(dat    , fname_out[5])
    }
    lapply(years, run4years)
}

makeJob <- function(job) 
    mapply(run, names(file_ids), file_ids, MoreArgs = list(job))

lapply(jobs, makeJob)
