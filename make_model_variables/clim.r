source("cfg.r")

years = list(2000:2006, 2001:2013)

file_ids = c(precip = 'Tot_PRECIP_RATE-00024', SW = 'TotalDownSW',
             tas = 'SURFACE_TEMP-00024.nc')

obs_files = c('data/pr/', 'data/SW/', 'data/tas/')

out_dir = 'outputs/clim/'
makeDir(out_dir)
run <- function(name, id, obs_file, job) {
    run4years <- function(year) {
        run4file <- function(file, type = 'sim', mask = NULL) {  

            fname_out = paste0(out_dir, '/', name , '/')
            makeDir(fname_out)
            fname_out = paste0(fname_out, '/',  min(year), '_', max(year), '/')
            makeDir(fname_out)
            print(type)
            if (type == 'sim') start = job
                else {
                    start = paste0(type, '_', filename.noPath(file, noExtension = TRUE))
            }
            fname_out = paste0(fname_out, start, '-', c('mean_annual', 'climatology',
                               'phase', 'concentration', 'annual'), '.nc')
            if (all(file.exists(fname_out))) return(brick(fname_out[5]))
            print(file)
            dat = brick(file)
            dat = getYrsFromLayers(dat, year, NULL)
            if (grepl('CMAP', start)) dat = dat  *60*60*24*365.25
            
            if (is.null(dat)) return(NULL)
            clim = convert2Climatology(dat)
            
            dat = getYrsFromLayers(dat, year)
            if (grepl('tas', file) &&   min.raster(clim, na.rm = TRUE) < (-1)) {        
                clim = clim + 273.15
                dat = dat + 273.15
            }
            pc = PolarConcentrationAndPhase(clim, phase_units = "months")
            MAD = mean(dat)
        
            writeRaster.Standard.mask <- function(r, ...) {
                if (!is.null(mask))  r = raster::resample(r, mask)
                writeRaster.Standard(r, ...)
            }

            writeRaster.Standard.mask(MAD    , fname_out[1])
            writeRaster.Standard.mask(clim   , fname_out[2])
            writeRaster.Standard.mask(pc[[1]], fname_out[3])
            writeRaster.Standard.mask(pc[[2]], fname_out[4])
            writeRaster.Standard.mask(dat    , fname_out[5])
            return(dat)
        }
        
        file = mod_files[grepl(id, mod_files) & grepl(job, mod_files)]
        dat = run4file(file) 
        if (job == jobs[1]) {
            obs_file = list.files(obs_file, full.name = TRUE)            
            if (length(obs_file) > 0) lapply(obs_file, run4file, type = 'Observation', mask = dat)
        }   
        print(job)   
    }
    lapply(years, run4years)
}

makeJob <- function(job) 
    mapply(run, names(file_ids), file_ids, obs_files, MoreArgs = list(job))

lapply(jobs, makeJob)
