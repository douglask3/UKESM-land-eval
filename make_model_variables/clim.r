source("cfg.r")

years = list(2000:2006, 2001:2013)

file_ids = c(precip = 'Tot_PRECIP_RATE-00024', SW = 'TotalDownSW',
             tas = 'SURFACE_TEMP-00024.nc')

obs_files = c('data/pr/', 'data/SW/', 'data/tas/')

out_dir = 'outputs/clim/'
makeDir(out_dir)
run <- function(name, id, obs_file, job) {
    run4years <- function(year) {
        run4file <- function(file, type = 'sim') {
            
            dat = brick(file)
            dat = getYrsFromLayers(dat, year, NULL)
            clim = convert2Climatology(dat)   
            pc = PolarConcentrationAndPhase(clim, phase_units = "months")
            dat = getYrsFromLayers(dat, year)
            MAD = mean(dat)

            fname_out = paste0(out_dir, '/', name , '/')
            makeDir(fname_out)
            fname_out = paste0(fname_out, '/',  min(year), '_', max(year), '/')
            makeDir(fname_out)
            print(type)
            if (type == 'sim') start = job
                else start = paste0(type, '_', filename.noPath(file, noExtension = TRUE))

            
            fname_out = paste0(fname_out, start, '-', c('mean_annual', 'climatology',
                               'phase', 'concentration', 'annual'), '.nc')
            
            writeRaster.Standard(MAD    , fname_out[1])
            writeRaster.Standard(clim   , fname_out[2])
            writeRaster.Standard(pc[[1]], fname_out[3])
            writeRaster.Standard(pc[[2]], fname_out[4])
            writeRaster.Standard(dat    , fname_out[5])
        }
        file = mod_files[grepl(id, mod_files) & grepl(job, mod_files)]
        run4file(file)
        if (job == jobs[1]) {
            obs_file = list.files(obs_file, full.name = TRUE)            
            if (length(obs_file) > 0) lapply(obs_file, run4file, type = 'Observation')
        }       
        
       
    }
    lapply(years, run4years)
}

makeJob <- function(job) 
    mapply(run, names(file_ids), file_ids, obs_files, MoreArgs = list(job))

lapply(jobs, makeJob)
