library(raster)
library(rasterExtras)
source("libs/convert_pacific_centric_2_regular.r")

sim_dir = 'data/vegFrac/'
file = 'VegFracs-19013.nc'

years = 2001:2013

levels = list(trees = c("BDT", "BET-Tr", "BET-Te", "NDT", "NET"),
              wood  = c("BDT", "BET-Tr", "BET-Te", "NDT", "NET", "DSH", "ESH"),
              shrub = c("DSH", "ESH"),
              herb  = c("DSH", "ESH", "C3G", "C4G", "C3C", "C4C", "C3P", "C4P"),
              grass = c("C3G", "C4G", "C3C", "C4C", "C3P", "C4P"),
              bares = c("Bare_soil"))
        
igbp_file = 'data/vegfrac_igbp.nc'
cci__file = 'data/vegfrac_refLC_refCW.nc'

obs_files = list(trees = list(igbp = c(igbp_file, c(1, 2)),
                              cci  = c(cci__file, c(1,2)),
                              VCF = 'data/treecover2000-2014.nc'),
                 wood  = list(igbp = c(igbp_file, c(1, 2, 5)),
                              cci  = c(cci__file, c(1, 2, 5))),
                 shrub = list(igbp = c(igbp_file, c(5)),
                              cci  = c(cci__file, c(5))),
                 herb  = list(igbp = c(igbp_file, c(3:5)),
                              cci  = c(cci__file, c(3:5)),
                              VCF = 'data/nontree2000-2014.nc'),
                 grass = list(igbp = c(igbp_file, c(3,4)),
                              cci  = c(cci__file, c(3,4))),
                 bare = list(igbp = c(igbp_file, c(8)),
                              cci  = c(cci__file, c(8)),
                              VCF = 'data/bareground2000-2014.nc'))

out_dir = 'outputs/'


run <- function(name, obs_file, lvls, file, job) { 
    print(name)
    print(job)
    openDat <- function(lvl) {
        dat = dat0 = brick(file, varname = lvl)
        dates =  sapply(names(dat), function(i) strsplit(i, '.', fixed = TRUE)[[1]])
        yrs = sapply(dates[1,], function(i) strsplit(i, 'X')[[1]][2])
        index = apply(sapply(years, '==', yrs), 2, which)
        dat = layer.apply(index, function(i) mean(dat[[i]]))
        names(dat) = paste0('X', years)
        return(dat)
    }
    
    dat = lapply(lvls, openDat)
    for (r in dat[-1]) dat[[1]] = dat[[1]] + r
    dat = dat[[1]]
    
    
    print("regridding")
    dati = convert_pacific_centric_2_regular(dat)
    
    processObs <- function(obs) {
        if (length(obs) > 1) {
            dat = brick(obs[1])
            levels = as.numeric(obs[-1])
            dat = sum(dat[[levels]])
        } else {
            dat = brick(obs)
            dat = dat[[c(seq(1, nlayers(dat), 12))[-1]]]            
            names(dat) = names(dati)
        }
        dat[dat > 9E9] = NaN
        dat = convert_pacific_centric_2_regular(dat)
        dat = raster::resample(dat, dati)
        return(dat)
    }
    
    print("processing obs")
    obs = lapply(obs_file, processObs)
    
    print("outputting")
    out_file = paste0(out_dir, job, '-', name, '-fracCover.nc')
    
    writeRaster(dati, out_file[1], overwrite = TRUE)
    
    out_file = paste0(out_dir, names(obs_file), '-', name, '-fracCover.nc')
    mapply(writeRaster, obs , out_file, overwrite = TRUE)
}
jobs = sapply(list.files(sim_dir), function(i) strsplit(i, "-V")[[1]][1])

files = list.files(sim_dir, full.names = TRUE)
makeJob <- function(job) {
    file = files[grepl(job, files)]
    mapply(run, names(levels), obs_files, levels, MoreArgs = list(file, job))
}

lapply(jobs, makeJob)
