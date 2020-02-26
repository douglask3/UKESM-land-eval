library(raster)
library(rasterExtras)
source("libs/convert_pacific_centric_2_regular.r")
job = 'u-bb075'

file = 'VegFracs-19013.nc'

years = 2001:2013

levels = list(trees = c("BDT", "BET-Tr", "BET-Te", "NDT", "NET"),
              wood  = c("BDT", "BET-Tr", "BET-Te", "NDT", "NET", "DSH", "ESH"),
              shrub = c("DSH", "ESH"),
              grass = c("C3G", "C4G", "C3C", "C4C", "C3P", "C4P"))
        
igbp_file = 'data/vegfrac_igbp.nc'
cci__file = 'data/vegfrac_refLC_refCW.nc'

obs_files = list(trees = list(igbp = c(igbp_file, c(1, 2)),
                              cci  = c(cci__file, c(1,2))),
                 wood  = list(igbp = c(igbp_file, c(1, 2, 5)),
                              cci  = c(cci__file, c(1, 2, 5)),
                              VCF = 'data/treecover2000-2014.nc'),
                 shrub = list(igbp = c(igbp_file, c(5)),
                              cci  = c(cci__file, c(5))),
                 grass = list(igbp = c(igbp_file, c(3,4)),
                              cci  = c(cci__file, c(2,4)),
                              VCF = 'data/nontree2000-2014.nc'))

out_dir = 'outputs/'

file = paste('data/', job, file, sep = '/')
run <- function(name, obs_file, lvls) {
    print(name)
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
            dat = sum(dat[[as.numeric(obs[-1])]])
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

mapply(run, names(levels), obs_files, levels)
