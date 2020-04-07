library(raster)
library(rasterExtras)
source("libs/convert_pacific_centric_2_regular.r")

sim_dirs = c(LAI = 'data/LAI/', VegFracs = 'data/vegFrac/')
#files = c(LAI = 'LAI-19014.nc', vegFrac = 'VegFracs-19013.nc')

years = 2000:2006

levels = c("BDT", "BET-Tr", "BET-Te", "NDT", "NET", "DSH", "ESH", "C3G", "C4G", "C3C", "C4C", "C3P", "C4P")
layers = c(1    , 1       , 1       , 2    , 2    , 5    , 5    , 3    , 4    , 3    , 4    , 3    , 4    )
out_dir = 'outputs/'

dat_file = 'data/lai_0.5x0.5_2000-2005.nc'
obs_file_out = 'lai_0.5x0.5_2000-2005'

run <- function(name, files, job) {
    obs = brick(dat_file)

    openDat <- function(file, years) {
        dat = brick(file)
        if (nlayers(dat) == 9) {
            w = sapply(layers, function(i) 1/sum(layers == i))
            dat = dat[[layers]] * w
            return(dat)
        }
        dat = brick(file, varname = levels[1])
        dates =  sapply(names(dat), function(i) strsplit(i, '.', fixed = TRUE)[[1]])
        yrs = sapply(dates[1,], function(i) strsplit(i, 'X')[[1]][2])
        index = which(apply(sapply(years, '==', yrs), 1, any))
        dat = lapply(levels, function(i)  brick(file, varname = i)[[index]])#brick(file, varname = i)[[index]])
    }
    print("opening")
    dat = lapply(files, openDat, years)
    
    print("grid box LAI")
    dati = dat[[1]][[1]]
    dati[] = 0
    print(length(levels))
    for (i in 1:length(levels)) {
        print(i)
        dati = dati +  dat[[1]][[i]] * dat[[2]][[i]]
    }
    
    print("regridding")
    dati = convert_pacific_centric_2_regular(dati)
    names(dati) = names(dat[[1]][[1]])
    dat0 = dati
    checkMonthly <- function(r){
        getYrMnth <- function(rd) {
            dates  = names(rd) 
            yrmnth = sapply(dates, function(i) paste(strsplit(i, '.', fixed = TRUE)[[1]][1:2], collapse = '.'))
        }
        yrmnth  = getYrMnth(r)
        yrmntho = getYrMnth(obs)
        yrmnthi = unique(yrmnth)
        
        ri = layer.apply(yrmnthi, function(m) mean(r[[which(yrmnth == m)]]))
        names(ri) = yrmnthi
        index = sapply(yrmnthi, function(i) any(yrmntho == i))
        ri = ri[[which(index)]]
        return(ri)
    }

    dati = checkMonthly(dati) 

    obs[obs > 9E9] = NaN
    obs = raster::resample(obs, dati[[1]])
    names(obs) = names(dati)
    
    print("outputting")
    out_file = paste0(out_dir, c(job, obs_file_out), '-', name, '-LAI.nc')
    
    print(out_file)
    writeRaster(dati, out_file[1], overwrite = TRUE)
    writeRaster(obs , out_file[2], overwrite = TRUE)
}
findJobs <- function(sim_dir, pattern)
    sapply(list.files(sim_dir), function(i) strsplit(i, paste0("-", pattern))[[1]][1])

jobs = mapply(findJobs, sim_dirs, names(sim_dirs))
jobs = jobs[[1]][sapply(jobs[[1]], function(i) any(i == jobs[[2]]))]
makeJob <- function(job) {
    findFiles <- function(sim_dir) {
        files = list.files(sim_dir, full.names = TRUE)
        file = files[grepl(job, files)]
        return(file)
    }
    files = sapply(sim_dirs, findFiles)
    
    runi <- function(name) run(name, files, job)

    runi('control')

    files0 = files
    files[2] = 'data/vegfrac_igbp.nc'
    runi('obsVegDist')

    levels = c("BDT", "BET-Tr", "BET-Te", "NDT", "NET")
    layers = c(1    , 1       , 1       , 2    , 2    )
    runi('trees-obsVegDist')

    files = files0
    runi('trees')
}

lapply(jobs, makeJob)
