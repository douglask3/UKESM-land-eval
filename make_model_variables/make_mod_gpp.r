source("cfg.r")

file_ids = c(GPP = 'GPP-19182.nc', vegFrac = 'VegFracs-19013.nc')

years = 2000:2006

levels = c("BDT", "BET-Tr", "BET-Te", "NDT", "NET", "DSH", "ESH", "C3G", "C4G", "C3C", "C4C", "C3P", "C4P")
layers = c(1    , 1       , 1       , 2    , 2    , 5    , 5    , 3    , 4    , 3    , 4    , 3    , 4    )
out_dir = 'outputs/GPP/'
makeDir(out_dir)

dat_fnames = rev(c('gpp_0.5x0.5.nc', 'gpp.nc'))
dat_files = paste0('data/', dat_fnames)
obs_file_out = dat_fnames

run <- function(name, files, job, dat_file) {
    obs = try(brick(dat_file))
    if (class(obs) == "try-error") {
        return(invisible())
    }
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
    
    print("grid box GPP")
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
    
    oyrs = sapply(names(obs), function(i) substr(strsplit(i, '.', fixed = TRUE)[[1]][1], 2, 5))
    tyrs = which(sapply(oyrs, function(i) any(i==years)))
    obs = obs[[tyrs]]

    obs[obs > 9E9] = NaN
    obs = raster::resample(obs, dati[[1]])
     
    names(obs) = names(dati)
    
    print("outputting")
    out_file = paste0(out_dir, c(job, 'Observation'), '-', name,
                      filename.noPath(dat_file, noExtension=TRUE), '.nc')
    
    print(out_file)
    
    writeRaster(dati, out_file[1], overwrite = TRUE)
    writeRaster(obs , out_file[2], overwrite = TRUE)
}
findJobs <- function(sim_dir, pattern)
    sapply(list.files(sim_dir), function(i) strsplit(i, paste0("-", pattern))[[1]][1])

#jobs = mapply(findJobs, sim_dirs, names(sim_dirs))
#jobs = jobs[[1]][sapply(jobs[[1]], function(i) any(i == jobs[[2]]))]
makeJob <- function(job, dat_file) {
    
    findFiles <- function(id)
        mod_files[grepl(id, mod_files) & grepl(job, mod_files)]

    files = sapply(file_ids, findFiles)
    
    runi <- function(name) run(name, files, job, dat_file)

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

lapply(dat_files, function(i) lapply(jobs, makeJob, i))
