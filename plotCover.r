source("libs/plotStandardMap.r")
source("libs/addLetLab.r")
source("libs/sd.raster.r")
library("plotrix")
library(maps)

library(raster)
library(rasterExtras)
source("../gitProjectExtras/gitBasedProjects/R/sourceAllLibs.r")
sourceAllLibs("../rasterextrafuns/rasterPlotFunctions/R/")
graphics.off()

obs_files = c(IGBP = "igbp", CCI = "cci", VCF = "VCF")

jobs = c("u-az513", "u-az515", "u-az524", "u-bb277", "u-bc179", "u-bc292", "u-bc370", "u-bc470", "u-bd288", "u-bd416", "u-bd483")

vars = c("tree", "wood", "shrub", "grass")

limits = seq(0, 0.9, 0.1)*100
cols = c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529')

files = list.files("outputs/", full.names = TRUE)


MM <- function(a, b) {
    if (nlayers(a) == 1) {  
        a = addLayer(a, 1-a)
        b = addLayer(b, 1-b) 
    }
    ar = raster::area(a)
    ar[is.na(a[[1]])] = NaN
    score = 2*sum.raster(abs(a-b) * ar, na.rm = TRUE)/ sum.raster(ar, na.rm = T)
    return(score)
}

median.raster <- function(...) 
    FUN.raster(median, ...)


compmn <- function(r, FUN = mean.raster) {
    averageize <- function(rl) {
        mn = rl
        mn[!is.na(rl)] = FUN(rl, na.rm = TRUE)
        return(mn)
    }
    if (nlayers(r)==1) mn = averageize(r) else mn = layer.apply(r, averageize)
    return(MM(r, mn))
}

compRR <- function(r) {
    mask = which(!is.na(r[[1]][]))
    Mod <- function(...) {
        randomize <- function(rl) {
            index = sample(mask, length(mask), replace = FALSE)
            rr = rl
            rr[mask] = rl[index]
            return(rr)
        }
        if (nlayers(r) ==1) rr = randomize(r) else rr = layer.apply(r, randomize)
        score = MM(r, rr)
        return(score)
    }
    scores = sapply(1:10, Mod)
    return(c(mean(scores), sd(scores)))
}

plotVariable <- function(var) {
     files = files[grepl(var, files)]

    openFile <- function(nm, nmtext, plotMe = TRUE) {
            
        addText <- function() {
            if (var == vars[[1]]) mtext(nmtext, side = 3)
            if (nm == obs_files[1]) {
                mtext(var, side = 2)
                #if (var == vars[length(vars)])
                    
            }
        }
        file = files[grepl(nm, files)]
        if (length(file) == 0) {
            if (plotMe) {
                plot.new()
                addText()
            }
            return(NULL)
        }
        dat = brick(file)
        if (nlayers(dat) == 1) dat = dat[[1]] else dat = mean(dat)
        dat = raster::crop(dat, c(-180, 180, -60, 90))
        if (nm == "VCF") dat = dat/100
        if (plotMe) {
            plotStandardMap(dat*100, cols = cols, limits = limits)
            addText()
        }
        return(dat)
    }
    obss = mapply(openFile, obs_files, names(obs_files))
    
    obss = obss[!sapply(obss, is.null)]
    
    sims = lapply(jobs, openFile, plotMe = FALSE)
    test = !sapply(sims, is.null)
    jobs = jobs[test]
    sims = sims[test]
    
    sims_mean =  mean(layer.apply(sims, function(i) i))
    sims_sd =  sd.raster( (layer.apply(sims, function(i) i)))
    plotStandardMap(sims_mean*100, e = sims_sd, cols = cols, limits = limits,  ePatternRes = 22, ePatternThick = 0.38, limits_error = c(0.01, 0.05))
    if (var == vars[[1]]) mtext(side = 3, 'UKESM')
    
    tab = sapply(sims, function(sim) sapply(obss, MM, sim))
    tabi = cbind(apply(tab, 1, mean), apply(tab, 1, sd))
    
    tab = cbind(tab, sapply(obss, compmn, median.raster))
    tab = cbind(tab, sapply(obss, compmn))
    
    rrs = sapply(obss, compRR)
    
    tab = cbind(tab, t(rrs))
    colnames(tab) = c(jobs, 'median', 'mean', 'randomly-resampled mean', 'randomly-resampled sd')
    
    fname = paste0("docs/", var, "-MM-full.csv")
    tab = round(tab, 3)
    write.csv(tab, file = fname)
    
    colnames(tabi) = c("UKESM-mean", "UKESM-sd")
    tabi = cbind(tabi, tab[, (ncol(tab)-3):ncol(tab)])
    
    fname = paste0("docs/", var, "-MM-summ.csv")
    tabi = round(tabi, 3)
    write.csv(tabi, file = fname)
    return(list(obss, sims))
}

png("figs/vegDist.png", height = 183/2, width = 183, units = 'mm', res = 450)
    layout(rbind(t(matrix(1:16, nrow = 4, ncol = 4)), 17), heights = c(1, 1, 1, 1, 0.3))
    par( mar = rep(0,4), oma = rep(1.5, 4))
    out = lapply(vars, plotVariable)
    StandardLegend(cols, limits, out[[1]][[1]][[1]], extend_max = FALSE, maxLab = 100, add = FALSE)
dev.off()
itemComparison <- function(obs_name) {
    if(any(names(out[[1]][[1]])==obs_name)) index = c(1, 3, 4) else index = c(2, 4)
    makeItemObs <- function(i) out[[i]][[1]][[which(names(out[[i]][[1]]) == obs_name)]]
    obsi = layer.apply(index, makeItemObs)
    obsi = addLayer(obsi, 1 - sum(obsi))
    
    makeItemsMod <- function(mno) {
       makeItemMod <- function(i)  out[[i]][[2]][[mno]]
       modi = layer.apply(index, makeItemMod)
       modi = addLayer(modi, 1-sum(modi))
       return(modi)
    }       
    modsi = lapply(1:length(out[[1]][[2]]), makeItemsMod)
    
    scores = sapply(modsi, MM, obsi)
    scores = c(scores, mean(scores), sd(scores),
               compmn(obsi, median.raster), compmn(obsi), compRR(obsi))
    names(scores) = c(jobs, 'UKESM-mean', 'UKESM-sd',
                      'median', 'mean', 'randomly-resampled mean', 'randomly-resampled sd')
    return(scores)
}

items_scores = sapply(names(obs_files), itemComparison)
tab = round(t(items_scores), 3)

tabFull = tab[,c(1:(ncol(tab)-6), (ncol(tab)-3):ncol(tab))]
write.csv(tabFull, file = "docs/items-MM-full.csv")

tabSumm = tab[,(ncol(tab)-5):ncol(tab)]
write.csv(tabSumm, file = "docs/items-MM-summ.csv")

