library(raster)
source("libs/plotStandardMap.r")
source("libs/addLetLab.r")
source("libs/sd.raster.r")
library("plotrix")
source("libs/loadRegionInfo.r")
library(maps)

library(raster)
library(rasterExtras)
source("../gitProjectExtras/gitBasedProjects/R/sourceAllLibs.r")
sourceAllLibs("../rasterextrafuns/rasterPlotFunctions/R/")
graphics.off()

obs_files = c(IGBP = "igbp", CCI = "cci", VCF = "VCF")

jobs = c("u-az513", "u-az515", "u-az524", "u-bb277", "u-bc179", "u-bc292", "u-bc370", "u-bc470", "u-bd288", "u-bd416", "u-bd483")[1:3]

vars = c(Tree = "tree", Wood = "wood", Shrub = "shrub", Herb = "herb", Grass = "grass",
         "Bare Soil" = "bares")

limits = seq(0, 0.9, 0.1)*100
cols = c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529')

files = list.files("outputs/", full.names = TRUE)


MM <- function(a, b, byRegions = TRUE) {
    print("running MM")
    if (nlayers(a) == 1) {  
        a = addLayer(a, 1-a)
        b = addLayer(b, 1-b) 
    }
    
    ar = raster::area(a)
    ar[is.na(regions)] = NaN
    ar[is.na(a[[1]])] = NaN
    MMi <- function(region) {
        if (!is.na(region)) ar[regions!=region] = 0
        score = 2*sum.raster(abs(a-b) * ar, na.rm = TRUE)/ sum.raster(ar, na.rm = T)    
        return(score)
    }
    if (byRegions) {
        scores = sapply(c(NaN, 1:28), MMi)
        names(scores) = c("Global", region_names)
    }
    else scores = MMi(NaN)
    return(scores)
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
    return(cbind(apply(scores, 1, mean), apply(scores, 1, sd)))
}

areaWmean <- function(r) {
    ar = raster::area(r)
    ar[is.na(r)] = 0.0
    ar[is.na(regions)] = 0.0
    r[is.na(regions)] = NaN

    fRegion <- function(region, std = FALSE) {
        if (!is.na(region)) {
            test = regions != region
            r[test] = NaN
            ar[test] = 0.0
        }
        if (std) out = FUN.raster(sd, r, na.rm = TRUE)
        else out = sum.raster(r * ar, na.rm = TRUE)/sum.raster(ar, na.rm = TRUE)
        return(out)
    }
    fRegions <- function(...) {
        out = sapply(c(NaN, 1:28), fRegion, ...)
        names(out) = c("Global", region_names)
        return(out)
    }
    mns = fRegions()
    sds = fRegions(std = TRUE)  
    out = cbind(mns, sds)
    colnames(out) = c("cover mean", "cover sd")
    return(out)   
}


plotVariable <- function(var, vname) {
     files = files[grepl(var, files)]

    openFile <- function(nm, nmtext, plotMe = TRUE) {
            
        addText <- function() {
            if (var == vars[[1]]) mtext(nmtext, side = 3)
            if (nm == obs_files[1]) {
                mtext(vname, side = 2)
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
        if (nm == "VCF" && var != "bares") dat = dat/100
        if (var == "bares") dat = 1 - dat
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
    plotStandardMap(sims_mean*100, e = sims_sd, cols = cols, limits = limits,
                    ePatternRes = 22, ePatternThick = 0.38, limits_error = c(0.01, 0.05))
    if (var == vars[[1]]) mtext(side = 3, 'UKESM')
    
    benchmarkObs <- function(obs, name) {
        tab = lapply(sims, areaWmean)
        tab = do.call(cbind, tab)
        colnames(tab) = paste(rep(jobs, each = 2), '-', colnames(tab))
        tab_mns = cbind(areaWmean(obs), tab)
        colnames(tab_mns)[1:2] = c("Obs - cover mean", "Obs - sd")

        tab_ben = sapply(sims, MM, obs)
        colnames(tab_ben) = paste(jobs, '- MM')
        
        tab_null = cbind(compmn(obs, median.raster), compmn(obs), compRR(obs))       
        colnames(tab_null) = c('median', 'mean', 'randomly-resampled mean',
                          'randomly-resampled sd')

        tabi <- function(tab) cbind(apply(tab, 1, mean), apply(tab, 1, sd))
        
        tab_ens = cbind(tab_mns[,1],
                        tabi(tab_mns[,seq(3, ncol(tab_mns)-1, by = 2)]),
                        tabi(tab_ben))
        colnames(tab_ens) = c("Obs - cover", 
                              "UKESM - cover mean", "UKESM - cover sd",
                              "UKESM - MM mean", "UKESM - MM sd")
        
        tab = cbind(tab_mns, tab_ben, tab_null)
        fname = paste0("docs/", var, '-', name, "-MM-full.csv")
        tab = round(tab, 3)
        write.csv(tab, file = fname)

        tab_ens = cbind(tab_ens, tab_null)
        fname = paste0("docs/", var, '-', name, "-MM-summ.csv")
        tab_ens = round(tab_ens, 3)
        write.csv(tab_ens, file = fname)

        #ens_range = cbind(tab_ens[, 4] - tab_ens[, 5], tab_ens[, 4] + tab_ens[, 5])
        nulls = cbind(tab_ens[,6:7], tab_ens[,8] - tab_ens[,9], tab_ens[,8] + tab_ens[,9])
        index = 1:ncol(nulls)
        
        nbeats = apply(cbind(nulls, tab_ben), 1,
                       function(i) sum(sapply(i[-index], '<', i[index])))
        return(nbeats)    
    }
    bench = mapply(benchmarkObs, obss, names(obss))
    #name = paste0("docs/", var, "-MM-full.csv")
    #tab = round(tab, 3)
    #write.csv(tab, file = fname)
    
    #colnames(tabi) = c("UKESM-mean", "UKESM-sd")
    #tabi = cbind(tabi, tab[, (ncol(tab)-3):ncol(tab)])
    
    #fname = paste0("docs/", var, "-MM-summ.csv")
    #tabi = round(tabi, 3)
    #write.csv(tabi, file = fname)
    return(list(obss, sims, bench))
}

png("figs/vegDist.png", height = 3 * 183/4, width = 183, units = 'mm', res = 450)
    layout(rbind(t(matrix(1:24, nrow = 4)), 25), heights = c(1, 1, 1, 1, 1, 1, 0.3))
    par( mar = rep(0,4), oma = rep(1.5, 4))
    out = mapply(plotVariable, vars, names(vars))
    StandardLegend(cols, limits, out[[1]][[1]][[1]], extend_max = FALSE,
                   maxLab = 100, add = FALSE)
dev.off()

scores = out[3,]

png("figs/MMscoreCols.png", height = 15, width = 5, units = 'in', res = 300)
plot(c(-2, length(scores) + 0.5), c(-3, 29.5), axes = FALSE, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
text(y = 0, x = 1:6,  names(vars))
text(y = 1:29, x = 0, adj = 1, xpd = TRUE, c("Global", region_names), srt = 0)

bcols = make_col_vector(c('#a50026', '#fee090', '#313695'), ncol = 13)
addScoreCols <- function(score, x_main) {
    addCol <- function(rn) {
        cs = score[rn,]
        if (length(cs) == 2) {
            xs = list(c(-0.5, 0.5, -0.5), c(-0.5, 0.5, 0.5))
            ys = list(c(-0.5, 0.5, 0.5), c(-0.5, 0.5, -0.5))
        } else {
            xs = list(c(0, -0.5, -0.5, 0), c(0, 0.5, 0.5, 0), c(0, -0.5, 0.5))
            ys = list(c(0, -0.5, 0.5, 0.5), c(0, -0.5, 0.5, 0.5), c(0, -0.5, -0.5))
        }
        addPoly <- function(xs, ys, col) 
            polygon(x_main + xs, rn + ys, col = col)
        mapply(addPoly, xs, ys, bcols[cs+1])
    }
    
    lapply(1:nrow(score), addCol)
}
mapply(addScoreCols, scores, 1:length(scores))
dev.off()
browser()
itemComparison <- function(obs_name) {
    if(any(names(out[[2]][[1]])==obs_name)) index = c(1, 3, 5, 6) else index = c(1, 4, 6)
    makeItemObs <- function(i) out[[i]][[1]][[which(names(out[[i]][[1]]) == obs_name)]]
    obsi = layer.apply(index, makeItemObs)
    #obsi = addLayer(obsi, 1 - sum(obsi))
    
    makeItemsMod <- function(mno) {
       makeItemMod <- function(i)  out[[i]][[2]][[mno]]
       modi = layer.apply(index, makeItemMod)
       #modi = addLayer(modi, 1-sum(modi))
       return(modi)
    }       
    modsi = lapply(1:length(out[[1]][[2]]), makeItemsMod)
    
    scores = sapply(modsi, MM, obsi)
    
    scores = cbind(scores, mean(scores), sd(scores),
               compmn(obsi, median.raster), compmn(obsi), compRR(obsi))
    colnames(scores) = c(jobs, 'UKESM-mean', 'UKESM-sd',
                      'median', 'mean', 'randomly-resampled mean', 'randomly-resampled sd')
    
    tab = round(scores, 3)

    tabFull = tab[,c(1:(ncol(tab)-6), (ncol(tab)-3):ncol(tab))]
    write.csv(tabFull, file = paste0("docs/items-", obs_name, "-MM-full.csv"))

    tabSumm = tab[,(ncol(tab)-5):ncol(tab)]
    write.csv(tabSumm, file = paste0("docs/items-", obs_name, "-MM-summ.csv"))
    return(scores)
}

items_scores = sapply(names(obs_files), itemComparison)


