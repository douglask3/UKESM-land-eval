source("libs/PolarConcentrationAndPhase.r")
source("libs/polarPlot.r")
source("libs/plotStandardMap.r")
source("libs/SeasonLegend.r")
source("libs/YearlySeason.r")
source("libs/addLetLab.r")
library("plotrix")
library(maps)

library(raster)
library(rasterExtras)
source("../gitProjectExtras/gitBasedProjects/R/sourceAllLibs.r")
sourceAllLibs("../rasterextrafuns/rasterPlotFunctions/R/")
graphics.off()

global_extent = extent(c(-180, 180, -60, 90))

files = c("Observation" = 'outputs/lai_0.5x0.5_2000-2005-LAI.nc',
          "Simulation"  = "outputs/u-bb075-control-LAI.nc")
          
Biomes = "outputs/full_biome_realms.nc"
               
regions = list('Eurasia Boreal\nForest' = c(-10, 180, 60, 90),
               'Europe Northern Temperate\nForest' = c(-10, 40, 30, 60),
               'Africa Northern Tropical\nForest' = c(-20, 50, 0, 30),
               'Africa Southern Tropical\nForest' = c(-10, 50, -30, 0),
               'South America Southern Temperate\nForest' = c(-80, 30, -60, -30))
axisMonth = c(2, 6, 4, 8)
greens9   = c("#F7FFFB", "#DEF7EB", "#C6EFDB", "#9EE1CA", "#6BD6AE", 
            "#42C692", "#21B571", "#089C51", "#086B30")

units_aa = ''                
limits_aa = c(0, 0.1, 0.2, 0.5, 1, 2, 5)                
aa_cols = c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529')

phase_cols = c('#313695', '#a50026', '#ffff00','#313695')
conc_cols = c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58')

modal_cols = c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177','#49006a')#c('#34eeba','#d95f02','#7570b3')
modal_limits = c(1, 1.1, 1.2, 1.5, 2)

limits_rank = seq(0, 0.9, 0.1)
cols_rank = c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529')

Biomes = raster(Biomes)
#ObsCover = convert_pacific_centric_2_regular(ObsCover)


run <- function(name) {
    obs = lapply(files, brick)
    mxLayers = min(sapply(obs, nlayers))
    obs = obs_ia =lapply(obs, function(i) i[[1:mxLayers]])

    convert2Climatology <- function(r) 
        layer.apply(1:12, function(mn) mean(r[[seq(mn, nlayers(r), by = 12)]]))

    obs = lapply(obs, convert2Climatology)
    obs = lapply(obs, raster::crop, global_extent)


    ModalMap <- function(obs, txt, addLegend, let) {
        modal = testModal(obs)
        modal_approx = layer.apply(2:6, function(i) modal[[i]] * (0.5-cos(2*pi * modal[[i+6]]/12)/2))
        modal_approx = 1 + sum(modal_approx, na.rm = TRUE)/modal[[1]]

        plotStandardMap(modal_approx -1, limits = modal_limits -1, cols = modal_cols)
        addLetLab(let)
        mtext(txt, side = 2)
        if (addLegend) 
            StandardLegend(limits = modal_limits - 1, cols = modal_cols, dat = modal_approx-1,
                           extend_max = TRUE,
                           labelss = modal_limits, add = TRUE) 
        return(modal)
    }

    srank.raster <- function(r1, r2, lab = '', name = '',season = NULL) {
        if(!is.null(season)) {
            r1 = YearlySeason(season, r1)
            r2 = YearlySeason(season, r2)
        }
        mask = !any(is.na(r1+r2))
        srank.cell <- function(v1, v2) 
             cor.test(v1, v2, method = "spearman")[[4]]
        
        out = r1[[1]]
        out[mask] = mapply(srank.cell, as.data.frame(t(r1[mask])), as.data.frame(t(r2[mask])))
        out[!mask] = NaN
        
        plotStandardMap(out, limits = limits_rank, cols = cols_rank)
        mtext(name, side = 2, adj = 0.9, line = -0.2)
        addLetLab(lab)
        StandardLegend(out, limits = limits_rank, cols = cols_rank,
                       extend_max = FALSE, maxLab = 1, add = TRUE, oneSideLabels = FALSE)
        return(out)
    }
    
    fname = paste0("figs/fire_var_seasonality-maps-", name, ".png")
    
    png(fname, height = 110, width = 183, res = 300, units = 'mm')
        par(oma = c(1, 2, 1, 0))
        layout(cbind(c(1, 1, 6, 6, 7, 7), c(2, 3, 8, 8, 11, 11), c(2, 3, 8, 9, 9, 11), c(2, 3, 8, 8, 11, 11), c(4, 5, 10, 10, 12, 12)),
               heights = c(1, 1, 0.6, 0.4, 0.65, 0.35), width = c(1, 0.55, 0.3, 0.15, 1))
        
        mask = !any(is.na(obs[[1]]+obs[[2]]))
        x = as.vector(obs[[1]][mask]); y = as.vector(obs[[2]][mask])
        #x  = log(x0+0.0001); y = log(y0+0.0001)
        
        cols = densCols(x,y, colramp = colorRampPalette(greens9), bandwidth = 1)
        par(mar = c(3, 2, 0.5, 1.5)) 
        plot(y~x, pch = 19, col = cols, cex = 1, xlab = '', ylab = '')
        addLetLab('a')
        mtext.units(side = 1, line = 2, names(files)[1])
        mtext.units(side = 2, line = 2, names(files)[2])
        addAxis <- function(labels, side) {
            at = log(labels + 0.0001)
            axis(at = at, labels = labels, side = side)
            if (side == 1 || side == 3) FUN = function(i, ...)lines(c(i, i), c(-9E9, 9E9), ...)
                else  FUN = function(i, ...)lines(c(-9E9, 9E9), c(i, i), ...)
            lapply(at, FUN, lty = 2, col = make.transparent("black", 0.67))
        }
        #addAxis(c(0, 0.001, 0.01, 0.1, 1, 10, 100), 1)
        #addAxis(c(0, 0.001, 0.01, 0.1, 1, 10, 100), 2)
        abline(lm(y~x))
        mtext.units(side = 3, paste0("~R2~: ",  round(cor(x, y)^2, 2)), line = -2.8, adj = 0.1)
        mtext.units(side = 3, "p < 0.001", line = -4, adj = 0.1)
        
        par(mar = rep(0, 4)) 
        plotAA <- function(r, lab, name) {
            aa = mean(r)        
            plotStandardMap(aa, limits = limits_aa, cols = aa_cols)
            mtext(name, side = 2, adj = 0.9, line = -0.2)
            addLetLab(lab)
            StandardLegend(aa, limits = limits_aa, cols = aa_cols, units = units_aa,
                           add = TRUE, oneSideLabels = FALSE)
            return(aa)
        }
        aas = mapply(plotAA, obs, c('b', 'c'), names(files))
        
        srank.month = srank.raster(obs_ia[[1]], obs_ia[[2]], 'j', 'monthly rank')
        #srank.annual = srank.raster(obs_ia[[1]], obs_ia[[2]], 'k', 'annual rank', season = 8:13)
        srank.annual = srank.raster(obs_ia[[1]], obs_ia[[2]], 'j', 'monthly rank')
        
        modals = mapply(ModalMap, obs, names(files), c(T, F), c('d', 'g')) 
        
        obs_in = obs
        obs[[1]] = obs_in[[1]][[c(12, 1:11)]]
        #obs[[2]] = obs_in[[2]][[c(8:12, 1:7)]]
        obs[[2]] = obs_in[[2]][[c(12, 1:11)]]
        #obs[[2]] = obs_in[[2]][[c(6:12, 1:6)]]
        
        pc = lapply(obs, PolarConcentrationAndPhase.RasterBrick, phase_units = "months")

        plotConPhase <- function(pc, let, addLegend = FALSE) {
            plotStandardMap(pc[[1]], limits = 0.5:11.5, cols = phase_cols)
            addLetLab(let[1])
            if (addLegend) SeasonLegend(0.5:11.5, cols = phase_cols, add = FALSE)
            plotStandardMap(pc[[2]], limits = seq(0, 1, 0.1), cols = conc_cols)
            addLetLab(let[2])
            if (addLegend) StandardLegend(limits = seq(0, 0.9, 0.1), cols = conc_cols, extend_max = FALSE,
                                          maxLab = 1, dat = pc[[2]], add = TRUE, oneSideLabels = FALSE) 
        }

        mapply(plotConPhase, pc, list(c('e', 'f'), c('h', 'i')), c(TRUE, FALSE))  
    dev.off()    
    
    #writeRaster(pc[[1]], 'outputs/MODIS_pc.nc')
    #writeRaster(modals[[1]], 'outputs/MODIS_modal.nc')
    #writeRaster(aas[[1]], 'outputs/MODIS_aa.nc')

    plotRegion <- function(region, name, axisMonth) {
        print(region)
        obs = lapply(obs, function(i) layer.apply(i, function(j) {j[Biomes[]!= region | is.na(Biomes[])] = NaN; j}))
        #obs = lapply(obs, raster::crop, extent(region))
        #   ObsCover = raster::resample(ObsCover, obs[[1]][[1]])
        
        
        #obs = lapply(obs, function(i) layer.apply(i, function(j) {j[sum(ObsCover[[1:2]]) < 0.5] = NaN; j}))
        
        #obs[ObsCover < 0.5] = NaN
        
        getQuants <- function(obs) {
            obsv = layer.apply(obs, function(i) quantile(i, c(0.25, 0.5, 0.75)))  
            obsv = matrix(unlist(obsv), nrow = 3)
            return(obsv)
        }
        obsv = lapply(obs, getQuants)
        maxObsV = sapply(obsv, max)
        obsv = mapply('/', obsv, maxObsV, SIMPLIFY = FALSE)
        
        xlim =  c(-1,1)

        polarPlot.setup(1:12, obsv[[1]][2,], type = 'l', xlim = xlim, col = 'blue', lwd = 2)
        polarPlot.lines(1:12, obsv[[2]][2,], col = "red", lwd = 2)
        polarPlot.addGuides(xlim = xlim, axisMonth = axisMonth, labScale = maxObsV[1], nguides = 4, col = "blue")
        polarPlot.addGuides(xlim = xlim, axisMonth = axisMonth+1, labScale = maxObsV[2], nguides = 4, col = "red")

        polarPlot.polygon(1:12, obsv[[1]][c(1, 3),], col = 'blue', alpha = 0.67, border = TRUE)
        polarPlot.polygon(1:12, obsv[[2]][c(1, 3),], col = 'red', alpha = 0.67, border = TRUE)
        mtext(side = 3, line = 0, name, adj = 0.1)
        return(obsv)
    }
    fname = paste0("figs/fire_var_seasonality-TS-", name, ".png")
    
    index =  unique(Biomes)
    lmat = rbind(t(matrix(index, nrow = 4)), 29)
    png(fname, height = 183*nrow(lmat)/ncol(lmat), width = 183, res = 300, units = 'mm')    
        
        layout(lmat, heights = c(rep(1, nrow(lmat)), 0.3))
        par(mar = c(0, 0, 3, 1.5))
        
        obsv = mapply(plotRegion, index, paste(letters[1:length(index)],  index, sep = ') '),
                      axisMonth, SIMPLIFY = FALSE)
        par(mar = c(2, 3, 2, 0))
        plot(c(0, 1), c(0,1), type = 'n', axes = FALSE, xlab = '', ylab = '')
        mtext.units(names(files)[1], col = "blue", adj = 0, side = 3, line = -2)
        mtext.units(names(files)[2], col = "red", adj = 0, side = 3, line = -3.8)
    dev.off()
}

run("control")

files[2] = "outputs/u-bb075-trees-LAI.nc"
run("trees")

files[2] = "outputs/u-bb075-obsVegDist-LAI.nc"
run("obsVegDist")

files[2] = "outputs/u-bb075-trees-obsVegDist-LAI.nc"
run("obsVegDist-trees")
