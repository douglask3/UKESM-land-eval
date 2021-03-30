source("libs/PolarConcentrationAndPhase.r")
source("libs/polarPlot.r")
source("libs/plotStandardMap.r")
source("libs/SeasonLegend.r")
source("libs/YearlySeason.r")
source("libs/addLetLab.r")
source("libs/ModalMap.r")
source("libs/srank.raster.r")
source("libs/plotAA.r")
source("libs/plotConPhase.r") 
source("libs/convert2Climatology.r")
source("libs/loadRegionInfo.r")
library("plotrix")
library(maps)

library(raster)
library(rasterExtras)
source("../gitProjectExtras/gitBasedProjects/R/sourceAllLibs.r")
sourceAllLibs("../rasterextrafuns/rasterPlotFunctions/R/")
sourceAllLibs("../benchmarkmetrics/benchmarkMetrics/R/")
graphics.off()

global_extent = extent(c(-180, 180, -60, 90))

          
sim_files = list.files("outputs/GPP/", pattern  = "u-", full.names = TRUE)
files = list("Observation" = 'outputs/GPP/Observation-controlgpp_0.5x0.5.nc',
             "Simulation"  = sim_files[grepl("control", sim_files)])
          
obsSc = 60*60*24*360

axisMonth = c(2, 6, 4, 8)
greens9   = c("#FBFFFE", "#F7FFFB", "#EAFDEF",  "#DEF7EB", "#C6EFDB", "#9EE1CA", "#6BD6AE", 
            "#42C692", "#21B571", "#089C51", "#086B30", "#043617", "#001108")

aa_cols = c('#ffffcc','#999996','#2c7fb8','#202030','#101020', '#000020')

limits_aa = c(0, 0.1, 0.2, 0.5, 1, 2, 3, 4)
dlims_aa = c(-2, -1, -0.5, -0.2, -0.1, 0.1, 0.2, 0.5, 1, 2)
dcols_aa = c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788','#2d004b')

aa_title = "Simulated veg distribution"
units_aa = 'kg~m-2~ ~yr-1~'

modMapping = NULL#list(7:12, 1:6)               

run <- function(name) {    
    dlapply <- function(r,  ...) lapply(r, lapply, ...)
    dsapply <- function(r,  ...) sapply(r, sapply, ...)
    obs = dlapply(files,  brick)
    obs[[1]][[1]]= obs[[1]][[1]] * obsSc
    mxLayers = min(unlist(dsapply(obs, nlayers)))
    obs = obs_ia = dlapply(obs, function(i) i[[1:mxLayers]])
    
    obs = dlapply(obs, convert2Climatology)
    obs = dlapply(obs, raster::crop, global_extent)    
    
    switchMonths <- function(r, switchy = NULL) {
        r_in = r
        r = r_in[[c(12, 1:11)]]
        r_in = r
        if (!is.null(switchy)) {
           r = r[[c(7:12,1:6)]] 
        }
        return(r)
    }
    #obs = list(lapply(obs[[1]], switchMonths), lapply(obs[[2]], switchMonths, modMapping))   
    
    #obs_in = obs
    #obs[[1]] = obs_in[[1]][[c(12, 1:11)]]
    #obs[[2]] = obs_in[[2]][[c(12, 1:11)]]
    #obs[[2]] = obs_in[[2]][[c(7:12, 1:6)]]
    
    fname = paste0("figs/fire_var_seasonality-maps-AA-scatter", name, ".png")
    png(fname, height = 4, width = 7.2/2, res = 300, units = 'in')
        par(oma = c(1, 2, 1, 0))
        
        mask = !any(is.na(obs[[1]][[1]]+obs[[2]][[1]]))
        maskFUN <- function(r) lapply(r, function(i) as.vector(mean(i)[mask]))
        x = maskFUN(obs[[1]]); y = maskFUN(obs[[2]])
        
        if (length(x) == 1) {
            x = rep(x[[1]], length(y))
            y = unlist(y)
        } else {
            browser()
        }
        xp  = log(x+0.2); yp = log(y+0.2)
        cols = densCols(xp,yp, colramp = colorRampPalette(greens9), bandwidth = 1)
        par(mar = c(3, 2, 0.5, 1.5)) 
        plot(yp~xp, pch = 19, col = cols, cex = 1, xaxs = 'i', yaxs = 'i',
             xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
        
        #addLetLab('a')
        mtext.units(side = 1, line = 2, names(files)[1])
        mtext.units(side = 2, line = 2, names(files)[2])
        addAxis <- function(labels, side) {
            at = log(labels + 0.2)
            axis(at = at, labels = labels, side = side)
            if (side == 1 || side == 3) FUN = function(i, ...)lines(c(i, i), c(-9E9, 9E9), ...)
                else  FUN = function(i, ...)lines(c(-9E9, 9E9), c(i, i), ...)
            lapply(at, FUN, lty = 2, col = make.transparent("black", 0.67))
        }
        addAxis(c(0, 0.1, 0.2, 0.5, 1, 2, 5, 10, 2, 5, 100), 1)
        addAxis(c(0, 0.1, 0.2, 0.5, 1, 2, 5, 10, 2, 5, 100), 2)
        #abline(lm(y~x))
        mtext.units(side = 3, paste0("~R2~: ",  round(cor(x, y)^2, 2)), line = -2.8, adj = 0.1)
        mtext.units(side = 3, "p < 0.001", line = -4, adj = 0.1)
    dev.off()
    fname = paste0("figs/fire_var_seasonality-maps-AA-maps", name, ".png")
    png(fname, height = 5.6, width = 4, res = 300, units = 'in')
        par(oma = c(1, 3, 3, 0), mfrow = c(3, 1), mar = rep(0, 4)) 
        
        aas = mapply(plotAA, obs[2:1], '', c('UKESM', 'Observed'),
                     units = units_aa,addLegend = c(FALSE, TRUE) )
        diff = layer.apply(obs[[2]], function(i) i- obs[[1]][[1]])
       
        plotStandardMap(mean(diff), e = eFromRange(diff),
                        limits = dlims_aa, 
                        cols = dcols_aa, units = units_aa)
        mtext("Difference", side = 2, adj = 0.9, line = -0.2)
        StandardLegend(mean(diff), limits = dlims_aa, cols = dcols_aa, units = units_aa,
                                  add = TRUE, oneSideLabels = FALSE, extend_min = TRUE)
        mtext(side = 3, outer = TRUE, aa_title)
        scoresRegion <- function(region, r, regions) {
            scoreEns <- function(sim, obs) {
                if (!is.nan(region)) {
                    obs[regions != region] = NaN
                    sim[regions != region] = NaN
                }
            
                score(NME(obs, sim, w = raster::area(r[[1]])))
            }
            obs = layers2list(r[[1]])
            sim = layers2list(r[[2]])
            compareObs <- function(i) {
                scores = sapply(sim, scoreEns, i)
                return(apply(scores, 1, range))
            }
            lapply(obs, compareObs)
        }
        layers2list <- function(r) unlist(layer.apply(r, function(i) c(i)))
        #aa_scores = lapply(c(NaN, 1:28), scoresRegion, aas, regions) 
    dev.off()
        ##srank.month = srank.raster(obs_ia[[1]], obs_ia[[2]], 'j', 'monthly rank')
        ##srank.annual = srank.raster(obs_ia[[1]], obs_ia[[2]], 'j', 'monthly rank')
    fname = paste0("figs/fire_var_seasonality-maps-MPC", name, ".png")
    png(fname, height = 3.5, width = 7.2, res = 300, units = 'in')
        
        par(oma = c(1, 2, 1, 0), mar = rep(0, 4))
        layout(cbind(c(1, 2, 3), c(4, 6, 7), c(5, 8, 0)))

        modals = mapply(ModalMap, obs, c('', ''), c(F, T), let = c('Modality', '')) 
    
        mtext(side = 2, 'Observation', adj = 1-1/8, outer = TRUE)
        mtext(side = 2, 'Simulation', adj = 0.5, outer = TRUE)
        mtext(side = 2, '(min)', adj = 0.45, outer = TRUE, line = -1)
        mtext(side = 2, '(max)', adj = 1/8, outer = TRUE, line = -1)
        
        mapply(plotConPhase, obs, list(c('Phase', 'Concentration'), c('', '')), c(F, T))  
    dev.off()    
        
    plotRegion <- function(region, name, axisMonth) {
        
        tempFile = paste0("temp/polarClim_for_region-3ID-", region, 
                          tail(strsplit(files[[2]][1], '-')[[1]], 2)[1], ".Rd")
        if (file.exists(tempFile) && F) {
            load(tempFile)
        } else {
            print(region)
            maskRegion <- function(j) {
                j[regions[]!= region | is.na(regions[])] = NaN
                return(j)
            }
            
            obs = dlapply(obs, function(i) layer.apply(i, maskRegion))       
        
            getQuants <- function(obs) {
                obsv = layer.apply(obs, function(i) quantile(i, c(0.25, 0.5, 0.75)))  
                obsv = matrix(unlist(obsv), nrow = 3)
                return(obsv)
            }
            obsv = dlapply(obs, getQuants)
            
            maxObsV = max(unlist(dlapply(obsv, max)))
            obsv = dlapply(obsv, '/', maxObsV)
            #obsv = mapply( function(i, j) mapply('/', i, j, SIMPLIFY = FALSE), , maxObsV )
            save(obsv, maxObsV, file = tempFile)
        }
        xlim =  c(-1,1)
        
       
        polarPlot.setup(1:12, obsv[[1]][[1]][2,],
                        type = 'l', xlim = xlim, col = 'blue', lwd = 2)
        lapply(obsv[[1]], function(i) polarPlot.lines(1:12, i[2,], col = 'blue', lwd = 2))
        lapply(obsv[[2]], function(i) polarPlot.lines(1:12, i[2,], col = "red", lwd = 2))
        polarPlot.addGuides(xlim = xlim, axisMonth = axisMonth, labScale = maxObsV
, nguides = 4, col = "black")
        #if (maxObsV[2] == 0 ) maxObsV[2] = maxObsV[1]
        #polarPlot.addGuides(xlim = xlim, axisMonth = axisMonth+1, labScale = maxObsV[2], nguides = 4, col = "red")
        
        lapply(obsv[[2]], function(i) polarPlot.polygon(1:12, i[c(1, 3),], col = 'red', alpha = 1-0.33/length(obsv[[2]]), border = FALSE))
        lapply(obsv[[1]], function(i) polarPlot.polygon(1:12, i[c(1, 3),], col = 'blue', alpha = 0.67, border = FALSE))
        mtext(side = 3, line = 0, name, adj = 0.1)
        return(obsv)
    }
    fname = paste0("figs/fire_var_seasonality-TS-", name, ".png")
    
    index =  unique(regions)
    lmat = rbind(t(matrix(index, nrow = 4)), 29)
    png(fname, height = 183*nrow(lmat)/ncol(lmat), width = 183, res = 300, units = 'mm')    
        
        layout(lmat, heights = c(rep(1, nrow(lmat)), 0.3))
        par(mar = c(0, 0, 3, 1.5))
        
        obsv = mapply(plotRegion, index,
                      paste(letters[1:length(index)], region_names, sep = ') '),
                      axisMonth, SIMPLIFY = FALSE)
        par(mar = c(2, 3, 2, 0))
        plot(c(0, 1), c(0,1), type = 'n', axes = FALSE, xlab = '', ylab = '')
        mtext.units(names(files)[1], col = "blue", adj = 0, side = 3, line = -2)
        mtext.units(names(files)[2], col = "red", adj = 0, side = 3, line = -3.8)
    dev.off()
}

run("control-gpp")

#files[2] = sim_files[grepl("control", sim_files)
#run("trees")
aa_title = "Corrected veg distribution"
files[2] = sim_files[grepl("-obsVegDist", sim_files) & !grepl("trees", sim_files)]
run("obsVegDist-gpp")

aa_cols = c('#f7fcfd','#e5f5f9','#ccece6','#99d8c9','#66c2a4','#41ae76','#238b45','#006d2c','#00441b')
limits_aa = c(0, 0.1, 0.2, 0.5, 1, 2, 3, 4)
dlims_aa = c(-2, -1, -0.5, -0.2, -0.1, 0.1, 0.2, 0.5, 1, 2)
dcols_aa = c('#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e')
units_aa = '' 


sim_files = list.files("outputs/LAI/", pattern  = "u-", full.names = TRUE)
files = list("Observation" = 'outputs/lai_0.5x0.5_2000-2005-LAI.nc',
             "Simulation"  = sim_files[grepl("control", sim_files)])

obsSc = 1
run("control-lai")

files[2] = sim_files[grepl("-obsVegDist", sim_files) & !grepl("trees", sim_files)]
run("obsVegDist-lai")
#files[2] = sim_files[grepl("control", sim_files)
#run("obsVegDist-trees")
