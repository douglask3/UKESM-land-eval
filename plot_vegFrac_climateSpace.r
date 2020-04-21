source("cfg.r")


listFiles <- function(path, pattern) 
    list.files(path, pattern = pattern, full.names = TRUE)

dir_vegFrac = "outputs/veg_frac/"
vegType = c(Tree = '-trees-', Wood = '-wood-', Shrub = '-shrub-',
            Herb = '-herb-', Grass = '-grass-')

obs_clim_files = NULL
sim_clim_files = c("MAP" = listFiles("outputs/clim/precip/", "mean_annual-2001_2013"),
                   "SW"  = listFiles("outputs/clim/SW/"    , "mean_annual-2001_2013"))
obs_ids = c('cci', 'igbp', 'VCF')

lims = seq(0, 90, 10)
cols = c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529')
dcols = c('#40004b','#762a83','#9970ab','#c2a5cf','#e7d4e8','#f7f7f7','#d9f0d3','#a6dba0','#5aae61','#1b7837','#00441b')
dlims = c(-40, -30, -20, -10, -1, 1, 10, 20, 30, 40)

xlim = c(0, 3000); ylim = c(50, 300)
xscale = 60*60*24*360; yscale = 1
xshift = 0; yshift = 0
xlab = 'MAP (mm ~yr-1~)'; ylab = 'SW (W~m-2~)'
fname = "figs/veg_SW_MAP.png"

plotFUN <- function() {
    cols0 = make_col_vector(cols, limits = lims)
    cols = make.transparent(cols0, 0.97)

    plotVar <- function(var, name) {
        gridRes = 50
        gridded = array(0, c(gridRes, gridRes, gridRes))
        gx = seq(xlim[1], xlim[2], length.out = gridRes + 1)
        gy = seq(ylim[1], ylim[2], length.out = gridRes + 1)
        gz = seq(0, 100, length.out = gridRes + 1)
        plotNew <- function(axis2 = FALSE) {
            plot(xlim, ylim, pch = 19, cex = 1000, col = "black",
                 xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
            mtext(side = 3, adj = 0.9, name, col = "white", line = -1)
            axis(1, labels = FALSE)
            axis(2, labels = FALSE)
            if (axis2) axis(2)
            if (var == tail(vegType,1 )) axis(1)
        }
        scatterLayer <- function(xs, ys, zs, ns = 1) {
            mask = !is.na(xs + ys + zs)[] & yFromCell(xs, 1:length(xs)) > (-60)
            
            x = xs[mask]; y = ys[mask]; z = zs[mask]
            z =  cut_results(z, lims)
            col = cols[z]
            for (cex in c(1, 0.5, 0.3, 0.2, 0.1, 0.05))
                points(x, y, col = col, pch = 19, cex = cex)
            xc = cut_results(x, gx) - 1
            yc = cut_results(y, gy) - 1
            zc = cut_results(z, gz) - 1
            test = xc <= gridRes & yc <= gridRes & zc <= gridRes
            
            for (nn in 1:ns) for (i in 1:length(xc)) 
                if (test[i]) gridded[xc[i], yc[i], zc[i]] = gridded[xc[i], yc[i], zc[i]] + 1
            return(gridded)
        }        
        addJob <- function(job) {
            zs = mean(brick(files_vegFrac[grepl(job, files_vegFrac)])) * 100
            files = sim_clim_files[grepl(job, sim_clim_files)]
            xs = raster(files[1]) * xscale - xshift
            ys = raster(files[2]) * yscale - yshift
            scatterLayer(xs, ys, zs)
        }
        plotNew(TRUE)
        files_vegFrac = listFiles(dir_vegFrac, var)
        sim_grid = lapply(jobs, addJob)
        sumGrid <- function(gridi) {
            for (g in gridi) gridded = gridded + g
            return(gridded)
        }
        meanGrid <- function(gridi) {
            grid = gridded[,,1:2]
            for (i in 1:gridRes) {
                grid[,,1] = grid[,,1] + gridi[,,i] * mean(gz[i:(i+1)])
                grid[,,2] = grid[,,2] + gridi[,,i]
            }
            out = grid[,,1]/grid[,,2]
            return(out)
        }
        sim_grid  =  sumGrid(sim_grid)
        sim_mgrid = meanGrid(sim_grid)
        if (is.null(obs_clim_files)) {
            plot.new()
            return()
        }
        plotNew()
        xs = obs_clim[[1]] * 12
        ys = obs_clim[[2]]
        addObs <- function(id) {
            test = grepl(id, files_vegFrac)
            if (!any(test)) return(gridded)
            zs =  raster(files_vegFrac[]) * 100
            scatterLayer(xs, ys, zs, ns = 2)
        }
        obs_grid = lapply(obs_ids, addObs)
        obs_grid  =  sumGrid(obs_grid)
        obs_mgrid = meanGrid(obs_grid)
        diff_grid = sim_mgrid - obs_mgrid
        diff_grid = cut_results(diff_grid*10, dlims)
        plotNew()
        image(gx, gy, diff_grid, col = dcols, xaxt = 'n', yaxt = 'n',  add = TRUE)
        
    }
    r_eg = raster(sim_clim_files[1])
    openObsClim <- function(file) {
        r = mean(brick(file))
        r = raster::resample(r, r_eg)
        return(r)
    }
    if (!is.null(obs_clim_files)) obs_clim = lapply(obs_clim_files, openObsClim)
    
    png(fname, height = 10, width = 7.2, res = 300, units = 'in')
        par(mfrow = c(length(vegType) + 1, 3), mar = rep(0.5, 4), oma = c(4, 4, 0, 0))
        mapply(plotVar, vegType, names(vegType))
        mtext.units(side = 1 , line = -10, xlab, outer = TRUE)
        mtext.units(side = 2 , line = 2.5, ylab, outer = TRUE)
        plot.new()
        add_raster_legend2(transpose = FALSE, col = cols0, limits = lims, maxLab = 100,
                          plot_loc = c(0.1, 0.9, 0.6, 0.68))
        plot.new()
        plot.new()
        add_raster_legend2(transpose = FALSE, col = dcols, limits = dlims,
                          extend_max = T, extend_min = T,
                          plot_loc = c(0.1, 0.9, 0.6, 0.68))
    dev.off()
}
plotFUN()

sim_clim_files = c("MAP" = listFiles("outputs/clim/precip/", "mean_annual-2001_2013"),
                   "MAT" = listFiles("outputs/clim/tas/"   , "mean_annual-2001_2013"))
obs_clim_files = c("MAP" = "../LimFIRE/outputs/Prc2000-2014.nc",
                   "MAT" = "../LimFIRE/outputs/Tas2000-2014.nc")
xlim = c(0, 3000); ylim = c(-30, 40)
xlab = 'MAP (mm ~yr-1~)'; ylab = 'MAT (~DEG~C)'
yshift = 273.15

fname = "figs/veg_MAT_MAP.png"

plotFUN()

