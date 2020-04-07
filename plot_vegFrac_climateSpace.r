source("cfg.r")


listFiles <- function(path, pattern) 
    list.files(path, pattern = pattern, full.names = TRUE)

dir_vegFrac = "outputs/veg_frac/"
vegType = c(Tree = '-trees-', Wood = '-wood-', Shrub = '-shrub-',
            Herb = '-herb-', Grass = '-grass-')

clim_files = c("MAP" = listFiles("outputs/clim/precip/", "mean_annual-2001_2013"),
               "SW"  = listFiles("outputs/clim/SW/"    , "mean_annual-2001_2013"))

lims = seq(0, 90, 10)
cols = c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529')

xlim = c(0, 3000); ylim = c(50, 300)
xscale = 60*60*24*360; yscale = 1
xshift = 0; yshift = 0
xlab = 'MAP (mm ~yr-1~)'; ylab = 'SW (W~m-2~)'
fname = "figs/veg_SW_MAP.png"

plotFUN <- function() {
    cols = make.transparent(make_col_vector(cols, limits = lims), 0.97)
    #zs = brick(files_vegFrac[1])

    plotVar <- function(var, name) {
        files_vegFrac = listFiles(dir_vegFrac, var)
        plot(xlim, ylim, pch = 19, cex = 1000, col = "black",
            xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
        mtext(side = 3, adj = 0.9, name, col = "white", line = -1)
        axis(2)
        if (var == tail(vegType,1 )) axis(1)

        addJob <- function(job) {
            zs = mean(brick(files_vegFrac[grepl(job, files_vegFrac)])) * 100
            files = clim_files[grepl(job, clim_files)]
            xs = raster(files[1]) * xscale - xshift
            ys = raster(files[2]) * yscale - yshift
            
            mask = !is.na(xs + ys + zs)
            x = xs[mask]; y = ys[mask]; z = zs[mask]
            z =  cut_results(z, lims)
            col = cols[z]
            for (cex in c(1, 0.5, 0.3, 0.2, 0.1, 0.05))
                points(x, y, col = col, pch = 19, cex = cex)
    
        }
        lapply(jobs, addJob)
        plot.new()
    }

    png(fname, height = 10, width = 5, res = 300, units = 'in')
        par(mfrow = c(length(vegType), 2), mar = rep(0.5, 4), oma = c(4, 4, 0, 0))
        mapply(plotVar, vegType, names(vegType))
        mtext.units(side = 1 , line = 2.5, xlab, outer = TRUE)
        mtext.units(side = 2 , line = 2.5, ylab, outer = TRUE)
    dev.off()
}
#plotFUN()

clim_files = c("MAP" = listFiles("outputs/clim/precip/", "mean_annual-2001_2013"),
               "MAT" = listFiles("outputs/clim/tas/"   , "mean_annual-2001_2013"))

xlim = c(0, 3000); ylim = c(-60, 40)
xlab = 'MAP (mm ~yr-1~)'; ylab = 'MAT (~DEG~C)'
yshift = 273.15

fname = "figs/veg_MAT_MAP.png"

plotFUN()

