library(raster)
graphics.off()
library(rasterPlot)

herb = raster('outputs/veg_frac/VCF-herb-fracCover.nc')
tree = raster('outputs/veg_frac/VCF-trees-fracCover.nc')
fire = mean(brick('../LimFIRE/outputs/fire2000-2014.nc')[[1:12]])*12
prec = mean(brick('../LimFIRE/outputs/Prc2000-2014.nc')[[1:12]])*12
cols = c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d',
         '#238443','#006837','#004529')

rmask = raster('../UKESM-land-evaluation/outputs/realms.nc')

extent = c(25.1, 25.2, -2, 22)

herb = herb_obs = raster::resample(herb, rmask)
tree = tree_obs = raster::resample(tree, rmask)
fire = tree_obs = raster::resample(fire, rmask)
prec = tree_obs = raster::resample(prec, rmask)

png("figs/trasect_AFRICA.png", width = 5, height = 5, units = 'in',res = 300)
par(mfrow = c(2,1), mar = c(0.7, 3, 0.7, 0.5), oma = c(2.75, 0, 0, 0))
transect <- function(name, addFire = TRUE) {
    tl = raster::crop(tree, extent)
    hl = raster::crop(herb, extent)
    

    xl = yFromCell(tl, 1:length(tl))
    plot(range(xl), c(0, 100), ty= 'n', xlab = '', ylab = '', xaxt = 'n',
         xaxs = 'i', yaxs = 'i')
#lines(xh, 100*th[])
    polygon(c(max(xl), xl, min(xl)), 100*c(0, rep(1, length(xl)), 0), col = "#7570b3" , lwd = 2)
    polygon(c(max(xl), xl, min(xl)), 100*c(0, 1-tl[], 0), col = "#1b9e77", lwd = 2)
    polygon(c(max(xl), xl, min(xl)), 100*c(0, 1-hl[]-tl[], 0), col = "#d95f02", lwd = 2)
    mtext(name, adj = 0.1, side = 3, line = -1.5)

    if (addFire) {1

        fl = raster::crop(fire, extent)
        fl = 100* fl[]/max(fl[])
        lines(xl, fl, lwd = 3)
        lines(xl, fl, lwd = 3, lty = 2, col = 'red')
    }
    pr = raster::crop(prec, extent)
    pr = 100* pr[]/max(pr[])
    lines(xl, pr, lwd = 3)
    lines(xl, pr, lwd = 3, lty = 2, col = 'blue')
}
transect('Observations')

mask = !is.na(herb + tree)
forRegion <- function(rid, name) {
    
    if (!is.na(rid)) mask = mask & (rmask == rid)
    hs = herb[mask]
    ts = tree[mask]
    bs = 1 - ts - hs

    x = bs + 0.5 * ts
    y = ts
    if (length(x) == 0) {
        plot(0, 0, pch = 20, cex = 2, col = cols, axes = FALSE, type = 'n',
        xlab = '', ylab = '', xlim = c(0, 1), ylim = c(0,1))
    } else {
        cols = cols[unlist(mapply(rep, 1:9, 9 + (1:9)^3))]
        cols = densCols(x,y, colramp = colorRampPalette(cols))
        plot(y~x, pch = 20, cex = 2, col = cols, axes = FALSE,
            xlab = '', ylab = '', xlim = c(0, 1), ylim = c(0,1))
    }
    lines(c(0, 1, 0.5, 0), c(0, 0, 1,0))
    text(x = 0.5, y = -0.07, 'Bare Ground', xpd = TRUE)
    text(x = 0.15, y = 0.54, 'Herb Cover', xpd = TRUE, srt = 60)
    text(x = 0.85, y = 0.54, 'Wood Cover', xpd = TRUE, srt = -60)

    x = seq(0, 1, 0.2)

    lapply(x, function(i) {
            lines(c(0.5*i, 1-0.5*i), c(i,i), lty = 2, col = '#00000099');
            text(x=1-0.5*i+0.05, y=i, paste(i*100, '%'), xpd = TRUE, srt = -60)})

    lapply(x, function(i) {
            lines(c(i, 0.5+0.5*i), c( 0, 1-i), lty = 3, col = '#00000099');
            text(x=i, y=-0.03, paste(i*100, '%'), xpd = TRUE, srt = 0)})

    lapply(x, function(i) {
            lines(c(0.5-0.5*i, 1-i), c( 1-i, 0), lty = 4, col = '#00000099');
            text(x=0.45-0.5*i, y=1-i, paste(i*100, '%'), xpd = TRUE, srt = 60)})
    mtext(side = 3, adj = 0.9, name, line = -2)
}

png("figs/obs_vegDist.png", width = 7.2, height = 7.2 * sqrt(3) * 0.5 * 4/3, res = 300, units = 'in')
layout(rbind(c(1, 0, 11), 2:4, 5:7, 8:10))
par( mar = rep(1, 4))
mapply(forRegion, c(NaN, 1:9, NaN), c("Global", "Australia", "Southern\nAfrica", "Northern\nAfrica", "SE\nAsia", "Southern\nAmerica", "Temperate\nAmerica", "Boreal\nAmerica", "Temperate\nEurasia", "Boreal\nEurasia", ''))

text(0.85, 0.11, 'Desert', cex = 1.5, font = 2, srt = -30)
text(0.23, 0.05, 'Grassland', cex = 1.5, font = 2, srt = 0)
text(0.5, 0.2, 'Scrub', cex = 1.5, font = 2, srt = 0)
text(0.23, 0.2, 'Savanna', cex = 1.5, font = 2, srt = 30)
text(0.35, 0.6, 'Forest', cex = 1.5, font = 2, srt = 60)
dev.off()


tree = tree_mod = raster('../UKESM-land-evaluation/outputs/veg_frac/u-bc179-wood-fracCover.nc')
herb = herb_mod = raster('../UKESM-land-evaluation/outputs/veg_frac/u-bc179-grass-fracCover.nc')
prec = prec_mod = raster("../UKESM-land-evaluation/outputs/clim/precip/2001_2013/u-bc179-annual.nc")
png("figs/LPX_vegDist.png", width = 7.2, height = 7.2 * sqrt(3) * 0.5 * 4/3, res = 300, units = 'in')
    layout(rbind(c(1, 0, 11), 2:4, 5:7, 8:10))
    par( mar = rep(1, 4))
    mapply(forRegion, c(NaN, 1:9), c("Global", "Australia", "Southern\nAfrica", "Northern\nAfrica", "SE\nAsia", "Southern\nAmerica", "Temperate\nAmerica", "Boreal\nAmerica", "Temperate\nEurasia", "Boreal\nEurasia"))

dev.off()

transect('UKESM', FALSE)
axis(1)
mtext.units(side = 2, outer = TRUE, line = 0, '% cover')
mtext.units(side = 1, outer = TRUE, line = 1.2, 'Latitude (~DEG~N)')
dev.off()

extent = c(143.1, 143.2, -39, -25)

par(mfrow = c(2,1), mar = c(0.25, 3, 0.25, 0.5), oma = c(2.75, 0, 0, 0))
tree = tree_obs
herb = herb_obs
transect('Observations')
tree = tree_mod
herb = herb_mod
transect('UKESM')

