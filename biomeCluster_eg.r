library(raster)
graphics.off()
library(rasterPlot)
source("../rasterextrafuns/rasterPlotFunctions/R/mtext.units.r")

dir = "outputs/veg_frac/"

obs  = c("VCF", "cci", "igbp")
vars = c("trees", "herb", "bare") 

realmNames = c("Global", "Australia", "Southern\nAfrica", "Northern\nAfrica", "SE\nAsia",
               "Southern\nAmerica", "Temperate\nAmerica", "Boreal\nAmerica",
               "Temperate\nEurasia", "Boreal\nEurasia")

cols = c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d',
         '#238443','#006837','#004529')

rmask = raster('../UKESM-land-evaluation/outputs/realms.nc')

files = list.files(dir, full.names=TRUE)
forDataset <- function(id) {
    files = files[grepl(id, files)]
    
    tree = files[grepl(vars[1], files)]
    herb = files[grepl(vars[2], files)]
    bare = files[grepl(vars[3], files)]
    
    tree = raster(tree)   
    herb = raster(herb)  
    bare = raster(bare)
    
    if (grepl("VCF", files[1])) bare = 1-tree-herb
    tot = tree + herb + bare
    
    tree = tree/tot
    herb = herb/tot
    bare = bare/tot
     
    mask = !is.na(tot + rmask)
    forRegion <- function(rid, name) {    
        if (!is.na(rid)) mask = mask & (rmask == rid)
        hs = herb[mask]
        ts = tree[mask]
        bs = 1 - ts - hs
        
        x = bs + 0.5 * ts
        y = ts
        #dev.new()
        if (length(x) == 0) {
            plot(0, 0, pch = 20, cex = 2, col = cols, axes = FALSE, type = 'n',
            xlab = '', ylab = '', xlim = c(0, 1), ylim = c(0,1))
        } else {
            cols = cols[unlist(mapply(rep, 1:9, 9 + (1:9)^3))]
            cols = densCols(x,y, colramp = colorRampPalette(cols))
            plot(y~x, pch = 20, cex = 2, col = cols, axes = FALSE,
                xlab = '', ylab = '', xlim = c(0, 1), ylim = c(0,1))
        }
        #browser()
        A = (ts/bs)
        A[is.na(A)] = 1
        A[A>1] = 1/A[A>1]
        A = A+1

        logit <- function(x) {
            x[x<0.000001] = 0.000001
            x[x>0.999999] = 0.999999
            log(x/(1-x))
        }
        xf = logit(bs *A); yf = logit(ts*A)
        xf = xf[!(ts == 0 & bs == 1)]; yf = yf[!(ts == 0 & bs == 1)]
        fit = lm(yf~xf)
        xnew = seq(0, 1, 0.001)
        xnewt = logit(xnew)
        ynewt = predict(fit, newdata = data.frame(xf = xnewt))
        ynew = 1/(1+exp(-ynewt))
        print(name)
        #if ("Boreal\nEurasia" == name) browser()
        A = (ynew/xnew)
        A[A>1] = 1/A[A>1]
        A = A+1
        xnew = xnew /A; ynew = ynew/A
        
        lines(xnew+0.5*ynew, ynew, lwd = 2)
        text.units(x = 0.67, y = 0.55, paste0("R~2~: ", round(summary(fit)$r.squared,2)))

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
    fname = paste0("figs/", id, "_vegDist.png")
    png(fname, width = 7.2, height = 7.2 * sqrt(3) * 0.5 * 4/3, res = 300, units = 'in')
        layout(rbind(c(1, 0, 11), 2:4, 5:7, 8:10))
        par( mar = rep(1, 4))
        mapply(forRegion, c(NaN, 1:9), realmNames)
    dev.off()
}
lapply(obs, forDataset)
browser()
png("figs/trasect_AFRICA.png", width = 5, height = 5, units = 'in',res = 300)

fire = mean(brick('../LimFIRE/outputs/fire2000-2014.nc')[[1:12]])*12
prec = mean(brick('../LimFIRE/outputs/Prc2000-2014.nc')[[1:12]])*12
par(mfrow = c(2,1), mar = c(0.7, 3, 0.7, 0.5), oma = c(2.75, 0, 0, 0))


extent = c(25.1, 25.2, -2, 22)
fire = tree_obs = raster::resample(fire, rmask)
prec = tree_obs = raster::resample(prec, rmask)
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

