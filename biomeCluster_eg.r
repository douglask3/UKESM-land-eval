library(raster)
graphics.off()
library(rasterPlot)
source("../rasterextrafuns/rasterPlotFunctions/R/mtext.units.r")

dir = "outputs/veg_frac/"

obs  = c(UKESM = "u-", VCF = "ObservationVCF", CCI = "Observationcci", IGBP = "Observationigbp")
vars = c("trees", "herb", "bare") 

realmNames = c("Global", "Australia", "Southern\nAfrica", "Northern\nAfrica", "SE\nAsia",
               "Southern\nAmerica", "Temperate\nAmerica", "Boreal\nAmerica",
               "Temperate\nEurasia", "Boreal\nEurasia")

cols = c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d',
         '#238443','#006837','#004529')

rmask = raster('../UKESM-land-evaluation/outputs/realms.nc')
meanBrick <-function(...) mean(brick(...))
files = list.files(dir, full.names=TRUE)
forDataset <- function(id, idn) {
    
    files = files[grepl(id, files)]
    
    tree = files[grepl(vars[1], files)]
    herb = files[grepl(vars[2], files)]
    bare = files[grepl(vars[3], files)]
    
   

    open <- function(f) {
        if (length(f) > 1) out = mean(layer.apply(f, meanBrick))
        else out = raster(f)
        return(out)
    }
    tree = open(tree)
    herb = open(herb)
    bare = open(bare)
    
    if (grepl("VCF", files[1])) {
        tree = tree
        bare = 1-tree-herb
    }
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
        #print(name)
        #if ("Boreal\nEurasia" == name) browser()
        A = (ynew/xnew)
        A[A>1] = 1/A[A>1]
        A = A+1
        xnew = xnew /A; ynew = ynew/A
        
        lines(xnew+0.5*ynew, ynew, lwd = 2)
        text.units(x = 0.6, y = 0.4, paste0("~R2~: ", round(summary(fit)$r.squared,2)))

        lines(c(0, 1, 0.5, 0), c(0, 0, 1,0))
        
        if (name == realmNames[1]) mtext(side = 3, adj = 0.9, idn, line = -1)
        if (tail(realmNames, 1) == name)
            text(x = 1, y = -0.19, 'Bare (%)', xpd = TRUE, adj = 1, xpd = NA)
        if (id == obs[1]) {
            mtext(side = 2, adj = 0.9, name, line = -2)
            text(x = 0.06, y = 0.38, 'Herb (%)', xpd = TRUE, srt = 60, adj = 1)
        }
        if (id == tail(obs, 1)) 
            text(x = 0.94, y = 0.5, 'Wood (%)', xpd = TRUE, srt = -60, adj = 1)

        x = seq(0, 1, 0.2)

        lapply(x, function(i) {
                lines(c(0.5*i, 1-0.5*i), c(i,i), lty = 2, col = '#00000099');
                text(x=1-0.5*i+0.05, y=i, paste(i*100, ''), xpd = TRUE, srt = -60)})

        lapply(x, function(i) {
                lines(c(i, 0.5+0.5*i), c( 0, 1-i), lty = 3, col = '#00000099');
                text(x=i, y=-0.07, paste(i*100, ''), xpd = TRUE, srt = 0)})

        lapply(x, function(i) {
                lines(c(0.5-0.5*i, 1-i), c( 1-i, 0), lty = 4, col = '#00000099');
                text(x=0.45-0.5*i, y=1-i, paste(i*100, ''), xpd = TRUE, srt = 60)})
        
        return(cbind(ts, hs, bs))
    }
    
    out = mapply(forRegion, c(NaN, 1:9), realmNames)

}

png("figs/VegDistTriangle.png", width = 7.2, height = 7.2 * sqrt(3) * 0.5 * 10/4,
    res = 300, units = 'in')        
    par(mfcol = c(10, 4), mar = rep(1, 4), oma = c(0.5, 0, 0, 0))
    out = mapply( forDataset, obs, names(obs), SIMPLIFY = FALSE)
dev.off()

plotHist <- function(outi, id) {
    plotRegion <- function(thb, name) {
        breaks = seq(0, 1, 0.02)
        hist = apply(thb, 2, hist,  breaks = breaks, plot = FALSE)
        dens = 100*sapply(hist, function(i) i$density)
        dens = dens/sum(dens[,1])
        dens = log10(dens + 0.001)
        
        plot(c(0, 100), c(-3, 0), xlab = '', ylab = '',
             xaxs = 'i', yaxs = 'i', xaxt = 'n', yaxt = 'n', type = 'n')
        labels = c(0, 0.1, 0.3, 1, 3, 10, 30, 100)
        if (id == names(obs)[1]) {
            axis(2, labels = labels, at = log10((labels/100)+0.001))
            mtext(side = 2, name, line = 3)
        }
        polyFun <- function(den, col, dx, lwd) {
            colb = make.transparent(col, 0.95)
            coll = make.transparent(col, 0.6)
            addBar <- function(x, y)  {
                #lines(c(x, x)+dx, c(-3, y), col = col)
                lines(c(x, x)+dx, c(-3, y), col = coll, lwd = lwd)
            }
            mapply(addBar, 100*breaks[-1] + 1, den)
            #polygon(c(0,1+breaks[-1]*100, 100), c(-3, den, -3), border = colb, col = coll, lwd = 2)
        }
        cols = c("#8da0cb",'#66c2a5', '#fc8d62')
        for (i in 1:5) {for (lwd in c(2, 1, 0.5, 0.25, 0.125, 0.06, 0.03)) {
            polyFun(dens[,1], cols[1], -0.2, lwd)
            polyFun(dens[,2], cols[2],0 , lwd)
            polyFun(dens[,3], cols[3], 0.2, lwd)
            polyFun(dens[,3], cols[3], 0.2, lwd)
            polyFun(dens[,2], cols[2],0 , lwd)
            polyFun(dens[,1], cols[1], -0.2, lwd)
        }}
        if (name == realmNames[1]) {
            axis(3)
            mtext(id, side = 3, line = 2)
        }
        if (name == tail(realmNames, 1)) axis(1)
    }
    mapply(plotRegion, outi, realmNames)
}

#png("figs/VegDistHist.png", width = 7.2, height = 7.2 * sqrt(3) * 0.5 * 10/4,
#    res = 300, units = 'in')        
#    par(mfcol = c(10, 4), mar = rep(0.5, 4), oma = c(3, 6, 4, 0))
#    mapply(plotHist, out, names(obs))
#    mtext(side = 2, outer = TRUE, 'density (%)', line = 1.5)
#    mtext(side = 1, outer = TRUE, '% cover', line = 1.5)
#dev.off()
#browser()

    
transect <- function(name, trees, herbs, precips, pconcs, fire = NULL,
                     precipSc = 2200, pconcSc = 1) {
    xl = raster::crop(trees[[1]], extent)
    xl = yFromCell(xl, 1:length(xl))
    
    plot(range(xl), c(0, 100), ty= 'n', xlab = '', ylab = '', xaxt = 'n',
        xaxs = 'i', yaxs = 'i')

    addTransects <- function(tree, herb) {
        tl = raster::crop(tree, extent)
        hl = raster::crop(herb, extent)
        col = c("#7570b3", "#1b9e77", "#d95f02")
        colt = make.transparent(col, 1 - 1/length(trees))
        
        x = c(xl, rev(xl))
        tl2 = 1-tl[]
        hl2 = 1-hl[] - tl[]
    
        yt = c(rep(1, length(xl)), rev(tl2))*100
        yh = c(tl2, rev(hl2))*100
        yb = c(hl2, rep(0, length(xl)))*100
        polygon(x, yt, col = colt[1], lwd = 2, border = NA)
        polygon(x, yh, col = colt[2], lwd = 2, border = NA)
        polygon(x, yb, col = colt[3], lwd = 2, border = NA)
         
        #polygon(c(max(xl), xl, min(xl)), 100*c(0, rep(1, length(xl)), 0), col = col[1], lwd = 2)
        #polygon(c(max(xl), xl, min(xl)), 100*c(0, 1-tl[], 0), col = col[2], lwd = 2)
        #polygon(c(max(xl), xl, min(xl)), 100*c(0, 1-hl[]-tl[], 0), col = col[3], lwd = 2)
        
    }
    
    for (i in 1:4) mapply(addTransects, trees, herbs)
    addLines <- function(precip, pconc) {
        addLine <- function(dat, scal = NULL, ...) {
            fl = raster::crop(dat, extent)
            print(max(fl[]))
            if (is.null(scal)) scal = max(fl[])
            fl = 100* fl[]/scal
            lines(xl, fl, lwd = 1.5)
            lines(xl, fl, lwd = 1.5, ...)
        }
        if (!is.null(fire)) addLine(fire, lty = 2, col = 'red')
        
        addLine(precip, lty = 2, col = 'blue', scal = precipSc)
        addLine(pconc, lty = 3, col = 'white', scal = pconcSc)
    }
    #browser()
    mapply(addLines, precips, pconcs)
   

    
    labels = c(0, 500, 1000, 1500, 2000, 2500)
    axis(side = 4, at = 100*labels/2200, labels = labels, col = 'blue', lwd = 2)
    
    labels = seq(0, 1, 0.2)
    axis(side = 4, at = 100*labels, labels = labels, line = 3, lwd = 2)  
    
    if (!is.null(fire)) axis(side = 4, at = 100*labels/0.8892197, labels = 100*labels, col = 'red', line = 6, lwd = 2)
    grid()
     mtext(name, adj = 0.05, side = 3, line = -6)
}

grepls <- function(v, p) v[grepl(p, v)]

getFiles <- function(dir, pattern, p, openFUN = NULL) {
    files = list.files(dir, pattern = pattern, full.names = TRUE)
    
    out = grepls(files, p)
    if (!is.null(openFUN)) out = lapply(out, openFUN)
    return(out)
}
png("figs/trasect_AFRICA.png", width = 7.2, height = 6, units = 'in',res = 300)
    par(mfrow = c(2,1), mar = c(0.7, 3, 0.7, 0.5), oma = c(2.75, 0, 0, 9))
    
    prec_obs = getFiles('outputs/clim/precip/2001_2013/', 'Observation', 'mean_annual', raster)
    pcon_obs = getFiles('outputs/clim/precip/2001_2013/', 'Observation','concentration', raster)
    prec_obs[[1]] = prec_obs[[1]]*12 
    tree_obs  = getFiles('outputs/veg_frac/', 'Observation', 'tree', raster)
    grass_obs = getFiles('outputs/veg_frac/', 'Observation', 'herb', raster)
   
    fire = mean(brick('../LimFIRE/outputs/fire2000-2014.nc')[[1:12]])*12
    fire  = raster::resample(fire, tree_obs[[1]])

    extent = c(25.1, 25.2, -2, 22)

    transect('Observations', tree_obs, grass_obs, prec_obs, pcon_obs, fire)
    mtext.units(side = 4, "Burnt area (%~yr-1~)", line = 8.2, col = 'red') 
    files = list.files('outputs/veg_frac/', pattern = 'u-', full.names = TRUE)
    tree_mod = lapply(grepls(files, 'tree'), meanBrick)
    
    herb_mod = lapply(grepls(files, 'grass'), meanBrick)
    files = list.files('outputs/clim/precip/2001_2013/', pattern = 'u-', full.names = TRUE)
    prec_mod = lapply(files[grepl('mean_annual', files)], raster)
    pcon_mod = lapply(files[grepl('concentration', files)], raster)

    transect('UKESM', tree_mod, herb_mod, prec_mod, pcon_mod, precipSc = 2200/(60 *60*360*24))
    axis(1)
    mtext.units(side = 2, outer = TRUE, line = -1, '% cover')
    mtext.units(side = 1, outer = TRUE, line = 1.2, 'Latitude (~DEG~N)')

     
    mtext.units(side = 4, outer = TRUE, "Precip. (mm~yr-1~)", line = 1.2, col = 'blue')
    mtext.units(side = 4, outer = TRUE, "Precip. concentration", line = 4.2)  
dev.off()
browser()


png("figs/LPX_vegDist.png", width = 7.2, height = 7.2 * sqrt(3) * 0.5 * 4/3, res = 300, units = 'in')
    layout(rbind(c(1, 0, 11), 2:4, 5:7, 8:10))
    par( mar = rep(1, 4), oma = c(0, 0, 0, 5))
    mapply(forRegion, c(NaN, 1:9), c("Global", "Australia", "Southern\nAfrica", "Northern\nAfrica", "SE\nAsia", "Southern\nAmerica", "Temperate\nAmerica", "Boreal\nAmerica", "Temperate\nEurasia", "Boreal\nEurasia"))

dev.off()

transect('UKESM', FALSE)

dev.off()

extent = c(143.1, 143.2, -39, -25)

par(mfrow = c(2,1), mar = c(0.25, 3, 0.25, 0.5), oma = c(2.75, 0, 0, 0))
tree = tree_obs
herb = herb_obs
transect('Observations')
tree = tree_mod
herb = herb_mod
transect('UKESM')

