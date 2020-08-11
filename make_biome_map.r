library(raster)
library(rasterExtras)
library(rgdal)
source("libs/plotStandardMap.r")
source("../gitProjectExtras/gitBasedProjects/R/sourceAllLibs.r")
sourceAllLibs("../rasterextrafuns/rasterPlotFunctions/R/")
graphics.off()
#predictors = c(vegFrac = "data/vegfrac_igbp.nc",
#               LAI_aa  = "outputs/MODIS_AA.nc",
#               LAI_pc  = "outputs/MODIS_pc.nc",
#               LAI_md  = "outputs/MODIS_modal.nc")
#               
#dat = lapply(predictors, brick)
#
#dati = dat[[1]]
#for (di in dat[-1]) dati = addLayer(dati, di)

dat =  readOGR(dsn = "data/biome", layer = "wwf_terr_ecos_oRn")

#ext  = extent (-180, 180, -90, 90)
#xy   = abs(apply(as.matrix(bbox(ext)), 1, diff))
#n    = 2
#r    = raster(ext, ncol=xy[1]*n, nrow=xy[2]*n)
r = raster("outputs/u-bb075-LAI.nc")

## Rasterize the shapefile
biome = rasterize(dat, r, 'BIOME')
realm = rasterize(dat, r, "REALM")
realm0 = realm
realms = c("Australiasia" = 1, "Southern Africa" = 2, "Northen Africa" = 3,
           "SE Asia" = 4, "South America" = 5, "Temperate North America" = 6,
           "Boreal North America" = 7,
           "Temperate Eurasia" = 8, "Boreal Eurasia" = 9)


realm_symbol = c(15, 15, 4, 4, 15, 4, 15, 15, 4)
notRealms = c(2,7)

for (Rlm in notRealms) realm[realm == Rlm] = NaN
ll = xyFromCell(realm, 1:length(realm))
south = ll[,2] < 0
realm[realm[] == 3 &  south] = 2
realm[realm[] == 6 & !south] = 5
realm[realm[] == 4 &  south] = 1

EA = realm[] == 8
realm[EA & ll[,1] < 39 & ll[,2] < 23] = 3
realm[EA & ll[,1] < 36 & ll[,2] < 26] = 3
realm[EA & ll[,1] < 35 & ll[,2] < 29] = 3
realm[EA & ll[,1] < 33 & ll[,2] < 32] = 3
realm[EA & ll[,1] < 34 & ll[,2] < 30] = 3
realm[EA & ll[,1] < 15 & ll[,2] < 36] = 3
realm[EA & ll[,1] < 15 & ll[,1] > 3 & ll[,2] < 37] = 3

BR =  ll[,2] > 60
realm[BR & realm[] == 5] = 7
realm[BR & realm[] == 8] = 9

realmi = realm
realm[realmi == 5] = 6
realm[realmi == 6] = 5


biomeCodes = list("tropical\nwet forest" = c(1, 3),
			  "tropical\ndry forest" = 2,
			  "tropical\nsavanna/\ngrassland" = c(7, 9),
			  "mediterranean\nforest/woodland\nand scrub" = 12,
			  "temperate\nforest and\nwoodland" = c(4, 8, 5),
			  "boreal\nforests" = 6,
			  "shrublands" = c(10, 11, 13))
              
biomeCodes = list("Evergreen forest/wood" = c(1, 3),
                  "Seasonal/temperate forest/wood" = c(2, 4, 5),
                  "Boreal forest"       = c(6),
			      "Savanna/grassland" = c(7, 8, 9),
			      "Mediterranean" = 12,
			      "Desert/shrubland/tundra" = c(10, 11, 13))
              
biomeCols = c("#00441b", "#5aae61", '#40004b', '#abd9e9', '#9970ab', "#fee090")
              
biomes = layer.apply(biomeCodes, function(i) any(layer.apply(i, '==', biome)) )             

biomes = sum(biomes * 1:7)
biomes[biomes == 0] = NaN
biomesR = biomes + 10 * realm

## make names

numb = rep(realms*10, each = length(biomeCodes))+ 1:length(biomeCodes)
name = paste(rep(names(realms), each = length(biomeCodes)), names(biomeCodes))
pch = rep(realm_symbol, each = length(biomeCodes))

tab =  cbind(name, numb,
             sapply(numb, function(i) sum(i == biomesR[], na.rm = TRUE)),
             biomeCols, pch)
tab = cbind(tab, rownames(tab))
tab = cbind(tab, names(biomeCodes))


biomesR[biomesR == 22] = 24
biomesR[biomesR == 25] = 24
biomesR[biomesR == 35] = 85
biomesR[biomesR == 44] = 42
biomesR[biomesR == 46] = 86
biomesR[biomesR[] == 52 & ll[,2] >(-35)] = 51
biomesR[biomesR[] == 52 & ll[,2] <(-35)] = 54
biomesR[biomesR == 55] = 56
biomesR[biomesR == 63] = 73
biomesR[biomesR == 65] = 66
biomesR[biomesR == 81] = 41
biomesR[biomesR == 83] = 93
biomesR[biomesR == 92] = 93


#biomesR[biomesR == 24] = 23
#biomesR[biomesR == 34] = 84
#biomesR[biomesR == 43] = 42
#biomesR[biomesR == 45] = 65
#biomesR[biomesR == 54] = 53
#biomesR[biomesR == 63] = 65
#biomesR[biomesR == 64] = 65
#biomesR[biomesR == 81] = 41
#biomesR[biomesR == 83] = 82

#biomesR = biomesR0
biomesRi = biomesR
biomesR[] = NaN
n = 0
tabi = matrix(0, ncol = 7, nrow = 0)
for (b in unique(biomesRi)) {
    n = n+1
    biomesR[biomesRi == b] = n
    index = which(tab[,2] == b)
    tabi = rbind(tabi,
                 c(tab[index, 1], n,
                   sum(n == biomesR[], na.rm = TRUE),
                 tab[index, 4:7]))
}

pBiomesR1 = round(raster::resample(biomesR, raster(ncol = 360, nrow = 180), method = 'ngb'))
pBiomesR2 = round(raster::resample(biomesR, raster(ncol = 80, nrow = 40), method = 'ngb'))
xy1 = xyFromCell(pBiomesR1 , 1:length(pBiomesR1))
xy2 = xyFromCell(pBiomesR2, 1:length(pBiomesR2))

out = biomesR
out[!is.na(out)] = 0
addPoints <- function(info, skip4 = FALSE) {
    if (info[5] == 15) {
        pch = 15
        col = info[4]
        xyi = xy1
        r = pBiomesR1
        cex = 0.25
     } else {
        if (skip4) return()
        pch = 4
        col = "white"
        xyi = xy2
        r = pBiomesR2
        cex = 1
     }
    
    xyi = xyi[r[] == info[2],]
    points(xyi[,1], xyi[,2], pch = pch, col = col, cex = cex)
    
    out[biomesR == as.numeric(info[2])] = 1
    if (info[5] == 15) return()
    return(out)
}
png("figs/biomeMap.png", width = 16, height = 14, units = 'cm', res = 300) 
    par(mar = rep(0,4))
    plotStandardMap(biomesR, limits = 1.5: max.raster(biomesR, na.rm = TRUE), cols = tabi[,4], readyCut = TRUE)
    rbound = apply( tabi[nrow(tabi):1,], 1,  addPoints)
    apply( tabi[nrow(tabi):1,], 1,  addPoints, skip4 = TRUE)
    rbound = sum(layer.apply(rbound, function(i) i))

    contour(rbound, col = "white", add = TRUE, levels = 0.5, drawlabels = FALSE, lwd = 2)
    contour(rbound, col = "black", add = TRUE, lty = 2, levels = 0.5, drawlabels = FALSE, lwd = 2)

    addCoastlineAndIce2map()
    leg1 <- function(y, i)
        legend(x = -50, y = y, names(biomeCodes)[i], col = biomeCols[i], pch = 15, bty = 'n', y.intersp=1.25, pt.cex = 1.6)
    mapply(leg1, c(-32, -42, -52), 1:3)
    legend(x =  35 , y = -52, names(biomeCodes)[4], col = biomeCols[4], pch = 15, bty = 'n', horiz = T, pt.cex = 1.6)
    legend(x = -50, y = -62, names(biomeCodes)[5], col = biomeCols[5], pch = 15, bty = 'n', horiz = T, pt.cex = 1.6)
    legend(x =  35, y = -62, names(biomeCodes)[6], col = biomeCols[6], pch = 15, bty = 'n', horiz = T, pt.cex = 1.6)
    #legend(x = -50, y = -62, names(biomeCodes)[6], col = biomeCols[6], pch = 15, bty = 'n', horiz = T)
dev.off()
#plotStandardMap(biomesR, limits = 2:5, cols = make.transparent(c("white", "white"), 1), add = TRUE)
#plot_raster_from_raster(biomesR, limits = 2:5, cols = make.transparent(c("white", "white"), 1), add = TRUE)
#legend('bottomright', names(biomeCodes), col = biomeCols, pch = 15, ncol = 4, bty = 'n')

makeSub <- function(cn) {
    index = sapply(unique(tabi[,cn]), function(i) which(tabi[,cn] == i))
    out = sum(layer.apply(index, function(i) sum(layer.apply(i, function(j) biomesR == j))) * 1:length(index))
    return(out)
}


RealmOnly = makeSub(6)
biomeOnly = makeSub(7)

biomeOnly[biomeOnly ==5 & RealmOnly == 7] = 7
biomeOnly[biomeOnly ==5 & RealmOnly == 9] = 7


writeRaster(biomesR  , file = "outputs/full_biome_realms.nc", overwrite=TRUE)
writeRaster(biomeOnly, file = "outputs/biomes.nc", overwrite=TRUE)
writeRaster(RealmOnly, file = "outputs/realms.nc", overwrite=TRUE)


biomesR = convert_regular_2_pacific_centric(biomesR)
writeRaster(biomesR  , file = "outputs/full_biome_realms_pacific_centric.nc", overwrite=TRUE)

write.csv(tabi, "outputs/regions_table.csv")
