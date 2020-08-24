
source("cfg.r")
library("plotrix")
library(maps)

graphics.off()

cols = c('#d01c8b','#f1b6da','#f7f7f7','#b8e186','#4dac26')
load('outputs/bench/cover.Rd')
bcolss = rep(list(cols), length(scores))
vars = names(vars)

cols = c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6')
load('outputs/bench/tas_pr.Rd')

scores = c(scores, list( pr_aa_NME), pr_season_comp[1:3],
                   list(tas_aa_NME), pr_season_comp[4:6])
bcolss = c(bcolss, rep(list(cols), 8))
varSeason = c('Average', 'Phase', 'Conc.', 'Modality')
vars = c(vars, paste0('MAP\n',varSeason)  , paste0('MAT\n',varSeason))


png("figs/MMscoreCols.png", height = 15, width = 3 + length(vars)/2, units = 'in', res = 300)
plot(c(-4, length(scores) + 0.5), c(-3, 29.5), axes = FALSE, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', type = 'n', ylim = c(29.5, -3))
text(y = 0, x = 1:length(vars),  vars, srt = 90, adj = 0)
text(y = 1:29, x = 0, adj = 1, xpd = TRUE, c("Global", region_names), srt = 0)
lapply(0.5:29.5, function(y) lines(c(0, length(scores) + 0.5), rep(y, 2)))

addScoreCols <- function(score, x_main, bcols) {

    nsegs1 = length(score)
    rs = seq(0, 2*pi, length.out = nsegs1 + 1)
    addRegion <- function(rn) {
        addCol <- function(on) {        
            
            Addpie <- function(id = NULL, scale = 1, xp =  0, yp = 0) {
                if (!is.null(id)) cs = sapply(cs, function(i) i[id])
                nsegs2 = length(cs)
                x = seq(rs[on], rs[on+1], length.out = nsegs2+1)
                getCord <- function(FUN, ysh) {
                    y =  scale * FUN(x)/2 #sqrt(2) *
                    #y[y>  0.5  ] =  0.5
                    #y[y<(-0.5)] = -0.5
                    y = mapply(c, 0, y[-1], head(y, -1), SIMPLIFY = FALSE)
                    y = lapply(y, '+', ysh)
                    return(y)
                }
                xs = getCord(sin, xp); ys = getCord(cos, yp)           
        
                addPoly <- function(xs, ys, col, ...) 
                    polygon(x_main + xs, rn + ys, col = col, ...)            
            
                mapply(addPoly, xs, ys, bcols[cs+1], border = NA)
            
           
                addPoly(c(xp, sapply(xs, function(i) i[3:2]), xp),
                        c(yp, sapply(ys, function(i) i[3:2]), yp), 'transparent' ,
                        border = 'black')
    
            }
            cs = score[[on]][,rn]
            if (class(cs) == "integer") Addpie()
            else {
                if (length(cs[[1]])== 1) Addpie(1)
                else mapply(Addpie, 1:3, 0.5, c(0.0, -0.25, 0.25), c(0.25, -0.25, -0.25))
            }
        }
        lapply(1:length(score), addCol)        
    }
    
    ni = ncol(score[[1]])
    if (is.null(ni)) ni = length(score[[1]])
    lapply(1:ni, addRegion)
}
mapply(addScoreCols, scores, 1:length(scores), bcolss)

dev.off()

