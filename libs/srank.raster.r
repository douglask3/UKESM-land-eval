limits_rank = seq(0, 0.9, 0.1)
cols_rank = c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529')

srank.raster <- function(r1, r2, lab = '', name = '',season = NULL,
                         cols = cols_rank, limits = limits_rank) {
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
        
    plotStandardMap(out, limits = limits, cols = cols)
    mtext(name, side = 2, adj = 0.9, line = -0.2)
    addLetLab(lab)
    StandardLegend(out, limits = limits, cols = cols,
                   extend_max = FALSE, maxLab = 1, add = TRUE, oneSideLabels = FALSE)
    return(out)
}
