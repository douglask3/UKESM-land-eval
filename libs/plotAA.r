limits_aa = c(0, 0.1, 0.2, 0.5, 1, 2, 5)                
aa_cols = c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d',
            '#238443','#006837','#004529')


plotAA <- function(r, lab = '', name = '', units = '',
                   cols = aa_cols, limits = limits_aa, regions = NULL,
                   addLegend = FALSE) {
    
    if (is.list(r)) r = layer.apply(r, mean)
    aa = mean(r)
    if (nlayers(r) == 1) e = NULL
        else  e = eFromRange(r)
    
    plotStandardMap(aa, e = e, limits = limits, cols = cols)
    mtext(name, side = 2, adj = 0.9, line = -0.2)
    addLetLab(lab)
    if (addLegend) StandardLegend(aa, limits = limits, cols = cols, units = units,
                                  add = TRUE, oneSideLabels = FALSE)

    return(r)
}

eFromRange <- function(r) {
    rr = range(r)
    if (any(rr[[1]][] <0, na.rm = TRUE) && any(rr[[2]][] > 0, na.rm = TRUE)) {
        e = abs(rr[[2]] - rr[[1]])/max(abs(rr))/2
        e[rr[[2]]>0 & rr[[1]] <0] = 1                
    } else  e = 1-rr[[1]]/rr[[2]]
    return(e)
}
    
