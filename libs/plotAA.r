limits_aa = c(0, 0.1, 0.2, 0.5, 1, 2, 5)                
aa_cols = c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d',
            '#238443','#006837','#004529')


plotAA <- function(r, lab = '', name = '', units = '',
                   cols = aa_cols, limits = limits_aa) {
    aa = mean(r)        
    plotStandardMap(aa, limits = limits_aa, cols = aa_cols)
    mtext(name, side = 2, adj = 0.9, line = -0.2)
    addLetLab(lab)
    StandardLegend(aa, limits = limits_aa, cols = aa_cols, units = units,
                   add = TRUE, oneSideLabels = FALSE)
    return(aa)
}
