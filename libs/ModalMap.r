modal_cols = c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177','#49006a')#c('#34eeba','#d95f02','#7570b3')
modal_limits = c(1, 1.1, 1.2, 1.5, 2)

ModalMap <- function(obs, txt, addLegend, let = NULL,
                     cols = modal_cols, limits = modal_limits, 
                     regions = NULL) {
    modal = testModal(obs)
    modal_approx = layer.apply(2:6, function(i)
                              modal[[i]] * (0.5-cos(2*pi * modal[[i+6]]/12)/2))
    modal_approx = 1 + sum(modal_approx, na.rm = TRUE)/modal[[1]]

    plotStandardMap(modal_approx -1, limits = limits -1, cols = cols)
    if (!is.null(let)) addLetLab(let)
    mtext(txt, side = 2)
    if (addLegend) 
        StandardLegend(limits = limits - 1, cols = cols, dat = modal_approx-1,
                       extend_max = TRUE,
                       labelss = modal_limits, add = TRUE) 

    if (!is.null(regions)) {
        browser()
    }
    return(modal)
}
