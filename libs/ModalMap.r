modal_cols = c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177','#49006a')#c('#34eeba','#d95f02','#7570b3')
modal_limits = c(1, 1.1, 1.2, 1.5, 2)

Modalise <- function(r) {
    modal = testModal(r)
    modal_approx = layer.apply(2:6, function(i)
                              modal[[i]] * (0.5-cos(2*pi * modal[[i+6]]/12)/2))
    modal_approx = 1 + sum(modal_approx, na.rm = TRUE)/modal[[1]]
    return(modal_approx)
}

ModalMap <- function(obs, txt, addLegend, let = NULL, 
                     cols = modal_cols, limits = modal_limits, 
                     regions = NULL) {
    
    if (length(obs) == 1) {
        modal_approx = Modalise(obs[[1]])        
        plotStandardMap(modal_approx-1, limits = limits -1, cols = cols)
        
    } else {
        modal_approx = layer.apply(obs, Modalise)
        modal_a = modal_approx[[1]]
        modal_a[] = apply(modal_approx[], 1, median)
        modal_e = range(modal_approx)

        model_a = list(min(modal_approx)-1, max(modal_approx)-1)
        lapply(model_a, plotStandardMap, limits = limits -1, cols = cols)
    }
    
    addLetLab(let)
    
    mtext(txt, side = 2)
    if (addLegend) 
        ModalLegend(modal_approx, limits = limits, cols = cols)

    if (!is.null(regions)) {
        browser()
    }
    return(modal_approx)
}

ModalLegend <- function(modals, cols = modal_cols, limits = modal_limits) {
    StandardLegend(limits = limits - 1, cols = cols, dat = modals-1,
                       extend_max = TRUE, oneSideLabels = NA,
                       labelss = limits, add = TRUE)
}
