source("libs/PolarConcentrationAndPhase.r")
library(plotrix)
plotConPhase <- function(r, let = c('', ''), addLegend = FALSE,
                         phase_cols = phase_cols,
                         conc_cols = conc_cols,
                         phase_lims = phase_lims,   conc_lims =conc_lims,
                         regions = NULL) {

    pc = PolarConcentrationAndPhase.RasterBrick(r, phase_units = "months")
    plotStandardMap(pc[[1]], limits = phase_lims, cols = phase_cols)
    addLetLab(let[1])
    if (addLegend) SeasonLegend(phase_lims, cols = phase_cols, add = FALSE)
    plotStandardMap(pc[[2]], limits = conc_lims, cols = conc_cols)
    addLetLab(let[2])
    if (addLegend) conLegend(pc, conc_lims, conc_cols)

    if (!is.null(regions)) {
        browser()
    }
}

conLegend <- function(pc, limits = conc_lims, cols = conc_cols) {
    if (nlayers(pc) == 2) pc = pc[[2]]
    StandardLegend(limits = limits, cols = cols, extend_max = FALSE,
                        maxLab = 1, dat = pc, add = TRUE, oneSideLabels = FALSE)
}
