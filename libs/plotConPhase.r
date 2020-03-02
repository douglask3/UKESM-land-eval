source("libs/PolarConcentrationAndPhase.r")
plotConPhase <- function(r, let = c('', ''), addLegend = FALSE,
                         phase_cols = c('#313695', '#a50026', '#ffff00','#313695'),
                         conc_cols = c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4',
                                       '#1d91c0','#225ea8','#253494','#081d58'),
                         phase_lims = 0.5:11.5,   conc_lims = seq(0, 1, 0.1)) {

    pc = PolarConcentrationAndPhase.RasterBrick(r, phase_units = "months")
    plotStandardMap(pc[[1]], limits = phase_lims, cols = phase_cols)
    addLetLab(let[1])
    if (addLegend) SeasonLegend(phase_lims, cols = phase_cols, add = FALSE)
    plotStandardMap(pc[[2]], limits = conc_lims, cols = conc_cols)
    addLetLab(let[2])
    if (addLegend)
        StandardLegend(limits = conc_lims, cols = conc_cols, extend_max = FALSE,
                        maxLab = 1, dat = pc[[2]], add = TRUE, oneSideLabels = FALSE) 
}
