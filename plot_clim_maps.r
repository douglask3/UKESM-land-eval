source("cfg.r")
graphics.off()

listFiles <- function(path, pattern) 
    list.files(path, pattern = pattern, full.names = TRUE)

sim_clim_files = list("MAP" = list("Annual average" = listFiles("outputs/clim/precip/", 
                                                                "mean_annual-2001_2013"),
                                   "Phase"          = listFiles("outputs/clim/precip/",
                                                                "phase-2001_2013"),
                                   "Concentration"  = listFiles("outputs/clim/precip/",
                                                                "concentration"),
                                   "Clim"           = listFiles("outputs/clim/precip/",
                                                                "climatology-2001_2013")),
                      "MAT" = list("Annual average" = listFiles("outputs/clim/tas/", 
                                                                "mean_annual-2001_2013"),
                                   "Phase"          = listFiles("outputs/clim/tas/",
                                                                "phase-2001_2013"),
                                   "Concentration"  = listFiles("outputs/clim/tas/",
                                                                "concentration"),
                                   "Clim"           = listFiles("outputs/clim/tas/",
                                                                "climatology-2001_2013")))

obs_clim_files = list("MAP" = "../LimFIRE/outputs/Prc2000-2014.nc",
                   "MAT" = "../LimFIRE/outputs/Tas2000-2014.nc")

cols = list("MAP" = c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4',
                      '#1d91c0','#225ea8','#253494','#081d58'), 
            "MAT" = c('#a50026','#d73027','#f46d43','#fdae61','#fee090',
                      '#abd9e9','#74add1','#4575b4','#313695'))

limits = list("MAP" = c(0, 10, 50, 100, 500, 1000, 2000, 3000, 4000),       
              "MAT" = c(-20, -15, -10, -5, 0, 5, 10, 15, 20))

mod_scale = c("MAP" = 60*60*24*365.25, "MAT" = 1)
obs_scale = c("MAP" = 12, "MAT" = 1)

#browser()

mtextLabs <- function(let, lab, var)
    mtext(paste0(let, ') ', lab, ' ', var), adj = 0.1, line = -1)

plotClimVar <- function(file, cols, limits, scale, addLegend, names, labs, ...) {
    #aa = stack(file[[1]]) * scale
    #plotAA(aa, cols = cols, limits = limits, ...)
    clims = lapply(file[["Clim"]], brick)
    modals = layer.apply(clims, Modalise)  
    mask = raster('data/seamask.nc')
    mask = raster::resample(mask, modals[[1]]) == 0
    plotStandardMap(modals, limits = modal_limits, cols = modal_cols,
                    limits_error = quantile(sd.raster(modals), c(0.1, 0.5)))
    mtextLabs(labs[1], names, ' modality')
    if (addLegend) ModalLegend(modals[[1]]) 

    conc = stack(file[['Concentration']])
    conc = layer.apply(conc, function(i) {i[mask] = NaN; i})
    conc = layer.apply(conc, function(i) {i = i - min.raster(i, na.rm = TRUE); i/max.raster(i, na.rm = TRUE)})
    plotStandardMap(conc, limits = conc_lims, cols = conc_cols, limits_error = quantile(sd.raster(conc), c(0.1, 0.5)))
    mtextLabs(labs[2], names, ' concentration')
    if (addLegend) conLegend(conc)
    
    climAll = clims[[1]]    
    for (f in clims[-1]) climAll = climAll + f
    
    phase_mean = PolarConcentrationAndPhase(climAll, phase_units = 'months', justPhase = TRUE)
    phase = stack(file[[2]])
    phase_mode0 = round(phase)
     
    phase_mode = getmode.raster(phase_mode0)
    phase_e = 1 - mean(phase_mode == phase_mode0)
    plotStandardMap(phase_mean, limits = phase_lims, cols = phase_cols, e = phase_e, limits_error = c(0.1, 0.2))
    mtextLabs(labs[3], names, ' phase')
    if (addLegend) SeasonLegend(phase_lims, cols = phase_cols, add = FALSE, mar = rep(0, 4))
    return(phase) 
}

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode.raster <- function(r) {
    out = r[[1]]
    mask = !is.na(out)
    out[mask] = apply(r[mask], 1, getmode)
    return(out)
}

mean.phase <- function(x) {
    an = x * 2 * pi / 12
    xc = sin(an)
    yc = cos(an)
    out = atans(sum(xc), sum(yc), 'months')
    if (out >6) out = out - 12
    return(out)
}
mean.phase.raster <- function(r) {
    out = r[[1]]
    test = (max(r) - min(r)) <6
    out[ test] = apply(r[ test], 1, mean)
    out[!test] = apply(r[!test], 1, mean.phase)
    return(out)
}

calPhaseDiff <- function(rs) {
    r = rs[[2]] - rs[[1]]
    r[r<(-6)] = r[r<(-6)] + 12
    r[r>6] = r[r>6] -12
    r = r + 6
    r[r>6] = r[r>6] -12
    r
}
plotStuff <- function(name, files, scales, ...) {
    fname = paste0("figs/climStuff-", name, ".png")
        png(fname, width = 7.2, height = 6.8, res = 300, units = 'in')
        layout(cbind(c(1, 2, 3, 3, 8, 8), c(1, 2, 3, 4, 8, 9),
                    c(1, 2, 3, 3, 8, 8), c(5, 6, 7, 7, 0, 0)),
               heights = c(1, 1, 0.6, 0.4, 0.6, 0.4), width = c(0.45, 0.45, 0.1, 1))
        par(mar = rep(0, 4))
        phase = mapply(plotClimVar,files, cols, limits, scales,
                       addLegend = c(T, F), names = c('Precip', 'Temp'),
                       labs = list(letters[c(1, 3, 5)], letters[c(2, 4, 6)]), ...)
        dphase = layer.apply(1:nlayers(phase[[1]]), function(i)
                            calPhaseDiff(c(phase[[1]][[i]], phase[[2]][[i]])))
        dphase_mean = mean.phase.raster(dphase)
        phase_mode0 = round(dphase)
    
        phase_mode = getmode.raster(phase_mode0)
        phase_e = 1 - mean(phase_mode == phase_mode0)

        plotStandardMap(dphase_mean, limits = dphase_lims, cols = dphase_cols,
                        e = phase_e, limits_error = c(0.1, 0.2))
        mtextLabs(letters[7], 'Phase', ' difference')
        SeasonLegend(c(0.5:5.5,-5.5:-0.5), cols = dphase_cols, add = FALSE, mar = rep(0, 4))
    dev.off()
}

plotStuff("UKESM", sim_clim_files, mod_scale)


#plotStuff("CRUTS4.01", obs_clim_files, obs_scale)
