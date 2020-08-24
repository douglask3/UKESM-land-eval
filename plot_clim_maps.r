source("cfg.r")

sourceAllLibs("../benchmarkmetrics_github/benchmarkMetrics/R/")
graphics.off()

listFiles <- function(path, pattern) 
    list.files(path, pattern = pattern, full.names = TRUE)

dir_pr  = "outputs/clim/precip/2001_2013/"
dir_tas = "outputs/clim/tas/2001_2013/"
clim_files = list("MAP" = list("Annual average" = listFiles(dir_pr , "mean_annual"),
                               "Phase"          = listFiles(dir_pr , "phase"),
                               "Concentration"  = listFiles(dir_pr , "concentration"),
                               "Clim"           = listFiles(dir_pr , "climatology")),
                  "MAT" = list("Annual average" = listFiles(dir_tas, "mean_annual"),
                               "Phase"          = listFiles(dir_tas, "phase"),
                               "Concentration"  = listFiles(dir_tas, "concentration"),
                               "Clim"           = listFiles(dir_tas, "climatology")))

#obs_clim_files = list("MAP" = "../LimFIRE/outputs/Prc2000-2014.nc",
#                   "MAT" = "../LimFIRE/outputs/Tas2000-2014.nc")

cols = list("MAP" = c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4',
                      '#1d91c0','#225ea8','#253494','#081d58'), 
            "MAT" = rev(c('#a50026','#d73027','#f46d43','#fdae61','#fee090',
                      '#abd9e9','#74add1','#4575b4','#313695')))

limits = list("MAP" = c(0, 10, 50, 100, 500, 1000, 1500, 2000, 3000),       
              "MAT" = c(-15, -10, -5, 0, 5, 10, 15, 20, 25, 30))

mod_scale = c("MAP" = 60*60*24*365.25, "MAT" = 1)
obs_scale = c("MAP" = 24, "MAT" = 1)

mod_shift = c("MAP" = 0, "MAT" = -273.15)
obs_shift = c("MAP" = 0, "MAT" = -273.150)


mtextLabs <- function(let, lab, var)
    mtext(paste0(let, ') ', lab, ' ', var), adj = 0.1, line = -1)

plotClimVar <- function(files, cols, limits, scale, addLegend, names, labs, observation = TRUE, 
                        ...) {

    selectFiles <- function(file) {
        test = grepl('Observation', file)
        if (!observation) test = !test
        file[test]
    }
    
    file = lapply(files, selectFiles)
    clims = lapply(file[["Clim"]], brick)
    conc = stack(file[['Concentration']])
    phase = stack(file[["Phase"]])
    
    modals = layer.apply(clims, Modalise)  
    mask = raster('data/seamask.nc')
    mask = raster::resample(mask, modals[[1]]) == 0
    plotStandardMap(modals, limits = modal_limits, cols = modal_cols,
                    limits_error = quantile(sd.raster(modals), c(0.1, 0.5)))
    mtextLabs(labs[1], names, ' modality')
    if (addLegend) ModalLegend(modals[[1]]) 

    conc = layer.apply(conc, function(i) {i[mask] = NaN; i})
    conc = layer.apply(conc, function(i) {i = i - min.raster(i, na.rm = TRUE);
                                          i/max.raster(i, na.rm = TRUE)})
    plotStandardMap(conc, limits = conc_lims, cols = conc_cols,
                    limits_error = quantile(sd.raster(conc), c(0.1, 0.5)))
    mtextLabs(labs[2], names, ' concentration')
    if (addLegend) conLegend(conc)
    
    climAll = clims[[1]]    
    for (f in clims[-1]) climAll = climAll + f
    
    if (nlayers(phase) > 1) {
        phase_mean = PolarConcentrationAndPhase(climAll, phase_units = 'months',
                                                justPhase = TRUE)
        phase_mode0 = round(phase)     
        phase_mode = getmode.raster(phase_mode0)
        phase_e = 1 - mean(phase_mode == phase_mode0)
    } else {
        phase_mean = phase
        phase_e = NULL
    }
    plotStandardMap(phase_mean, limits = phase_lims, cols = phase_cols,
                    e = phase_e, limits_error = c(0.1, 0.2))
    mtextLabs(labs[3], names, ' phase')
    if (addLegend) SeasonLegend(phase_lims, cols = phase_cols, add = FALSE, mar = rep(0, 4))
    
    return(list(phase, conc, modals)) 
}

plotStuff <- function(name, scales, ...) {
    fname = paste0("figs/climStuff-", name, ".png")
        png(fname, width = 7.2, height = 6.8, res = 300, units = 'in')
        layout(cbind(c(1, 2, 3, 3, 8, 8), c(1, 2, 3, 4, 8, 9),
                    c(1, 2, 3, 3, 8, 8), c(5, 6, 7, 7, 0, 0)),
               heights = c(1, 1, 0.6, 0.4, 0.6, 0.4), width = c(0.45, 0.45, 0.1, 1))
        par(mar = rep(0, 4), oma = c(0, 0, 0.5, 0))
        out = mapply(plotClimVar,clim_files, cols, limits, scales,
                       addLegend = c(T, F), names = c('Precip', 'Temp'),
                       labs = list(letters[c(1, 3, 5)], letters[c(2, 4, 6)]), ...)
        phase = out[1,]
        
        dphase = layer.apply(1:nlayers(phase[[1]]), function(i)
                 PhaseDiff(c(phase[[1]][[i]], phase[[2]][[min(nlayers(phase[[2]]), i)]])))
        if (nlayers(dphase) > 1) {
            dphase_mean = mean.phase.raster(dphase)
            phase_mode0 = round(dphase)
    
            phase_mode = getmode.raster(phase_mode0)
            phase_e = 1 - mean(phase_mode == phase_mode0)
        } else {
            dphase_mean = dphase
            phase_e = NULL
        }
        plotStandardMap(dphase_mean, limits = dphase_lims, cols = dphase_cols,
                        e = phase_e, limits_error = c(0.1, 0.2))
        mtextLabs(letters[7], 'Phase', ' difference')
        SeasonLegend(c(0.5:5.5,-5.5:-0.5), cols = dphase_cols, add = FALSE, mar = rep(0, 4))
    dev.off.gitWatermark(comment = 'DRAFT')
    return(out)
}

season_sim = plotStuff("UKESM", mod_shift, observation = FALSE)
season_obs = plotStuff("CRUTS4.01", obs_shift)

plotAAVar <- function(var, let, ...) {   
    clim_files = clim_files[[var]][[1]]
    
    otest = grepl('Observation', clim_files)
    sim = stack(clim_files[!otest]) 
    sim = layer.apply(sim, '*', mod_scale[var])
    sim = layer.apply(sim, '+', mod_shift[var])
    obs = stack(clim_files[otest]) * obs_scale[var] + obs_shift[var]
    
    cols = cols[[var]]
    limits = limits[[var]]
    e = sd.raster(sim + mod_shift[var])
    mask = raster('data/seamask.nc')
    mask = raster::resample(mask, e) == 0
    e[mask] = NaN
    limits_error = quantile(e, c(0.1, 0.5), na.rm = TRUE)
    plotStandardMap(sim, e = e, limits = limits, cols = cols, limits_error = limits_error)
    mtextLabs(let[1], 'UKESM', var)     
    plotStandardMap(obs, limits = limits, cols = cols)
    mtextLabs(let[2], 'CRUTS4.01', var)
    StandardLegend(obs[[1]], limits = limits, cols = cols,
                   add = TRUE, oneSideLabels = FALSE, ...)
    
    return(c(obs, sim))
}
max.rater <- function(...) max.raster(...)
png('figs/annual_average_clims.png', res = 300, units = 'in', width = 7.2, height = 3.7)
    par(mfcol = c(2,2), mar = rep(0, 4))
    pr_aa  = plotAAVar('MAP', letters[c(1,3)], units = 'mm/yr')
    tas_aa = plotAAVar('MAT', letters[c(2,4)], units = '~DEG~C'   , extend_min = TRUE)
dev.off.gitWatermark()

metric_comps.obs <- function(obs, sims,FUN = NME, nullFUN = null.NME) {   
    obs = raster::resample(obs, sims[[1]])    
    obs[is.na(regions)] = NaN
    sims[is.na(regions)] = NaN
    forRegion <- function(r) {
        if (!is.na(r)) {
            obs[regions != r] = NaN
            sims[regions != r] = NaN
        }     
        aa_comp <- function(sim) {
            sc = score(FUN(obs, sim,w = raster::area(obs)))
            sapply(sc, function(i) sum(i< nulls))
        }
        
        nulls0 = summary(nullFUN(obs, n = 10)) 
        nulls = nulls0[1:4]
        nulls[3] = nulls0[3] - nulls0[4]
        nulls[4] = nulls0[3] + nulls0[4]
        scores = layer.apply(sims, aa_comp)
        return(scores)
    }
    out = sapply(c(NaN, 1:max.raster(regions, na.rm = TRUE)), forRegion)
    rownames(out) = jobs
    colnames(out) = c("Global", region_names)
    return(out)
}

metric_comps <- function(sims, obs, ...) 
    layer.apply(obs, metric_comps.obs, sims, ...)


PhaseMet <- function(obs, sim, w) 
    out = MPDonly(obs, sim, w)


ModalMet <- function(...) NME(...)

nullPhase <- function(obs, n) 
    null.FUN.default(obs, MPDonly, n = n, step1only=FALSE, w = raster::area(obs))


nullModal <- function(...) 
    null.NME(...)


pr_season_comp = mapply(metric_comps, season_sim, season_obs,
                        c(PhaseMet, NME, ModalMet),
                        c(nullPhase, null.NME, nullModal), SIMPLIFY = FALSE)

pr_aa_NME  = metric_comps( pr_aa[[2]],  pr_aa[[1]])
tas_aa_NME = metric_comps(tas_aa[[2]], tas_aa[[1]])

save(pr_aa_NME, tas_aa_NME, pr_season_comp, file = "outputs/bench/tas_pr.Rd")

       
