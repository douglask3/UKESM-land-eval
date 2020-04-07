getYrsFromLayers <- function(dat, years = NULL, FUN = mean) {
    nms = names(dat)
    dates =  sapply(nms, function(i) strsplit(i, '.', fixed = TRUE)[[1]])
    yrs = sapply(dates[1,], function(i) strsplit(i, 'X')[[1]][2])
    if (is.null(years)) return(yrs)
    index = apply(sapply(years, '==', yrs), 2, which)
    dat = convert_pacific_centric_2_regular(dat)
    if (is.null(FUN)) {
        index = unlist(index)
        dat = dat[[index]]
        names(dat) = nms[index]
    } else {
        dat = layer.apply(index, function(i) FUN(dat[[i]]))
        names(dat) = paste0('X', years)
    }
    
    return(dat)
}
