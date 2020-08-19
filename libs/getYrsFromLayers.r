getYrsFromLayers <- function(dat, years = NULL, FUN = mean) {
    nms = names(dat)
    dates =  sapply(nms, function(i) strsplit(i, '.', fixed = TRUE)[[1]])
    yrs = sapply(dates[1,], function(i) strsplit(i, 'X')[[1]][2])
    if (is.null(years)) return(yrs)
    index = apply(sapply(years, '==', yrs), 2, which)
    if (class(index) == "matrix") index = matrix2list.col(index)
    if (!all(sapply(index, function(i) length(i) >= 12))) {
        
        return(NULL)
    }
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

matrix2list.col <- function(x)
    lapply(seq_len(ncol(x)), function(i) x[,i])
matrix2list.row <- function(x)
    matrix2list.col(t(x))
