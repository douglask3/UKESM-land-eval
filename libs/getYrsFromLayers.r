convert365_2_360 <- function(dat) {
    nms = names(dat)
    dates =  sapply(nms, function(i) strsplit(i, '.', fixed = TRUE)[[1]])
    yrs = sapply(dates[1,], function(i) strsplit(i, 'X')[[1]][2])

    yrLength = c(0, 24 * rep(c(365, 365, 366, 365), 100))
    mnthLength = 24 *c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30)
    hrs = sapply(as.numeric(yrs) - 1969, function(yr) sum(yrLength[1:yr])) +
          sapply(as.numeric(dates[2,]), function(mn) sum(mnthLength[1:mn])) +
          as.numeric(dates[3,]) * 24 + as.numeric(dates[4,]) +
          as.numeric(dates[5,]) / 60 + as.numeric(dates[6,]) / (60*60)

    nyr = floor(hrs/(24*360))
    nmnth = ceiling(hrs/(24*30) - nyr*12)
    nday = round(hrs/24 - nyr*360 - (nmnth-1) * 30)
    nmnth[nmnth<10] = paste0(0, nmnth[nmnth<10])
    names(dat) = paste0('X', nyr + 1970, '.', nmnth, '.', nday, '.', '00', '.', '00', '.', '00')
    return(dat)
}

getYrsFromLayers <- function(dat, years = NULL, FUN = mean, convert360 = FALSE) {
    
    makeName <- function(yr, mn) paste0("X", yr,".", mn, ".15.23.00.00")
    nm = names(dat)[1]   
    
        
    #if (nm == "X1979.11.24.23.00.00") {
    #    nm = strsplit(nm, '.', fixed = TRUE)[[1]][1:2]
    #    mnths = seq(as.numeric(nm[2]) + 2, length.out = nlayers(dat)) 
    #    yrs = ceiling(mnths/12)-1
    #    mnths = mnths - yrs*12
    #    yrs = as.numeric(substr(nm[1], 2, 5))+yrs
    #    names(dat) = makeName(yrs, mnths)
    #}
    
    if (convert360) dat = convert365_2_360(dat)        
    
    nms = names(dat)
    dates =  sapply(nms, function(i) strsplit(i, '.', fixed = TRUE)[[1]])
    yrs = sapply(dates[1,], function(i) strsplit(i, 'X')[[1]][2])

    if (is.null(years)) return(yrs)
    index = apply(sapply(years, '==', yrs), 2, which)
    if (class(index) == "matrix") index = matrix2list.col(index)
    if (!all(sapply(index, function(i) length(i) >= 12))) {    
        
        return(NULL)
    }
    if (is.null(FUN)) {
        index = unlist(index)
        
        dat = dat[[index]]
        dat = convert_pacific_centric_2_regular(dat)
        names(dat) = nms[index]
    } else {
        dat = layer.apply(index, function(i) FUN(dat[[i]]))        
        dat = convert_pacific_centric_2_regular(dat)
        names(dat) = paste0('X', years)
    }
    
    return(dat)
}

matrix2list.col <- function(x)
    lapply(seq_len(ncol(x)), function(i) x[,i])
matrix2list.row <- function(x)
    matrix2list.col(t(x))
