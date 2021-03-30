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
