convert2Climatology <- function(r) {
    nms = sapply(names(r), function(i) strsplit(i, '.', fixed = TRUE)[[1]][2])[1:12]
    nms = paste0('X', nms)
    out = layer.apply(1:12, function(mn) mean(r[[seq(mn, nlayers(r), by = 12)]]))
    names(out) = nms
    return(out)
}
