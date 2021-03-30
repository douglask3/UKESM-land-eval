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
