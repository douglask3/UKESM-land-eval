MM <- function(a, b, byRegions = TRUE) {
    print("running MM")
    if (nlayers(a) == 1) {  
        a = addLayer(a, 1-a)
        b = addLayer(b, 1-b) 
    }
    
    ar = raster::area(a)
    ar[is.na(regions)] = NaN
    ar[is.na(a[[1]])] = NaN
    MMi <- function(region) {
        if (!is.na(region)) ar[regions!=region] = 0
        score = 2*sum.raster(abs(a-b) * ar, na.rm = TRUE)/ sum.raster(ar, na.rm = T)    
        return(score)
    }
    if (byRegions) {
        scores = sapply(c(NaN, 1:28), MMi)
        names(scores) = c("Global", region_names)
    }
    else scores = MMi(NaN)
    return(scores)
}
