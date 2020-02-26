library(gitBasedProjects)
library(ncdf4)

writeRaster.Standard <- function(r, file, ...) 
    writeRaster.gitInfo(r, file, overwrite = TRUE, ...)
