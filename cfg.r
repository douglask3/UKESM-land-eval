library(raster)
library(rasterExtras)
source("../gitProjectExtras/gitBasedProjects/R/sourceAllLibs.r")
sourceAllLibs("libs/")
sourceAllLibs("../rasterextrafuns/rasterPlotFunctions/R/")

jobs = c("u-az513", "u-az515", "u-az524", "u-bb277", "u-bc179", "u-bc292", "u-bc370", "u-bc470", "u-bd288", "u-bd416", "u-bd483")

jobs = c("u-az513", "u-az515", "u-az524", "u-bb277", "u-bc179", "u-bc370", "u-bc470", "u-bb075")

mod_dir = 'data/model_output/'
mod_files = list.files(mod_dir, full.names = TRUE, recursive = TRUE)

