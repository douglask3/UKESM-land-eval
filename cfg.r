library(raster)
library(rasterExtras)
source("../gitProjectExtras/gitBasedProjects/R/sourceAllLibs.r")
sourceAllLibs("libs/")
sourceAllLibs("../rasterextrafuns/rasterPlotFunctions/R/")

jobs = c("u-az513", "u-az515", "u-az524", "u-bb277", "u-bc179", "u-bc292", "u-bc370", "u-bc470", "u-bd288", "u-bd416", "u-bd483")

jobs = c("u-az513", "u-az515", "u-az524", "u-bb277", "u-bc179", "u-bc370", "u-bc470", "u-bb075")

mod_dir = 'data/model_output/'
mod_files = list.files(mod_dir, full.names = TRUE, recursive = TRUE)

phase_cols = c('#313695', '#a50026', '#ffff00','#313695')
conc_cols = c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4',
                                       '#1d91c0','#225ea8','#253494','#081d58')


phase_lims = 0.5:11.5 
conc_lims =  seq(0, 0.9, 0.1)

dphase_cols = c("#f7f7f7", "#b35806", "#003300", "#542788", "#f7f7f7")
dphase_lims = (-5.5:5.5)

