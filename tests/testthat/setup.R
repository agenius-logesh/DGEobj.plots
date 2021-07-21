require(testthat)
require(stats)
require(limma)
require(DGEobj)
require(DGEobj.plots)
require(DGEobj.utils)

t_obj1 <- readRDS(system.file("exampleObj.RDS", package = "DGEobj", mustWork = TRUE))
source("~/DGEobj.plots/R/internal.R")
source("~/DGEobj.plots/R/plot_utils.R")

data = readRDS("/efs/bms_shared/data/cdb/APJ_RHR_RatHeart_29Nov2016.RDS")
data$BMTL


system("git fetch DGEobj.plots_MA-upstream")

system("git merge origin/MA-removing-parameters-logRatio")
system("git merge origin/2021-07-volcano-refac")
system("git merge origin/profile-plot-refac")
system("git merge DGEobj.plots_saran-upstream/2021-07-refactor-mapDGEobj")



saranya-ag:2021-07-refactor-mapDGEobj
