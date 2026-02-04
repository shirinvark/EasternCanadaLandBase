## =========================================================
## 0) CLEAN SESSION
## =========================================================
rm(list = ls())
gc()

## =========================================================
## 1) LOAD PACKAGES
## =========================================================
library(SpaDES.core)
library(SpaDES.project)
library(terra)
library(sf)

## =========================================================
## 2) SET PATHS (محل دانلود ماژول)
## =========================================================
setPaths(
  modulePath  = "E:/MODULES_TESTS/modules",
  inputPath   = "E:/MODULES_TESTS/inputs",
  outputPath  = "E:/MODULES_TESTS/outputs",
  cachePath   = "E:/MODULES_TESTS/cache",
  scratchPath = "E:/MODULES_TESTS/scratch"
)

## =========================================================
## 3) DOWNLOAD MODULE ONLINE
## =========================================================
getModule(
  modules    = "shirinvark/EasternCanadaLandbase",
  modulePath = getPaths()$modulePath,
  overwrite  = TRUE
)

## =========================================================
## 4) BUILD MINIMAL TEST OBJECTS
## =========================================================
# Planning raster
PlanningRaster <- rast(
  nrows = 10, ncols = 10,
  res   = 250,
  crs   = "EPSG:5070"
)
values(PlanningRaster) <- 1

# Land cover (dummy)
LandCoverAligned <- PlanningRaster
values(LandCoverAligned) <- sample(c(20, 50, 100), ncell(LandCoverAligned), replace = TRUE)

# Dummy protected areas
CPCAD <- st_as_sf(
  st_sfc(
    st_polygon(list(
      matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)
    )),
    crs = 5070
  )
)

## =========================================================
## 5) INIT + RUN SIMULATION
## =========================================================
sim <- simInit(
  times   = list(start = 1, end = 1),
  modules = "EasternCanadaLandbase",
  objects = list(
    PlanningRaster   = PlanningRaster,
    LandCoverAligned = LandCoverAligned,
    CPCAD            = CPCAD
  )
)

sim <- spades(sim)

## =========================================================
## 6) CHECK OUTPUT
## =========================================================
names(sim)
sim$Landbase
