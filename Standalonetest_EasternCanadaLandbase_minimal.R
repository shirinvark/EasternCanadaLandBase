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
## 2) SET PATHS
## =========================================================
setPaths(
  modulePath  = "E:/EasternCanadaLandBase/modules",
  inputPath   = "E:/EasternCanadaLandBase/inputs",
  outputPath  = "E:/EasternCanadaLandBase/outputs",
  cachePath   = "E:/EasternCanadaLandBase/cache",
  scratchPath = "E:/EasternCanadaLandBase/scratch"
)

## =========================================================
## 3) DOWNLOAD MODULE
## =========================================================
getModule(
  modules    = "shirinvark/EasternCanadaLandbase",
  modulePath = getPaths()$modulePath,
  overwrite  = TRUE
)

## =========================================================
## 4) LOAD STUDY AREA
## =========================================================
studyArea <- sf::st_read(
  "E:/EasternCanadaDataPrep/BOUNDARIES/Sudbury_FMU_5070.shp",
  quiet = TRUE
)

studyArea <- sf::st_make_valid(studyArea)

## =========================================================
## 5) BUILD PLANNING GRID
## =========================================================
PlanningGrid_250m <- rast(
  ext(vect(studyArea)),
  resolution = 250,
  crs = crs(vect(studyArea))
)

PlanningGrid_250m[] <- 1

## =========================================================
## 6) CREATE SYNTHETIC INPUTS
## =========================================================

# ---- LandCover
LandCover_250m <- rast(PlanningGrid_250m)
LandCover_250m[] <- sample(
  c(210, 220, 230, 100),
  ncell(LandCover_250m),
  replace = TRUE
)

# ---- Stand Age
standAge_250m <- rast(PlanningGrid_250m)
standAge_250m[] <- sample(
  1:120,
  ncell(standAge_250m),
  replace = TRUE
)

# ---- Riparian Fraction (0–0.4)
riparianFraction <- rast(PlanningGrid_250m)
riparianFraction[] <- runif(
  ncell(riparianFraction),
  0,
  0.4
)

Riparian <- list(
  riparianFraction = riparianFraction
)

## =========================================================
## 7) CREATE LegalConstraints (Protected Raster)
## =========================================================
protectedRaster <- rast(PlanningGrid_250m)
protectedRaster[] <- 0

# 10% protected
protectedRaster[
  sample(
    ncell(protectedRaster),
    size = round(0.1 * ncell(protectedRaster))
  )
] <- 1

LegalConstraints <- list(
  CPCAD_Raster_250m = protectedRaster
)

## =========================================================
## 8) INIT + RUN SIMULATION
## =========================================================
sim <- simInit(
  times   = list(start = 1, end = 1),
  modules = "EasternCanadaLandbase",
  objects = list(
    PlanningGrid_250m = PlanningGrid_250m,
    LandCover_250m    = LandCover_250m,
    standAge_250m     = standAge_250m,
    Riparian          = Riparian,
    LegalConstraints  = LegalConstraints
  )
)

sim <- spades(sim)

## =========================================================
## 9) OUTPUT CHECKS
## =========================================================

cat("\nForest cells:\n")
print(global(sim$forestCoverMask, "sum", na.rm = TRUE))

cat("\nProtected cells:\n")
print(global(sim$protectedAreaMask, "sum", na.rm = TRUE))

cat("\nTotal harvestable area (fractional sum):\n")
print(global(sim$harvestableFraction, "sum", na.rm = TRUE))

cat("\nCheck: No protected cell harvestable\n")
print(
  global(
    (sim$harvestableFraction > 0) &
      (sim$protectedAreaMask == 1),
    "sum",
    na.rm = TRUE
  )
)

cat("\nCheck: Harvestable subset of forest\n")
print(
  global(
    (sim$harvestableFraction > 0) &
      (sim$forestCoverMask == 0),
    "sum",
    na.rm = TRUE
  )
)

cat("\n---- TEST COMPLETE ----\n")