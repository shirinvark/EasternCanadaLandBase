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
## 3) LOAD SMALL STUDY AREA
## =========================================================

studyArea <- sf::st_read(
  "D:/BOUNDARIES/Sudbury_FMU_5070.shp",
  quiet = TRUE
)

studyArea <- sf::st_make_valid(studyArea)

studyArea <- sf::st_union(studyArea)

studyArea <- sf::st_sf(
  id = 1,
  geometry = studyArea
)

studyArea <- sf::st_transform(
  studyArea,
  "ESRI:102001"
)

## =========================================================
## 4) OPTIONAL: REMOVE OLD CACHE
## =========================================================

unlink(
  file.path(getPaths()$cachePath, "*"),
  recursive = TRUE,
  force = TRUE
)

## =========================================================
## 5) GET MODULE
## =========================================================

#getModule(
  modules    = "shirinvark/EasternCanadaLandbase",
  modulePath = getPaths()$modulePath,
  overwrite  = TRUE
)

## =========================================================
## 6) INIT SIM
## =========================================================

sim <- simInit(
  
  times = list(
    start = 1,
    end   = 1
  ),
  
  modules = "EasternCanadaLandbase",
  
  objects = list(
    studyArea = studyArea
  ),
  
  paths = getPaths()
)

## =========================================================
## 7) RUN MODULE
## =========================================================

system.time({
  
  sim <- spades(sim)
  
})

## =========================================================
## 8) OUTPUT CHECKS
## =========================================================

cat("\n============================\n")
cat("OUTPUT OBJECTS\n")
cat("============================\n")

print(names(sim))

cat("\n============================\n")
cat("FOREST CELLS\n")
cat("============================\n")

print(
  terra::global(
    sim$forestCoverMask,
    "sum",
    na.rm = TRUE
  )
)

cat("\n============================\n")
cat("PROTECTED CELLS\n")
cat("============================\n")

print(
  terra::global(
    sim$protectedAreaMask,
    "sum",
    na.rm = TRUE
  )
)

cat("\n============================\n")
cat("TOTAL HARVESTABLE AREA\n")
cat("============================\n")

print(
  terra::global(
    sim$harvestableFraction,
    "sum",
    na.rm = TRUE
  )
)

cat("\n============================\n")
cat("CHECK: NO PROTECTED CELL HARVESTABLE\n")
cat("============================\n")

print(
  terra::global(
    (sim$harvestableFraction > 0) &
      (sim$protectedAreaMask == 1),
    "sum",
    na.rm = TRUE
  )
)

cat("\n============================\n")
cat("CHECK: HARVESTABLE ⊂ FOREST\n")
cat("============================\n")

print(
  terra::global(
    (sim$harvestableFraction > 0) &
      (sim$forestCoverMask == 0),
    "sum",
    na.rm = TRUE
  )
)

cat("\n============================\n")
cat("CHECK: RIPARIAN FRACTION RANGE\n")
cat("============================\n")

print(
  terra::global(
    sim$Landbase$fractional$riparianFraction,
    c("min", "max"),
    na.rm = TRUE
  )
)

cat("\n============================\n")
cat("CHECK: HARVESTABLE FRACTION RANGE\n")
cat("============================\n")

print(
  terra::global(
    sim$harvestableFraction,
    c("min", "max"),
    na.rm = TRUE
  )
)

## =========================================================
## 9) QUICK PLOTS
## =========================================================

plot(
  sim$forestCoverMask,
  main = "Forest Cover Mask"
)

plot(
  sim$protectedAreaMask,
  main = "Protected Area Mask"
)

plot(
  sim$Landbase$fractional$riparianFraction,
  main = "Riparian Fraction"
)

plot(
  sim$harvestableFraction,
  main = "Harvestable Fraction"
)