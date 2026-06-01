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
## 4) PURE STANDALONE TEST
## =========================================================
## No objects supplied externally.
## Module must create/load everything itself.
## =========================================================

sim <- simInit(
  times = list(start = 1, end = 1),
  modules = "EasternCanadaLandbase"
)

## =========================================================
## 5) RUN MODULE
## =========================================================
sim <- spades(sim)

## =========================================================
## 6) OUTPUT CHECKS
## =========================================================

cat("\n============================\n")
cat("OUTPUT OBJECTS\n")
cat("============================\n")

print(names(sim))

cat("\n============================\n")
cat("FOREST CELLS\n")
cat("============================\n")

print(
  global(
    sim$forestCoverMask,
    "sum",
    na.rm = TRUE
  )
)

cat("\n============================\n")
cat("PROTECTED CELLS\n")
cat("============================\n")

print(
  global(
    sim$protectedAreaMask,
    "sum",
    na.rm = TRUE
  )
)

cat("\n============================\n")
cat("TOTAL HARVESTABLE AREA\n")
cat("============================\n")

print(
  global(
    sim$harvestableFraction,
    "sum",
    na.rm = TRUE
  )
)

cat("\n============================\n")
cat("CHECK: NO PROTECTED CELL HARVESTABLE\n")
cat("============================\n")

print(
  global(
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
  global(
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
  global(
    sim$Landbase$fractional$riparianFraction,
    c("min", "max"),
    na.rm = TRUE
  )
)

cat("\n============================\n")
cat("CHECK: HARVESTABLE FRACTION RANGE\n")
cat("============================\n")

print(
  global(
    sim$harvestableFraction,
    c("min", "max"),
    na.rm = TRUE
  )
)

cat("\n============================\n")
cat("PURE STANDALONE TEST COMPLETE\n")
cat("============================\n")