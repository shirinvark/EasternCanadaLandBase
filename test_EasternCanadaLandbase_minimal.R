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
## 3) DOWNLOAD MODULE (optional)
## =========================================================
getModule(
  modules    = "shirinvark/EasternCanadaLandbase",
  modulePath = getPaths()$modulePath,
  overwrite  = TRUE
)

## =========================================================
## 4) BUILD DUMMY TEST DATA
## =========================================================

# ---- Planning grid ----
PlanningGrid_250m <- rast(
  nrows = 20, ncols = 20,
  res   = 250,
  crs   = "EPSG:5070"
)
values(PlanningGrid_250m) <- 1

# ---- Stand Age ----
standAgeMap <- PlanningGrid_250m
values(standAgeMap) <- sample(1:120, ncell(standAgeMap), replace = TRUE)

# ---- Analysis Unit ----
analysisUnitMap <- PlanningGrid_250m
values(analysisUnitMap) <- sample(c(0,1,2,3), ncell(analysisUnitMap), replace = TRUE)

# ---- Land Cover (not used heavily here but required)
LandCover <- PlanningGrid_250m
values(LandCover) <- sample(c(20,50,100), ncell(LandCover), replace = TRUE)

# ---- Dummy protected polygon ----
CPCAD <- st_as_sf(
  st_sfc(
    st_polygon(list(
      matrix(c(
        0,0,
        2000,0,
        2000,2000,
        0,2000,
        0,0
      ), ncol = 2, byrow = TRUE)
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
    PlanningGrid_250m = PlanningGrid_250m,
    LandCover         = LandCover,
    standAgeMap       = standAgeMap,
    analysisUnitMap   = analysisUnitMap,
    CPCAD             = CPCAD
  )
)

sim <- spades(sim)

## =========================================================
## 6) OUTPUT CHECKS
## =========================================================

cat("\n---- BASIC CHECKS ----\n")

print(names(sim))

cat("\nResolution check:\n")
print(res(sim$analysisUnitMap))
print(res(sim$standAgeMap))

cat("\nExtent check:\n")
print(ext(sim$analysisUnitMap))
print(ext(sim$standAgeMap))

cat("\n---- LOGICAL CHECKS ----\n")

cat("Forested cells:\n")
print(global(sim$forestedMask, "sum", na.rm = TRUE))

cat("Protected cells:\n")
print(global(sim$protectedMask, "sum", na.rm = TRUE))

cat("Net productive cells:\n")
print(global(sim$netProductiveForest, "sum", na.rm = TRUE))

cat("\nCheck: No protected cell should be productive\n")
print(
  global(
    sim$netProductiveForest == 1 &
      sim$protectedMask == 1,
    "sum"
  )
)

cat("\nCheck: Productive <= Forested\n")
print(
  global(
    sim$netProductiveForest == 1 &
      sim$forestedMask == 0,
    "sum"
  )
)

cat("\n---- TEST COMPLETE ----\n")

