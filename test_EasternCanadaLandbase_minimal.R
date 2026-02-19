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
## 4) LOAD OFFICIAL NTEMS LANDCOVER
## =========================================================

LandCover <- rast("E:/EasternCanadaLandBase/Sudbury_rstLCC_official.tif")

e <- ext(LandCover)

## =========================================================
## 5) BUILD PLANNING GRID INSIDE LANDCOVER EXTENT
## =========================================================

PlanningGrid_250m <- rast(
  nrows = 50,
  ncols = 50,
  xmin  = e$xmin + 20000,
  xmax  = e$xmin + 20000 + (50 * 250),
  ymin  = e$ymin + 20000,
  ymax  = e$ymin + 20000 + (50 * 250),
  crs   = crs(LandCover)
)

PlanningGrid_250m[] <- 1

## =========================================================
## 6) STUDY AREA
## =========================================================

studyArea <- as.polygons(ext(PlanningGrid_250m)) |>
  st_as_sf()

## =========================================================
## 7) STAND AGE
## =========================================================

standAgeMap <- rast(PlanningGrid_250m)
standAgeMap[] <- sample(1:120, ncell(standAgeMap), replace = TRUE)

## =========================================================
## 8) RIPARIAN
## =========================================================

riparianFraction <- rast(PlanningGrid_250m)
riparianFraction[] <- runif(ncell(riparianFraction), 0, 0.4)

## =========================================================
## 9) DUMMY PROTECTED AREA
## =========================================================

CPCAD <- st_as_sf(
  st_sfc(
    st_polygon(list(
      matrix(c(
        xmin(PlanningGrid_250m)+2000, ymin(PlanningGrid_250m)+2000,
        xmin(PlanningGrid_250m)+8000, ymin(PlanningGrid_250m)+2000,
        xmin(PlanningGrid_250m)+8000, ymin(PlanningGrid_250m)+8000,
        xmin(PlanningGrid_250m)+2000, ymin(PlanningGrid_250m)+8000,
        xmin(PlanningGrid_250m)+2000, ymin(PlanningGrid_250m)+2000
      ), ncol = 2, byrow = TRUE)
    )),
    crs = st_crs(studyArea)
  )
)

## =========================================================
## 10) INIT + RUN SIMULATION
## =========================================================

sim <- simInit(
  times   = list(start = 1, end = 1),
  modules = "EasternCanadaLandbase",
  objects = list(
    PlanningGrid_250m = PlanningGrid_250m,
    LandCover         = LandCover,
    studyArea         = studyArea,
    standAgeMap       = standAgeMap,
    riparianFraction  = riparianFraction,
    CPCAD             = CPCAD
  )
)

sim <- spades(sim)

## =========================================================
## 11) OUTPUT CHECKS
## =========================================================

cat("\n---- LOGICAL CHECKS ----\n")

cat("Forest base cells:\n")
print(global(sim$forestBase, "sum", na.rm = TRUE))

cat("Protected cells:\n")
print(global(sim$protectedMask, "sum", na.rm = TRUE))

cat("Merchantable total (fractional sum):\n")
print(global(sim$merchantableForest, "sum", na.rm = TRUE))

cat("\nCheck: No protected cell should be merchantable\n")
print(
  global(
    sim$merchantableForest > 0 &
      sim$protectedMask == 1,
    "sum"
  )
)

cat("\nCheck: Merchantable must be subset of forestBase\n")
print(
  global(
    sim$merchantableForest > 0 &
      sim$forestBase == 0,
    "sum"
  )
)

cat("\nUnique LandCover classes:\n")
print(head(freq(sim$Landbase$landcover), 10))

cat("\n---- TEST COMPLETE ----\n")

