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
## 4) LOAD OFFICIAL LANDCOVER
## =========================================================

LandCover <- rast("E:/EasternCanadaLandBase/Sudbury_rstLCC_official.tif")

cat("\nLandCover loaded.\n")
print(LandCover)

## =========================================================
## 5) BUILD PLANNING GRID ON VALID PIXEL
## =========================================================

valid_pt <- spatSample(
  LandCover,
  size = 1,
  method = "random",
  na.rm = TRUE,
  as.points = TRUE
)

xy <- crds(valid_pt)

PlanningGrid_250m <- rast(
  nrows = 50,
  ncols = 50,
  xmin  = xy[1] - 25*250,
  xmax  = xy[1] + 25*250,
  ymin  = xy[2] - 25*250,
  ymax  = xy[2] + 25*250,
  crs   = crs(LandCover)
)

PlanningGrid_250m[] <- 1

cat("\nPlanningGrid created on valid landcover area.\n")

## ========================================================
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

cat("\nForest base cells:\n")
print(global(sim$forestBase, "sum", na.rm = TRUE))

cat("\nProtected cells:\n")
print(global(sim$protectedMask, "sum", na.rm = TRUE))

cat("\nMerchantable total (fractional sum):\n")
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

cat("\nUnique LandCover classes inside planning grid:\n")
print(freq(sim$Landbase$landcover))

cat("\n---- TEST COMPLETE ----\n")

