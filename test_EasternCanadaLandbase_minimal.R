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
library(LandR)

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
## 4) BUILD TEST DATA (SAFE VERSION)
## =========================================================

# ---- Load NTEMS once to get correct CRS + extent ----
nt <- rast("E:/EasternCanadaLandBase/CA_forest_VLCE2_2001.tif")
e  <- ext(nt)

# ---- Planning grid INSIDE NTEMS extent ----
PlanningGrid_250m <- rast(
  nrows = 50,
  ncols = 50,
  xmin  = e$xmin + 20000,
  xmax  = e$xmin + 20000 + (50 * 250),
  ymin  = e$ymin + 20000,
  ymax  = e$ymin + 20000 + (50 * 250),
  crs   = crs(nt)
)

values(PlanningGrid_250m) <- 1

# ---- Study Area polygon (from grid extent) ----
studyArea <- as.polygons(ext(PlanningGrid_250m)) |>
  st_as_sf()

# ---- Stand Age ----
standAgeMap <- PlanningGrid_250m
values(standAgeMap) <- sample(1:120, ncell(standAgeMap), replace = TRUE)

# ---- Riparian (fractional 0–0.4 random) ----
riparianFraction <- PlanningGrid_250m
values(riparianFraction) <- runif(ncell(riparianFraction), 0, 0.4)

# ---- Dummy protected polygon ----
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
## 5) INIT + RUN SIMULATION
## =========================================================
sim <- simInit(
  times   = list(start = 1, end = 1),
  modules = "EasternCanadaLandbase",
  objects = list(
    PlanningGrid_250m = PlanningGrid_250m,
    studyArea         = studyArea,
    standAgeMap       = standAgeMap,
    riparianFraction  = riparianFraction,
    CPCAD             = CPCAD
    # LandCover عمداً داده نمی‌شود
    # ماژول خودش NTEMS می‌سازد
  )
)

sim <- spades(sim)

## =========================================================
## 6) OUTPUT CHECKS
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

cat("\nUnique LandCover classes after alignment:\n")
print(head(freq(sim$LandCover), 10))

cat("\n---- TEST COMPLETE ----\n")
