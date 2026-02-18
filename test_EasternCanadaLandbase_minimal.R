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
## 4) BUILD TEST DATA
## =========================================================

# ---- Planning grid (EPSG:5070, 250m) ----
PlanningGrid_250m <- rast(
  nrows = 50, ncols = 50,
  xmin = 0, xmax = 12500,
  ymin = 0, ymax = 12500,
  crs  = "EPSG:5070"
)
values(PlanningGrid_250m) <- 1


# ---- Stand Age ----
standAgeMap <- PlanningGrid_250m
values(standAgeMap) <- sample(1:120, ncell(standAgeMap), replace = TRUE)


# ---- Riparian (fractional 0–0.4 random) ----
riparianFraction <- PlanningGrid_250m
values(riparianFraction) <- runif(ncell(riparianFraction), 0, 0.4)


# ---- Real SCANFI LandCover ----
lc_path <- "E:/MODULES_TESTS/SCANFI_att_nfiLandCover_CanadaLCCclassCodes_S_2010_v1_1.tif"
LandCover <- rast(lc_path)

# IMPORTANT: do NOT reproject here
# Let module handle alignment


# ---- Dummy protected polygon ----
CPCAD <- st_as_sf(
  st_sfc(
    st_polygon(list(
      matrix(c(
        2000,2000,
        8000,2000,
        8000,8000,
        2000,8000,
        2000,2000
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
    riparianFraction  = riparianFraction,
    CPCAD             = CPCAD
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
print(head(terra::freq(sim$Landbase$landcover), 20))

cat("\n---- TEST COMPLETE ----\n")
