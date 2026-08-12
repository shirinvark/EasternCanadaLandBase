

rm(list = ls())
gc()

library(SpaDES.core)
library(SpaDES.project)
library(terra)
library(sf)

# =========================================================
# PATHS
# =========================================================

setPaths(
  cachePath   = "E:/EasternCanadaLandBase/cache",
  inputPath   = "E:/EasternCanadaLandBase/inputs",
  outputPath  = "E:/EasternCanadaLandBase/outputs",
  modulePath  = "E:/EasternCanadaLandBase/modules",
  scratchPath = "E:/EasternCanadaLandBase/scratch"
)

# =========================================================
# SMALL SUDBURY PATCH
# =========================================================

sudbury <- st_read(
  "D:/BOUNDARIES/NL_EB_Poly_50k_Upload.shp  ",
  quiet = TRUE
)

sudbury <- st_make_valid(sudbury)

cent <- st_centroid(st_union(sudbury))
xy <- st_coordinates(cent)

small_ext <- ext(
  xy[1] - 2500,
  xy[1] + 2500,
  xy[2] - 2500,
  xy[2] + 2500
)

small_poly <- as.polygons(
  small_ext,
  crs = crs(vect(sudbury))
)

small_poly <- intersect(
  vect(sudbury),
  small_poly
)
#######################all of sudbary
# =========================================================
# WHOLE SUDBURY FMU
# =========================================================

studyArea <- st_read(
  "D:/BOUNDARIES/NL_EB_Poly_50k_Upload.shp",
  quiet = TRUE
)

studyArea <- st_make_valid(studyArea)

# =========================================================
# MODULES
# =========================================================

getModule(
  modules = c(
    "shirinvark/EasternCanadaDataPrep",
    "shirinvark/RiparianBuffers",
    "shirinvark/EasternCanadaLandbase"
  ),
  modulePath = getPaths()$modulePath,
  overwrite = FALSE
)

# =========================================================
# INIT
# =========================================================

sim <- simInit(
  
  times = list(
    start = 1,
    end   = 1
  ),
  
  modules = c(
    "EasternCanadaDataPrep",
    "RiparianBuffers",
    "EasternCanadaLandbase"
  ),
  
  objects = list(
    studyArea =small_poly
  ),
  
  params = list(
    
    EasternCanadaDataPrep = list(
      devMode = FALSE
    ),
    
    RiparianBuffers = list(
      hydroRaster_m = 25
    )
    
  ),
  
  paths = getPaths()
)

# =========================================================
# RUN
# =========================================================

system.time({
  sim <- spades(sim)
})

# =========================================================
# CHECKS
# =========================================================

names(sim)

terra::global(
  sim$forestCoverMask,
  "sum",
  na.rm = TRUE
)

terra::global(
  sim$protectedAreaMask,
  "sum",
  na.rm = TRUE
)

terra::global(
  sim$harvestableFraction,
  "sum",
  na.rm = TRUE
)

terra::global(
  sim$Riparian$riparianFraction,
  c("min", "max"),
  na.rm = TRUE
)

terra::global(
  sim$harvestableFraction,
  c("min", "max"),
  na.rm = TRUE
)

plot(sim$forestCoverMask)

plot(sim$protectedAreaMask)

plot(sim$Riparian$riparianFraction)

plot(sim$harvestableFraction)








