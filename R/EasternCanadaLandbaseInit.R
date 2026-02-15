Init <- function(sim) {
  
  checkObject(sim, "PlanningGrid_250m", "SpatRaster")
  checkObject(sim, "LandCover", "SpatRaster")
  checkObject(sim, "standAgeMap", "SpatRaster")
  checkObject(sim, "analysisUnitMap", "SpatRaster")
  checkObject(sim, "CPCAD", "sf")
  
  # =========================================================
  # 1) Align ALL rasters to PlanningGrid FIRST
  # =========================================================
  
  message("Aligning rasters to PlanningGrid")
  
  sim$standAgeMap <- terra::resample(
    sim$standAgeMap,
    sim$PlanningGrid_250m,
    method = "near"
  )
  
  sim$analysisUnitMap <- terra::resample(
    sim$analysisUnitMap,
    sim$PlanningGrid_250m,
    method = "near"
  )
  
  # =========================================================
  # 2) Mask Protected Areas
  # =========================================================
  
  message("Masking protected areas from analysisUnitMap")
  
  if (!terra::same.crs(sim$CPCAD, sim$PlanningGrid_250m)) {
    sim$CPCAD <- terra::project(sim$CPCAD, sim$PlanningGrid_250m)
  }
  
  protectedRaster <- terra::rasterize(
    sim$CPCAD,
    sim$PlanningGrid_250m,
    field = 1,
    background = 0
  )
  
  sim$analysisUnitMap <- terra::ifel(
    protectedRaster == 1,
    0,
    sim$analysisUnitMap
  )
  
  # =========================================================
  # 3) Net Productive Forest (SAFE VERSION)
  # =========================================================
  
  message("Creating net productive forest")
  
  sim$netProductiveForest <- terra::ifel(
    sim$analysisUnitMap > 0 &
      !is.na(sim$standAgeMap) &
      sim$standAgeMap > 0,
    1,
    0
  )
  
  # =========================================================
  # 4) Final Landbase container
  # =========================================================
  
  sim$Landbase <- list(
    planningRaster       = sim$PlanningGrid_250m,
    landcover            = sim$LandCover,
    standAgeMap          = sim$standAgeMap,
    analysisUnitMap      = sim$analysisUnitMap,
    netProductiveForest  = sim$netProductiveForest
  )
  
  invisible(sim)
}
