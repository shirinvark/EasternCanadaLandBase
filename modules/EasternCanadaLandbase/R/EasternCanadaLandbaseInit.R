Init <- function(sim) {
  
  checkObject(sim, "PlanningGrid_250m", "SpatRaster")
  checkObject(sim, "LandCover", "SpatRaster")
  checkObject(sim, "standAgeMap", "SpatRaster")
  checkObject(sim, "analysisUnitMap", "SpatRaster")
  checkObject(sim, "CPCAD", "sf")
  
  # ---- Mask Protected Areas ----
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
  
  sim$analysisUnitMap[protectedRaster == 1] <- 0
  
  # ---- Net Productive Forest ----
  message("Creating net productive forest")
  
  netProductiveForest <- sim$analysisUnitMap
  
  netProductiveForest[
    sim$analysisUnitMap > 0 &
      !is.na(sim$standAgeMap) &
      sim$standAgeMap > 0
  ] <- 1
  
  netProductiveForest[
    !(sim$analysisUnitMap > 0 &
        !is.na(sim$standAgeMap) &
        sim$standAgeMap > 0)
  ] <- 0
  
  sim$netProductiveForest <- netProductiveForest
  
  
  sim$Landbase <- list(
    planningRaster       = sim$PlanningGrid_250m,
    landcover            = sim$LandCover,
    standAgeMap          = sim$standAgeMap,
    analysisUnitMap      = sim$analysisUnitMap,
    netProductiveForest  = sim$netProductiveForest
  )
  
  invisible(sim)
}
