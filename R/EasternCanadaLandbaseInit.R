Init <- function(sim) {
  
  checkObject(sim, "PlanningGrid_250m", "SpatRaster")
  checkObject(sim, "LandCoverAligned", "SpatRaster")
  checkObject(sim, "standAgeMap", "SpatRaster")
  checkObject(sim, "analysisUnitMap", "SpatRaster")
  
  sim$Landbase <- list(
    planningRaster   = sim$PlanningGrid_250m,
    landcover        = sim$LandCoverAligned,
    standAgeMap      = sim$standAgeMap,
    analysisUnitMap  = sim$analysisUnitMap
  )
  
  invisible(sim)
}
