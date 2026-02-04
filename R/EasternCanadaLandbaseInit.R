# EasternCanadaLandbase assembles and maintains the spatial landbase
# on which subsequent modules will classify managed forested stands
# into analysis units. No ecological or management classification
# is performed in this module.
Init <- function(sim) {
  
  checkObject(sim, "PlanningRaster", "SpatRaster")
  checkObject(sim, "LandCoverAligned", "SpatRaster")
  checkObject(sim, "CPCAD")
  
  # EasternCanadaLandbase assembles and maintains the spatial landbase
  # on which subsequent modules will classify managed forested stands
  # into analysis units. No ecological or management classification
  # is performed in this module.
  
  sim$Landbase <- list(
    planningRaster = sim$PlanningRaster,
    landcover      = sim$LandCoverAligned
  )
  
  invisible(sim)
}
