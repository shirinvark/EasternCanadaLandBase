Init <- function(sim) {
  sim <- .inputObjects(sim)
  
  checkObject(sim, "PlanningGrid", "SpatRaster")
  checkObject(sim, "SYU", "SpatRaster")
  checkObject(sim, "LandCover", "SpatRaster")
  checkObject(sim, "Riparian", "list")
  checkObject(sim, "protectedArea", "SpatRaster")
  checkObject(sim, "jurisdiction", "SpatRaster")
  checkObject(sim, "bcr", "SpatRaster")
  checkObject(sim, "yieldCurveFamily", "SpatRaster")
  checkObject(sim, "Ownership", "SpatRaster")
  checkObject(sim, "DMFL", "SpatRaster")
  checkObject(sim, "SYULookup", c("data.frame", "data.table"))
  
  landCoverAligned <- sim$LandCover

  
  riparianAligned <- sim$Riparian$riparianFraction
  # =========================================================
  # Check raster geometry
  # =========================================================
  
  geomOK <- c(
    LandCover = terra::compareGeom(
      sim$PlanningGrid,
      landCoverAligned,
      stopOnError = FALSE
    )
  )
  
  if (!all(geomOK)) {
    stop(
      "Raster geometry mismatch: ",
      paste(names(geomOK)[!geomOK], collapse = ", ")
    )
  }
  
  sim <- buildLandbaseClass(sim)
  
  # ======================================================
  # 3) forestCoverMask
  # ========================================================
  
  sim <- buildForestCoverMask(sim)
  # =========================================================
  # Harvest eligibility - preliminary landbase filter
  # =========================================================
  
  sim <- buildHarvestEligibility(sim)
  
  # =========================================================
  # Apply riparian reduction
  # =========================================================
  
  sim <- applyRiparianReduction(sim)
  sim <- buildLandbaseRaster(sim)
  sim <- validateLandbase(sim)
  # =========================================================
  # 8) FINAL LANDBASE
  # =========================================================

  sim$Landbase <- list(
    
    baseData = list(
      planningRaster = sim$PlanningGrid,
      landcover      = landCoverAligned,
    ),
    masks = list(
      forestCoverMask   = sim$forestCoverMask,
      landbaseClass     = sim$landbaseClass,
      isHarvestEligible = sim$isHarvestEligible
    ),
    
    fractional = list(
      riparianFraction    = riparianAligned,
      harvestableFraction = sim$harvestableFraction
    )
  )
  
  invisible(sim)
}
