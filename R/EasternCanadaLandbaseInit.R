Init <- function(sim) {
  
  checkObject(sim, "PlanningGrid_250m", "SpatRaster")
  checkObject(sim, "LandCover", "SpatRaster")
  checkObject(sim, "standAgeMap", "SpatRaster")
  checkObject(sim, "analysisUnitMap", "SpatRaster")
  checkObject(sim, "CPCAD", "sf")
  
  # =========================================================
  # 1) FORCE SAME CRS + ALIGN
  # =========================================================
  
  message("Aligning rasters to PlanningGrid")
  
  # Project to exact same CRS
  sim$standAgeMap <- terra::project(
    sim$standAgeMap,
    sim$PlanningGrid_250m,
    method = "near"
  )
  
  sim$analysisUnitMap <- terra::project(
    sim$analysisUnitMap,
    sim$PlanningGrid_250m,
    method = "near"
  )
  
  # Then snap to exact grid
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
  
  # =========================================================
  # 2) Create Protected Mask (DO NOT MODIFY AU)
  # =========================================================
  
  message("Creating protectedMask")
  
  if (!terra::same.crs(sim$CPCAD, sim$PlanningGrid_250m)) {
    sim$CPCAD <- terra::project(sim$CPCAD, sim$PlanningGrid_250m)
  }
  
  sim$protectedMask <- terra::rasterize(
    sim$CPCAD,
    sim$PlanningGrid_250m,
    field = 1,
    background = 0
  )
  
  # =========================================================
  # 3) Forest Mask
  # =========================================================
  
  message("Creating forestedMask")
  
  sim$forestedMask <- terra::ifel(
    sim$analysisUnitMap > 0,
    1,
    0
  )
  
  # =========================================================
  # 3) Net Productive Forest (SAFE VERSION)
  # =========================================================
  
  message("Creating net productive forest")
  
  sim$netProductiveForest <- terra::ifel(
    sim$analysisUnitMap > 0 &
      sim$protectedMask == 0 &
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
    forestedMask         = sim$forestedMask,
    protectedMask        = sim$protectedMask,
    netProductiveForest  = sim$netProductiveForest
  )
  
  invisible(sim)
  
  }
