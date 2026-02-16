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
  standAgeAligned <- terra::project(
    sim$standAgeMap,
    sim$PlanningGrid_250m,
    method = "near"
  )
  
  standAgeAligned <- terra::resample(
    standAgeAligned,
    sim$PlanningGrid_250m,
    method = "near"
  )
  
  analysisUnitAligned <- terra::project(
    sim$analysisUnitMap,
    sim$PlanningGrid_250m,
    method = "near"
  )
  
  analysisUnitAligned <- terra::resample(
    analysisUnitAligned,
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
  
  CPCAD_aligned <- sim$CPCAD
  
  if (!terra::same.crs(CPCAD_aligned, sim$PlanningGrid_250m)) {
    CPCAD_aligned <- terra::project(CPCAD_aligned, sim$PlanningGrid_250m)
  }
  
  sim$protectedMask <- terra::rasterize(
    CPCAD_aligned,
    sim$PlanningGrid_250m,
    field = 1,
    background = 0
  )
  
  
  # =========================================================
  # 3) Forest Mask
  # =========================================================
  
  message("Creating forestedMask")
  
  sim$forestedMask <- terra::ifel(
    !is.na(analysisUnitAligned) & analysisUnitAligned > 0,
    1,
    0
  )
  
  
  # =========================================================
  # 3) Net Productive Forest (SAFE VERSION)
  # =========================================================
  
  message("Creating net productive forest")
  
  sim$netProductiveForest <- terra::ifel(
    analysisUnitAligned > 0 &
      sim$protectedMask == 0 &
      !is.na(standAgeAligned) &
      standAgeAligned > 0,
    1,
    0
  )
  
  
  # =========================================================
  # 4) Final Landbase container
  # =========================================================
  
  sim$Landbase <- list(
    planningRaster       = sim$PlanningGrid_250m,
    landcover            = sim$LandCover,
    standAgeMap     = standAgeAligned,
    analysisUnitMap = analysisUnitAligned,
    forestedMask         = sim$forestedMask,
    protectedMask        = sim$protectedMask,
    netProductiveForest  = sim$netProductiveForest
  )
  
  invisible(sim)
  
  }
