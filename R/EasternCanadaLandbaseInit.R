Init <- function(sim) {
  
  checkObject(sim, "PlanningGrid_250m", "SpatRaster")
  checkObject(sim, "LandCover", "SpatRaster")
  checkObject(sim, "standAgeMap", "SpatRaster")
  checkObject(sim, "CPCAD", "sf")
  checkObject(sim, "riparianFraction", "SpatRaster")
  
  # =========================================================
  # 1) ALIGN ALL RASTERS
  # =========================================================
  
  message("Aligning rasters to PlanningGrid")
  
  standAgeAligned <- terra::project(sim$standAgeMap,
                                    sim$PlanningGrid_250m,
                                    method = "near")
  standAgeAligned <- terra::resample(standAgeAligned,
                                     sim$PlanningGrid_250m,
                                     method = "near")
  
  riparianAligned <- terra::project(sim$riparianFraction,
                                    sim$PlanningGrid_250m,
                                    method = "near")
  riparianAligned <- terra::resample(riparianAligned,
                                     sim$PlanningGrid_250m,
                                     method = "near")
  # Align LandCover
  landCoverAligned <- terra::project(
    sim$LandCover,
    sim$PlanningGrid_250m,
    method = "near"
  )
  
  landCoverAligned <- terra::resample(
    landCoverAligned,
    sim$PlanningGrid_250m,
    method = "near"
  )
  
  
  # =========================================================
  # 2) PROTECTED MASK
  # =========================================================
  
  message("Creating protectedMask")
  
  CPCAD_aligned <- sim$CPCAD
  
  if (!terra::same.crs(CPCAD_aligned, sim$PlanningGrid_250m)) {
    CPCAD_aligned <- terra::project(CPCAD_aligned, sim$PlanningGrid_250m)
  }
  
  protTmp <- terra::rasterize(
    CPCAD_aligned,
    sim$PlanningGrid_250m,
    field = 1
  )
  
  sim$protectedMask <- terra::ifel(
    is.na(protTmp),
    0,
    1
  )
  
  
  # =========================================================
  # 3) FOREST BASE (exclude wetlands)
  # =========================================================
  
  # =========================================================
  # 3) FOREST BASE (exclude wetlands)
  # =========================================================
  
  message("Creating base forest mask")
  
  forestProductiveClasses <- c(210, 220, 230)
  
  sim$forestBase <- terra::ifel(
    landCoverAligned %in% forestProductiveClasses,
    1,
    0
  )
  
  
  
  
  # =========================================================
  # 4) BUILD SIMPLE ANALYSIS UNIT (DEV MODE)
  # =========================================================
  
  message("Building simple analysisUnitMap")
  
  analysisUnitMap <- sim$PlanningGrid_250m
  analysisUnitMap[] <- NA
  
  analysisUnitMap[landCoverAligned == 210] <- 1
  analysisUnitMap[landCoverAligned == 220] <- 2
  analysisUnitMap[landCoverAligned == 230] <- 3
  
  # =========================================================
  # 5) BASE MASK (forest + protected + age)
  # =========================================================
  
  baseMask <- terra::ifel(
    sim$forestBase == 1 &
      sim$protectedMask == 0 &
      !is.na(standAgeAligned) &
      standAgeAligned > 0,
    1,
    0
  )
  
  
  # =========================================================
  # 6) APPLY RIPARIAN REDUCTION
  # =========================================================
  riparianAligned <- terra::clamp(riparianAligned, 0, 1)
  
  message("Applying riparian reduction")
  
  sim$merchantableForest <- baseMask * (1 - riparianAligned)
  
  # =========================================================
  # 7) MASK ANALYSIS UNIT
  # =========================================================
  
  message("Masking analysis units by merchantable forest")
  
  analysisUnitMasked <- terra::ifel(
    sim$merchantableForest > 0,
    analysisUnitMap,
    NA
  )
  
  # =========================================================
  # 8) FINAL LANDBASE
  # =========================================================
  sim$analysisUnitMap <- analysisUnitMasked
  
  sim$Landbase <- list(
    planningRaster      = sim$PlanningGrid_250m,
    landcover           = landCoverAligned,
    standAgeMap         = standAgeAligned,
    analysisUnitMap     = sim$analysisUnitMap,
    forestBase          = sim$forestBase,
    protectedMask       = sim$protectedMask,
    merchantableForest  = sim$merchantableForest
  )
  
  invisible(sim)
}
