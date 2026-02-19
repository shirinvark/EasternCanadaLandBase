Init <- function(sim) {
  
  checkObject(sim, "PlanningGrid_250m", "SpatRaster")
  checkObject(sim, "LandCover", "SpatRaster")
  checkObject(sim, "standAgeMap", "SpatRaster")
  checkObject(sim, "CPCAD", "sf")
  checkObject(sim, "riparianFraction", "SpatRaster")
  
  # =========================================================
  # 1) ALIGN ALL RASTERS
  # =========================================================
  # =========================================================
  # 1) ALIGN ALL RASTERS
  # =========================================================
  
  # =========================================================
  # 1) ALIGN ALL RASTERS
  # =========================================================
  
  message("Aligning rasters to PlanningGrid")
  
  # ---- StandAge ----
  if (!terra::same.crs(sim$standAgeMap, sim$PlanningGrid_250m)) {
    standAgeAligned <- terra::project(
      sim$standAgeMap,
      sim$PlanningGrid_250m,
      method = "near"
    )
  } else {
    standAgeAligned <- sim$standAgeMap
  }
  
  standAgeAligned <- terra::resample(
    standAgeAligned,
    sim$PlanningGrid_250m,
    method = "near"
  )
  
  
  # ---- Riparian ----
  if (!terra::same.crs(sim$riparianFraction, sim$PlanningGrid_250m)) {
    riparianAligned <- terra::project(
      sim$riparianFraction,
      sim$PlanningGrid_250m,
      method = "near"
    )
  } else {
    riparianAligned <- sim$riparianFraction
  }
  
  riparianAligned <- terra::resample(
    riparianAligned,
    sim$PlanningGrid_250m,
    method = "near"
  )
  
  
  # ---- LandCover ----
  if (!terra::same.crs(sim$LandCover, sim$PlanningGrid_250m)) {
    landCoverAligned <- terra::project(
      sim$LandCover,
      sim$PlanningGrid_250m,
      method = "near"
    )
  } else {
    landCoverAligned <- sim$LandCover
  }
  
  landCoverAligned <- terra::resample(
    landCoverAligned,
    sim$PlanningGrid_250m,
    method = "near"
  )
  
  
  # 2) PROTECTED MASK
  # =========================================================
  
  message("Creating protectedMask")
  
  CPCAD_aligned <- sim$CPCAD
  
  # اگر CPCAD خالی باشد یا CRS نداشته باشد
  if (is.null(CPCAD_aligned) ||
      nrow(CPCAD_aligned) == 0 ||
      is.na(sf::st_crs(CPCAD_aligned))) {
    
    message("CPCAD empty or missing CRS → protectedMask = 0")
    
    sim$protectedMask <- terra::rast(sim$PlanningGrid_250m)
    sim$protectedMask[] <- 0
    
  } else {
    
    # اگر CRS متفاوت باشد
    if (sf::st_crs(CPCAD_aligned) != sf::st_crs(terra::crs(sim$PlanningGrid_250m))) {
      
      CPCAD_aligned <- sf::st_transform(
        CPCAD_aligned,
        terra::crs(sim$PlanningGrid_250m)
      )
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
  }
  
  
  # ========================================================
  # 3) FOREST BASE (exclude wetlands)
  # =========================================================
  
  # =========================================================
  # 3) FOREST BASE (exclude wetlands)
  # =========================================================
  
  message("Creating base forest mask")
  
  sim$forestBase <- terra::ifel(
    landCoverAligned == 210 |
      landCoverAligned == 220 |
      landCoverAligned == 230,
    1,
    0
  )
  
  
  
  # =========================================================
  # 4) BUILD SIMPLE ANALYSIS UNIT (DEV MODE)
  # =========================================================
  
  message("Building simple analysisUnitMap")
  
  analysisUnitMap <- terra::rast(sim$PlanningGrid_250m)
  analysisUnitMap[] <- NA
  
  
  analysisUnitMap[landCoverAligned == 210] <- 1
  analysisUnitMap[landCoverAligned == 220] <- 2
  analysisUnitMap[landCoverAligned == 230] <- 3
  
  # =========================================================
  # 5) BASE MASK (forest + protected + age)
  # =========================================================
  
  ageValid <- terra::ifel(
    !is.na(standAgeAligned) & standAgeAligned > 0,
    1,
    0
  )
  
  baseMask <- terra::ifel(
    sim$forestBase == 1 &
      sim$protectedMask == 0 &
      ageValid == 1,
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
