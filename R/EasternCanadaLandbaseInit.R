Init <- function(sim) {
  sim <- .inputObjects(sim)
  
  checkObject(sim, "PlanningGrid", "SpatRaster")
  checkObject(sim, "LandCover", "SpatRaster")
  checkObject(sim, "standAge", "SpatRaster")
  checkObject(sim, "Riparian", "list")
  checkObject(sim, "LegalConstraints", "list")
  
  landCoverAligned <- terra::resample(
    sim$LandCover,
    sim$PlanningGrid,
    method = "near"
  )
  
  standAgeAligned <- terra::resample(
    sim$standAge,
    sim$PlanningGrid,
    method = "near"
  )
  
  riparianAligned <- sim$Riparian$riparianFraction
  print(
    terra::compareGeom(
      sim$PlanningGrid,
      landCoverAligned,
      stopOnError = FALSE
    )
  )
  
  print(
    terra::compareGeom(
      sim$PlanningGrid,
      standAgeAligned,
      stopOnError = FALSE
    )
  )
  
  print(terra::ext(sim$PlanningGrid))
  print(terra::ext(landCoverAligned))
  print(terra::ext(standAgeAligned))
  
  message("===== SIZE CHECK =====")
  
  print(
    terra::ncell(sim$PlanningGrid)
  )
  
  print(
    terra::ncell(landCoverAligned)
  )
  
  print(
    terra::ncell(standAgeAligned)
  )
  
  message("======================")
  # 2) protectedAreaMask
  # =========================================================
  
  message("Preparing protectedAreaMask")
  
  if (!is.null(sim$LegalConstraints) &&
      !is.null(sim$LegalConstraints$CPCAD_Raster) &&
      inherits(sim$LegalConstraints$CPCAD_Raster, "SpatRaster")) {
    
    sim$protectedAreaMask <- terra::resample(
      sim$LegalConstraints$CPCAD_Raster,
      sim$PlanningGrid,
      method = "near"
    )
    
    print(
      terra::compareGeom(
        sim$PlanningGrid,
        sim$protectedAreaMask,
        stopOnError = FALSE
      )
    )
    
  } else {
    
    message("No valid CPCAD raster found → protectedAreaMask = 0")
    
    sim$protectedAreaMask <- terra::rast(sim$PlanningGrid)
    sim$protectedAreaMask[] <- 0
  }
  # ======================================================
  # 3) forestCoverMask
  # ========================================================
  
  sim$forestCoverMask <- terra::ifel(
    !is.na(landCoverAligned) &
      (
        landCoverAligned == 210 |
          landCoverAligned == 220 |
          landCoverAligned == 230
      ),
    1,
    0
  )
  
  # =========================================================
  # 5) isHarvestEligible (forest + protected + age)
  # =========================================================
  
  ageValid <- terra::ifel(
    !is.na(standAgeAligned) & standAgeAligned > 0,
    1,
    0
  )
  print(
    terra::compareGeom(
      sim$forestCoverMask,
      sim$protectedAreaMask,
      stopOnError = FALSE
    )
  )
  
  print(
    terra::compareGeom(
      sim$forestCoverMask,
      ageValid,
      stopOnError = FALSE
    )
  )
  
  print(terra::ext(sim$forestCoverMask))
  print(terra::ext(sim$protectedAreaMask))
  print(terra::ext(ageValid))
  isHarvestEligible <- terra::ifel(
    sim$forestCoverMask == 1 &
      sim$protectedAreaMask == 0 &
      ageValid == 1,
    1,
    0
  )
  
  
  
  # =========================================================
  # 6) APPLY RIPARIAN REDUCTION
  # =========================================================
  
  if (!inherits(riparianAligned, "SpatRaster")) {
    stop("Riparian$riparianFraction must be a SpatRaster")
  }
  
  message("Applying riparian reduction")
  
  sim$harvestableFraction <- isHarvestEligible * (1 - riparianAligned)
  
  
  # =========================================================
  # 8) FINAL LANDBASE
  # =========================================================
  sim$isHarvestEligible <- isHarvestEligible
  
  sim$Landbase <- list(
    
    baseData = list(
      planningRaster = sim$PlanningGrid,
      landcover      = landCoverAligned,
      standAge       = standAgeAligned
    ),
    
    masks = list(
      forestCoverMask     = sim$forestCoverMask,
      protectedAreaMask   = sim$protectedAreaMask,
      isHarvestEligible   = sim$isHarvestEligible
    ),
    
    fractional = list(
      riparianFraction    = riparianAligned,
      harvestableFraction = sim$harvestableFraction
    )
  )
  
  invisible(sim)
}
