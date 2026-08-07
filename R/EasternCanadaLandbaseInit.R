Init <- function(sim) {
  sim <- .inputObjects(sim)
  
  checkObject(sim, "PlanningGrid", "SpatRaster")
  checkObject(sim, "LandCover", "SpatRaster")
  checkObject(sim, "standAge", "SpatRaster")
  checkObject(sim, "Riparian", "list")
  checkObject(sim, "protectedArea", "SpatRaster")
  checkObject(sim, "jurisdiction", "SpatRaster")
  checkObject(sim, "bcr", "SpatRaster")
  checkObject(sim, "yieldCurveFamily", "SpatRaster")
  checkObject(sim, "Ownership", "SpatRaster")
  landCoverAligned <- sim$LandCover
  
  standAgeAligned <- sim$standAge
  
  riparianAligned <- sim$Riparian$riparianFraction
  sim <- buildLandbaseRaster(sim)
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
  
  # =========================================================
  # 2) Protected Areas
  # =========================================================
  
  message("Using protectedArea from EasternCanadaDataPrep")
  
  checkObject(
    sim,
    "protectedArea",
    "SpatRaster"
  )
  
  sim$protectedAreaMask <- terra::ifel(
    sim$protectedArea > 0,
    1,
    0
  )
  
  print(
    terra::compareGeom(
      sim$PlanningGrid,
      sim$protectedAreaMask,
      stopOnError = FALSE
    )
  )
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
  print(
    terra::compareGeom(
      isHarvestEligible,
      riparianAligned,
      stopOnError = FALSE
    )
  )
  
  print(terra::ext(isHarvestEligible))
  print(terra::ext(riparianAligned))
  
  print(dim(isHarvestEligible))
  print(dim(riparianAligned))
  
  print(terra::res(isHarvestEligible))
  print(terra::res(riparianAligned))
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
