Init <- function(sim) {
  
  checkObject(sim, "PlanningGrid_250m", "SpatRaster")
  checkObject(sim, "LandCover_250m", "SpatRaster")
  checkObject(sim, "standAge_250m", "SpatRaster")
  checkObject(sim, "CPCAD", "sf")
  checkObject(sim, "Riparian", "list")
  
  landCoverAligned  <- sim$LandCover_250m
  standAgeAligned   <- sim$standAge_250m
  riparianAligned   <- sim$Riparian$riparianFraction  
  # 2) protectedAreaMask
  # =========================================================
  
  message("Creating protectedAreaMask")
  
  CPCAD_aligned <- sim$CPCAD
  
  # اگر CPCAD خالی باشد یا CRS نداشته باشد
  if (is.null(CPCAD_aligned) ||
      nrow(CPCAD_aligned) == 0 ||
      is.na(sf::st_crs(CPCAD_aligned))) {
    
    message("CPCAD empty or missing CRS → protectedAreaMask = 0")
    
    sim$protectedAreaMask <- terra::rast(sim$PlanningGrid_250m)
    sim$protectedAreaMask[] <- 0
    
  } else {
    
    # اگر CRS متفاوت باشد
    if (!terra::same.crs(terra::vect(CPCAD_aligned),
                         sim$PlanningGrid_250m)) {
      
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
    
    sim$protectedAreaMask <- protTmp
    sim$protectedAreaMask[is.na(sim$protectedAreaMask)] <- 0
    sim$protectedAreaMask[sim$protectedAreaMask > 0] <- 1
  }
  
  
  # ========================================================
  # 3) forestCoverMask
  # ========================================================
  
  forestClasses <- c(210, 220, 230)
  
  sim$forestCoverMask <- terra::classify(
    landCoverAligned,
    rcl = matrix(
      c(210,210,1,
        220,220,1,
        230,230,1),
      ncol = 3,
      byrow = TRUE
    ),
    others = 0
  )
  
  # =========================================================
  # 4) BUILD SIMPLE ANALYSIS UNIT (DEV MODE)
  # =========================================================
  
  message("Building simple analysisUnitMap")
  
  analysisUnitMap <- terra::ifel(
    sim$forestCoverMask == 1,
    terra::classify(
      landCoverAligned,
      rcl = matrix(
        c(210, 1,
          220, 2,
          230, 3),
        ncol = 2,
        byrow = TRUE
      )
    ),
    NA
  )
  # =========================================================
  # 5) isHarvestEligible (forest + protected + age)
  # =========================================================
  
  ageValid <- terra::ifel(
    !is.na(standAgeAligned) & standAgeAligned > 0,
    1,
    0
  )
  
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
  
  riparianAligned <- terra::clamp(riparianAligned, 0, 1)
  
  message("Applying riparian reduction")
  
  sim$harvestableFraction <- isHarvestEligible * (1 - riparianAligned)
  
  # =========================================================
  # 7) MASK ANALYSIS UNIT
  # =========================================================
  
  message("Masking analysis units by harvestableFraction")  
  analysisUnitMasked <- terra::ifel(
    sim$harvestableFraction > 0,
    analysisUnitMap,
    NA
  )
  sim$analysisUnitMap <- analysisUnitMasked
  # =========================================================
  # AREA PER ANALYSIS UNIT (AAC-ready)
  # =========================================================
  
  resXY <- terra::res(sim$PlanningGrid_250m)
  cellArea_ha <- (resXY[1] * resXY[2]) / 10000
  
  # Eligible area per AU
  eligibleArea_by_AU <- terra::zonal(
    isHarvestEligible,
    sim$analysisUnitMap,
    fun = "sum",
    na.rm = TRUE
  )
  
  # Harvestable area per AU (fractional)
  harvestableArea_by_AU <- terra::zonal(
    sim$harvestableFraction,
    sim$analysisUnitMap,
    fun = "sum",
    na.rm = TRUE
  )
  
  # Convert to hectares
  eligibleArea_by_AU$sum <- eligibleArea_by_AU$sum * cellArea_ha
  harvestableArea_by_AU$sum <- harvestableArea_by_AU$sum * cellArea_ha
  
  # Rename columns cleanly
  colnames(eligibleArea_by_AU) <- c("analysisUnit", "eligibleArea_ha")
  colnames(harvestableArea_by_AU) <- c("analysisUnit", "harvestableArea_ha")
  
  # Merge
  areaByAU <- merge(
    eligibleArea_by_AU,
    harvestableArea_by_AU,
    by = "analysisUnit",
    all = TRUE
  )
  # =========================================================
  # AGE CLASS PER AU (AAC-ready)
  # =========================================================
  
  ageBreaks <- c(0, 20, 40, 60, 80, 100, 150, Inf)
  
  ageClassRaster <- terra::classify(
    standAgeAligned,
    rcl = cbind(
      ageBreaks[-length(ageBreaks)],
      ageBreaks[-1],
      seq_len(length(ageBreaks) - 1)
    )
  )
  
  au_age_combo <- terra::ifel(
    sim$analysisUnitMap > 0,
    sim$analysisUnitMap * 1000 + ageClassRaster,
    NA
  )
  
  ageAreaTable <- terra::zonal(
    sim$harvestableFraction,
    au_age_combo,
    fun = "sum",
    na.rm = TRUE
  )
  
  if (is.null(ageAreaTable) || nrow(ageAreaTable) == 0) {
    
    ageAreaTable <- data.frame(
      analysisUnit = numeric(0),
      ageClass = numeric(0),
      harvestableArea_ha = numeric(0)
    )
    
  } else {
    
    ageAreaTable <- ageAreaTable[ageAreaTable$zone > 0, ]
    
    if (nrow(ageAreaTable) == 0) {
      
      ageAreaTable <- data.frame(
        analysisUnit = numeric(0),
        ageClass = numeric(0),
        harvestableArea_ha = numeric(0)
      )
      
    } else {
      
      ageAreaTable$sum <- ageAreaTable$sum * cellArea_ha
      colnames(ageAreaTable) <- c("AU_AgeCode", "harvestableArea_ha")
      
      ageAreaTable$analysisUnit <- ageAreaTable$AU_AgeCode %/% 1000
      ageAreaTable$ageClass     <- ageAreaTable$AU_AgeCode %% 1000
      
      ageAreaTable <- ageAreaTable[, c("analysisUnit", "ageClass", "harvestableArea_ha")]
    }
  }
  #==========================================================
  # AREA METRICS (AAC-ready)
  # =========================================================
  
   
  totalEligibleArea_ha <- terra::global(
    isHarvestEligible,
    "sum",
    na.rm = TRUE
  )[1,1] * cellArea_ha
  
  totalHarvestableArea_ha <- terra::global(
    sim$harvestableFraction,
    "sum",
    na.rm = TRUE
  )[1,1] * cellArea_ha
  # =========================================================
  # 8) FINAL LANDBASE
  # =========================================================
  sim$isHarvestEligible <- isHarvestEligible
  
  sim$Landbase <- list(
    
    baseData = list(
      planningRaster = sim$PlanningGrid_250m,
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
    ),
    
    planning = list(
      analysisUnitMap = sim$analysisUnitMap,
      areaByAU        = areaByAU,
      ageAreaByAU     = ageAreaTable
    ),
    
    metrics = list(
      totalEligibleArea_ha    = totalEligibleArea_ha,
      totalHarvestableArea_ha = totalHarvestableArea_ha
    )
  )
    
  invisible(sim)
}
