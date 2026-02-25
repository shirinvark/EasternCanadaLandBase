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
      is.null(sf::st_crs(CPCAD_aligned))) {
    
    message("CPCAD empty or missing CRS → protectedAreaMask = 0")
    
    sim$protectedAreaMask <- terra::rast(sim$PlanningGrid_250m)
    sim$protectedAreaMask[] <- 0
    
  } else {
    
    # اگر CRS متفاوت باشد
    CPCAD_aligned <- sf::st_transform(
      CPCAD_aligned,
      terra::crs(sim$PlanningGrid_250m)
    )
     
    
    CPCAD_vect <- terra::vect(CPCAD_aligned)
    
    protTmp <- terra::rasterize(
      CPCAD_vect,
      sim$PlanningGrid_250m,
      field = 1,
      background = 0
    )
    
    sim$protectedAreaMask <- protTmp
    sim$protectedAreaMask[is.na(sim$protectedAreaMask)] <- 0
    sim$protectedAreaMask[sim$protectedAreaMask > 0] <- 1
  }
  
  
  # ========================================================
  # 3) forestCoverMask
  # ========================================================
  

  sim$forestCoverMask <- terra::ifel(
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
  
  # ---------------------------
  # ZONAL CALCULATIONS
  # ---------------------------
  
  eligibleArea_by_AU <- terra::zonal(
    isHarvestEligible,
    sim$analysisUnitMap,
    fun = "sum",
    na.rm = TRUE
  )
  
  harvestableArea_by_AU <- terra::zonal(
    sim$harvestableFraction,
    sim$analysisUnitMap,
    fun = "sum",
    na.rm = TRUE
  )
  
  # ---------------------------
  # Eligible area
  # ---------------------------
  
  if (is.null(eligibleArea_by_AU) ||
      nrow(eligibleArea_by_AU) == 0 ||
      !"sum" %in% names(eligibleArea_by_AU)) {
    
    eligibleArea_by_AU <- data.frame(
      analysisUnit = numeric(0),
      eligibleArea_ha = numeric(0)
    )
    
  } else {
    
    eligibleArea_by_AU$sum <- eligibleArea_by_AU$sum * cellArea_ha
    colnames(eligibleArea_by_AU) <- c("analysisUnit", "eligibleArea_ha")
  }
  
  # ---------------------------
  # Harvestable area
  # ---------------------------
  
  if (is.null(harvestableArea_by_AU) ||
      nrow(harvestableArea_by_AU) == 0 ||
      !"sum" %in% names(harvestableArea_by_AU)) {
    
    harvestableArea_by_AU <- data.frame(
      analysisUnit = numeric(0),
      harvestableArea_ha = numeric(0)
    )
    
  } else {
    
    harvestableArea_by_AU$sum <- harvestableArea_by_AU$sum * cellArea_ha
    colnames(harvestableArea_by_AU) <- c("analysisUnit", "harvestableArea_ha")
  }
  
  # ---------------------------
  # Merge
  # ---------------------------
  
  areaByAU <- merge(
    eligibleArea_by_AU,
    harvestableArea_by_AU,
    by = "analysisUnit",
    all = TRUE
  )
  # =========================================================
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
  
  # ---------------------------
  # SAFE handling
  # ---------------------------
  
  if (is.null(ageAreaTable) ||
      nrow(ageAreaTable) == 0 ||
      !"sum" %in% names(ageAreaTable)) {
    
    ageAreaTable <- data.frame(
      analysisUnit = numeric(0),
      ageClass = numeric(0),
      harvestableArea_ha = numeric(0)
    )
    
  } else {
    
    colnames(ageAreaTable) <- c("AU_AgeCode", "sum")
    
    ageAreaTable <- ageAreaTable[ageAreaTable$AU_AgeCode > 0, ]
    
    if (nrow(ageAreaTable) == 0) {
      
      ageAreaTable <- data.frame(
        analysisUnit = numeric(0),
        ageClass = numeric(0),
        harvestableArea_ha = numeric(0)
      )
      
    } else {
      
      ageAreaTable$harvestableArea_ha <- ageAreaTable$sum * cellArea_ha
      
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
