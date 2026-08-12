#------------------------------------------------------------------------------
# Landbase Rule Configuration
#
# Defines default Protected Area and Ownership landbase inclusion rules,
# with optional jurisdiction- and SYU-specific overrides.
#
# landbase inclusion class:
# 0 = excluded from landbase
# 1 = included but not harvestable
# 2 = included and harvestable
#------------------------------------------------------------------------------

landbaseRuleExceptions <- function() {
  
  list(
    
    #----------------------------------------------------------------------
    # Protected Areas
    #----------------------------------------------------------------------
    
    protectedArea = list(
      
      default = c(
        `0` = 2,  # not a protected area
        `1` = 1,  # IUCN Ia
        `2` = 1,  # IUCN Ib
        `3` = 1,  # IUCN II
        `4` = 1,  # IUCN III
        `5` = 1,  # IUCN IV
        `6` = 1,  # IUCN V
        `7` = 2,  # IUCN VI
        `8` = 1,  # Not Reported
        `9` = 1   # Not Applicable
      ),
      
      jurisdiction = list(),
      
      SYU = list()
    ),
    
    #----------------------------------------------------------------------
    # Ownership
    #----------------------------------------------------------------------
    
    ownership = list(
      
      default = c(
        `11`  = 2,
        `12`  = 2,
        `13`  = 2,
        `20`  = 1,
        `31`  = 0,
        `32`  = 0,
        `33`  = 1,
        `40`  = 1,
        `50`  = 0,
        `100` = 0
      ),
      
      jurisdiction = list(),
      
      SYU = list()
    )
  )
}
#------------------------------------------------------------------------------
# Build Landbase Class
#
# Assigns each PlanningGrid cell a landbase inclusion class.
#
# landbaseClass:
#   0 = excluded from landbase
#   1 = included but not harvestable
#   2 = included and harvestable
#
# Decision order:
#   1. Outside DMFL -> 0
#   2. Protected Area -> Protected Area rule
#   3. Otherwise -> Ownership rule
#------------------------------------------------------------------------------

buildLandbaseClass <- function(sim) {
  
  message("Building landbase inclusion classes...")
  
  rules <- landbaseRuleExceptions()
  
  #----------------------------------------------------------------------
  # Check required rasters
  #----------------------------------------------------------------------
  
  stopifnot(
    inherits(sim$PlanningGrid, "SpatRaster"),
    inherits(sim$DMFL, "SpatRaster"),
    inherits(sim$protectedArea, "SpatRaster"),
    inherits(sim$Ownership, "SpatRaster"),
    inherits(sim$jurisdiction, "SpatRaster"),
    inherits(sim$SYU, "SpatRaster"),
    inherits(sim$SYULookup, c("data.frame", "data.table"))
  )
  
  #----------------------------------------------------------------------
  # Apply default Protected Area rules
  #----------------------------------------------------------------------
  
  paRules <- rules$protectedArea$default
  
  paClass <- terra::subst(
    sim$protectedArea,
    from = as.numeric(names(paRules)),
    to = as.numeric(paRules)
  )
  
  #----------------------------------------------------------------------
  # Apply default Ownership rules
  #----------------------------------------------------------------------
  
  ownershipRules <- rules$ownership$default
  
  ownershipClass <- terra::subst(
    sim$Ownership,
    from = as.numeric(names(ownershipRules)),
    to = as.numeric(ownershipRules)
  )
  #----------------------------------------------------------------------
  # Apply jurisdiction-specific overrides
  #
  # Jurisdiction rules override the default PA or Ownership rules.
  # No changes are made when no jurisdiction-specific rules are defined.
  #----------------------------------------------------------------------
  
  jurLevels <- terra::levels(sim$jurisdiction)
  
  if (
    length(jurLevels) > 0 &&
    !is.null(jurLevels[[1]]) &&
    nrow(jurLevels[[1]]) > 0
  ) {
    
    jurisdictionLevels <- jurLevels[[1]]
    
    jurisdictionIDField <- names(jurisdictionLevels)[1]
    jurisdictionNameField <- names(jurisdictionLevels)[2]
    
    # Protected Area jurisdiction overrides
    paJurRules <- rules$protectedArea$jurisdiction
    
    if (length(paJurRules) > 0) {
      
      for (jurName in names(paJurRules)) {
        
        jurID <- jurisdictionLevels[
          jurisdictionLevels[[jurisdictionNameField]] == jurName,
          jurisdictionIDField
        ]
        
        if (length(jurID) == 0) {
          warning(
            "Protected Area rule references unknown jurisdiction: ",
            jurName
          )
          next
        }
        
        thisRule <- paJurRules[[jurName]]
        
        for (paCode in names(thisRule)) {
          
          paClass <- terra::ifel(
            sim$jurisdiction == jurID &
              sim$protectedArea == as.numeric(paCode),
            as.numeric(thisRule[[paCode]]),
            paClass
          )
        }
      }
    }
    
    # Ownership jurisdiction overrides
    ownershipJurRules <- rules$ownership$jurisdiction
    
    if (length(ownershipJurRules) > 0) {
      
      for (jurName in names(ownershipJurRules)) {
        
        jurID <- jurisdictionLevels[
          jurisdictionLevels[[jurisdictionNameField]] == jurName,
          jurisdictionIDField
        ]
        
        if (length(jurID) == 0) {
          warning(
            "Ownership rule references unknown jurisdiction: ",
            jurName
          )
          next
        }
        
        thisRule <- ownershipJurRules[[jurName]]
        
        for (ownershipCode in names(thisRule)) {
          
          ownershipClass <- terra::ifel(
            sim$jurisdiction == jurID &
              sim$Ownership == as.numeric(ownershipCode),
            as.numeric(thisRule[[ownershipCode]]),
            ownershipClass
          )
        }
      }
    }
  }
  #----------------------------------------------------------------------
  # Apply SYU-specific overrides
  #
  # SYU rules have higher priority than jurisdiction and default rules.
  #----------------------------------------------------------------------
  
  syuLookup <- sim$SYULookup
  
  stopifnot(
    all(c("SYU_ID", "SYU_NAME") %in% names(syuLookup))
  )
  
  #----------------------------------------------------------------------
  # Protected Area SYU overrides
  #----------------------------------------------------------------------
  
  paSYURules <- rules$protectedArea$SYU
  
  if (length(paSYURules) > 0) {
    
    for (syuName in names(paSYURules)) {
      
      syuID <- syuLookup$SYU_ID[
        syuLookup$SYU_NAME == syuName
      ]
      
      if (length(syuID) == 0) {
        warning(
          "Protected Area rule references unknown SYU: ",
          syuName
        )
        next
      }
      
      thisRule <- paSYURules[[syuName]]
      
      for (paCode in names(thisRule)) {
        
        paClass <- terra::ifel(
          sim$SYU == syuID &
            sim$protectedArea == as.numeric(paCode),
          as.numeric(thisRule[[paCode]]),
          paClass
        )
      }
    }
  }
  
  #----------------------------------------------------------------------
  # Ownership SYU overrides
  #----------------------------------------------------------------------
  
  ownershipSYURules <- rules$ownership$SYU
  
  if (length(ownershipSYURules) > 0) {
    
    for (syuName in names(ownershipSYURules)) {
      
      syuID <- syuLookup$SYU_ID[
        syuLookup$SYU_NAME == syuName
      ]
      
      if (length(syuID) == 0) {
        warning(
          "Ownership rule references unknown SYU: ",
          syuName
        )
        next
      }
      
      thisRule <- ownershipSYURules[[syuName]]
      
      for (ownershipCode in names(thisRule)) {
        
        ownershipClass <- terra::ifel(
          sim$SYU == syuID &
            sim$Ownership == as.numeric(ownershipCode),
          as.numeric(thisRule[[ownershipCode]]),
          ownershipClass
        )
      }
    }
  }
  #----------------------------------------------------------------------
  # Build landbase class
  #
  # Protected Area rules take precedence over Ownership rules.
  #----------------------------------------------------------------------
  
  if (isTRUE(P(sim)$.useLandbaseRules)) {
    
    message("Applying Protected Area and Ownership landbase rules.")
    
    sim$landbaseClass <- terra::ifel(
      sim$DMFL == 0,
      0,
      terra::ifel(
        sim$protectedArea > 0,
        paClass,
        ownershipClass
      )
    )
    
  } else {
    
    message("Protected Area and Ownership landbase rules disabled.")
    
    sim$landbaseClass <- terra::ifel(
      sim$DMFL == 0,
      0,
      2
    )
  }
  
  names(sim$landbaseClass) <- "landbaseClass"
  message("✔ Landbase inclusion classes created.")
  
  sim
}
#------------------------------------------------------------------------------
# Build Landbase Raster Stack
#
# Combines prepared spatial attributes into a common multi-layer SpatRaster
# aligned to the PlanningGrid.
#------------------------------------------------------------------------------

buildLandbaseRaster <- function(sim) {
  
  message("Building Landbase raster stack...")
  
  layers <- list(
    SYU = sim$SYU,
    jurisdiction = sim$jurisdiction,
    bcr = sim$bcr,
    yieldCurveFamily = sim$yieldCurveFamily,
    ownership = sim$Ownership,
    protectedArea = sim$protectedArea,
    DMFL = sim$DMFL,
    landbaseClass = sim$landbaseClass,
    forestCoverMask = sim$forestCoverMask,
    isHarvestEligible = sim$isHarvestEligible,
    riparianFraction = sim$Riparian$riparianFraction,
    harvestableFraction = sim$harvestableFraction
  )
  
  # Check geometry against PlanningGrid
  geomOK <- vapply(
    layers,
    function(x) {
      terra::compareGeom(
        sim$PlanningGrid,
        x,
        stopOnError = FALSE
      )
    },
    logical(1)
  )
  
  if (!all(geomOK)) {
    stop(
      "Landbase raster geometry mismatch: ",
      paste(names(geomOK)[!geomOK], collapse = ", ")
    )
  }
  
  sim$landbaseRaster <- do.call(
    c,
    layers
  )
  
  names(sim$landbaseRaster) <- names(layers)
  
  message(
    "✔ Landbase raster created with ",
    terra::nlyr(sim$landbaseRaster),
    " layers."
  )
  
  sim
}
#------------------------------------------------------------------------------
# Build Forest Cover Mask
#
# Reclassifies the original LandCover raster into forest / non-forest.
#
# Forest LandCover classes:
#   210 = Coniferous forest
#   220 = Broadleaf forest
#   230 = Mixedwood forest
#
# Output:
#   1 = forest
#   0 = non-forest
#------------------------------------------------------------------------------

buildForestCoverMask <- function(sim) {
  
  message("Building forest cover mask...")
  
  stopifnot(
    inherits(sim$LandCover, "SpatRaster"),
    inherits(sim$PlanningGrid, "SpatRaster")
  )
  
  forestClasses <- c(
    210,
    220,
    230
  )
  
  sim$forestCoverMask <- terra::ifel(
    !is.na(sim$LandCover) &
      sim$LandCover %in% forestClasses,
    1,
    0
  )
  
  names(sim$forestCoverMask) <- "forestCoverMask"
  
  message("✔ Forest cover mask created.")
  
  sim
}
#------------------------------------------------------------------------------
# Build Preliminary Harvest Eligibility
#
# Identifies forest cells that are potentially available for harvest
# based on the landbase inclusion class.
#
# A cell is preliminarily eligible when:
#   landbaseClass == 2
#   forestCoverMask == 1
#
# Final harvestability may be further constrained downstream.
#------------------------------------------------------------------------------

buildHarvestEligibility <- function(sim) {
  
  message("Building preliminary harvest eligibility mask...")
  
  stopifnot(
    inherits(sim$landbaseClass, "SpatRaster"),
    inherits(sim$forestCoverMask, "SpatRaster")
  )
  
  sim$isHarvestEligible <- terra::ifel(
    sim$landbaseClass == 2 &
      sim$forestCoverMask == 1,
    1,
    0
  )
  
  names(sim$isHarvestEligible) <- "isHarvestEligible"
  
  message("✔ Preliminary harvest eligibility mask created.")
  
  sim
}
#------------------------------------------------------------------------------
# Apply Riparian Reduction
#
# Reduces the preliminarily harvest-eligible area according to the
# riparian fraction supplied by the RiparianBuffers module.
#------------------------------------------------------------------------------

applyRiparianReduction <- function(sim) {
  
  message("Applying riparian landbase rules...")
  
  stopifnot(
    inherits(sim$isHarvestEligible, "SpatRaster"),
    is.list(sim$Riparian),
    inherits(sim$Riparian$riparianFraction, "SpatRaster")
  )
  
  riparianFraction <- sim$Riparian$riparianFraction
  
  if (isTRUE(P(sim)$.includeRiparianBuffers)) {
    
    message("Applying riparian reduction.")
    
    sim$harvestableFraction <-
      sim$isHarvestEligible * (1 - riparianFraction)
    
  } else {
    
    message("Riparian buffers disabled.")
    
    sim$harvestableFraction <- sim$isHarvestEligible
  }
  
  names(sim$harvestableFraction) <- "harvestableFraction"
  
  sim
}
  #------------------------------------------------------------------------------
  # Validate Landbase Outputs
  #
  # Performs consistency checks on the final landbase outputs.
  #------------------------------------------------------------------------------
  
  validateLandbase <- function(sim) {
    
    message("Validating landbase outputs...")
    
    #----------------------------------------------------------------------
    # Required outputs
    #----------------------------------------------------------------------
    
    stopifnot(
      inherits(sim$PlanningGrid, "SpatRaster"),
      inherits(sim$landbaseClass, "SpatRaster"),
      inherits(sim$forestCoverMask, "SpatRaster"),
      inherits(sim$isHarvestEligible, "SpatRaster"),
      inherits(sim$harvestableFraction, "SpatRaster"),
      inherits(sim$landbaseRaster, "SpatRaster")
    )
    
    #----------------------------------------------------------------------
    # Geometry
    #----------------------------------------------------------------------
    
    outputs <- list(
      landbaseClass = sim$landbaseClass,
      forestCoverMask = sim$forestCoverMask,
      isHarvestEligible = sim$isHarvestEligible,
      harvestableFraction = sim$harvestableFraction
    )
    
    geomOK <- vapply(
      outputs,
      function(x) {
        terra::compareGeom(
          sim$PlanningGrid,
          x,
          stopOnError = FALSE
        )
      },
      logical(1)
    )
    
    if (!all(geomOK)) {
      stop(
        "Landbase output geometry mismatch: ",
        paste(names(geomOK)[!geomOK], collapse = ", ")
      )
    }
    
    #----------------------------------------------------------------------
    # Allowed values
    #----------------------------------------------------------------------
    
    landbaseValues <- unique(
      na.omit(terra::values(sim$landbaseClass))
    )
    
    if (!all(landbaseValues %in% c(0, 1, 2))) {
      stop("landbaseClass contains values outside 0, 1, 2.")
    }
    
    forestValues <- unique(
      na.omit(terra::values(sim$forestCoverMask))
    )
    
    if (!all(forestValues %in% c(0, 1))) {
      stop("forestCoverMask contains values outside 0 and 1.")
    }
    
    harvestValues <- unique(
      na.omit(terra::values(sim$isHarvestEligible))
    )
    
    if (!all(harvestValues %in% c(0, 1))) {
      stop("isHarvestEligible contains values outside 0 and 1.")
    }
    
    #----------------------------------------------------------------------
    # Harvestable fraction must be between 0 and 1
    #----------------------------------------------------------------------
    
    harvestRange <- terra::global(
      sim$harvestableFraction,
      fun = c("min", "max"),
      na.rm = TRUE
    )
    
    harvestMin <- harvestRange[1, "min"]
    harvestMax <- harvestRange[1, "max"]
    
    if (
      harvestMin < 0 ||
      harvestMax > 1
    ) {
      stop(
        "harvestableFraction contains values outside [0, 1]. ",
        "Range: ", harvestMin, " to ", harvestMax
      )
    }
    #----------------------------------------------------------------------
    # Harvest eligibility consistency
    #----------------------------------------------------------------------
    
    invalidHarvest <- terra::global(
      terra::ifel(
        sim$isHarvestEligible == 1 &
          (
            sim$landbaseClass != 2 |
              sim$forestCoverMask != 1
          ),
        1,
        0
      ),
      "sum",
      na.rm = TRUE
    )[1, 1]
    
    if (invalidHarvest > 0) {
      stop(
        "Found ",
        invalidHarvest,
        " harvest-eligible cells inconsistent with landbase/forest rules."
      )
    }
    
    message("✔ Landbase validation passed.")
    
    sim
  }